-- |
-- Module      : Interpreters.Common.Metatheory
-- Description : Tests of preservation and progress on generated terms
--
-- PAL can test two classic properties of the user's type system:
--
-- * __Preservation__: if @e : T@ and @e@ reduces in one step to @e'@, then
--   @e'@ also has the type @T@.
-- * __Progress__: a well-typed term that does not reduce is a value (see
--   the @value@ statement).
--
-- A test generates random closed terms from the constructors of the
-- @expr@ declarations and the rules, keeps the well-typed ones, and checks
-- the property on each of them. The generator uses a fixed seed, so a test
-- gives the same result every time. The smallest counterexample is
-- reported.
module Interpreters.Common.Metatheory
  ( runTest,
    generateTerms,
    reducts,
    isValue,
    termSize,
  )
where

import Control.Monad (foldM, replicateM)
import Data.Bits (shiftR, xor)
import Data.Either (isLeft)
import Data.List (elemIndex, minimumBy, nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Word (Word64)
import Interpreters.Common.Actions
  ( InferState (..),
    binderNames,
    binders,
    canonical,
    checkDetailed,
    exprVars,
    freeVars,
    initialState,
    matchConclusion,
    ruleTypeVars,
    runInfer,
    stepRoot,
    substituteExpr,
    typeOf,
    unify,
  )
import qualified Types

--------------------------------------------------------------------------------

-- | Running a test

--------------------------------------------------------------------------------

-- | Run a test on terms generated from the context.
runTest :: Types.Ctx -> Types.TestSpec -> Types.TestResult
runTest ctx spec@(Types.TestSpec property n) =
  Types.TestResult spec (length typed) (tidy <$> smallest (concatMap check typed))
  where
    typed = generateTerms ctx n
    check (e, t) = case property of
      Types.Preservation -> notPreserved ctx e t
      Types.Progress -> [Types.Stuck e t | null (reducts ctx e), not (isValue ctx e)]
    smallest = \case
      [] -> Nothing
      cs -> Just (minimumBy (comparing (termSize . subject)) cs)
    subject = \case
      Types.NotPreserved e _ _ _ -> e
      Types.Stuck e _ -> e
    -- Give the bound variables short names: the generated names are
    -- unique, so renaming every occurrence is safe.
    tidy = \case
      Types.NotPreserved e t e' r -> Types.NotPreserved (rename e) t (rename e') r
      Types.Stuck e t -> Types.Stuck (rename e) t
    rename e = substituteExpr (M.fromList (zip (nub (binderNames ctx e)) (fmap Types.Var shortNames))) e
    shortNames = [c : suffix | suffix <- "" : fmap show [1 :: Int ..], c <- "xyzwvu"]

-- | The one-step reducts of @e : T@ that do not have the type @T@. Each
--   reduct is checked against @T@ (with the free variables of @T@ rigid),
--   so a reduct may also be a term that can only be checked. A
--   counterexample shows the reduct's own type, or why it has none.
notPreserved :: Types.Ctx -> Types.Expr -> Types.Type -> [Types.Counterexample]
notPreserved ctx e t =
  [ Types.NotPreserved e t e' (either (Left . Types.failure'err . NE.head) (Right . Types.deriv'type) (typeOf ctx e'))
    | e' <- reducts ctx e,
      isLeft (checkDetailed ctx e' t)
  ]

-- | All the terms that a term reduces to in one step: one reduction, at the
--   root or inside.
reducts :: Types.Ctx -> Types.Term -> [Types.Term]
reducts ctx t = root <> inside
  where
    root = case stepRoot ctx t of
      Right (Just t') -> [t']
      _ -> []
    inside = case t of
      Types.Con b c ts ->
        [ Types.Con b c (before <> [a'] <> drop 1 after)
          | (i, a) <- zip [0 ..] ts,
            let (before, after) = splitAt i ts,
            a' <- reducts ctx a
        ]
      _ -> []

-- | Does a term match one of the @value@ patterns?
isValue :: Types.Ctx -> Types.Term -> Bool
isValue ctx e = any (\p -> either (const False) (const True) (matchConclusion "value" p e)) (Types.ctx'values ctx)

-- | The number of constructors and variables in a term.
termSize :: Types.Term -> Int
termSize = \case
  Types.Var _ -> 1
  Types.Con _ _ ts -> 1 + sum (fmap termSize ts)
  Types.Subst body x e -> termSize body + termSize x + termSize e

--------------------------------------------------------------------------------

-- | Generating terms

--------------------------------------------------------------------------------

-- | Generate the given number of random closed terms, and keep the ones
--   that have a type, without repeats. Each comes with its type.
--
--   The well-typed terms go into a pool, and later candidates take most of
--   their arguments from the pool. Thus a candidate is much more likely to
--   have a type than a term that is random all the way down.
generateTerms :: Types.Ctx -> Int -> [(Types.Term, Types.Type)]
generateTerms ctx n = reverse (fst (runGen (go n M.empty []) (Seed 0x9E3779B97F4A7C15 0)))
  where
    sig = signature ctx
    go 0 _ found = pure found
    -- Each candidate is inferred once: a repeat (up to the names of bound
    -- variables) or a term that is too large is skipped first.
    go k tried found = do
      e <- candidate found
      let key = show (canonical ctx e)
      if M.member key tried || termSize e > maxSize
        then go (k - 1) tried found
        else case typeOf ctx e of
          Right d -> go (k - 1) (M.insert key () tried) ((e, Types.deriv'type d) : found)
          Left _ -> go (k - 1) (M.insert key () tried) found
    -- One candidate in four is random all the way down, to find new shapes.
    candidate pool = do
      r <- choose 4
      if r == 0 || null pool then choose 4 >>= \d -> term ctx sig (1 + d) [] else fromPool pool
    -- A rule applied to arguments that are chosen by type: the type of
    -- each argument must unify with the type of its premise, and the
    -- solutions are shared, so that e.g. the argument of App gets the
    -- type that the function expects.
    fromPool pool =
      pick [(r, c, ps) | r@(Types.TypingRule _ _ (Types.ECon c ps@(_ : _), _)) <- Types.ctx'rules ctx, all isVar ps] >>= \case
        Nothing -> term ctx sig 1 []
        Just (rule@(Types.TypingRule _ premises _), c, ps) -> do
          let k = length ps
              bs = binders ctx c k
              binderPositions = nub (fmap fst bs)
          names <- traverse (const freshVar) binderPositions
          tag <- freshVar
          let named = zip binderPositions names
              boundIn j = [v | (i, v) <- named, (i, j) `elem` bs]
              meta v = Types.Var ('\'' : tag <> v)
              typeVars = M.fromList [(v, meta v) | v <- ruleTypeVars rule]
              premiseType j = listToMaybe [t | Types.Premise [] (p, t) <- premises, Just j == elemIndex p ps]
              -- The types in the rule: of its premises, hypotheses and conclusion.
              ruleTypes = snd (Types.typingRule'ruleConclusion rule) : concat [t : fmap Types.hyp'type hs | Types.Premise hs (_, t) <- premises]
              -- The argument j: a name, a term chosen by type, or a
              -- random one.
              fill (args, st) j = case (lookup j named, premiseType j) of
                (Just v, _) -> pure (Types.Var v : args, st)
                (Nothing, Just t) | null (boundIn j) -> do
                  let chosen = M.fromList (zip [v | Types.Var v <- ps] (reverse args))
                      patterns = M.fromList [(v, M.findWithDefault (meta ("p" <> v)) v chosen) | Types.Var v <- ps]
                      expected = substituteExpr (patterns <> typeVars) t
                      fits (e, te) = case runInfer (unify ctx expected (withMetas (tag <> "a" <> show j) te)) st of
                        (st', Right ()) -> Just (e, st')
                        _ -> Nothing
                  sample <- catMaybes <$> replicateM 12 (pick pool)
                  case mapMaybe fits sample of
                    (e, st') : _ -> pure (e : args, st')
                    [] -> (\e -> (e : args, st)) <$> argument (fmap fst pool) boundIn j
                -- An argument that the rule uses as a type, and that no premise
                -- checks (like t in Ann(e, t) => t): a type from the pool.
                (Nothing, Nothing)
                  | Types.Var v <- ps !! j,
                    v `elem` concatMap exprVars ruleTypes,
                    not (null pool) ->
                      (\ty -> (fromMaybe (Types.Var v) ty : args, st)) <$> pick (fmap snd pool)
                _ -> (\e -> (e : args, st)) <$> argument (fmap fst pool) boundIn j
          (args, _) <- foldM fill ([], initialState {inferSupply = 1}) [0 .. k - 1]
          pure (Types.ECon c (reverse args))
    -- An argument that is not chosen by type.
    argument pool boundIn j = do
      r <- choose 3
      case (r, boundIn j) of
        (0, scope@(_ : _)) -> maybe (term ctx sig 2 scope) pure =<< pick (fmap Types.Var scope)
        (1, scope) -> term ctx sig 2 scope
        _ -> maybe (term ctx sig 1 (boundIn j)) pure =<< pick pool
    isVar = \case
      Types.Var _ -> True
      _ -> False
    -- The free variables of a type become metavariables with the prefix.
    withMetas prefix ty = substituteExpr (M.fromList [(v, Types.Var ('\'' : prefix <> v)) | v <- freeVars ctx ty]) ty

-- | The largest term that a test keeps, so that inference stays fast.
maxSize :: Int
maxSize = 40

-- | The constructors of terms: each @expr@ (no arguments) and the
--   conclusion of each rule, with its number of arguments.
signature :: Types.Ctx -> [(String, Int)]
signature ctx =
  nub $
    [(n, 0) | Types.ExprDecl n _ <- reverse (Types.ctx'exprs ctx)]
      <> [(n, length ps) | Types.TypingRule _ _ (Types.ECon n ps, _) <- reverse (Types.ctx'rules ctx)]

-- | A random term of at most the given depth. The variables in scope are
--   the names of the binders around it.
term :: Types.Ctx -> [(String, Int)] -> Int -> [String] -> Gen Types.Term
term ctx sig depth scope
  | depth <= 0 || null nodes = leaf
  | null leaves = node
  | otherwise = do
      r <- choose 3
      if r == 0 then leaf else node
  where
    leaves = [Types.ECon c [] | (c, 0) <- sig] <> fmap Types.Var scope
    nodes = [(c, k) | (c, k) <- sig, k > 0]
    leaf = maybe node pure =<< pick leaves
    node =
      pick nodes >>= \case
        Nothing -> pure (Types.ECon "?" [])
        Just (c, k) -> do
          let bs = binders ctx c k
              binderPositions = nub (fmap fst bs)
          names <- traverse (const freshVar) binderPositions
          let named = zip binderPositions names
              boundIn j = [v | (i, v) <- named, (i, j) `elem` bs]
          args <-
            traverse
              (\j -> maybe (term ctx sig (depth - 1) (boundIn j <> scope)) (pure . Types.Var) (lookup j named))
              [0 .. k - 1]
          pure (Types.ECon c args)

--------------------------------------------------------------------------------

-- | A small random generator with a fixed seed

--------------------------------------------------------------------------------

-- | The state of the generator: a random state, and a counter for the names
--   of bound variables.
data Seed = Seed Word64 Int

newtype Gen a = Gen {runGen :: Seed -> (a, Seed)}

instance Functor Gen where
  fmap f (Gen g) = Gen $ \s -> let (a, s') = g s in (f a, s')

instance Applicative Gen where
  pure a = Gen (a,)
  Gen f <*> Gen g = Gen $ \s -> let (h, s') = f s; (a, s'') = g s' in (h a, s'')

instance Monad Gen where
  Gen g >>= k = Gen $ \s -> let (a, s') = g s in runGen (k a) s'

-- | A random number from 0 to @n - 1@ (SplitMix64).
choose :: Int -> Gen Int
choose n = Gen $ \(Seed w c) ->
  let w' = w + 0x9E3779B97F4A7C15
      z1 = (w' `xor` (w' `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
   in (fromIntegral (z3 `mod` fromIntegral (max 1 n)), Seed w' c)

-- | A random element of a list, if it is not empty.
pick :: [a] -> Gen (Maybe a)
pick [] = pure Nothing
pick xs = Just . (xs !!) <$> choose (length xs)

-- | A new name for a bound variable: @x0@, @x1@, …
freshVar :: Gen String
freshVar = Gen $ \(Seed w c) -> ("x" <> show c, Seed w (c + 1))
