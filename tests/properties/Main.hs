-- |
-- Module      : Main
-- Description : Property-based tests of the PAL engine
--
-- QuickCheck properties of the parts of the engine that the golden tests
-- (@tests/run.sh@) cannot cover well: substitution, α-equivalence,
-- unification, reduction, the parser, and inference. The terms come from a
-- small λΠ system, so the binders of @Pi@ and @Lam@ are known.
module Main (main) where

import Data.List (nub, sort, (\\))
import qualified Data.Map as M
import Interpreters.Common.Actions
  ( InferState (..),
    binders,
    canonical,
    checkDetailed,
    equivalent,
    forceTerm,
    freeVars,
    inferDetailed,
    initialState,
    normalForm,
    runInfer,
    substVar,
    unify,
    whnf,
  )
import qualified Interpreters.Common.Actions as Actions
import Interpreters.Common.Expectations (checkExpectation)
import Parser.Parser (pExprApp)
import Program (parsePalSourceLocated, runSteps)
import System.Exit (exitFailure)
import Test.QuickCheck
import Text.Megaparsec (eof, parse)
import Types (Brackets (..), Ctx, Derivation (..), Expectation (..), Failure (..), Term (..))

main :: IO ()
main = do
  results <- mapM run properties
  if and results then putStrLn "All properties passed." else exitFailure
  where
    run (name, p) = do
      putStrLn ("--- " <> name)
      isSuccess <$> quickCheckWithResult stdArgs {maxSuccess = 500} p

properties :: [(String, Property)]
properties =
  [ ("substitution of x by x changes nothing", property prop_substIdentity),
    ("substitution for a variable that is not free changes nothing", property prop_substNotFree),
    ("substitution gives the expected free variables (no capture)", property prop_substFreeVars),
    ("substitution respects α-equivalence", property prop_substAlpha),
    ("renaming bound variables gives an α-equivalent term", property prop_alphaRename),
    ("canonical forms are idempotent", property prop_canonicalIdempotent),
    ("a term unifies with itself and solves nothing", property prop_unifySelf),
    ("unification is sound: a solution makes both sides α-equivalent", property prop_unifySound),
    ("unification finds a solution for two instances of one term", property prop_unifyComplete),
    ("unification up to reduction is sound", property prop_unifySoundReduce),
    ("weak head normal form is idempotent", property prop_whnfIdempotent),
    ("normal form is idempotent", property prop_normalFormIdempotent),
    ("the parser reads back what show prints", property prop_parseShow),
    ("inference does not depend on the names of bound variables", property prop_inferAlpha),
    ("an inferred type passes check", property prop_inferCheck),
    ("a term checks against its inferred type (bidirectional)", property prop_checkInferred)
  ]

--------------------------------------------------------------------------------
-- The type system

-- | λΠ with natural numbers and vectors, without reductions.
plain :: Ctx
plain = load lambdaPi

-- | The same, with reductions for Plus and β.
computing :: Ctx
computing = load (lambdaPi <> reductions)

load :: String -> Ctx
load source = case parsePalSourceLocated "properties" source of
  Right actions -> fst (runSteps mempty actions)
  Left err -> error err

lambdaPi :: String
lambdaPi =
  unlines
    [ "expr Type : Type",
      "expr Nat : Type",
      "expr Zero : Nat",
      "rule Succ: n : Nat -> Succ(n) : Nat",
      "rule Plus: m : Nat  n : Nat -> Plus(m, n) : Nat",
      "rule Vec: n : Nat  a : Type -> Vec(n, a) : Type",
      "rule Replicate: n : Nat  x : a -> Replicate(n, x) : Vec(n, a)",
      "rule Pi: t : Type  x : t |- u : Type -> Pi(x, t, u) : Type",
      "rule Lam: t : Type  x : t |- body : u -> Lam(x, t, body) : Pi(x, t, u)",
      "rule App: f : Pi(x, t, u)  a : t -> App(f, a) : u[x := a]"
    ]

reductions :: String
reductions =
  unlines
    [ "reduce PlusZero: Plus(Zero, n) ~> n",
      "reduce PlusSucc: Plus(Succ(m), n) ~> Succ(Plus(m, n))",
      "reduce Beta: App(Lam(x, t, body), a) ~> body[x := a]"
    ]

--------------------------------------------------------------------------------
-- Generators

-- | The names of rigid variables in generated terms.
names :: [String]
names = ["x", "y", "z", "n"]

con :: String -> [Term] -> Term
con = Con Parens

-- | A term built from the constructors of 'lambdaPi'. Variables are taken
--   from 'names'; the binders of Pi and Lam too.
genTerm :: Gen Term
genTerm = sized go
  where
    go k
      | k <= 1 = leaf
      | otherwise =
          frequency
            [ (2, leaf),
              (2, binder "Pi" k),
              (2, binder "Lam" k),
              (2, con "App" <$> sequence [go (k `div` 2), go (k `div` 2)]),
              (1, con "Vec" <$> sequence [go (k `div` 2), go (k `div` 2)]),
              (1, con "Plus" <$> sequence [go (k `div` 2), go (k `div` 2)]),
              (1, con "Succ" . pure <$> go (k - 1))
            ]
    leaf = oneof [Var <$> elements names, elements (fmap (`con` []) ["Zero", "Nat", "Type"])]
    binder c k = do
      v <- elements names
      t <- go (k `div` 3)
      b <- go (k `div` 2)
      pure (con c [Var v, t, b])

-- | Smaller terms: the arguments, and the term with a smaller argument.
shrinkTerm :: Term -> [Term]
shrinkTerm = \case
  Con b c ts -> ts <> [Con b c ts' | ts' <- shrinkArgs ts]
  Subst body _ e -> [body, e]
  Var _ -> []
  where
    shrinkArgs = \case
      [] -> []
      t : rest -> [t' : rest | t' <- shrinkTerm t, not (isBinderVar t)] <> fmap (t :) (shrinkArgs rest)
    isBinderVar = \case
      Var _ -> True
      _ -> False

newtype AnyTerm = AnyTerm Term

instance Show AnyTerm where
  show (AnyTerm t) = show t

instance Arbitrary AnyTerm where
  arbitrary = AnyTerm <$> genTerm
  shrink (AnyTerm t) = AnyTerm <$> shrinkTerm t

-- | Replace some subterms (not in binder positions) with fresh
--   metavariables. The names start with the given prefix.
withHoles :: String -> Term -> Gen Term
withHoles prefix t0 = snd <$> go (0 :: Int) t0
  where
    go n t = do
      hole <- frequency [(1, pure True), (4, pure False)]
      if hole
        then pure (n + 1, Var ('\'' : prefix <> show n))
        else case t of
          Con b c ts -> do
            let bs = fmap fst (binders plain c (length ts))
            (n', ts') <- goArgs n (zip [0 ..] ts) bs
            pure (n', Con b c ts')
          other -> pure (n, other)
    goArgs n [] _ = pure (n, [])
    goArgs n ((k, t) : rest) bs = do
      (n1, t') <- if k `elem` bs then pure (n, t) else go n t
      (n2, rest') <- goArgs n1 rest bs
      pure (n2, t' : rest')

-- | Rename every bound variable to a new name, by the position of its
--   binder. The result is α-equivalent to the term.
alphaRename :: Term -> Term
alphaRename = go "r"
  where
    go path = \case
      Con b c ts ->
        let bs = binders plain c (length ts)
            ts' = [go (path <> show k) t | (k, t) <- zip [0 :: Int ..] ts]
            renameBinder args i = case args !! i of
              Var v ->
                let new = v <> "_" <> path <> show i
                 in [ if k == i then Var new else if (i, k) `elem` bs then substVar plain v (Var new) t else t
                      | (k, t) <- zip [0 ..] args
                    ]
              _ -> args
         in Con b c (foldl renameBinder ts' (nub (fmap fst bs)))
      other -> other

--------------------------------------------------------------------------------
-- Helpers

-- | α-equivalence: the same canonical form and the same free variables.
alphaEqual :: Ctx -> Term -> Term -> Bool
alphaEqual ctx a b = canonical ctx a == canonical ctx b && freeVars ctx a == freeVars ctx b

-- | Run a unification from the initial state.
runUnify :: Ctx -> Term -> Term -> Maybe InferState
runUnify ctx a b = case runInfer (unify ctx a b) initialState {inferSupply = 1000} of
  (st, Right ()) -> Just st
  (_, Left _) -> Nothing

-- | A term with the solved metavariables replaced.
resolve :: Ctx -> InferState -> Term -> Term
resolve ctx st = forceTerm ctx . Actions.applySubst (inferSubst st)

sameSet :: [String] -> [String] -> Bool
sameSet a b = sort (nub a) == sort (nub b)

--------------------------------------------------------------------------------
-- Properties

prop_substIdentity :: AnyTerm -> Property
prop_substIdentity (AnyTerm t) =
  forAll (elements names) $ \x -> alphaEqual plain (substVar plain x (Var x) t) t

prop_substNotFree :: AnyTerm -> AnyTerm -> Property
prop_substNotFree (AnyTerm t) (AnyTerm e) =
  forAll (elements names) $ \x ->
    x `notElem` freeVars plain t ==> alphaEqual plain (substVar plain x e t) t

prop_substFreeVars :: AnyTerm -> AnyTerm -> Property
prop_substFreeVars (AnyTerm t) (AnyTerm e) =
  forAll (elements names) $ \x ->
    let expected
          | x `elem` freeVars plain t = (freeVars plain t \\ [x]) <> freeVars plain e
          | otherwise = freeVars plain t
     in counterexample (show (substVar plain x e t)) $
          sameSet (freeVars plain (substVar plain x e t)) expected

prop_substAlpha :: AnyTerm -> AnyTerm -> Property
prop_substAlpha (AnyTerm t) (AnyTerm e) =
  forAll (elements names) $ \x ->
    alphaEqual plain (substVar plain x e t) (substVar plain x e (alphaRename t))

prop_alphaRename :: AnyTerm -> Property
prop_alphaRename (AnyTerm t) =
  counterexample (show (alphaRename t)) $
    alphaEqual plain t (alphaRename t) .&&. equivalent plain t (alphaRename t)

prop_canonicalIdempotent :: AnyTerm -> Bool
prop_canonicalIdempotent (AnyTerm t) = canonical plain (canonical plain t) == canonical plain t

prop_unifySelf :: AnyTerm -> Property
prop_unifySelf (AnyTerm t) = case runUnify plain t (alphaRename t) of
  Just st -> property (M.null (inferSubst st))
  Nothing -> counterexample "no unifier" False

prop_unifySound :: AnyTerm -> Property
prop_unifySound (AnyTerm t) =
  forAll ((,) <$> withHoles "l" t <*> (alphaRename <$> withHoles "r" t)) $ \(a, b) ->
    case runUnify plain a b of
      Just st ->
        counterexample (show (resolve plain st a) <> "  vs  " <> show (resolve plain st b)) $
          alphaEqual plain (resolve plain st a) (resolve plain st b)
      Nothing -> property True

prop_unifyComplete :: AnyTerm -> Property
prop_unifyComplete (AnyTerm t) =
  forAll ((,) <$> withHoles "l" t <*> withHoles "r" t) $ \(a, b) ->
    counterexample (show a <> "  vs  " <> show b) $ case runUnify plain a b of
      Just _ -> True
      Nothing -> False

prop_unifySoundReduce :: AnyTerm -> Property
prop_unifySoundReduce (AnyTerm t) =
  forAll ((,) <$> withHoles "l" t <*> withHoles "r" t) $ \(a, b) ->
    case runUnify computing a b of
      Just st -> case (normalForm computing (resolve computing st a), normalForm computing (resolve computing st b)) of
        (Right a', Right b') -> counterexample (show a' <> "  vs  " <> show b') (alphaEqual computing a' b')
        _ -> property True -- the reductions did not stop
      Nothing -> property True

prop_whnfIdempotent :: AnyTerm -> Property
prop_whnfIdempotent (AnyTerm t) = case whnf computing t of
  Right t' -> whnf computing t' === Right t'
  Left _ -> property True

prop_normalFormIdempotent :: AnyTerm -> Property
prop_normalFormIdempotent (AnyTerm t) = case normalForm computing t of
  Right t' -> normalForm computing t' === Right t'
  Left _ -> property True

prop_parseShow :: AnyTerm -> Property
prop_parseShow (AnyTerm t) = case parse (pExprApp <* eof) "show" (show t) of
  Right t' -> counterexample (show t') (t' == t)
  Left err -> counterexample (show err) False

prop_inferAlpha :: AnyTerm -> Property
prop_inferAlpha (AnyTerm t) =
  case (inferDetailed plain t, inferDetailed plain (alphaRename t)) of
    (Right d, Right d') -> counterexample (show (deriv'type d) <> "  vs  " <> show (deriv'type d')) (equivalent plain (deriv'type d) (deriv'type d'))
    (Left _, Left _) -> property True
    _ -> counterexample "only one of them has a type" False

prop_inferCheck :: AnyTerm -> Property
prop_inferCheck (AnyTerm t) = case inferDetailed computing t of
  Right d ->
    let (_, _, met) = checkExpectation computing (ExpectType t (deriv'type d))
     in counterexample (show (deriv'type d)) met
  Left _ -> property True

prop_checkInferred :: AnyTerm -> Property
prop_checkInferred (AnyTerm t) = case inferDetailed computing t of
  Right d -> case checkDetailed computing t (deriv'type d) of
    Right _ -> property True
    Left failures -> counterexample (show (deriv'type d) <> ": " <> show (fmap failure'err failures)) False
  Left _ -> property True
