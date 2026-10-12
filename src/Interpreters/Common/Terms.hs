-- |
-- Module      : Interpreters.Common.Terms
-- Description : Terms: variables, binders and substitution
--
-- Pure operations on terms ("Types.Term"): their variables, the binding
-- structure of constructors (read from the rules), capture-avoiding
-- substitution, α-equivalence ('canonical'), and first-order unification of
-- the patterns of reductions.
module Interpreters.Common.Terms where

import Control.Monad (foldM)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Types

-- | The subterms of a pattern that are not variables, each with a way to
--   put a new term in its place, and whether it is the root.
patternPlaces :: Types.Term -> [(Types.Term, Types.Term -> Types.Term, Bool)]
patternPlaces = go True id
  where
    go root rebuild t = case t of
      Types.Con b c ts ->
        (t, rebuild, root)
          : concat [go False (\x -> rebuild (Types.Con b c (before <> [x] <> drop 1 after))) a | (i, a) <- zip [0 ..] ts, let (before, after) = splitAt i ts]
      _ -> []

-- | A most general unifier of two patterns (first-order), if there is one.
unifyPatterns :: Types.Term -> Types.Term -> Maybe (Map String Types.Term)
unifyPatterns a0 b0 = fmap (\s -> M.map (resolve s) s) (go M.empty [(a0, b0)])
  where
    go s [] = Just s
    go s ((a, b) : rest) = case (walk s a, walk s b) of
      (Types.Var x, Types.Var y) | x == y -> go s rest
      (Types.Var x, t) -> if x `elem` exprVars (resolve s t) then Nothing else go (M.insert x t s) rest
      (t, Types.Var y) -> go s ((Types.Var y, t) : rest)
      (Types.Con _ c as, Types.Con _ d bs)
        | c == d && length as == length bs -> go s (zip as bs <> rest)
      _ -> Nothing
    walk s = \case
      Types.Var x | Just t <- M.lookup x s -> walk s t
      t -> t
    resolve s = \case
      Types.Var x | Just t <- M.lookup x s -> resolve s t
      Types.Con b c ts -> Types.Con b c (fmap (resolve s) ts)
      t -> t

-- | Does a term contain a substitution?
hasSubst :: Types.Term -> Bool
hasSubst = \case
  Types.Subst {} -> True
  Types.ECon _ ts -> any hasSubst ts
  Types.EVar _ -> False

-- | Elements that occur more than once, each listed once.
duplicates :: (Eq a) => [a] -> [a]
duplicates xs = nub [x | (i, x) <- zip [0 :: Int ..] xs, x `elem` take i xs]

-- | The variables of an expression, in order, without repeats.
exprVars :: Types.Expr -> [String]
exprVars = nub . go
  where
    go = \case
      Types.EVar v -> [v]
      Types.ECon _ es -> concatMap go es
      Types.Subst body x e -> go body <> go x <> go e

-- | Match a rule’s conclusion pattern against a target expression.
--
--   If successful, returns an environment mapping each pattern variable to
--   the actual subexpression and its path within the target. Otherwise a
--   descriptive error (such as 'ArityMismatch' or 'NoRuleMatched').
matchConclusion :: String -> Types.Expr -> Types.Expr -> Either Types.Err (Map String (Types.Expr, [Int]))
matchConclusion ruleName pat target = go [] M.empty pat target
  where
    go path env (Types.ECon pn ps) (Types.ECon en es)
      | pn == en && length ps == length es =
          foldM (\acc (i, p, e) -> go (path <> [i]) acc p e) env (zip3 [0 ..] ps es)
      | pn == en = Left (Types.ArityMismatch (length ps) (length es))
      | otherwise = Left (Types.NoRuleMatched (Types.ECon en es))
    go path env (Types.EVar var) e = case M.lookup var env of
      Nothing -> Right (M.insert var (e, path) env)
      Just (ePrev, _)
        | ePrev == e -> Right env
        | otherwise ->
            Left . Types.CustomErr $
              "rule " <> ruleName <> " needs every " <> var <> " in " <> show pat <> " to be the same expression"
    go _ _ p e =
      Left . Types.CustomErr $
        "rule " <> ruleName <> " needs " <> show e <> " to be of the form " <> show p

-- | Perform substitution of variables within an expression.
--
--   Given a mapping from variable names to expressions, replaces all
--   occurrences of those variables recursively.
substituteExpr :: Map String Types.Expr -> Types.Expr -> Types.Expr
substituteExpr env = \case
  Types.EVar v -> M.findWithDefault (Types.EVar v) v env
  Types.Con b n args -> Types.Con b n (fmap (substituteExpr env) args)
  Types.Subst body x e -> Types.Subst (substituteExpr env body) (substituteExpr env x) (substituteExpr env e)

-- | A substitution from type variable names to types.
type Subst = Map String Types.Type

-- | Replace every bound type variable in a type with its binding.
applySubst :: Subst -> Types.Type -> Types.Type
applySubst s = \case
  Types.TVar v -> M.findWithDefault (Types.TVar v) v s
  Types.Con b n ts -> Types.Con b n (fmap (applySubst s) ts)
  Types.Subst body x e -> Types.Subst (applySubst s body) (applySubst s x) (applySubst s e)

-- | All the variables of a type, in order of first appearance.
freeTypeVars :: Types.Type -> [String]
freeTypeVars = exprVars

-- | Is this variable a metavariable: an unknown that unification can solve?
--   Only 'fresh' makes metavariables. Each use of a rule or a declaration
--   replaces its type variables with fresh ones. The other variables (e.g.
--   @n@ in @Vec(n, Nat)@, which comes from the expression) are rigid.
isMeta :: String -> Bool
isMeta = \case
  '\'' : _ -> True
  _ -> False

-- | The metavariables of a type, in order of first appearance.
metaVars :: Types.Type -> [String]
metaVars = filter isMeta . freeTypeVars

-- | Rename free variables at the same time (e.g. @x@ to @y@ and @y@ to
--   @x@), without capture.
renameFree :: Types.Ctx -> Map String String -> Types.Term -> Types.Term
renameFree ctx m t
  | M.null m = t
  | otherwise = foldr toTarget (foldr toTemporary t steps) steps
  where
    pairs = M.toList m
    used = exprVars t <> concatMap (\(a, b) -> [a, b]) pairs
    temporaries = [v | i <- [0 :: Int ..], let v = "tmp" <> show i, v `notElem` used]
    steps = zip pairs temporaries
    toTemporary ((old, _), tmp) = substVar ctx old (Types.TVar tmp)
    toTarget ((_, new), tmp) = substVar ctx tmp (Types.TVar new)

substPremise :: Subst -> Types.Premise -> Types.Premise
substPremise s = mapPremise (applySubst s)

mapPremise :: (Types.Type -> Types.Type) -> Types.Premise -> Types.Premise
mapPremise f p@(Types.Premise hyps (e, t)) =
  p {Types.premise'hypotheses = fmap (\h -> h {Types.hyp'type = f (Types.hyp'type h)}) hyps, Types.premise'judgment = (e, f t)}

premiseTypeList :: Types.Premise -> [Types.Type]
premiseTypeList (Types.Premise hyps (_, t)) = fmap Types.hyp'type hyps <> [t]

-- | Rename the free variables and the bound variables of a term in order
--   of appearance. Two terms have the same canonical form exactly when they
--   are equal up to α-equivalence and up to the names of free variables.
canonical :: Types.Ctx -> Types.Term -> Types.Term
canonical ctx t0 = snd (go free 0 t0)
  where
    free = M.fromList (zip (freeVars ctx t0) [Types.TVar ("'f" <> show i) | i <- [0 :: Int ..]])
    boundName m = Types.TVar ("'b" <> show m)
    go names n = \case
      Types.EVar v -> (n, M.findWithDefault (Types.EVar v) v names)
      Types.ECon c ts ->
        let bs = binders ctx c (length ts)
            numbered = zip (nub (fmap fst bs)) [n ..]
            -- The names that are bound in argument k, and their new names.
            boundIn k = M.fromList [(v, boundName m) | (i, m) <- numbered, (i, k) `elem` bs, Types.EVar v <- [ts !! i]]
            step (m, done) (k, t) = case lookup k numbered of
              Just m' -> (m, done <> [boundName m'])
              Nothing -> let (m'', t') = go (boundIn k <> names) m t in (m'', done <> [t'])
            (n', ts') = foldl step (n + length numbered, []) (zip [0 ..] ts)
         in (n', Types.ECon c ts')
      Types.Subst body x e ->
        let (n1, body') = go names n body
            (n2, x') = go names n1 x
            (n3, e') = go names n2 e
         in (n3, Types.Subst body' x' e')

-- | The binding structure of a constructor, which PAL reads from the rules
--   (see 'Types.ruleBinders'; the context keeps them in an index).
--   @(i, j)@ means that argument @i@ is a variable that is bound in argument
--   @j@. For example, a rule with the conclusion @Pi(x, t, u)@ and the
--   premise @x : t |- u : Type@ gives @(0, 2)@ for @Pi@ with 3 arguments.
binders :: Types.Ctx -> String -> Int -> [(Int, Int)]
binders ctx name n = M.findWithDefault [] (name, n) (Types.ctx'binders ctx)

-- | The free variables of a term, in order of first appearance. A variable
--   in a binder position (see 'binders') is not free, and neither are its
--   occurrences in the bodies that it binds.
freeVars :: Types.Ctx -> Types.Term -> [String]
freeVars ctx = nub . go
  where
    go = \case
      Types.EVar v -> [v]
      Types.ECon c ts ->
        let bs = binders ctx c (length ts)
            boundIn k = [v | (i, j) <- bs, j == k, Types.EVar v <- [ts !! i]]
         in concat [filter (`notElem` boundIn k) (go t) | (k, t) <- zip [0 ..] ts, k `notElem` fmap fst bs]
      Types.Subst body x e -> filter (`notElem` exprVars x) (go body) <> go e

-- | A variant of a name (@x1@, @x2@, …) that is not in the list.
freshName :: [String] -> String -> String
freshName avoid x = case [x' | i <- [1 :: Int ..], let x' = x <> show i, x' `notElem` avoid] of
  x' : _ -> x'
  [] -> x -- unreachable: the list of candidates is infinite

-- | Substitute a term for a variable, @t[x := e]@, without capture: a binder
--   with the name @x@ hides @x@ in its bodies, and a binder that would
--   capture a free variable of @e@ gets a new name first.
substVar :: Types.Ctx -> String -> Types.Term -> Types.Term -> Types.Term
substVar ctx x e = go
  where
    freeInE = freeVars ctx e
    go = \case
      Types.EVar v
        | v == x -> e
        | otherwise -> Types.EVar v
      Types.Con b c ts -> Types.Con b c (substArgs (binders ctx c (length ts)) ts)
      Types.Subst body y r -> Types.Subst (if y == Types.EVar x then body else go body) y (go r)
    substArgs bs ts0 =
      let ts = foldl (avoidCapture bs) ts0 (nub (fmap fst bs))
          hidden k = or [ts !! i == Types.EVar x | (i, j) <- bs, j == k]
       in [ if k `elem` fmap fst bs || hidden k then t else go t
            | (k, t) <- zip [0 ..] ts
          ]
    avoidCapture bs ts i = case ts !! i of
      Types.EVar y
        | y /= x,
          y `elem` freeInE,
          any (\j -> x `elem` freeVars ctx (ts !! j)) bodies ->
            let y' = freshName (freeInE <> concatMap (freeVars ctx) ts <> [x]) y
             in [ if k == i then Types.EVar y' else if k `elem` bodies then substVar ctx y (Types.EVar y') t else t
                  | (k, t) <- zip [0 ..] ts
                ]
      _ -> ts
      where
        bodies = [j | (i', j) <- bs, i' == i]

-- | Apply every substitution @u[x := e]@ in a term whose body is known. A
--   substitution whose body is still a metavariable stays, so that it can
--   be applied when that metavariable is solved.
forceTerm :: Types.Ctx -> Types.Term -> Types.Term
forceTerm ctx = \case
  Types.Con b c ts -> Types.Con b c (fmap (forceTerm ctx) ts)
  Types.Subst body x e ->
    let body' = forceTerm ctx body
        e' = forceTerm ctx e
     in case (body', x) of
          (Types.EVar m, Types.EVar v) | isMeta m && m /= v -> Types.Subst body' x e'
          (_, Types.EVar v) -> substVar ctx v e' body'
          _ -> Types.Subst body' x e'
  t -> t

-- | The names in binder positions in a term.
binderNames :: Types.Ctx -> Types.Term -> [String]
binderNames ctx = \case
  Types.Var _ -> []
  Types.Con _ c ts ->
    [v | i <- nub (fmap fst (binders ctx c (length ts))), Types.Var v <- [ts !! i]]
      <> concatMap (binderNames ctx) ts
  Types.Subst body x e -> exprVars x <> binderNames ctx body <> binderNames ctx e

-- | The type variables of a rule: every variable in its types, except the
--   pattern variables of its conclusion (those stand for the matched
--   expressions).
ruleTypeVars :: Types.TypingRule -> [String]
ruleTypeVars (Types.TypingRule _ premises (conclExpr, conclTy)) =
  filter (`notElem` exprVars conclExpr) (nub (concatMap freeTypeVars (conclTy : concatMap premiseTypeList premises)))

-- | Apply a type substitution throughout a rule.
renameRule :: Subst -> Types.TypingRule -> Types.TypingRule
renameRule s rule@(Types.TypingRule _ premises (ce, ct)) =
  rule {Types.typingRule'premises = fmap (substPremise s) premises, Types.typingRule'ruleConclusion = (ce, applySubst s ct)}
