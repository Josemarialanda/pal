-- |
-- Module      : Interpreters.Common.Search
-- Description : Proof search: find a term of a given type
--
-- @find T@ looks for a closed term of type @T@. With types as propositions
-- (see @logic.pal@), the term is a proof of @T@.
--
-- The search works backwards from the goal, like Prolog. For a goal type
-- it tries, in order:
--
-- * a variable in scope whose type unifies with the goal;
-- * a constant (@expr@) whose type unifies with the goal;
-- * a rule whose conclusion type unifies with the goal. Its premises
--   become new goals, and the hypotheses of a premise (@x : A |- …@) bring
--   new variables into scope.
--
-- The search deepens step by step (iterative deepening), so it finds a
-- small term first, and it stops after a fixed number of steps. Every term
-- that it finds is checked against the goal with the normal type checker
-- before it is reported.
module Interpreters.Common.Search
  ( findTerm,
    defaultDepth,
    NotFound (..),
    notFoundMessage,
  )
where

import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (isJust, listToMaybe)
import Interpreters.Common.Actions
  ( Infer,
    InferState (..),
    applySubst,
    binders,
    checkDetailed,
    freshen,
    getsI,
    initialState,
    modifyI,
    renameRule,
    ruleTypeVars,
    runInfer,
    substituteExpr,
    unify,
  )
import qualified Types

-- | The depth of a search if the program does not give one: the most rules
--   on a path from the goal to a leaf.
defaultDepth :: Int
defaultDepth = 6

-- | The most search steps in all, so that a search stops in time.
budget :: Int
budget = 200000

-- | Why a search found no term.
data NotFound
  = -- | The search went to the given depth and found nothing.
    NotWithin Int
  | -- | The search used all its steps; it went completely to the given
    --   depth.
    OutOfSteps Int

-- | A message that tells why the search found no term.
notFoundMessage :: NotFound -> String
notFoundMessage = \case
  NotWithin d -> "no term of this type found (the search goes " <> show d <> " rules deep)"
  OutOfSteps d ->
    "no term of this type found: the search stopped after "
      <> show budget
      <> " steps"
      <> (if d > 0 then ", and it looked " <> show d <> " rules deep" else "")

-- | Find a closed term of the given type, at most the given number of
--   rules deep. The free variables of the type are rigid: the term must
--   have the type for every value of them.
findTerm :: Types.Ctx -> Int -> Types.Type -> Either NotFound Types.Term
findTerm ctx depthLimit goal = deepen 1 budget
  where
    deepen d steps
      | d > depthLimit = Left (NotWithin depthLimit)
      | steps <= 0 = Left (OutOfSteps (d - 1))
      | otherwise = case search ctx False [] goal d initialState steps done of
          Right term -> Right (tidy ctx term)
          Left steps'
            | steps' <= 0 -> Left (OutOfSteps (d - 1))
            | otherwise -> deepen (d + 1) steps'
    -- Accept a term only if the type checker agrees.
    done term st steps = case checkDetailed ctx (applySubst (inferSubst st) term) goal of
      Right _ -> Right (applySubst (inferSubst st) term)
      Left _ -> Left steps

-- | The result of a search: a term, or the steps that are left.
type Result = Either Int Types.Term

-- | What to do with a term for a goal: continue the search with it.
type Continue = Types.Term -> InferState -> Int -> Result

-- | Search for a term of the goal type with the variables in scope, at
--   most @depth@ rules deep. Each term found goes to the continuation; if
--   the continuation fails, the search tries the next possibility.
search :: Types.Ctx -> Bool -> [(String, Types.Type)] -> Types.Type -> Int -> InferState -> Int -> Continue -> Result
search ctx synthOnly scope goal depth st steps k
  | steps <= 0 = Left 0
  | otherwise = firstOf (variables <> constants <> rules) (steps - 1)
  where
    firstOf [] left = Left left
    firstOf (try : rest) left = case try left of
      Right term -> Right term
      Left left' -> firstOf rest left'
    -- Run a unification; if it succeeds, continue with the term.
    unifyThen ty term left = case runInfer (unify ctx goal ty) st of
      (st', Right ()) -> k term st' left
      _ -> Left left
    variables = [unifyThen ty (Types.Var v) | (v, ty) <- scope]
    constants =
      [ \left -> case runInfer (instantiateAll ty) st of
          (st', Right ty') -> case runInfer (unify ctx goal ty') st' of
            (st'', Right ()) -> k (Types.ECon c []) st'' left
            _ -> Left left
          _ -> Left left
        | Types.ExprDecl c ty <- Types.ctx'exprs ctx
      ]
    rules
      | depth <= 0 = []
      | otherwise = [applyRule ctx scope goal depth rule st k | rule <- ordered, not synthOnly || simpleElimination rule]
    -- Introductions first, then eliminations without hypotheses (like
    -- Fst), then the others (like Case, which splits the search).
    ordered =
      let rs = reverse (Types.ctx'rules ctx)
       in filter (not . isJust . majorPremise) rs <> filter simpleElimination rs <> filter (\r -> isJust (majorPremise r) && not (simpleElimination r)) rs
    simpleElimination r = isJust (majorPremise r) && null [() | Types.Premise (_ : _) _ <- Types.typingRule'premises r]

-- | Use a rule for the goal: unify its conclusion type with the goal, then
--   search for a term for each premise in order.
applyRule :: Types.Ctx -> [(String, Types.Type)] -> Types.Type -> Int -> Types.TypingRule -> InferState -> Continue -> Int -> Result
applyRule ctx scope goal depth rule st k steps =
  case runInfer prepare st of
    (st', Right (premises, pat, conclTy, patternVars)) -> case runInfer (unify ctx goal conclTy) st' of
      (st'', Right ()) -> premisesThen (0 :: Int) premises patternVars pat st'' steps
      _ -> Left steps
    _ -> Left steps
  where
    -- Fresh metavariables for the type variables and for the pattern
    -- variables; fresh rigid names for the binders.
    prepare = do
      renamed <- (`renameRule` rule) <$> freshen (ruleTypeVars rule)
      let Types.TypingRule _ premises0 (pat0, conclTy0) = renamed
          binderVars = [v | Types.Premise hyps _ <- premises0, Types.Hypothesis (Types.Var v) _ _ <- hyps]
          others = [v | v <- patternVariables pat0, v `notElem` binderVars]
      metas <- freshen others
      names <- traverse (\v -> (\n -> (v, Types.Var ("h" <> show n))) <$> nextName) binderVars
      let env = metas <> M.fromList names
          inst = substituteExpr env
          premises = [Types.PremiseWith m [h {Types.hyp'var = inst (Types.hyp'var h), Types.hyp'type = inst (Types.hyp'type h)} | h <- hyps] (inst e, inst t) | Types.PremiseWith m hyps (e, t) <- premises0]
      pure (premises, inst pat0, inst conclTy0, M.elems metas)
    -- Search the premises in order; then build the term from the pattern.
    premisesThen _ [] patternVars pat st' left
      | all (solved st') patternVars = k (applySubst (inferSubst st') pat) st' left
      | otherwise = Left left
    premisesThen i (Types.PremiseWith _ hyps (subject, ty) : rest) patternVars pat st' left =
      let scope' = [(v, t) | Types.Hypothesis (Types.Var v) t _ <- hyps] <> scope
       in case applySubst (inferSubst st') subject of
            -- A subject that unification already found: no search.
            known | not (isMeta' known) -> premisesThen (i + 1) rest patternVars pat st' left
            _ ->
              search ctx (Just i == majorPremise rule) scope' ty (depth - 1) st' left $ \term st'' left' ->
                case runInfer (unify ctx subject term) st'' of
                  (st''', Right ()) -> premisesThen (i + 1) rest patternVars pat st''' left'
                  _ -> Left left'
    solved st' = \case
      Types.Var m -> not (isMeta' (applySubst (inferSubst st') (Types.Var m)))
      _ -> True
    isMeta' = \case
      Types.Var ('\'' : _) -> True
      _ -> False

-- | The variables of a rule's conclusion pattern.
patternVariables :: Types.Term -> [String]
patternVariables = \case
  Types.Var v -> [v]
  Types.Con _ _ ts -> concatMap patternVariables ts
  Types.Subst {} -> []

-- | Replace the free variables of a declared type with fresh
--   metavariables, as each use of a constant does.
instantiateAll :: Types.Type -> Infer Types.Type
instantiateAll ty = (`applySubst` ty) <$> freshen (vars ty)
  where
    vars = \case
      Types.Var v -> [v]
      Types.Con _ _ ts -> concatMap vars ts
      Types.Subst body x e -> vars body <> vars x <> vars e

-- | A number for a new name.
nextName :: Infer Int
nextName = do
  n <- getsI inferSupply
  modifyI (\s -> s {inferSupply = n + 1})
  pure n

-- | Give the bound variables of a found term short names (@x@, @y@, …).
--   The search names them @h0@, @h1@, …, which are unique.
tidy :: Types.Ctx -> Types.Term -> Types.Term
tidy ctx t = substituteExpr (M.fromList (zip bound (fmap Types.Var shortNames))) t
  where
    bound = nub (binderNamesOf t)
    shortNames = [c : suffix | suffix <- "" : fmap show [1 :: Int ..], c <- "xyzwvu"]
    binderNamesOf = \case
      Types.Con _ c ts ->
        [v | i <- fmap fst (binders ctx c (length ts)), Types.Var v <- [ts !! i]]
          <> concatMap binderNamesOf ts
      _ -> []

-- | The major premise of an elimination rule: a rule whose conclusion type
--   is a variable (e.g. @Fst(p) : a@), so that it matches any goal. The
--   major premise is the first premise without hypotheses whose type is not
--   a variable (e.g. @p : And<a, b>@).
--
--   The search looks only for normal terms: an elimination is applied to a
--   variable, a constant or another elimination. So the major premise is
--   searched without introduction rules, which keeps the search small.
majorPremise :: Types.TypingRule -> Maybe Int
majorPremise (Types.TypingRule _ premises (_, conclTy)) = case conclTy of
  Types.Var _ -> listToMaybe [i | (i, Types.Premise [] (_, t)) <- zip [0 ..] premises, not (isVariable t)]
  _ -> Nothing
  where
    isVariable = \case
      Types.Var _ -> True
      _ -> False
