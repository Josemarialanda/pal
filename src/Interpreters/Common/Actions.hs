-- |
-- Module      : Interpreters.Common.Actions
-- Description : Core semantic actions shared by all PAL interpreters
--
-- This module defines the **core semantic actions** that power the PAL DSL.
-- Each function here corresponds to a fundamental operation within PAL — defining types,
-- expressions, and rules, and performing type inference based on the current context ('Ctx').
--
-- These actions are shared across all interpreter variants:
--
-- * "Interpreters.Core" — pure, minimal interpreters.
-- * "Interpreters.Debug" — traced interpreters that log execution.
--
-- All these interpreters delegate to the functions in this module to perform
-- the actual typechecking logic.  This separation keeps the **semantics**
-- independent of the **execution environment** (pure, IO, traced, etc.).
module Interpreters.Common.Actions where

import Control.Monad (foldM, forM_)
import Data.Foldable (find)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Polysemy (Member, Members, Sem, run)
import Polysemy.Error (Error, fromEither, runError, throw)
import Polysemy.State (State, evalState, gets, modify)
import qualified Types
import Utils.OneOfN (OneOf3 (..))

--------------------------------------------------------------------------------

-- | Definition actions (context mutation)

--------------------------------------------------------------------------------

-- | Insert a declaration of any of the three supported kinds
--   (TypeDecl, ExprDecl, TypingRule) into the current context.
insertIntoCtx ::
  (Member (State Types.Ctx) r) =>
  OneOf3 Types.TypeDecl Types.ExprDecl Types.TypingRule ->
  Sem r ()
insertIntoCtx = \case
  OneOf3_1 td ->
    modify $ \ctx -> ctx {Types.ctx'types = td : Types.ctx'types ctx}
  OneOf3_2 ed ->
    modify $ \ctx -> ctx {Types.ctx'exprs = ed : Types.ctx'exprs ctx}
  OneOf3_3 tr ->
    modify $ \ctx -> ctx {Types.ctx'rules = tr : Types.ctx'rules ctx}

-- | Define a new type declaration in the current context.
--
--   Adds a new 'TypeDecl' to 'ctx'types'.
defineType :: (Member (State Types.Ctx) r) => Types.TypeDecl -> Sem r ()
defineType = insertIntoCtx . OneOf3_1

-- | Define a new expression and its associated type.
--
--   Adds a new 'ExprDecl' to 'ctx'exprs'.
defineExpr :: (Member (State Types.Ctx) r) => Types.ExprDecl -> Sem r ()
defineExpr = insertIntoCtx . OneOf3_2

-- | Define a new typing rule for inference.
--
--   Adds a 'TypingRule' to 'ctx'rules'.
defineRule :: (Member (State Types.Ctx) r) => Types.TypingRule -> Sem r ()
defineRule = insertIntoCtx . OneOf3_3

--------------------------------------------------------------------------------

-- | Expression lookup and inference

--------------------------------------------------------------------------------

-- | Look up the declared type of an expression by name, if it exists.
--
--   Returns 'Nothing' if the expression is not defined in the context.
lookupExprType :: Types.Ctx -> String -> Maybe Types.Type
lookupExprType ctx name =
  listToMaybe [t | Types.ExprDecl n t <- Types.ctx'exprs ctx, n == name]

-- | Perform type inference for an expression in the given context.
--
--   Runs 'inferM' with a fresh substitution, then applies the final
--   substitution and renames any remaining type variables to @a@, @b@, …
--   (so @Lam(x, x)@ is reported as @Arrow<a, a>@).
infer :: Types.Expr -> Types.Ctx -> Either Types.Err Types.Type
infer e ctx =
  either (Left . normalizeErr) Right . run . runError . evalState (InferState 0 M.empty) $ do
    t <- inferM ctx e
    s <- gets inferSubst
    pure (normalize (applySubst s t))

-- | Infer the type of an expression, extending the current substitution.
--   Order:
--     1) Try to match a typing rule and apply it.
--     2) If no rule matches, fall back to local bindings and declared types.
--     3) If neither works, throw an appropriate error.
inferM :: (InferEffects r) => Types.Ctx -> Types.Expr -> Sem r Types.Type
inferM ctx e =
  case matchRule ctx e of
    -- Rule found → apply it.
    Right rule ->
      applyRule ctx e rule
    -- No rule matched → try local bindings and declared types.
    Left (Types.NoRuleMatched _) ->
      case e of
        -- Constructor/constant
        Types.ECon name args ->
          case lookupExprType ctx name of
            -- Zero-arg constructors/constants are base cases.
            Just t | null args -> instantiate t
            -- Has a declared thing but args present and no rule matched → keep the precise error.
            Just _ -> throw (Types.NoRuleMatched e)
            -- Unknown constructor symbol altogether.
            Nothing -> throw (Types.UnknownExpr name)
        -- Variable: a local binding (e.g. a lambda parameter) shadows declarations.
        Types.EVar v ->
          case M.lookup v (Types.ctx'env ctx) of
            Just t -> pure t
            Nothing -> maybe (throw (Types.UnknownExpr v)) instantiate (lookupExprType ctx v)
    -- If rule matching failed for another reason, propagate it.
    Left err ->
      throw err

--------------------------------------------------------------------------------

-- | Rule matching and validation

--------------------------------------------------------------------------------

-- | Attempt to find a typing rule in the context that matches a given expression.
--   Checks:
--     • the rule’s conclusion constructor name matches the expression, and
--     • the number of arguments (arity) matches.
--   If no rule matches, returns 'NoRuleMatched' (not 'UnknownExpr' — constants may have no rules).
matchRule :: Types.Ctx -> Types.Expr -> Either Types.Err Types.TypingRule
matchRule ctx e@(Types.ECon name args) =
  case rulesWithSameName ctx name of
    [] ->
      -- No rules for this constructor; not an unknown symbol (it may be a declared constant).
      Left (Types.NoRuleMatched e)
    rs ->
      case find ((== length args) . arity) rs of
        Just r -> Right r
        Nothing ->
          case listToMaybe (nub (fmap arity rs)) of
            Just expected -> Left (Types.ArityMismatch expected (length args))
            Nothing -> Left (Types.NoRuleMatched e)
matchRule _ e =
  -- Non-constructor expressions aren’t matched by rule conclusions here.
  Left (Types.NoRuleMatched e)

-- | Get all typing rules in the context that have the same constructor name.
rulesWithSameName :: Types.Ctx -> String -> [Types.TypingRule]
rulesWithSameName ctx name = filter match (Types.ctx'rules ctx)
  where
    match r = case fst (Types.typingRule'ruleConclusion r) of
      Types.ECon n _ -> n == name
      _ -> False

-- | Compute the arity (number of parameters) of a rule’s conclusion.
arity :: Types.TypingRule -> Int
arity r = case fst (Types.typingRule'ruleConclusion r) of
  Types.ECon _ ps -> length ps
  _ -> 0

--------------------------------------------------------------------------------

-- | Rule application and substitution

--------------------------------------------------------------------------------

-- | Attempt to match a rule’s conclusion pattern against a target expression.
--
--   If successful, returns an environment mapping pattern variables to
--   actual expressions. If the pattern does not match, returns a descriptive
--   error (such as 'ArityMismatch' or 'NoRuleMatched').
matchConclusion :: Types.Expr -> Types.Expr -> Either Types.Err (Map String Types.Expr)
matchConclusion = go M.empty
  where
    go env (Types.ECon pn ps) (Types.ECon en es)
      | pn == en && length ps == length es = foldM (\acc (p, e) -> go acc p e) env (zip ps es)
      | pn == en = Left (Types.ArityMismatch (length ps) (length es))
      | otherwise = Left (Types.NoRuleMatched (Types.ECon en es))
    go env (Types.EVar var) e = case M.lookup var env of
      Nothing -> Right (M.insert var e env)
      Just ePrev ->
        if ePrev == e
          then Right env
          else Left (Types.CustomErr $ "inconsistent binding for " <> var)
    go _ p e = Left (Types.CustomErr $ "unsupported pattern: " <> show (p, e))

-- | Perform substitution of variables within an expression.
--
--   Given a mapping from variable names to expressions, replaces all
--   occurrences of those variables recursively.
substituteExpr :: Map String Types.Expr -> Types.Expr -> Types.Expr
substituteExpr env = \case
  Types.EVar v -> M.findWithDefault (Types.EVar v) v env
  Types.ECon n args -> Types.ECon n (fmap (substituteExpr env) args)

--------------------------------------------------------------------------------

-- | Type variables and unification

--------------------------------------------------------------------------------

-- | A substitution from type variable names to types.
type Subst = Map String Types.Type

-- | State threaded through inference: a supply of fresh type variable names
--   and the substitution solved so far.
data InferState = InferState
  { inferSupply :: Int,
    inferSubst :: Subst
  }

-- | Effects needed by inference.
type InferEffects r = Members '[State InferState, Error Types.Err] r

-- | Replace every bound type variable in a type with its binding.
applySubst :: Subst -> Types.Type -> Types.Type
applySubst s = \case
  Types.TVar v -> M.findWithDefault (Types.TVar v) v s
  Types.TCon n ts -> Types.TCon n (fmap (applySubst s) ts)

-- | Free type variables of a type, in order of first appearance.
freeTypeVars :: Types.Type -> [String]
freeTypeVars = nub . go
  where
    go = \case
      Types.TVar v -> [v]
      Types.TCon _ ts -> concatMap go ts

-- | A fresh type variable. Names start with @'@, which the parser never
--   produces, so they cannot clash with user-written variables.
fresh :: (InferEffects r) => Sem r Types.Type
fresh = do
  n <- gets inferSupply
  modify (\st -> st {inferSupply = n + 1})
  pure (Types.TVar ('\'' : show n))

-- | Replace the given type variables with fresh ones.
freshen :: (InferEffects r) => [String] -> Sem r Subst
freshen vs = M.fromList . zip vs <$> traverse (const fresh) vs

-- | Instantiate a declared type: each of its type variables becomes fresh,
--   so e.g. @Nothing : Maybe<a>@ can be used at different types.
instantiate :: (InferEffects r) => Types.Type -> Sem r Types.Type
instantiate t = (`applySubst` t) <$> freshen (freeTypeVars t)

-- | Unify an expected type with an inferred one, extending the substitution.
unify :: (InferEffects r) => Types.Type -> Types.Type -> Sem r ()
unify expected got = do
  s <- gets inferSubst
  case (applySubst s expected, applySubst s got) of
    (Types.TVar a, Types.TVar b) | a == b -> pure ()
    (Types.TVar a, t) -> bind a t
    (t, Types.TVar a) -> bind a t
    (Types.TCon en ets, Types.TCon gn gts)
      | en == gn && length ets == length gts -> sequence_ (zipWith unify ets gts)
    (e, g) -> throw (Types.Mismatch e g)
  where
    bind a t
      | a `elem` freeTypeVars t = throw (Types.InfiniteType (Types.TVar a) t)
      | otherwise =
          modify $ \st ->
            st {inferSubst = M.insert a t (applySubst (M.singleton a t) <$> inferSubst st)}

-- | Rename type variables to @a@, @b@, …, @z@, @a1@, … in order of appearance.
renameVars :: [Types.Type] -> [Types.Type]
renameVars ts = fmap (applySubst names) ts
  where
    names = M.fromList (zip (nub (concatMap freeTypeVars ts)) (fmap Types.TVar pretty))
    pretty = [c : suffix | suffix <- "" : fmap show [1 :: Int ..], c <- ['a' .. 'z']]

normalize :: Types.Type -> Types.Type
normalize t = case renameVars [t] of
  [t'] -> t'
  _ -> t

-- | Give the types in an error readable variable names.
normalizeErr :: Types.Err -> Types.Err
normalizeErr = \case
  Types.Mismatch e g | [e', g'] <- renameVars [e, g] -> Types.Mismatch e' g'
  Types.InfiniteType v t | [v', t'] <- renameVars [v, t] -> Types.InfiniteType v' t'
  err -> err

--------------------------------------------------------------------------------

-- | Applying rules

--------------------------------------------------------------------------------

-- | Apply a typing rule to an expression, checking all its premises.
--
--   This function:
--     1. Renames the rule’s type variables to fresh ones.
--     2. Matches the rule’s conclusion pattern against the target expression.
--     3. For each premise, brings its hypotheses into scope as local bindings
--        (e.g. a lambda’s parameter), substitutes concrete expressions for
--        pattern variables, infers the premise’s type, and unifies it with
--        the expected type.
--
--   The conclusion type is returned; the caller applies the substitution.
applyRule :: (InferEffects r) => Types.Ctx -> Types.Expr -> Types.TypingRule -> Sem r Types.Type
applyRule ctx target rule = do
  renaming <- freshen (ruleTypeVars rule)
  let Types.TypingRule _ premises (conclExpr, conclTy) = renameRule renaming rule
  env <- fromEither (matchConclusion conclExpr target)
  forM_ premises $ \(Types.Premise hyps (pExpr, pTy)) -> do
    locals <- traverse (bindHypothesis env) hyps
    let ctx' = ctx {Types.ctx'env = M.fromList locals <> Types.ctx'env ctx}
    tGot <- inferM ctx' (substituteExpr env pExpr)
    unify pTy tGot
  pure conclTy
  where
    -- A hypothesis must name a variable of the target expression (a binder).
    bindHypothesis env (hExpr, hTy) =
      case substituteExpr env hExpr of
        Types.EVar v -> pure (v, hTy)
        other -> throw (Types.CustomErr ("expected a variable to bind, got " <> show other))

-- | All type variables mentioned anywhere in a rule.
ruleTypeVars :: Types.TypingRule -> [String]
ruleTypeVars (Types.TypingRule _ premises (_, conclTy)) =
  nub (concatMap freeTypeVars (conclTy : concatMap premiseTypes premises))
  where
    premiseTypes (Types.Premise hyps (_, t)) = t : fmap snd hyps

-- | Apply a type substitution throughout a rule.
renameRule :: Subst -> Types.TypingRule -> Types.TypingRule
renameRule s (Types.TypingRule name premises (ce, ct)) =
  Types.TypingRule name (fmap renamePremise premises) (ce, applySubst s ct)
  where
    renamePremise (Types.Premise hyps j) = Types.Premise (fmap judgment hyps) (judgment j)
    judgment (e, t) = (e, applySubst s t)
