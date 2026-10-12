-- |
-- Module      : Interpreters.Common.Inference
-- Description : Inference and checking with the user's rules
--
-- Inference ('inferM') and checking ('checkM', bidirectional typing) of
-- expressions with the rules of the context. Inference builds a
-- 'Types.Derivation' on success. On failure it reports /every/ independent
-- error, each with the path to the subterm at fault and the premises being
-- checked ('Types.Failure').
module Interpreters.Common.Inference where

import Control.Monad (forM_)
import Data.Either (fromRight)
import Data.List (groupBy, nub)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import Interpreters.Common.InferMonad
import Interpreters.Common.Reduce
import Interpreters.Common.Relations
import Interpreters.Common.Terms
import Interpreters.Common.Unify
import qualified Types

-- | Infer the type of a term in which the parameters are variables of
--   unknown types. Returns the types that the term needs for the
--   parameters, and its own type. The unknown types that remain are named
--   @a@, @b@, … (not the names of the parameters), so they become type
--   variables of a rule.
inferWithParams :: Types.Ctx -> [String] -> Types.Term -> Either Types.Err ([Types.Type], Types.Type)
inferWithParams ctx params body = case runInfer action initialState of
  (st, Right types) -> case reverse (inferFailures st) of
    f : _ -> Left (Types.failure'err f)
    [] -> Right (tidyNames types)
  (_, Left err) -> Left err
  where
    action = do
      paramTypes <- traverse (const fresh) params
      let ctx' = ctx {Types.ctx'env = M.fromList (zip params (fmap (Types.Forall []) paramTypes)) <> Types.ctx'env ctx}
      (t, _) <- inferM ctx' [] body
      settle
      s <- getsI inferSubst
      pure (fmap (forceTerm ctx . applySubst s) paramTypes, forceTerm ctx (applySubst s t))
    tidyNames (paramTypes, t) =
      let types = t : paramTypes
          vars = nub (concatMap freeTypeVars types)
          metas = filter isMeta vars
          taken = params <> filter (not . isMeta) vars
          names = M.fromList (zip metas (fmap Types.TVar (filter (`notElem` taken) prettyNames)))
       in (fmap (applySubst names) paramTypes, applySubst names t)

-- | Look up the declared type of an expression by name, if it exists.
--
--   Returns 'Nothing' if the expression is not defined in the context.
lookupExprType :: Types.Ctx -> String -> Maybe Types.Type
lookupExprType ctx name =
  listToMaybe [t | Types.ExprDecl n t <- Types.ctx'exprs ctx, n == name]

-- | Perform type inference for an expression in the given context.
--
--   The first error, or the inferred type with type variables renamed to
--   @a@, @b@, … (so @Lam(x, x)@ is reported as @Arrow<a, a>@).
infer :: Types.Expr -> Types.Ctx -> Either Types.Err Types.Type
infer e ctx = case inferDetailed ctx e of
  Left failures -> Left (Types.failure'err (NE.head failures))
  Right derivation -> Right (Types.deriv'type derivation)

-- | Infer the type of an expression, returning its derivation, or every
--   independent failure (a failed premise does not stop the others from
--   being checked).
--
--   Type variables are renamed to @a@, @b@, … consistently across the whole
--   derivation, in order of appearance starting with the root's type.
inferDetailed :: Types.Ctx -> Types.Expr -> Either (NonEmpty Types.Failure) Types.Derivation
inferDetailed ctx e = runDetailed ctx (inferM ctx [] e)

-- | Check an expression against a type (bidirectional typing), returning its
--   derivation or every failure, like 'inferDetailed'. The free variables of
--   the type are rigid: the expression must have the type for every value
--   of them.
checkDetailed :: Types.Ctx -> Types.Expr -> Types.Type -> Either (NonEmpty Types.Failure) Types.Derivation
checkDetailed ctx e t = runDetailed ctx (modifyI (\s -> s {inferRigid = freeVars ctx t}) >> checkM ctx [] e t)

-- | The type of an expression: inferred if possible. A term whose rules
--   only check it (e.g. an unannotated @Lam(x, x)@ with a @<=@ rule) is
--   checked against an unknown type instead, so that its checking rule
--   finds the type.
typeOf :: Types.Ctx -> Types.Expr -> Either (NonEmpty Types.Failure) Types.Derivation
typeOf ctx e = case inferDetailed ctx e of
  Left failures
    | any (isCannotInfer . Types.failure'err) failures -> runDetailed ctx (fresh >>= checkM ctx [] e)
  result -> result
  where
    isCannotInfer = \case
      Types.CannotInfer _ -> True
      _ -> False

-- | Run inference from the initial state, and collect its result.
runDetailed :: Types.Ctx -> Infer (Types.Type, Types.Derivation) -> Either (NonEmpty Types.Failure) Types.Derivation
runDetailed ctx action =
  case (nonEmpty failures, result) of
    (Just fs, _) -> Left (fmap normalizeFailure fs)
    (Nothing, Right (_, derivation)) -> Right (normalizeDerivation ctx (inferSubst st) derivation)
    (Nothing, Left err) -> Left (pure (Types.Failure err [] []))
  where
    (st, result) = runInfer (action <* settle) initialState
    -- An error at the root itself is not inside any premise, so nothing
    -- recorded it.
    failures = reverse (inferFailures st) <> either (\err -> [Types.Failure err [] []]) (const []) result

-- | Infer the type of an expression at the given path, extending the
--   substitution. Order:
--     1) Try to match a typing rule and apply it.
--     2) If no rule matches, fall back to local bindings and declared types.
--     3) If neither works, fail with an appropriate error.
--
--   A rule with a checking conclusion (@<=@) cannot infer a type, so it is
--   not used here (see 'checkM').
inferM :: Types.Ctx -> [Int] -> Types.Expr -> Infer (Types.Type, Types.Derivation)
inferM ctx path e =
  case matchingRules ctx e of
    -- Rule found → apply it, if it can infer.
    Right rules -> case filter ((/= Types.Checking) . Types.typingRule'mode) rules of
      rule : _ -> applyRule ctx path e rule Nothing
      [] -> throwErr (Types.CannotInfer e)
    -- No rule matched → try local bindings and declared types.
    Left (Types.NoRuleMatched _) ->
      case e of
        -- Constructor/constant
        Types.ECon name args ->
          case lookupExprType ctx name of
            -- Zero-arg constructors/constants are base cases.
            Just t | null args -> leaf Types.ByDeclaration <$> instantiate ctx t
            -- Has a declared thing but args present and no rule matched → keep the precise error.
            Just _ -> throwErr (Types.NoRuleMatched e)
            -- Unknown constructor symbol altogether.
            Nothing -> throwErr (Types.UnknownExpr name)
        -- Variable: a local binding (e.g. a lambda parameter) shadows declarations.
        -- A generalised binding is instantiated fresh at each use.
        Types.EVar v ->
          case M.lookup v (Types.ctx'env ctx) of
            Just scheme -> countUse v >> (leaf Types.ByAssumption <$> instantiateScheme ctx scheme)
            Nothing -> maybe (throwErr (Types.UnknownExpr v)) (fmap (leaf Types.ByDeclaration) . instantiate ctx) (lookupExprType ctx v)
        -- A substitution written in the expression: apply it, then infer.
        Types.Subst {} -> case forceTerm ctx e of
          Types.Subst {} -> throwErr (Types.StuckSubstitution e)
          e' -> inferM ctx path e'
    -- If rule matching failed for another reason, propagate it.
    Left err ->
      throwErr err
  where
    leaf by t = (t, Types.Derivation e t by [] [])

-- | Check an expression against an expected type at the given path
--   (bidirectional typing). A rule with a checking conclusion (@<=@) is
--   used if there is one: its conclusion type is unified with the expected
--   type before its premises are checked. Otherwise the type is inferred,
--   then unified with the expected type.
checkM :: Types.Ctx -> [Int] -> Types.Expr -> Types.Type -> Infer (Types.Type, Types.Derivation)
checkM ctx path e expected =
  case matchingRules ctx e of
    Right rules
      | rule : _ <- filter ((== Types.Checking) . Types.typingRule'mode) rules ->
          applyRule ctx path e rule (Just expected)
    _ -> do
      (t, derivation) <- inferM ctx path e
      unify ctx expected t
      pure (t, derivation)

-- | The typing rules in the context that can match a given expression,
--   newest first. Checks:
--     • the rule’s conclusion constructor name matches the expression, and
--     • the number of arguments (arity) matches.
--   If no rule matches, returns 'NoRuleMatched' (not 'UnknownExpr' — constants may have no rules).
matchingRules :: Types.Ctx -> Types.Expr -> Either Types.Err [Types.TypingRule]
matchingRules ctx e@(Types.ECon name args) =
  case rulesWithSameName ctx name of
    [] ->
      -- No rules for this constructor; not an unknown symbol (it may be a declared constant).
      Left (Types.NoRuleMatched e)
    rs ->
      case filter ((== length args) . arity) rs of
        matching@(_ : _) -> Right matching
        [] ->
          case listToMaybe (nub (fmap arity rs)) of
            Just expected -> Left (Types.ArityMismatch expected (length args))
            Nothing -> Left (Types.NoRuleMatched e)
matchingRules _ e =
  -- Non-constructor expressions aren’t matched by rule conclusions here.
  Left (Types.NoRuleMatched e)

-- | Get all typing rules in the context that have the same constructor name.
rulesWithSameName :: Types.Ctx -> String -> [Types.TypingRule]
rulesWithSameName ctx name = M.findWithDefault [] name (Types.ctx'rulesByName ctx)

-- | Compute the arity (number of parameters) of a rule’s conclusion.
arity :: Types.TypingRule -> Int
arity r = case fst (Types.typingRule'ruleConclusion r) of
  Types.ECon _ ps -> length ps
  _ -> 0

-- | Instantiate a declared type: each of its type variables becomes fresh,
--   so e.g. @Nothing : Maybe<a>@ can be used at different types.
instantiate :: Types.Ctx -> Types.Type -> Infer Types.Type
instantiate ctx t = do
  renamed <- freshen (freeTypeVars t)
  let t' = applySubst renamed t
  declareScopes ctx (M.elems renamed) [([], t')]
  pure t'

-- | Readable type variable names: @a@, @b@, …, @z@, @a1@, …
prettyNames :: [String]
prettyNames = [c : suffix | suffix <- "" : fmap show [1 :: Int ..], c <- ['a' .. 'z']]

-- | Map the metavariables of some types, in order of first appearance, to
--   'prettyNames'. Names of rigid variables in the types are not used.
renaming :: [Types.Type] -> Subst
renaming ts = M.fromList (zip metas (fmap Types.TVar (filter (`notElem` rigid) prettyNames)))
  where
    vars = nub (concatMap freeTypeVars ts)
    metas = filter isMeta vars
    rigid = filter (not . isMeta) vars

-- | Rename metavariables to @a@, @b@, … in order of appearance.
renameVars :: [Types.Type] -> [Types.Type]
renameVars ts = fmap (applySubst (renaming ts)) ts

normalize :: Types.Type -> Types.Type
normalize t = case renameVars [t] of
  [t'] -> t'
  _ -> t

-- | Give a failure readable type variable names, consistently across its
--   error and its premises (the error's types are named first).
normalizeFailure :: Types.Failure -> Types.Failure
normalizeFailure (Types.Failure err path frames) =
  Types.Failure (errTypes rename err) path (fmap (\f -> f {Types.frame'premise = mapPremise rename (Types.frame'premise f)}) frames)
  where
    rename = applySubst (renaming (errTypeList err <> concatMap (premiseTypeList . Types.frame'premise) frames))
    errTypeList = \case
      Types.Mismatch e g -> [e, g]
      Types.InfiniteType v t -> [v, t]
      Types.StuckSubstitution t -> [t]
      Types.ScopeEscape _ t -> [t]
      _ -> []
    errTypes f = \case
      Types.Mismatch e g -> Types.Mismatch (f e) (f g)
      Types.InfiniteType v t -> Types.InfiniteType (f v) (f t)
      Types.StuckSubstitution t -> Types.StuckSubstitution (f t)
      Types.ScopeEscape x t -> Types.ScopeEscape x (f t)
      other -> other

-- | Apply the final substitution to a whole derivation, then rename type
--   variables consistently, starting with the root's type (so it matches
--   the type that 'infer' reports). The root's type is also put in normal
--   form with the reductions, if that stops within the step limit.
normalizeDerivation :: Types.Ctx -> Subst -> Types.Derivation -> Types.Derivation
normalizeDerivation ctx subst d = mapDerivation (applySubst names) (renameScheme names) solved
  where
    -- Normalise the root before renaming: a renamed metavariable looks rigid,
    -- so a substitution into it would be applied.
    solved = normalRoot (mapDerivation (forceTerm ctx . applySubst subst) id d)
    normalRoot root = root {Types.deriv'type = fromRight (Types.deriv'type root) (normalForm ctx (Types.deriv'type root))}
    names = renaming (derivationTypes solved)
    renameScheme m (Types.Forall vs t) =
      Types.Forall [v' | v <- vs, Types.TVar v' <- [M.findWithDefault (Types.TVar v) v m]] (applySubst m t)

-- | Every type in a derivation, root first, in pre-order.
derivationTypes :: Types.Derivation -> [Types.Type]
derivationTypes (Types.Derivation _ t _ assumptions premises) =
  t : [st | (_, Types.Forall _ st) <- assumptions] <> concatMap derivationTypes premises

-- | Map over the types (and the assumptions' schemes) of a derivation.
mapDerivation :: (Types.Type -> Types.Type) -> (Types.Scheme -> Types.Scheme) -> Types.Derivation -> Types.Derivation
mapDerivation f g (Types.Derivation e t by assumptions premises) =
  Types.Derivation
    e
    (f t)
    by
    [(v, g (mapScheme s)) | (v, s) <- assumptions]
    (fmap (mapDerivation f g) premises)
  where
    mapScheme (Types.Forall vs st) = Types.Forall vs (f st)

-- | Apply a typing rule to an expression at the given path, checking all
--   its premises.
--
--   This function:
--     1. Renames the rule’s type variables to fresh ones.
--     2. Matches the rule’s conclusion pattern against the target expression.
--     3. With an expected type (a checking rule, see 'checkM'), unifies it
--        with the conclusion type.
--     4. For each premise, brings its hypotheses into scope as local bindings
--        (e.g. a lambda’s parameter), substitutes concrete expressions for
--        pattern variables, and checks the subject against the premise’s type
--        (or, for a synthesis premise @=>@, infers its type and unifies it with
--        the premise’s type).
--
--   A premise that fails is recorded (see 'attempt') and the remaining
--   premises are still checked. Returns the conclusion type (the caller
--   applies the substitution) and the derivation.
applyRule :: Types.Ctx -> [Int] -> Types.Expr -> Types.TypingRule -> Maybe Types.Type -> Infer (Types.Type, Types.Derivation)
applyRule ctx path target rule expected = do
  metas <- freshen (ruleTypeVars rule)
  let Types.TypingRule name premises0 (conclExpr, conclTy0) = renameRule metas rule
  env <- fromEither (matchConclusion name conclExpr target)
  -- A type can mention pattern variables (e.g. @Vec(n, a)@, where @n@ is
  -- matched): replace them with the matched expressions.
  let instantiateTerm = substituteExpr (fst <$> env)
      premises = fmap (mapPremise instantiateTerm) premises0
  -- The places of the metavariables, with the names bound around them: a
  -- premise's type is under its hypotheses, the other types are not.
  declareScopes ctx (M.elems metas) $
    concat
      [ fmap (([],) . Types.hyp'type) hyps
          <> [([v | Types.Hypothesis h _ _ <- hyps, Types.EVar v <- [instantiateTerm h]], pTy)]
        | Types.Premise hyps (_, pTy) <- premises
      ]
      <> [([], instantiateTerm conclTy0)]
  -- A checking rule (see 'checkM') learns from the expected type first.
  forM_ expected $ \t -> unify ctx t (instantiateTerm conclTy0)
  -- Premises next to each other that share (written share) use the same
  -- variables in scope; the others split them (see 'Types.premise'share').
  let runPremise premise@(Types.PremiseWith mode hyps (pExpr, pTy)) = do
        let subject = substituteExpr (fst <$> env) pExpr
            subjectPath = case pExpr of
              Types.EVar v | Just (_, rel) <- M.lookup v env -> path <> rel
              _ -> path
        withFrame (Types.Frame name premise subject) . attempt ctx subjectPath $ do
          locals <- traverse (bindHypothesis env) hyps
          let ctx' = ctx {Types.ctx'env = M.fromList locals <> Types.ctx'env ctx}
          -- A relation premise is derived with the rules of its relation. A
          -- synthesis premise infers; the others check against the type.
          derivation <-
            withUsage [(v, Types.hyp'usage h) | ((v, _), h) <- zip locals hyps] $
              if Types.isRelation (pExpr, pTy)
                then deriveRelation ctx pTy
                else case mode of
                  Types.Synthesis -> do
                    (tGot, derivation) <- inferM ctx' subjectPath subject
                    unify ctx pTy tGot
                    pure derivation
                  _ -> snd <$> checkM ctx' subjectPath subject pTy
          pure derivation {Types.deriv'assumptions = locals}
      runGroup = \case
        [p] -> pure <$> runPremise p
        ps -> sharing (fmap runPremise ps)
  children <- concat <$> traverse runGroup (groupBy (\a b -> Types.premise'share a && Types.premise'share b) premises)
  -- The premises have solved the metavariables, so substitutions like
  -- @u[x := a]@ in the conclusion can now be applied.
  s <- getsI inferSubst
  let conclTy = forceTerm ctx (applySubst s (instantiateTerm conclTy0))
  pure (conclTy, Types.Derivation target conclTy (Types.ByRule name) [] (catMaybes children))
  where
    -- A hypothesis must name a variable of the target expression (a binder).
    -- Its type is generalised against the enclosing context if marked @gen@.
    bindHypothesis env (Types.Hypothesis hExpr hTy gen) =
      case substituteExpr (fst <$> env) hExpr of
        Types.EVar v -> (v,) <$> if gen then generalize ctx hTy else pure (Types.Forall [] hTy)
        other -> throwErr (Types.CustomErr ("expected a variable to bind, got " <> show other))

-- | Generalise a type over the type variables that are not free in the
--   context (Hindley–Milner’s @gen(Γ, τ)@), under the current substitution.
--
--   Variables free in Γ belong to enclosing binders (e.g. a lambda’s
--   parameter) and must stay shared, so they are not quantified.
generalize :: Types.Ctx -> Types.Type -> Infer Types.Scheme
generalize ctx t = do
  s <- getsI inferSubst
  let t' = applySubst s t
      envVars = concatMap (schemeFreeVars s) (M.elems (Types.ctx'env ctx))
  pure (Types.Forall (filter (`notElem` envVars) (metaVars t')) t')
  where
    schemeFreeVars s (Types.Forall vs ty) = filter (`notElem` vs) (metaVars (applySubst s ty))

-- | Instantiate a type scheme: its quantified variables become fresh.
instantiateScheme :: Types.Ctx -> Types.Scheme -> Infer Types.Type
instantiateScheme ctx (Types.Forall vs t) = do
  renamed <- freshen vs
  let t' = applySubst renamed t
  declareScopes ctx (M.elems renamed) [([], t')]
  pure t'
