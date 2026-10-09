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
-- * "Interpreters.IO" — interpreters that print results.
--
-- All these interpreters delegate to the functions in this module to perform
-- the actual typechecking logic.  This separation keeps the **semantics**
-- independent of the **execution environment** (pure, IO, traced, etc.).
--
-- Inference ('inferDetailed') builds a 'Types.Derivation' on success. On
-- failure it reports /every/ independent error, each with the path to the
-- subterm at fault and the premises being checked ('Types.Failure').
module Interpreters.Common.Actions where

import Control.Monad (ap, foldM, forM, unless)
import Data.Either (isLeft)
import Data.Foldable (find)
import Data.List (intercalate, minimumBy, nub)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import Polysemy (Member, Sem)
import Polysemy.State (State, get, modify)
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

-- | Validate a definition, then add it to the context unless that found an
--   error. Returns the diagnostics either way.
defineWith ::
  (Member (State Types.Ctx) r) =>
  (Types.Ctx -> a -> [Types.Diagnostic]) ->
  (a -> OneOf3 Types.TypeDecl Types.ExprDecl Types.TypingRule) ->
  a ->
  Sem r [Types.Diagnostic]
defineWith validate wrap x = do
  ctx <- get
  let diagnostics = validate ctx x
  unless (hasErrors diagnostics) (insertIntoCtx (wrap x))
  pure diagnostics

-- | Define a new type declaration in the current context.
--
--   Adds a new 'TypeDecl' to 'ctx'types', unless it is malformed.
defineType :: (Member (State Types.Ctx) r) => Types.TypeDecl -> Sem r [Types.Diagnostic]
defineType = defineWith validateTypeDecl OneOf3_1

-- | Define a new expression and its associated type.
--
--   Adds a new 'ExprDecl' to 'ctx'exprs', unless its type uses an undeclared
--   type constructor or the wrong number of type arguments.
defineExpr :: (Member (State Types.Ctx) r) => Types.ExprDecl -> Sem r [Types.Diagnostic]
defineExpr = defineWith (\ctx (Types.ExprDecl _ t) -> validateType ctx t) OneOf3_2

-- | Define a new typing rule for inference.
--
--   Adds a 'TypingRule' to 'ctx'rules', unless it is malformed (see
--   'validateRule').
defineRule :: (Member (State Types.Ctx) r) => Types.TypingRule -> Sem r [Types.Diagnostic]
defineRule = defineWith validateRule OneOf3_3

--------------------------------------------------------------------------------

-- | Validating definitions

--------------------------------------------------------------------------------

hasErrors :: [Types.Diagnostic] -> Bool
hasErrors = any ((== Types.SevError) . Types.diag'severity)

errorDiag :: String -> Types.Diagnostic
errorDiag = Types.Diagnostic Types.SevError

-- | A type declaration's parameters must be distinct type variables.
validateTypeDecl :: Types.Ctx -> Types.TypeDecl -> [Types.Diagnostic]
validateTypeDecl _ (Types.TypeDecl name params) =
  [ errorDiag ("type " <> name <> " lists the parameter " <> p <> " more than once")
    | p <- duplicates params
  ]
    <> [ errorDiag ("the parameters of type " <> name <> " must be lowercase type variables, but " <> p <> " is not")
         | p@(c : _) <- params,
           c `notElem` ['a' .. 'z']
       ]

-- | Every type constructor in a type must be declared, and applied to as many
--   arguments as its declaration has parameters. Type variables are free.
validateType :: Types.Ctx -> Types.Type -> [Types.Diagnostic]
validateType ctx = nub . go
  where
    go = \case
      Types.TVar _ -> []
      Types.TCon name args -> constructor name args <> concatMap go args

    constructor name args = case lookupTypeDecl ctx name of
      Nothing -> [errorDiag ("undeclared type " <> name <> hint name (length args))]
      Just (Types.TypeDecl _ params)
        | length params /= length args ->
            [errorDiag (name <> " takes " <> count (length params) "type argument" <> ", but is given " <> show (length args))]
      Just _ -> []

    declared = Types.typeDecl'name <$> Types.ctx'types ctx
    hint name n = case closest name declared of
      Just suggestion -> " (did you mean " <> suggestion <> "?)"
      Nothing -> " (declare it first: type " <> name <> paramList n <> ")"
    paramList 0 = ""
    paramList n = "<" <> intercalate ", " (take n prettyNames) <> ">"

-- | A rule is well formed when:
--
--   * its conclusion is a constructor pattern, like @Add(x, y)@;
--   * every variable its premises mention also appears in that conclusion
--     (otherwise the premise could never be checked);
--   * every type it mentions is declared and correctly applied.
validateRule :: Types.Ctx -> Types.TypingRule -> [Types.Diagnostic]
validateRule ctx (Types.TypingRule name premises (conclusion, conclusionType)) =
  shape <> unbound <> nub (concatMap (validateType ctx) types)
  where
    shape = case conclusion of
      Types.ECon _ _ -> []
      _ -> [errorDiag ("the conclusion of rule " <> name <> " must be a constructor applied to its parts, like " <> name <> "(x, y)")]
    bound = exprVars conclusion
    mentioned = nub (concatMap premiseVars premises)
    premiseVars (Types.Premise hyps (e, _)) = concatMap (exprVars . Types.hyp'var) hyps <> exprVars e
    unbound =
      [ errorDiag ("premise variable " <> v <> " does not appear in the conclusion " <> show conclusion)
        | v <- mentioned,
          v `notElem` bound
      ]
    types = conclusionType : concatMap premiseTypes premises
    premiseTypes (Types.Premise hyps (_, t)) = t : fmap Types.hyp'type hyps

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

lookupTypeDecl :: Types.Ctx -> String -> Maybe Types.TypeDecl
lookupTypeDecl ctx name = find ((== name) . Types.typeDecl'name) (Types.ctx'types ctx)

-- | The closest name within a small edit distance, for "did you mean …?".
closest :: String -> [String] -> Maybe String
closest name candidates =
  case [(d, c) | c <- nub candidates, let d = editDistance name c, d <= max 1 (length name `div` 3), c /= name] of
    [] -> Nothing
    scored -> Just (snd (minimumBy (comparing fst) scored))

-- | Levenshtein distance.
editDistance :: String -> String -> Int
editDistance a b = last (foldl step [0 .. length a] b)
  where
    step prev@(p : ps) c = scanl compute (p + 1) (zip3 a prev ps)
      where
        compute left (ca, diag, up) = minimum [up + 1, left + 1, diag + fromEnum (ca /= c)]
    step [] _ = []

count :: Int -> String -> String
count n word = show n <> " " <> word <> (if n == 1 then "" else "s")

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
inferDetailed ctx e =
  case (nonEmpty failures, result) of
    (Just fs, _) -> Left (fmap normalizeFailure fs)
    (Nothing, Right (_, derivation)) -> Right (normalizeDerivation (inferSubst st) derivation)
    (Nothing, Left err) -> Left (pure (Types.Failure err [] []))
  where
    (st, result) = runInfer (inferM ctx [] e) (InferState 0 M.empty [] [])
    -- An error at the root itself is not inside any premise, so nothing
    -- recorded it.
    failures = reverse (inferFailures st) <> either (\err -> [Types.Failure err [] []]) (const []) result

-- | Infer the type of an expression at the given path, extending the
--   substitution. Order:
--     1) Try to match a typing rule and apply it.
--     2) If no rule matches, fall back to local bindings and declared types.
--     3) If neither works, fail with an appropriate error.
inferM :: Types.Ctx -> [Int] -> Types.Expr -> Infer (Types.Type, Types.Derivation)
inferM ctx path e =
  case matchRule ctx e of
    -- Rule found → apply it.
    Right rule ->
      applyRule ctx path e rule
    -- No rule matched → try local bindings and declared types.
    Left (Types.NoRuleMatched _) ->
      case e of
        -- Constructor/constant
        Types.ECon name args ->
          case lookupExprType ctx name of
            -- Zero-arg constructors/constants are base cases.
            Just t | null args -> leaf Types.ByDeclaration <$> instantiate t
            -- Has a declared thing but args present and no rule matched → keep the precise error.
            Just _ -> throwErr (Types.NoRuleMatched e)
            -- Unknown constructor symbol altogether.
            Nothing -> throwErr (Types.UnknownExpr name)
        -- Variable: a local binding (e.g. a lambda parameter) shadows declarations.
        -- A generalised binding is instantiated fresh at each use.
        Types.EVar v ->
          case M.lookup v (Types.ctx'env ctx) of
            Just scheme -> leaf Types.ByAssumption <$> instantiateScheme scheme
            Nothing -> maybe (throwErr (Types.UnknownExpr v)) (fmap (leaf Types.ByDeclaration) . instantiate) (lookupExprType ctx v)
    -- If rule matching failed for another reason, propagate it.
    Left err ->
      throwErr err
  where
    leaf by t = (t, Types.Derivation e t by [] [])

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

-- | Match a rule’s conclusion pattern against a target expression.
--
--   If successful, returns an environment mapping each pattern variable to
--   the actual subexpression and its path within the target. Otherwise a
--   descriptive error (such as 'ArityMismatch' or 'NoRuleMatched').
matchConclusion :: String -> Types.Expr -> Types.Expr -> Either Types.Err (Map String (Types.Expr, [Int]))
matchConclusion ruleName pattern target = go [] M.empty pattern target
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
              "rule " <> ruleName <> " needs every " <> var <> " in " <> show pattern <> " to be the same expression"
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
  Types.ECon n args -> Types.ECon n (fmap (substituteExpr env) args)

--------------------------------------------------------------------------------

-- | The inference monad

--------------------------------------------------------------------------------

-- | A substitution from type variable names to types.
type Subst = Map String Types.Type

-- | State threaded through inference.
data InferState = InferState
  { -- | Supply of fresh type variable names.
    inferSupply :: Int,
    -- | The substitution solved so far.
    inferSubst :: Subst,
    -- | The premises being checked, innermost first.
    inferFrames :: [Types.Frame],
    -- | Failures recorded so far, most recent first.
    inferFailures :: [Types.Failure]
  }

-- | State plus errors, where the state is /kept/ when an error is caught,
--   so failures recorded inside a failed region are not lost.
newtype Infer a = Infer {runInfer :: InferState -> (InferState, Either Types.Err a)}

instance Functor Infer where
  fmap f (Infer g) = Infer (fmap (fmap f) . g)

instance Applicative Infer where
  pure a = Infer (,Right a)
  (<*>) = ap

instance Monad Infer where
  Infer g >>= k = Infer $ \s -> case g s of
    (s', Left err) -> (s', Left err)
    (s', Right a) -> runInfer (k a) s'

throwErr :: Types.Err -> Infer a
throwErr err = Infer (,Left err)

catchErr :: Infer a -> (Types.Err -> Infer a) -> Infer a
catchErr (Infer g) handler = Infer $ \s -> case g s of
  (s', Left err) -> runInfer (handler err) s'
  ok -> ok

fromEither :: Either Types.Err a -> Infer a
fromEither = either throwErr pure

getsI :: (InferState -> a) -> Infer a
getsI f = Infer (\s -> (s, Right (f s)))

modifyI :: (InferState -> InferState) -> Infer ()
modifyI f = Infer (\s -> (f s, Right ()))

-- | Run an action while checking a premise, so failures report it.
withFrame :: Types.Frame -> Infer a -> Infer a
withFrame frame action = do
  modifyI (\s -> s {inferFrames = frame : inferFrames s})
  result <- action `catchErr` \err -> pop >> throwErr err
  pop
  pure result
  where
    pop = modifyI (\s -> s {inferFrames = drop 1 (inferFrames s)})

-- | Run an action; if it fails, record the failure (at the given path, with
--   the current premises) and carry on with 'Nothing'.
attempt :: [Int] -> Infer a -> Infer (Maybe a)
attempt path action =
  (Just <$> action) `catchErr` \err -> do
    subst <- getsI inferSubst
    frames <- getsI inferFrames
    -- Show each premise with the types known when it failed.
    let frames' = fmap (\f -> f {Types.frame'premise = substPremise subst (Types.frame'premise f)}) frames
    modifyI (\s -> s {inferFailures = Types.Failure err path frames' : inferFailures s})
    pure Nothing

--------------------------------------------------------------------------------

-- | Type variables and unification

--------------------------------------------------------------------------------

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
fresh :: Infer Types.Type
fresh = do
  n <- getsI inferSupply
  modifyI (\st -> st {inferSupply = n + 1})
  pure (Types.TVar ('\'' : show n))

-- | Replace the given type variables with fresh ones.
freshen :: [String] -> Infer Subst
freshen vs = M.fromList . zip vs <$> traverse (const fresh) vs

-- | Instantiate a declared type: each of its type variables becomes fresh,
--   so e.g. @Nothing : Maybe<a>@ can be used at different types.
instantiate :: Types.Type -> Infer Types.Type
instantiate t = (`applySubst` t) <$> freshen (freeTypeVars t)

-- | Unify an expected type with an inferred one, extending the substitution.
unify :: Types.Type -> Types.Type -> Infer ()
unify expected got = do
  s <- getsI inferSubst
  case (applySubst s expected, applySubst s got) of
    (Types.TVar a, Types.TVar b) | a == b -> pure ()
    (Types.TVar a, t) -> bind a t
    (t, Types.TVar a) -> bind a t
    (Types.TCon en ets, Types.TCon gn gts)
      | en == gn && length ets == length gts -> sequence_ (zipWith unify ets gts)
    (e, g) -> throwErr (Types.Mismatch e g)
  where
    bind a t
      | a `elem` freeTypeVars t = throwErr (Types.InfiniteType (Types.TVar a) t)
      | otherwise =
          modifyI $ \st ->
            st {inferSubst = M.insert a t (applySubst (M.singleton a t) <$> inferSubst st)}

-- | Readable type variable names: @a@, @b@, …, @z@, @a1@, …
prettyNames :: [String]
prettyNames = [c : suffix | suffix <- "" : fmap show [1 :: Int ..], c <- ['a' .. 'z']]

-- | Map the type variables of some types, in order of first appearance, to
--   'prettyNames'.
renaming :: [Types.Type] -> Subst
renaming ts = M.fromList (zip (nub (concatMap freeTypeVars ts)) (fmap Types.TVar prettyNames))

-- | Rename type variables to @a@, @b@, … in order of appearance.
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
      _ -> []
    errTypes f = \case
      Types.Mismatch e g -> Types.Mismatch (f e) (f g)
      Types.InfiniteType v t -> Types.InfiniteType (f v) (f t)
      other -> other

-- | Apply the final substitution to a whole derivation, then rename type
--   variables consistently, starting with the root's type (so it matches
--   the type that 'infer' reports).
normalizeDerivation :: Subst -> Types.Derivation -> Types.Derivation
normalizeDerivation subst d = mapDerivation (applySubst names) (renameScheme names) solved
  where
    solved = mapDerivation (applySubst subst) id d
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

substPremise :: Subst -> Types.Premise -> Types.Premise
substPremise s = mapPremise (applySubst s)

mapPremise :: (Types.Type -> Types.Type) -> Types.Premise -> Types.Premise
mapPremise f (Types.Premise hyps (e, t)) =
  Types.Premise (fmap (\h -> h {Types.hyp'type = f (Types.hyp'type h)}) hyps) (e, f t)

premiseTypeList :: Types.Premise -> [Types.Type]
premiseTypeList (Types.Premise hyps (_, t)) = fmap Types.hyp'type hyps <> [t]

--------------------------------------------------------------------------------

-- | Applying rules

--------------------------------------------------------------------------------

-- | Apply a typing rule to an expression at the given path, checking all
--   its premises.
--
--   This function:
--     1. Renames the rule’s type variables to fresh ones.
--     2. Matches the rule’s conclusion pattern against the target expression.
--     3. For each premise, brings its hypotheses into scope as local bindings
--        (e.g. a lambda’s parameter), substitutes concrete expressions for
--        pattern variables, infers the premise’s type, and unifies it with
--        the expected type.
--
--   A premise that fails is recorded (see 'attempt') and the remaining
--   premises are still checked. Returns the conclusion type (the caller
--   applies the substitution) and the derivation.
applyRule :: Types.Ctx -> [Int] -> Types.Expr -> Types.TypingRule -> Infer (Types.Type, Types.Derivation)
applyRule ctx path target rule = do
  renamed <- (`renameRule` rule) <$> freshen (ruleTypeVars rule)
  let Types.TypingRule name premises (conclExpr, conclTy) = renamed
  env <- fromEither (matchConclusion name conclExpr target)
  children <- forM premises $ \premise@(Types.Premise hyps (pExpr, pTy)) -> do
    let subject = substituteExpr (fst <$> env) pExpr
        subjectPath = case pExpr of
          Types.EVar v | Just (_, rel) <- M.lookup v env -> path <> rel
          _ -> path
    withFrame (Types.Frame name premise subject) . attempt subjectPath $ do
      locals <- traverse (bindHypothesis env) hyps
      let ctx' = ctx {Types.ctx'env = M.fromList locals <> Types.ctx'env ctx}
      (tGot, derivation) <- inferM ctx' subjectPath subject
      unify pTy tGot
      pure derivation {Types.deriv'assumptions = locals}
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
  pure (Types.Forall (filter (`notElem` envVars) (freeTypeVars t')) t')
  where
    schemeFreeVars s (Types.Forall vs ty) = filter (`notElem` vs) (freeTypeVars (applySubst s ty))

-- | Instantiate a type scheme: its quantified variables become fresh.
instantiateScheme :: Types.Scheme -> Infer Types.Type
instantiateScheme (Types.Forall vs t) = (`applySubst` t) <$> freshen vs

-- | All type variables mentioned anywhere in a rule.
ruleTypeVars :: Types.TypingRule -> [String]
ruleTypeVars (Types.TypingRule _ premises (_, conclTy)) =
  nub (concatMap freeTypeVars (conclTy : concatMap premiseTypeList premises))

-- | Apply a type substitution throughout a rule.
renameRule :: Subst -> Types.TypingRule -> Types.TypingRule
renameRule s (Types.TypingRule name premises (ce, ct)) =
  Types.TypingRule name (fmap (substPremise s) premises) (ce, applySubst s ct)

--------------------------------------------------------------------------------

-- | Expectations

--------------------------------------------------------------------------------

-- | Check an expectation. Returns diagnostics about the expected type (an
--   undeclared type makes the expectation unmet), the inference result, and
--   whether the expectation is met.
--
--   @check e : T@ compares types up to renaming of type variables, so
--   @check Lam(x, x) : Arrow<b, b>@ is met by @Arrow<a, a>@.
checkExpectation ::
  Types.Ctx ->
  Types.Expectation ->
  ([Types.Diagnostic], Either (NonEmpty Types.Failure) Types.Derivation, Bool)
checkExpectation ctx = \case
  Types.ExpectType e t ->
    let diagnostics = validateType ctx t
        result = inferDetailed ctx e
        met = not (hasErrors diagnostics) && either (const False) ((== normalize t) . normalize . Types.deriv'type) result
     in (diagnostics, result, met)
  Types.ExpectFailure e ->
    let result = inferDetailed ctx e
     in ([], result, isLeft result)
