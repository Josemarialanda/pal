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

import Control.Monad (ap, foldM, forM, forM_, unless, when)
import Data.Either (fromRight)
import Data.Foldable (find)
import Data.List (elemIndex, intercalate, intersect, minimumBy, nub, stripPrefix)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Polysemy (Member, Sem)
import Polysemy.State (State, get, modify)
import qualified Types
import Utils.OneOfN (OneOf5 (..))

--------------------------------------------------------------------------------

-- | Definition actions (context mutation)

--------------------------------------------------------------------------------

-- | A definition of any of the five supported kinds (the last is a value pattern).
type Definition = OneOf5 Types.TypeDecl Types.ExprDecl Types.TypingRule Types.Reduction Types.Term

-- | Insert a definition into the context.
insertDefinition :: Definition -> Types.Ctx -> Types.Ctx
insertDefinition = \case
  OneOf5_1 td -> \ctx -> ctx {Types.ctx'types = td : Types.ctx'types ctx}
  OneOf5_2 ed -> \ctx -> ctx {Types.ctx'exprs = ed : Types.ctx'exprs ctx}
  OneOf5_3 tr -> \ctx -> ctx {Types.ctx'rules = tr : Types.ctx'rules ctx}
  OneOf5_4 rd -> \ctx -> ctx {Types.ctx'reductions = rd : Types.ctx'reductions ctx}
  OneOf5_5 v -> \ctx -> ctx {Types.ctx'values = v : Types.ctx'values ctx}

-- | Insert a definition into the current context.
insertIntoCtx :: (Member (State Types.Ctx) r) => Definition -> Sem r ()
insertIntoCtx = modify . insertDefinition

-- | Validate a definition, then add it to the context unless that found an
--   error. Returns the diagnostics either way.
defineWith ::
  (Member (State Types.Ctx) r) =>
  (Types.Ctx -> a -> [Types.Diagnostic]) ->
  (a -> Definition) ->
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
defineType = defineWith validateTypeDecl OneOf5_1

-- | Define a new expression and its associated type.
--
--   Adds a new 'ExprDecl' to 'ctx'exprs', unless its type uses an undeclared
--   constructor or the wrong number of arguments. The type may mention the
--   expression itself (e.g. @expr Type : Type@).
defineExpr :: (Member (State Types.Ctx) r) => Types.ExprDecl -> Sem r [Types.Diagnostic]
defineExpr = defineWith (\ctx ed@(Types.ExprDecl _ t) -> validateType (insertDefinition (OneOf5_2 ed) ctx) t) OneOf5_2

-- | Define a new typing rule for inference.
--
--   Adds a 'TypingRule' to 'ctx'rules', unless it is malformed (see
--   'validateRule').
defineRule :: (Member (State Types.Ctx) r) => Types.TypingRule -> Sem r [Types.Diagnostic]
defineRule = defineWith validateRule OneOf5_3

-- | Define a new reduction.
--
--   Adds a 'Reduction' to 'ctx'reductions', unless it is malformed (see
--   'validateReduction').
defineReduction :: (Member (State Types.Ctx) r) => Types.Reduction -> Sem r [Types.Diagnostic]
defineReduction = defineWith validateReduction OneOf5_4

-- | Define a value pattern, for the progress test.
--
--   Adds the pattern to 'ctx'values', unless it is malformed (see
--   'validateValue').
defineValue :: (Member (State Types.Ctx) r) => Types.Term -> Sem r [Types.Diagnostic]
defineValue = defineWith validateValue OneOf5_5

-- | Define a name for a term (@def Name := body@, or @def Name(x, y) := body@
--   with parameters).
--
--   The body must have a type, and the name must be new. Without parameters,
--   the name is added as a constant of the body's type ('ctx'exprs'). With
--   parameters, it gets a typing rule: each parameter has the type that the
--   body needs for it (see 'inferWithParams'). Either way, a reduction goes
--   from the name (applied to the parameters) to the body.
defineTerm :: (Member (State Types.Ctx) r) => Types.TermDef -> Sem r [Types.Diagnostic]
defineTerm def@(Types.TermDef name params body) = do
  ctx <- get
  case validateTermDef ctx def of
    Left diagnostics -> pure diagnostics
    Right (paramTypes, t) -> do
      let pattern' = Types.ECon name (fmap Types.Var params)
      modify (insertDefinition (OneOf5_4 (Types.Reduction name pattern' body)))
      if null params
        then modify (insertDefinition (OneOf5_2 (Types.ExprDecl name t)))
        else modify (insertDefinition (OneOf5_3 (Types.TypingRule name (zipWith (Types.premise . Types.Var) params paramTypes) (pattern', t))))
      pure []

-- | The types of a definition's parameters and of its body, or why the
--   definition is rejected.
validateTermDef :: Types.Ctx -> Types.TermDef -> Either [Types.Diagnostic] ([Types.Type], Types.Type)
validateTermDef ctx (Types.TermDef name params body)
  | name `elem` declared = Left [errorDiag (name <> " is already declared")]
  | p : _ <- duplicates params = Left [errorDiag ("definition " <> name <> " lists the parameter " <> p <> " more than once")]
  | otherwise = case inferWithParams ctx params body of
      Right types -> Right types
      Left err -> Left [errorDiag ("the body of " <> name <> " has no type: " <> errText err)]
  where
    declared =
      fmap Types.typeDecl'name (Types.ctx'types ctx)
        <> fmap Types.exprDecl'name (Types.ctx'exprs ctx)
        <> [n | Types.TypingRule _ _ (Types.ECon n _, _) <- Types.ctx'rules ctx]
    errText err = let s = show err in fromMaybe s (stripPrefix "[Error] " s)

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

-- | Every constructor in a type must be declared, and applied to as many
--   arguments as its declaration takes. Because a type can mention
--   expressions (e.g. @Vec(Succ(n), a)@), a constructor is declared by a
--   @type@, by an @expr@ (no arguments) or by the conclusion of a rule.
--   Variables are free.
validateType :: Types.Ctx -> Types.Type -> [Types.Diagnostic]
validateType ctx = validateTerm ctx True

-- | Like 'validateType'. The flag tells whether the term is in type
--   position, which changes only the message for an undeclared
--   constructor. The top of a type and the arguments of a @type@ are in
--   type position. The arguments of a constructor of terms (e.g. @Succ@ in
--   @Vec(Succ(n), a)@) and the sides of a reduction are not.
validateTerm :: Types.Ctx -> Bool -> Types.Term -> [Types.Diagnostic]
validateTerm ctx inType0 = nub . go inType0
  where
    go inType = \case
      Types.TVar _ -> []
      Types.TCon name args -> constructor inType name args <> concatMap (go (isTypeDecl name)) args
      Types.Subst body x e -> go inType body <> go inType x <> go inType e

    isTypeDecl = isJust . lookupTypeDecl ctx

    constructor inType name args = case lookupTypeDecl ctx name of
      Just (Types.TypeDecl _ params)
        | length params /= length args ->
            [errorDiag (name <> " takes " <> count (length params) "type argument" <> ", but is given " <> show (length args))]
        | otherwise -> []
      Nothing -> case termArities ctx name of
        []
          | inType -> [errorDiag ("undeclared type " <> name <> hint name (typeHint name (length args)))]
          | otherwise -> [errorDiag ("undeclared constructor " <> name <> hint name (termHint name (length args)))]
        arities
          | length args `elem` arities -> []
          | otherwise ->
              [errorDiag (name <> " takes " <> intercalate " or " (fmap (`count` "argument") arities) <> ", but is given " <> show (length args))]

    declared =
      fmap Types.typeDecl'name (Types.ctx'types ctx)
        <> fmap Types.exprDecl'name (Types.ctx'exprs ctx)
        <> [n | Types.TypingRule _ _ (Types.ECon n _, _) <- Types.ctx'rules ctx]
    hint name declareIt = case closest name declared of
      Just suggestion -> " (did you mean " <> suggestion <> "?)"
      Nothing -> " (" <> declareIt <> ")"
    typeHint name n = "declare it first: type " <> name <> paramList "<" ">" n
    termHint name 0 = "declare it first with expr " <> name <> " : T, or with a rule for " <> name
    termHint name n = "declare it first with a rule for " <> name <> paramList "(" ")" n
    paramList _ _ 0 = ""
    paramList open close n = open <> intercalate ", " (take n prettyNames) <> close

-- | A rule is well formed when:
--
--   * its conclusion is a constructor pattern, like @Add(x, y)@;
--   * every variable its premises mention also appears in that conclusion
--     (otherwise the premise could never be checked);
--   * every type it mentions is declared and correctly applied.
validateRule :: Types.Ctx -> Types.TypingRule -> [Types.Diagnostic]
validateRule ctx rule@(Types.TypingRule name premises (conclusion, conclusionType)) =
  shape <> unbound <> nub (concatMap (validateType ctx') types) <> overlappingRules ctx rule
  where
    -- The rule's types may mention its own constructor.
    ctx' = insertDefinition (OneOf5_3 rule) ctx
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

-- | A reduction is well formed when:
--
--   * its left side is a constructor pattern, like @App(Lam(x, t, b), a)@,
--     without substitutions;
--   * every variable of its right side also appears in its left side;
--   * every constructor it mentions is declared and correctly applied.
validateReduction :: Types.Ctx -> Types.Reduction -> [Types.Diagnostic]
validateReduction ctx (Types.Reduction name lhs rhs) =
  shape <> unbound <> nub (concatMap (validateTerm ctx False) [lhs, rhs]) <> overlappingReductions ctx lhs name
  where
    shape = case lhs of
      Types.ECon _ _
        | hasSubst lhs -> [errorDiag ("the left side of reduction " <> name <> " cannot contain a substitution")]
        | otherwise -> []
      _ -> [errorDiag ("the left side of reduction " <> name <> " must be a constructor applied to its parts, like F(x, y)")]
    unbound =
      [ errorDiag ("variable " <> v <> " of reduction " <> name <> " does not appear in its left side " <> show lhs)
        | v <- exprVars rhs,
          v `notElem` exprVars lhs
      ]

-- | A warning for each earlier rule that a new rule hides. PAL uses only
--   the newest rule for a constructor and a number of arguments, in each
--   direction: a checking rule (@<=@) and a rule that infers can be used
--   together.
overlappingRules :: Types.Ctx -> Types.TypingRule -> [Types.Diagnostic]
overlappingRules ctx rule@(Types.TypingRule name _ (conclusion, _)) = case conclusion of
  Types.ECon c ps ->
    [ warningDiag ("rule " <> name <> " hides rule " <> Types.typingRule'name old <> ": both are for " <> c <> " with " <> count (length ps) "argument" <> ", and PAL uses only the newest")
      | old <- rulesWithSameName ctx c,
        arity old == length ps,
        checks old == checks rule
    ]
  _ -> []
  where
    checks r = Types.typingRule'mode r == Types.Checking

-- | A warning for each earlier reduction whose left side can match the
--   same term as the new one. PAL uses the first reduction that matches,
--   so the new one does not apply to those terms.
overlappingReductions :: Types.Ctx -> Types.Term -> String -> [Types.Diagnostic]
overlappingReductions ctx lhs name =
  [ warningDiag ("reduction " <> name <> " overlaps reduction " <> old <> ": a term can match both left sides, and PAL uses " <> old <> ", the first")
    | Types.Reduction old lhs' _ <- reverse (Types.ctx'reductions ctx),
      patternsOverlap (rename "1" lhs') (rename "2" lhs)
  ]
  where
    -- Keep the variables of the two patterns apart.
    rename suffix = substituteExpr (M.fromList [(v, Types.Var (v <> "'" <> suffix)) | v <- exprVars lhs <> concatMap (\(Types.Reduction _ l _) -> exprVars l) (Types.ctx'reductions ctx)])

-- | Can two patterns match the same term? (First-order unification of the
--   patterns.)
patternsOverlap :: Types.Term -> Types.Term -> Bool
patternsOverlap a0 b0 = isJust (go M.empty [(a0, b0)])
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

warningDiag :: String -> Types.Diagnostic
warningDiag = Types.Diagnostic Types.SevWarning

-- | Does a term contain a substitution?
hasSubst :: Types.Term -> Bool
hasSubst = \case
  Types.Subst {} -> True
  Types.ECon _ ts -> any hasSubst ts
  Types.EVar _ -> False

-- | A value pattern is well formed when it is a constructor pattern, like
--   @Succ(n)@, without substitutions, and every constructor in it is
--   declared and correctly applied.
validateValue :: Types.Ctx -> Types.Term -> [Types.Diagnostic]
validateValue ctx pat = shape <> validateTerm ctx False pat
  where
    shape = case pat of
      Types.ECon _ _
        | hasSubst pat -> [errorDiag ("the value pattern " <> show pat <> " cannot contain a substitution")]
        | otherwise -> []
      _ -> [errorDiag ("the value pattern " <> show pat <> " must be a constructor applied to its parts, like Succ(n)")]

-- | The numbers of arguments that a constructor of terms takes: none for an
--   @expr@, and the arity of each rule for it.
termArities :: Types.Ctx -> String -> [Int]
termArities ctx name =
  nub $
    [0 | Types.ExprDecl n _ <- Types.ctx'exprs ctx, n == name]
      <> fmap arity (rulesWithSameName ctx name)

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
inferDetailed ctx e = runDetailed ctx (inferM ctx [] e)

-- | Check an expression against a type (bidirectional typing), returning its
--   derivation or every failure, like 'inferDetailed'. The free variables of
--   the type are rigid: the expression must have the type for every value
--   of them.
checkDetailed :: Types.Ctx -> Types.Expr -> Types.Type -> Either (NonEmpty Types.Failure) Types.Derivation
checkDetailed ctx e t = runDetailed ctx (modifyI (\s -> s {inferRigid = freeVars ctx t}) >> checkM ctx [] e t)

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
            Just scheme -> leaf Types.ByAssumption <$> instantiateScheme ctx scheme
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

--------------------------------------------------------------------------------

-- | Rule matching and validation

--------------------------------------------------------------------------------

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
    -- | The path of the subterm being checked.
    inferPath :: [Int],
    -- | Failures recorded so far, most recent first.
    inferFailures :: [Types.Failure],
    -- | Unifications that wait for a metavariable, most recent first.
    inferPostponed :: [Postponed],
    -- | For each metavariable, the rigid variables that its solution can
    --   mention (see 'scopeOf'). A name in a scope can be a metavariable
    --   that stands for the name of a binder.
    inferScopes :: Map String [String],
    -- | Rigid variables that every metavariable can mention: the free
    --   variables of the type that a top-level @check@ checks against.
    inferRigid :: [String]
  }

-- | The state at the start of inference.
initialState :: InferState
initialState = InferState 0 M.empty [] [] [] [] M.empty []

-- | A unification that waits for a metavariable: one side is a
--   substitution @u[x := e]@ whose body @u@ is not solved yet. It is tried
--   again when a premise has been checked, and at the end of inference.
data Postponed = Postponed
  { postponed'ctx :: Types.Ctx,
    -- | The binders around it (see 'unifyUnder').
    postponed'bound :: [(String, String)],
    postponed'expected :: Types.Type,
    postponed'got :: Types.Type,
    -- | The stuck substitution, for the error if it stays stuck.
    postponed'stuck :: Types.Term,
    -- | Where it was postponed, for the error if it fails later.
    postponed'path :: [Int],
    postponed'frames :: [Types.Frame]
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
--
--   Then try the postponed unifications again, because the action can have
--   solved a metavariable that they wait for.
attempt :: Types.Ctx -> [Int] -> Infer a -> Infer (Maybe a)
attempt ctx path action = do
  result <- atPlace path Nothing ((Just <$> action) `catchErr` \err -> Nothing <$ recordFailure ctx err)
  retryPostponed
  pure result

-- | Run an action at the given path, and with the given premises if any.
atPlace :: [Int] -> Maybe [Types.Frame] -> Infer a -> Infer a
atPlace path frames action = do
  (outerPath, outerFrames) <- getsI (\s -> (inferPath s, inferFrames s))
  modifyI (\s -> s {inferPath = path, inferFrames = fromMaybe outerFrames frames})
  result <- action `catchErr` \err -> restore outerPath outerFrames >> throwErr err
  restore outerPath outerFrames
  pure result
  where
    restore p fs = modifyI (\s -> s {inferPath = p, inferFrames = fs})

-- | Record a failure at the current path, with the current premises.
recordFailure :: Types.Ctx -> Types.Err -> Infer ()
recordFailure ctx err = do
  subst <- getsI inferSubst
  frames <- getsI inferFrames
  path <- getsI inferPath
  -- Show each premise with the types known when it failed. Apply each
  -- substitution whose body is known, e.g. Vec(n, Nat)[n := Zero].
  let frames' = fmap (\f -> f {Types.frame'premise = mapPremise (forceTerm ctx . applySubst subst) (Types.frame'premise f)}) frames
  modifyI (\s -> s {inferFailures = Types.Failure err path frames' : inferFailures s})

-- | Postpone a unification that waits for a metavariable (see 'Postponed').
postpone :: Types.Ctx -> [(String, String)] -> Types.Type -> Types.Type -> Types.Term -> Infer ()
postpone ctx bound expected got stuck = do
  path <- getsI inferPath
  frames <- getsI inferFrames
  modifyI (\s -> s {inferPostponed = Postponed ctx bound expected got stuck path frames : inferPostponed s})

-- | Try the postponed unifications again. One that is still stuck is
--   postponed again. One that fails is recorded where it was postponed.
retryPostponed :: Infer ()
retryPostponed = do
  postponed <- getsI inferPostponed
  modifyI (\s -> s {inferPostponed = []})
  forM_ (reverse postponed) $ \(Postponed ctx bound expected got _ path frames) ->
    atPlace path (Just frames) (unifyUnder ctx bound expected got `catchErr` recordFailure ctx)

-- | At the end of inference, try the postponed unifications again until
--   that solves no more metavariables. The ones that are still stuck are
--   failures.
settle :: Infer ()
settle = do
  pending <- getsI inferPostponed
  unless (null pending) $ do
    solved <- getsI (M.size . inferSubst)
    retryPostponed
    solved' <- getsI (M.size . inferSubst)
    if solved' > solved
      then settle
      else do
        stuck <- getsI inferPostponed
        modifyI (\s -> s {inferPostponed = []})
        forM_ (reverse stuck) $ \p ->
          atPlace (postponed'path p) (Just (postponed'frames p)) (recordFailure (postponed'ctx p) (Types.StuckSubstitution (postponed'stuck p)))

--------------------------------------------------------------------------------

-- | Type variables and unification

--------------------------------------------------------------------------------

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
instantiate :: Types.Ctx -> Types.Type -> Infer Types.Type
instantiate ctx t = do
  renamed <- freshen (freeTypeVars t)
  let t' = applySubst renamed t
  declareScopes ctx (M.elems renamed) [([], t')]
  pure t'

-- | Unify an expected type with an inferred one, extending the substitution.
--
--   Only metavariables (see 'isMeta') get bound. Terms compare up to
--   α-equivalence (see 'unifyArgs'). When the terms do not match, and a
--   reduction applies to either of them, both are reduced to weak head
--   normal form and compared again, so types are equal up to computation.
--   A unification with a substitution @u[x := e]@, where @u@ is not solved
--   yet, waits until it is (see 'postpone').
unify :: Types.Ctx -> Types.Type -> Types.Type -> Infer ()
unify ctx = unifyUnder ctx []

-- | Unify under binders. Each pair is a name bound on the expected side and
--   the name bound at the same place on the inferred side, innermost first.
--   Two rigid variables are equal when they are bound by the same pair, or
--   when both are free and have the same name. A solution for a
--   metavariable is first written with the bound names of the
--   metavariable's own side.
unifyUnder :: Types.Ctx -> [(String, String)] -> Types.Type -> Types.Type -> Infer ()
unifyUnder ctx bound expected got = do
  s <- getsI inferSubst
  case (forceTerm ctx (applySubst s expected), forceTerm ctx (applySubst s got)) of
    (Types.TVar a, Types.TVar b) | sameVar a b -> pure ()
    (Types.TVar a, t) | isMeta a -> bindMeta ctx a (renameFree ctx (M.fromList [(r, l) | (l, r) <- reverse bound, l /= r]) t)
    (t, Types.TVar a) | isMeta a -> bindMeta ctx a (renameFree ctx (M.fromList [(l, r) | (l, r) <- reverse bound, l /= r]) t)
    (e@(Types.TCon en ets), g@(Types.TCon gn gts))
      | en == gn && length ets == length gts -> do
          before <- getsI id
          unifyArgs ctx bound en ets gts `catchErr` \err -> convert e g (modifyI (const before)) (throwErr err)
    (e@(Types.Subst body _ _), g) -> stuck body e g e
    (e, g@(Types.Subst body _ _)) -> stuck body e g g
    (e, g) -> convert e g (pure ()) (throwErr (Types.Mismatch e g))
  where
    sameVar a b = case (lookup a bound, lookup b [(r, l) | (l, r) <- bound]) of
      (Nothing, Nothing) -> a == b
      (inner, inner') -> inner == Just b && inner' == Just a
    -- A substitution whose body is a metavariable waits for that
    -- metavariable. Another substitution cannot be applied.
    stuck body e g term = case body of
      Types.TVar m | isMeta m -> postpone ctx bound e g term
      _ -> throwErr (Types.StuckSubstitution term)
    -- If a reduction applies to either side, reset the state and unify the
    -- weak head normal forms. Otherwise, give up.
    convert e g reset giveUp
      | null (Types.ctx'reductions ctx) = giveUp
      | otherwise = do
          e' <- fromEither (whnf ctx e)
          g' <- fromEither (whnf ctx g)
          if e' == e && g' == g then giveUp else reset >> unifyUnder ctx bound e' g'

-- | Record a solution for a metavariable, after the occurs check and the
--   scope check (see 'checkScope').
bindMeta :: Types.Ctx -> String -> Types.Type -> Infer ()
bindMeta ctx a t
  | t == Types.TVar a = pure ()
  | a `elem` freeTypeVars t = throwErr (Types.InfiniteType (Types.TVar a) t)
  | otherwise = do
      checkScope ctx a t
      modifyI $ \st ->
        st {inferSubst = M.insert a t (applySubst (M.singleton a t) <$> inferSubst st)}

-- | Unify the arguments of two applications of the same constructor.
--
--   A binder position (see 'binders') is not compared like the other
--   arguments. A metavariable there takes the other side's name. Two rigid
--   names make a new pair for the bodies that they bind (see
--   'unifyUnder'). Thus @Pi(x, Nat, Vec(x, a))@ and @Pi(y, Nat, Vec(y, a))@
--   unify.
unifyArgs :: Types.Ctx -> [(String, String)] -> String -> [Types.Type] -> [Types.Type] -> Infer ()
unifyArgs ctx bound name ets gts = do
  pairs <- catMaybes <$> traverse pairAt (nub (fmap fst bs))
  sequence_
    [ unifyUnder ctx ([pair | (i, pair) <- pairs, (i, k) `elem` bs] <> bound) e g
      | (k, e, g) <- zip3 [0 ..] ets gts,
        k `notElem` fmap fst pairs
    ]
  where
    bs = binders ctx name (length ets)
    pairAt i = do
      s <- getsI inferSubst
      case (applySubst s (ets !! i), applySubst s (gts !! i)) of
        (Types.TVar x, Types.TVar y) -> do
          if x /= y && isMeta x
            then bindMeta ctx x (Types.TVar y)
            else when (x /= y && isMeta y) (bindMeta ctx y (Types.TVar x))
          s' <- getsI inferSubst
          pure (Just (i, (resolveName s' x, resolveName s' y)))
        _ -> pure Nothing
    resolveName s v = case applySubst s (Types.TVar v) of
      Types.TVar n -> n
      _ -> v

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

substPremise :: Subst -> Types.Premise -> Types.Premise
substPremise s = mapPremise (applySubst s)

mapPremise :: (Types.Type -> Types.Type) -> Types.Premise -> Types.Premise
mapPremise f (Types.PremiseWith mode hyps (e, t)) =
  Types.PremiseWith mode (fmap (\h -> h {Types.hyp'type = f (Types.hyp'type h)}) hyps) (e, f t)

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
  children <- forM premises $ \premise@(Types.PremiseWith mode hyps (pExpr, pTy)) -> do
    let subject = substituteExpr (fst <$> env) pExpr
        subjectPath = case pExpr of
          Types.EVar v | Just (_, rel) <- M.lookup v env -> path <> rel
          _ -> path
    withFrame (Types.Frame name premise subject) . attempt ctx subjectPath $ do
      locals <- traverse (bindHypothesis env) hyps
      let ctx' = ctx {Types.ctx'env = M.fromList locals <> Types.ctx'env ctx}
      -- A synthesis premise infers; the others check against the type.
      (_, derivation) <- case mode of
        Types.Synthesis -> do
          (tGot, derivation) <- inferM ctx' subjectPath subject
          unify ctx pTy tGot
          pure (tGot, derivation)
        _ -> checkM ctx' subjectPath subject pTy
      pure derivation {Types.deriv'assumptions = locals}
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

--------------------------------------------------------------------------------

-- | Equivalence of types

--------------------------------------------------------------------------------

-- | Are two types equal up to computation (their normal forms), up to
--   α-equivalence, and up to the names of their free type variables? So
--   @Arrow\<a, a\>@ and @Arrow\<b, b\>@ are equivalent, and so are
--   @Pi(x, Nat, Vec(x, Nat))@ and @Pi(y, Nat, Vec(y, Nat))@.
equivalent :: Types.Ctx -> Types.Type -> Types.Type -> Bool
equivalent ctx a b = case (normalForm ctx a, normalForm ctx b) of
  (Right a', Right b') -> canonical ctx a' == canonical ctx b'
  _ -> False

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

--------------------------------------------------------------------------------

-- | Binders and substitution

--------------------------------------------------------------------------------

-- | The binding structure of a constructor, which PAL reads from the rules.
--   @(i, j)@ means that argument @i@ is a variable that is bound in argument
--   @j@. For example, a rule with the conclusion @Pi(x, t, u)@ and the
--   premise @x : t |- u : Type@ gives @(0, 2)@ for @Pi@ with 3 arguments.
binders :: Types.Ctx -> String -> Int -> [(Int, Int)]
binders ctx name n =
  nub
    [ (i, j)
      | Types.TypingRule _ premises (Types.ECon c ps, _) <- Types.ctx'rules ctx,
        c == name,
        length ps == n,
        Types.Premise hyps (body, _) <- premises,
        Just j <- [elemIndex body ps],
        Types.Hypothesis x _ _ <- hyps,
        Just i <- [elemIndex x ps]
    ]

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

--------------------------------------------------------------------------------

-- | Reductions

--------------------------------------------------------------------------------

-- | The most reduction steps for one normalisation, so that reductions that
--   never stop give an error ('Types.ReductionLimit').
reductionLimit :: Int
reductionLimit = 10000

-- | A computation that uses up reduction steps.
newtype Reduce a = Reduce {runReduce :: Int -> Either Types.Err (Int, a)}

instance Functor Reduce where
  fmap f (Reduce g) = Reduce (fmap (fmap f) . g)

instance Applicative Reduce where
  pure a = Reduce (\n -> Right (n, a))
  (<*>) = ap

instance Monad Reduce where
  Reduce g >>= k = Reduce $ \n -> g n >>= \(n', a) -> runReduce (k a) n'

-- | Use up one step to reduce the given term.
tick :: Types.Term -> Reduce ()
tick t = Reduce $ \n -> if n <= 0 then Left (Types.ReductionLimit t) else Right (n - 1, ())

-- | Reduce a term to weak head normal form: reduce at the root until no
--   reduction applies there.
whnf :: Types.Ctx -> Types.Term -> Either Types.Err Types.Term
whnf ctx t = snd <$> runReduce (whnfR ctx t) reductionLimit

-- | Reduce a term to normal form: no reduction applies anywhere in it.
normalForm :: Types.Ctx -> Types.Term -> Either Types.Err Types.Term
normalForm ctx t = snd <$> runReduce (go t) reductionLimit
  where
    go term =
      whnfR ctx term >>= \case
        Types.Con b c ts -> Types.Con b c <$> traverse go ts
        other -> pure other

whnfR :: Types.Ctx -> Types.Term -> Reduce Types.Term
whnfR ctx term = case forceTerm ctx term of
  t@(Types.ECon _ _) ->
    stepRootR ctx t >>= \case
      Just t' -> tick t >> whnfR ctx t'
      Nothing -> pure t
  t -> pure t

-- | Apply the first reduction (in order of definition) that matches at the
--   root of a term, once. 'Nothing' if none matches.
stepRoot :: Types.Ctx -> Types.Term -> Either Types.Err (Maybe Types.Term)
stepRoot ctx t = snd <$> runReduce (stepRootR ctx (forceTerm ctx t)) reductionLimit

stepRootR :: Types.Ctx -> Types.Term -> Reduce (Maybe Types.Term)
stepRootR ctx t = go (reverse (Types.ctx'reductions ctx))
  where
    go [] = pure Nothing
    go (Types.Reduction _ lhs rhs : rest) =
      matchRoot ctx lhs t >>= \case
        Just env -> pure (Just (forceTerm ctx (substituteExpr env rhs)))
        Nothing -> go rest

-- | Match the left side of a reduction against a term. The constructor at
--   the root must be the same. Below the root, a subterm that does not
--   match is reduced to weak head normal form and matched again, so that
--   e.g. @App(Lam(x, t, b), a)@ matches @App(Id, a)@ when @Id@ reduces to a
--   @Lam@.
matchRoot :: Types.Ctx -> Types.Term -> Types.Term -> Reduce (Maybe (Map String Types.Term))
matchRoot ctx = \cases
  (Types.ECon pn ps) (Types.ECon tn ts) | pn == tn && length ps == length ts -> matchArgs M.empty (zip ps ts)
  _ _ -> pure Nothing
  where
    matchArgs env = \case
      [] -> pure (Just env)
      (p, t) : rest -> go env p t >>= maybe (pure Nothing) (`matchArgs` rest)
    go env (Types.EVar v) t = pure $ case M.lookup v env of
      Nothing -> Just (M.insert v t env)
      Just t'
        | t' == t -> Just env
        | otherwise -> Nothing
    go env p@(Types.ECon pn ps) t = case t of
      Types.ECon tn ts | pn == tn && length ps == length ts -> matchArgs env (zip ps ts)
      _ -> do
        t' <- whnfR ctx t
        if t' == t then pure Nothing else go env p t'
    go _ _ _ = pure Nothing

--------------------------------------------------------------------------------

-- | Scopes of metavariables

--------------------------------------------------------------------------------

-- | Give new metavariables their scopes: the variables in scope where they
--   are made (the hypotheses in the context), and the names that are bound
--   around all their places in the given terms (see 'boundAround').
declareScopes :: Types.Ctx -> [Types.Type] -> [([String], Types.Term)] -> Infer ()
declareScopes ctx metas places =
  modifyI $ \st -> st {inferScopes = M.fromList scopes <> inferScopes st}
  where
    around = boundAround ctx places
    scopes = [(m, M.keys (Types.ctx'env ctx) <> M.findWithDefault [] m around) | Types.TVar m <- metas]

-- | For each variable in some terms, the names that are bound around all
--   of its places. Each term comes with the names that are bound around it.
--   A variable in a binder position, or in the variable position of a
--   substitution, is bound around itself.
--
--   For example, in @Pi(x, t, u)@ the names around @u@ are @[x]@. If @u@ is
--   also used alone, its names are @[]@: a solution of @u@ cannot mention
--   @x@.
boundAround :: Types.Ctx -> [([String], Types.Term)] -> Map String [String]
boundAround ctx = M.fromListWith intersect . concatMap (uncurry go)
  where
    go bound = \case
      Types.Var v -> [(v, bound)]
      Types.Con _ c ts ->
        let bs = binders ctx c (length ts)
            boundIn k = [v | (i, j) <- bs, j == k, Types.Var v <- [ts !! i]]
         in concat
              [ case t of
                  Types.Var v | k `elem` fmap fst bs -> [(v, v : bound)]
                  _ -> go (boundIn k <> bound) t
                | (k, t) <- zip [0 ..] ts
              ]
      Types.Subst body x e ->
        go (exprVars x <> bound) body
          <> [(v, v : bound) | Types.Var v <- [x]]
          <> go bound e

-- | Check a solution @t@ for the metavariable @a@ before it is recorded.
--   Every rigid variable that is free in @t@ must be in the scope of @a@.
--   Otherwise the variable would escape its binder, e.g. in the rule
--   conclusion @Weird(x, t, body) : u@, where @body : u@ is checked with
--   @x@ in scope. Then the metavariables in @t@ get the same limit.
checkScope :: Types.Ctx -> String -> Types.Type -> Infer ()
checkScope ctx a t = do
  scopes <- getsI inferScopes
  rigid <- getsI inferRigid
  -- With the new solution, so that a scope can name a binder through a
  -- metavariable, also @a@ itself.
  s <- M.insert a t <$> getsI inferSubst
  forM_ (M.lookup a scopes) $ \scope -> do
    let resolve v = case applySubst s (Types.TVar v) of
          Types.TVar n -> Just n
          _ -> Nothing
        allowed = mapMaybe resolve scope <> rigid
    case [x | x <- freeVars ctx t, not (isMeta x), x `notElem` allowed] of
      x : _ -> throwErr (Types.ScopeEscape x t)
      [] -> do
        let local = binderNames ctx t
            keep v = case resolve v of
              Just n | not (isMeta n) -> n `elem` allowed || n `elem` local
              _ -> True
        modifyI $ \st -> st {inferScopes = foldr (M.adjust (filter keep)) (inferScopes st) (metaVars t)}

-- | The names in binder positions in a term.
binderNames :: Types.Ctx -> Types.Term -> [String]
binderNames ctx = \case
  Types.Var _ -> []
  Types.Con _ c ts ->
    [v | i <- nub (fmap fst (binders ctx c (length ts))), Types.Var v <- [ts !! i]]
      <> concatMap (binderNames ctx) ts
  Types.Subst body x e -> exprVars x <> binderNames ctx body <> binderNames ctx e
