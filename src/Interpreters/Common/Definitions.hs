-- |
-- Module      : Interpreters.Common.Definitions
-- Description : Definitions and their checks
--
-- Adding definitions to the context (@type@, @expr@, @rule@, @reduce@,
-- @value@, @def@), with their checks: errors reject a definition, and
-- warnings (a rule that hides another one, reductions that may not be
-- confluent or may not terminate) do not.
module Interpreters.Common.Definitions where

import Control.Monad (unless)
import Data.Foldable (find)
import Data.List (intercalate, minimumBy, nub, stripPrefix)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import Interpreters.Common.Inference
import Interpreters.Common.Reduce
import Interpreters.Common.Relations (relationRules)
import Interpreters.Common.Terms
import Polysemy (Member, Sem)
import Polysemy.State (State, get, modify)
import qualified Types
import Utils.OneOfN (OneOf5 (..))

-- | A definition of any of the five supported kinds (the last is a value pattern).
type Definition = OneOf5 Types.TypeDecl Types.ExprDecl Types.TypingRule Types.Reduction Types.Term

-- | Insert a definition into the context.
insertDefinition :: Definition -> Types.Ctx -> Types.Ctx
insertDefinition = \case
  OneOf5_1 td -> \ctx -> ctx {Types.ctx'types = td : Types.ctx'types ctx}
  OneOf5_2 ed -> \ctx -> ctx {Types.ctx'exprs = ed : Types.ctx'exprs ctx}
  OneOf5_3 tr -> Types.addRule tr
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
--   * its conclusion is a constructor pattern, like @Add(x, y)@, or a
--     relation, like @a <: b@ (then all its premises are relations);
--   * every variable its premises mention also appears in that conclusion
--     (otherwise the premise could never be checked);
--   * every type it mentions is declared and correctly applied.
validateRule :: Types.Ctx -> Types.TypingRule -> [Types.Diagnostic]
validateRule ctx rule@(Types.TypingRule name premises conclusionJ@(conclusion, conclusionType)) =
  shape <> relationPremisesOnly <> unbound <> unknownRelations <> nub (concatMap (validateType ctx') types) <> overlappingRules ctx rule
  where
    -- The rule's types may mention its own constructor, and its premises
    -- its own relation.
    ctx' = insertDefinition (OneOf5_3 rule) ctx
    concludesRelation = Types.isRelation conclusionJ
    shape
      | concludesRelation = case conclusionType of
          Types.ECon _ (_ : _) -> []
          _ -> [errorDiag ("the conclusion of rule " <> name <> " must be a judgment, like x : T or a <: b")]
      | otherwise = case conclusion of
          Types.ECon _ _ -> []
          _ -> [errorDiag ("the conclusion of rule " <> name <> " must be a constructor applied to its parts, like " <> name <> "(x, y)")]
    relationPremisesOnly =
      [ errorDiag ("rule " <> name <> " concludes a relation, so its premises must be relations too, not " <> Types.showJudgment j)
        | concludesRelation,
          Types.Premise _ j <- premises,
          not (Types.isRelation j)
      ]
    bound = exprVars conclusion
    mentioned = nub (concatMap premiseVars premises)
    premiseVars (Types.Premise hyps j@(e, _)) = concatMap (exprVars . Types.hyp'var) hyps <> (if Types.isRelation j then [] else exprVars e)
    unbound =
      [ errorDiag ("premise variable " <> v <> " does not appear in the conclusion " <> show conclusion)
        | not concludesRelation,
          v <- mentioned,
          v `notElem` bound
      ]
    unknownRelations =
      [ errorDiag ("rule " <> name <> " uses the relation " <> show t <> ", but no rule concludes a judgment of this form")
        | Types.Premise _ j@(_, t) <- premises,
          Types.isRelation j,
          null (maybe [] (uncurry (relationRules ctx')) (Types.relationName t))
      ]
    -- Of a relation, PAL checks the arguments (its name is not a type).
    judgmentTypes j@(_, t)
      | Types.isRelation j = case t of
          Types.Con _ _ args -> args
          _ -> []
      | otherwise = [t]
    types = judgmentTypes conclusionJ <> concatMap premiseTypes premises
    premiseTypes (Types.Premise hyps j) = judgmentTypes j <> fmap Types.hyp'type hyps

-- | A reduction is well formed when:
--
--   * its left side is a constructor pattern, like @App(Lam(x, t, b), a)@,
--     without substitutions;
--   * every variable of its right side also appears in its left side;
--   * every constructor it mentions is declared and correctly applied.
validateReduction :: Types.Ctx -> Types.Reduction -> [Types.Diagnostic]
validateReduction ctx reduction@(Types.Reduction name lhs rhs) =
  shape <> unbound <> nub (concatMap (validateTerm ctx False) [lhs, rhs]) <> reductionWarnings ctx reduction
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

-- | Warnings about a new reduction: where it may not be confluent with
--   the reductions so far (see 'criticalPairs'), and where it may not
--   terminate (see 'terminationWarnings').
reductionWarnings :: Types.Ctx -> Types.Reduction -> [Types.Diagnostic]
reductionWarnings ctx new = criticalPairs ctx new <> terminationWarnings ctx' new
  where
    ctx' = insertDefinition (OneOf5_4 new) ctx

-- | A warning for each critical pair of the new reduction that does not
--   join. A critical pair is a term where two reductions (or one reduction
--   at two places) apply, so it reduces in two ways. If the two results
--   have different normal forms, the result depends on which reduction PAL
--   uses: the reductions are not confluent.
criticalPairs :: Types.Ctx -> Types.Reduction -> [Types.Diagnostic]
criticalPairs ctx new =
  [ warningDiag ("reductions " <> name1 <> " and " <> name2 <> " " <> verdict <> ": " <> show t <> " reduces to " <> show a <> " and to " <> show b)
    | (_, (name1, name2, t, a, b, verdict)) <- M.toList firstPerPair
  ]
  where
    -- One warning for each pair of reductions (the first critical pair).
    firstPerPair =
      M.fromListWith
        (\_ first -> first)
        [ (minMaxNames name1 name2, (name1, name2, t, a, b, verdict))
          | old <- reverse (Types.ctx'reductions ctx) <> [new],
            (outer, inner) <- [(new, old), (old, new)],
            (t, a, b) <- overlaps outer (apart outer inner) (Types.reduction'name outer == Types.reduction'name inner),
            Just verdict <- [problem a b],
            let name1 = Types.reduction'name inner
                name2 = Types.reduction'name outer
        ]
    minMaxNames x y = (min x y, max x y)
    ctx' = insertDefinition (OneOf5_4 new) ctx
    -- The inner reduction with variables that the outer one does not use.
    apart (Types.Reduction _ lhs _) (Types.Reduction n l r) =
      let taken = exprVars lhs <> exprVars l
          names = M.fromList [(v, Types.Var (freshName taken v)) | v <- exprVars l, v `elem` exprVars lhs]
       in Types.Reduction n (substituteExpr names l) (substituteExpr names r)
    -- Each place in the outer left side (not a variable) where the inner
    -- left side unifies: the term, and its two reducts.
    overlaps (Types.Reduction _ lhs rhs) (Types.Reduction _ l r) same =
      [ (t, apply (substituteExpr s rhs), apply (rebuild (substituteExpr s r)))
        | (sub, rebuild, atRoot) <- patternPlaces lhs,
          not (same && atRoot),
          Just s <- [unifyPatterns l sub],
          let t = apply (substituteExpr s lhs)
      ]
    apply = forceTerm ctx'
    -- The two reducts join if they have the same normal form. If one of
    -- them has no normal form within the step limit, PAL cannot tell.
    problem a b = case (normalForm ctx' a, normalForm ctx' b) of
      (Right a', Right b')
        | canonical ctx' a' == canonical ctx' b' && freeVars ctx' a' == freeVars ctx' b' -> Nothing
        | otherwise -> Just "are not confluent"
      _ -> Just "may not be confluent (the reductions do not stop, so PAL cannot compare the results)"

-- | Warnings if a new reduction may not terminate. PAL checks first-order
--   recursion only:
--
--   * each call of the reduction's own constructor on its right side must
--     have smaller arguments: from left to right, the first argument that
--     differs must be a part of the argument on the left side (like
--     @Plus(Succ(m), n) ~> Succ(Plus(m, n))@);
--   * the constructors of the reductions must not call each other in a
--     cycle.
--
--   A substitution on the right side (like β-reduction) can make new
--   redexes; PAL does not check that.
terminationWarnings :: Types.Ctx -> Types.Reduction -> [Types.Diagnostic]
terminationWarnings ctx (Types.Reduction name lhs rhs) = case lhs of
  Types.ECon f ps ->
    [ warningDiag ("reduction " <> name <> " may not terminate: " <> message)
      | call@(Types.ECon _ rs) <- callsOf f (length ps) rhs,
        not (smaller rs ps),
        let message = if null ps then f <> " reduces to a term that contains " <> f <> " again" else show call <> " on the right side has arguments that are not smaller than in " <> show lhs
    ]
      <> [ warningDiag ("reduction " <> name <> " may not terminate: " <> f <> " and " <> intercalate ", " others <> " call each other, and PAL does not check calls in a cycle")
           | Just others <- [cycleThrough f]
         ]
  _ -> []
  where
    -- The calls of a constructor with this number of arguments in a term.
    callsOf f n = \case
      t@(Types.ECon c ts) -> [t | c == f, length ts == n] <> concatMap (callsOf f n) ts
      Types.Subst body x e -> callsOf f n body <> callsOf f n x <> callsOf f n e
      _ -> []
    -- Lexicographic order: the first argument that differs is a part of
    -- the old one.
    smaller (r : rs) (p : ps)
      | r == p = smaller rs ps
      | otherwise = r `isPartOf` p
    smaller _ _ = False
    isPartOf r = \case
      Types.ECon _ ts -> any (\t -> t == r || r `isPartOf` t) ts
      _ -> False
    -- The constructors that the reductions for each constructor call.
    heads = nub [c | Types.Reduction _ (Types.ECon c _) _ <- Types.ctx'reductions ctx]
    calls c = nub [d | Types.Reduction _ (Types.ECon c' _) r <- Types.ctx'reductions ctx, c' == c, d <- constructors r, d `elem` heads, d /= c]
    constructors = \case
      Types.ECon c ts -> c : concatMap constructors ts
      Types.Subst body x e -> concatMap constructors [body, x, e]
      _ -> []
    -- A cycle from f back to f through other constructors.
    cycleThrough f = go [] (calls f)
      where
        go _ [] = Nothing
        go seen (c : rest)
          | c `elem` seen = go seen rest
          | f `elem` calls c = Just (reverse (c : seen))
          | otherwise = go (c : seen) (calls c <> rest)

warningDiag :: String -> Types.Diagnostic
warningDiag = Types.Diagnostic Types.SevWarning

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
