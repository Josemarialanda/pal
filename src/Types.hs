{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Types
-- Description : Core type definitions and data model for PAL
--
-- This module defines the **core data structures** that make up the PAL type system
-- sandbox — a playground for experimenting with type rules, inference, and
-- type system design in a declarative and inspectable way.
--
-- It contains the following key components:
--
-- * **Term representation** ('Term') — expressions and types, built from
--   constructors ('Con') and variables ('Var'). 'Expr' and 'Type' are the
--   names of a term in expression position and in type position.
--
-- * **Declarations** ('TypeDecl', 'ExprDecl') — user-defined base types and
--   expression signatures that populate the global context.
--
-- * **Typing rules** ('TypingRule') — declarative specifications of
--   type judgments that connect premises to a conclusion, forming
--   the logical backbone of a PAL type system.
--
-- * **Context** ('Ctx') — the mutable environment in which all
--   declarations, rules, and inference state are stored and combined.
--
-- * **Error reporting** ('Err') — structured and human-readable diagnostics
--   for type errors, arity mismatches, and unbound expressions.
--
-- * **PAL effect** ('PAL') — the foundational effect type for the
--   PAL DSL, exposing operations such as 'DefineType', 'DefineExpr',
--   'DefineRule', and 'Infer', used to build and execute PAL programs.
--
-- In short, this module defines **the data model and language core** for PAL:
-- every interpreter, DSL command, or type inference pass operates on the
-- structures defined here.
module Types where

import Control.Lens
  ( DefName (..),
    lensField,
    lensRules,
    makeLensesWith,
    (&),
    (.~),
  )
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Language.Haskell.TH (mkName, nameBase)
import Language.Haskell.TH.Syntax (Lift)
import Polysemy (makeSem)

--------------------------------------------------------------------------------

-- | Terms: expressions and types

--------------------------------------------------------------------------------

-- | A term of a PAL program. Expressions and types are both terms, so a type
--   can mention an expression (e.g. @Vec(n, a)@), and rules can check a type
--   like any other term (e.g. @Pi(x, t, u) : Type@). A term is:
--
--     * 'Var' — a variable (e.g. @x@, or a type variable like @a@);
--     * 'Con' — a constructor applied to its arguments (e.g. @Add(x, y)@,
--       @True@, @List<Num>@). @C(…)@ and @C<…>@ are the same term: the
--       'Brackets' only remember how to show it;
--     * 'Subst' — a substitution @body[x := e]@, which inference applies
--       when it knows the body.
data Term
  = Var String
  | Con Brackets String [Term]
  | -- | @Subst body var replacement@ is @body[var := replacement]@.
    Subst Term Term Term
  deriving (Lift)

-- | The brackets around the arguments of a constructor: @C(…)@ or @C<…>@.
data Brackets = Parens | Angles
  deriving (Eq, Show, Lift)

-- | Equality of terms ignores the 'Brackets'.
instance Eq Term where
  Var a == Var b = a == b
  Con _ m as == Con _ n bs = m == n && as == bs
  Subst a x e == Subst b y f = a == b && x == y && e == f
  _ == _ = False

-- | A term in type position (the right side of @e : T@).
type Type = Term

-- | A term in expression position (the left side of @e : T@).
type Expr = Term

pattern TVar, EVar :: String -> Term
pattern TVar v = Var v
pattern EVar v = Var v

-- | A constructor in type position. It is shown as @C\<…\>@.
pattern TCon :: String -> [Term] -> Term
pattern TCon n ts <- Con _ n ts
  where
    TCon n ts = Con Angles n ts

-- | A constructor in expression position. It is shown as @C(…)@.
pattern ECon :: String -> [Term] -> Term
pattern ECon n ts <- Con _ n ts
  where
    ECon n ts = Con Parens n ts

{-# COMPLETE TVar, TCon, Subst #-}

{-# COMPLETE EVar, ECon, Subst #-}

{-# COMPLETE TVar, Con, Subst #-}

{-# COMPLETE EVar, Con, Subst #-}

-- | Shows each constructor with its own brackets, e.g. @Vec(n, List<a>)@.
instance Show Term where
  show = \case
    Var v -> v
    Con _ name [] -> name
    Con Parens name ts -> name <> "(" <> intercalate ", " (fmap show ts) <> ")"
    Con Angles name ts -> name <> "<" <> intercalate ", " (fmap show ts) <> ">"
    Subst body x e -> show body <> "[" <> show x <> " := " <> show e <> "]"

--------------------------------------------------------------------------------

-- | Declarations: user-defined base types and expressions

--------------------------------------------------------------------------------

-- | Represents a type declaration in a PAL program, e.g. @type Arrow\<a, b\>@.
--   Every type constructor must be declared before it is used, and applied to
--   exactly as many arguments as it has parameters.
data TypeDecl = TypeDecl
  { typeDecl'name :: String,
    typeDecl'args :: [String]
  }
  deriving (Eq, Lift)

instance Show TypeDecl where
  show (TypeDecl n []) = "type " <> n
  show (TypeDecl n args) = "type " <> n <> "<" <> intercalate ", " args <> ">"

-- | Represents an expression declaration in a PAL program.
--   Associates a literal or constant constructor name with a known type.
data ExprDecl = ExprDecl
  { exprDecl'name :: String,
    exprDecl'type :: Type
  }
  deriving (Eq, Lift)

instance Show ExprDecl where
  show (ExprDecl n t) =
    n <> " : " <> show t

--------------------------------------------------------------------------------

-- | Typing rules

--------------------------------------------------------------------------------

-- | The direction of a judgment (bidirectional typing):
--
--   * 'Plain' — @e : T@. In a premise, @e@ is checked against @T@ (which
--     falls back to inference for a rule without a direction). A rule
--     with a plain conclusion is used in both directions.
--   * 'Synthesis' — @e => T@. In a premise, the type of @e@ is inferred.
--     A rule with this conclusion infers its type, like a plain one.
--   * 'Checking' — @e <= T@. In a premise, @e@ is checked against @T@. A
--     rule with this conclusion is used only when the expected type is
--     known: PAL unifies the expected type with @T@ before the premises.
data Mode = Plain | Synthesis | Checking
  deriving (Eq, Show, Lift)

-- | The symbol of a mode: @:@, @=>@ or @<=@.
modeSymbol :: Mode -> String
modeSymbol = \case
  Plain -> ":"
  Synthesis -> "=>"
  Checking -> "<="

-- | A single premise of a typing rule: a judgment @e : t@ (or @e => t@,
--   @e <= t@, see 'Mode'), optionally made under hypotheses that bring
--   variables into scope.
--
--   For example, the premise of a lambda rule
--
--   > x : a ⊢ body : b
--
--   says that @body@ has type @b@ when @x@ is assumed to have type @a@.
--   Hypothesis expressions must be pattern variables that are bound to
--   variables in the target expression (the binders).
data Premise = PremiseWith
  { premise'mode :: Mode,
    premise'hypotheses :: [Hypothesis],
    premise'judgment :: (Expr, Type)
  }
  deriving (Eq, Lift)

-- | A premise with a plain judgment @e : t@.
pattern Premise :: [Hypothesis] -> (Expr, Type) -> Premise
pattern Premise hs j <- PremiseWith _ hs j
  where
    Premise hs j = PremiseWith Plain hs j

{-# COMPLETE Premise #-}

instance Show Premise where
  show (PremiseWith m [] j) = showJudgmentWith m j
  show (PremiseWith m hs j) = intercalate ", " (fmap show hs) <> " ⊢ " <> showJudgmentWith m j

-- | A hypothesis @x : t@ that brings a variable into scope for one premise.
--
--   With 'hyp'generalize' set (written @x : gen t@), the variable gets the
--   type @t@ /generalised/ over the type variables not free in the context,
--   so it can be used at different types (let-polymorphism). Otherwise its
--   type is monomorphic, as a lambda’s parameter must be.
data Hypothesis = Hypothesis
  { hyp'var :: Expr,
    hyp'type :: Type,
    hyp'generalize :: Bool
  }
  deriving (Eq, Lift)

instance Show Hypothesis where
  show (Hypothesis e t g) = show e <> " : " <> (if g then "gen " else "") <> show t

-- | A premise without hypotheses: @premise e t@ is the judgment @e : t@.
premise :: Expr -> Type -> Premise
premise e t = Premise [] (e, t)

-- | A monomorphic hypothesis @x : t@.
hyp :: Expr -> Type -> Hypothesis
hyp e t = Hypothesis e t False

-- | A generalised hypothesis @x : gen t@.
genHyp :: Expr -> Type -> Hypothesis
genHyp e t = Hypothesis e t True

-- | A type scheme @∀vs. t@: the type of a variable that may be used at
--   different instances of its quantified variables.
data Scheme = Forall [String] Type
  deriving (Eq)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vs t) = "∀" <> unwords vs <> ". " <> show t

showJudgment :: (Expr, Type) -> String
showJudgment = showJudgmentWith Plain

-- | A judgment with the symbol of its mode, e.g. @Lam(x, b) <= Arrow<a, b>@.
showJudgmentWith :: Mode -> (Expr, Type) -> String
showJudgmentWith m (e, t) = show e <> " " <> modeSymbol m <> " " <> show t

-- | A 'TypingRule' encodes a single typing judgment in a PAL program.
--   Each rule consists of:
--     * A name identifying the rule (e.g. "Add", "If").
--     * A list of premises (each a judgment, possibly under hypotheses).
--     * A conclusion (the expression and type the rule defines).
--
--   For example:
--     Add:
--       x : Num
--       y : Num
--     —
--       Add(x, y) : Num
data TypingRule = TypingRuleWith
  { typingRule'name :: String,
    typingRule'premises :: [Premise],
    typingRule'ruleConclusion :: (Expr, Type),
    -- | The direction of the conclusion (see 'Mode').
    typingRule'mode :: Mode
  }
  deriving (Eq, Lift)

-- | A rule with a plain conclusion @C(…) : T@.
pattern TypingRule :: String -> [Premise] -> (Expr, Type) -> TypingRule
pattern TypingRule name premises conclusion <- TypingRuleWith name premises conclusion _
  where
    TypingRule name premises conclusion = TypingRuleWith name premises conclusion Plain

{-# COMPLETE TypingRule #-}

instance Show TypingRule where
  show (TypingRuleWith name premises conclusion mode) =
    unlines $
      [name <> ":"]
        <> fmap (("  " <>) . show) premises
        <> [ "—",
             "  " <> showJudgmentWith mode conclusion
           ]

-- | A reduction @reduce Name: lhs ~> rhs@: a term that matches @lhs@ is
--   definitionally equal to @rhs@, with the variables of @lhs@ replaced. For
--   example, β-reduction:
--
--   > reduce Beta: App(Lam(x, t, body), a) ~> body[x := a]
--
--   Unification uses the reductions to compare types up to computation.
data Reduction = Reduction
  { reduction'name :: String,
    reduction'lhs :: Term,
    reduction'rhs :: Term
  }
  deriving (Eq, Lift)

instance Show Reduction where
  show (Reduction name lhs rhs) = name <> ": " <> show lhs <> " ~> " <> show rhs

-- | A definition @def Name := body@, or @def Name(x, y) := body@ with
--   parameters: a name for a term. PAL infers the type of the body (and of
--   the parameters); then @Name@ is a constant of that type, or a
--   constructor with a typing rule, that reduces to the body.
data TermDef = TermDef
  { termDef'name :: String,
    termDef'params :: [String],
    termDef'body :: Term
  }
  deriving (Eq, Lift)

instance Show TermDef where
  show (TermDef name params body) = "def " <> name <> paramList <> " := " <> show body
    where
      paramList = if null params then "" else "(" <> intercalate ", " params <> ")"

--------------------------------------------------------------------------------

-- | Context

--------------------------------------------------------------------------------

-- | The typing context ('Ctx') holds all information about the current language
--   being built and interpreted in a PAL program:
--
--     * 'ctx'types' — the known type constructors.
--     * 'ctx'exprs' — base expressions and their declared types.
--     * 'ctx'rules' — user-defined typing rules.
--     * 'ctx'reductions' — user-defined reductions (definitional equality).
--     * 'ctx'values' — patterns of the values, for the progress test.
--     * 'ctx'env'   — local variable bindings (type schemes), introduced by rule hypotheses
--                     (e.g. a lambda's parameter) while checking a premise.
--
--   The context evolves as PAL actions are interpreted (e.g. when a new type
--   or rule is defined).
data Ctx = Ctx
  { ctx'types :: [TypeDecl],
    ctx'exprs :: [ExprDecl],
    ctx'rules :: [TypingRule],
    ctx'reductions :: [Reduction],
    ctx'values :: [Term],
    ctx'env :: Map String Scheme
  }

instance Show Ctx where
  show Ctx {..} =
    unlines
      [ "=== Context ===",
        "Types:",
        indent (unlines (fmap showTypeName ctx'types)),
        "Expressions:",
        indent (unlines (fmap showExprName ctx'exprs)),
        "Rules:",
        indent (unlines (fmap showRuleName ctx'rules)),
        "Reductions:",
        indent (unlines (fmap showRuleName ctx'reductions)),
        "Values:",
        indent (unlines (fmap showRuleName ctx'values)),
        "Env:",
        indent (unlines (fmap showEnvBinding (M.toList ctx'env)))
      ]
    where
      showTypeName td = "- " <> show td
      showExprName ed = "- " <> show ed
      showRuleName :: (Show a) => a -> String
      showRuleName tr = "- " <> show tr
      showEnvBinding (k, v) = k <> " :: " <> show v
      indent = unlines . fmap ("  " <>) . lines

instance Semigroup Ctx where
  c1 <> c2 =
    Ctx
      { ctx'types = ctx'types c1 <> ctx'types c2,
        ctx'exprs = ctx'exprs c1 <> ctx'exprs c2,
        ctx'rules = ctx'rules c1 <> ctx'rules c2,
        ctx'reductions = ctx'reductions c1 <> ctx'reductions c2,
        ctx'values = ctx'values c1 <> ctx'values c2,
        ctx'env = ctx'env c1 <> ctx'env c2
      }

instance Monoid Ctx where
  mempty =
    Ctx
      { ctx'types = mempty,
        ctx'exprs = mempty,
        ctx'rules = mempty,
        ctx'reductions = mempty,
        ctx'values = mempty,
        ctx'env = mempty
      }

--------------------------------------------------------------------------------

-- | Error reporting

--------------------------------------------------------------------------------

-- | Possible error cases produced during type inference or rule checking.
data Err
  = UnknownExpr String
  | Mismatch Type Type
  | ArityMismatch Int Int
  | NoRuleMatched Expr
  | -- | A type variable would have to contain itself (e.g. @a ~ Arrow<a, b>@).
    InfiniteType Type Type
  | -- | Reductions applied to a term did not stop within the step limit.
    ReductionLimit Term
  | -- | A substitution @u[x := e]@ whose body @u@ is still unknown, so it
    --   cannot be applied (PAL does no higher-order unification).
    StuckSubstitution Term
  | -- | A term whose rules only check it against a known type was used
    --   where its type must be inferred.
    CannotInfer Expr
  | -- | A solution would use a variable outside its binder: the variable,
    --   and the solution.
    ScopeEscape String Type
  | CustomErr String
  deriving (Eq)

instance Show Err where
  show = \case
    UnknownExpr s -> "[Error] " <> "Unknown expression → " <> s
    Mismatch e a -> "[Error] " <> "Type mismatch: expected " <> show e <> ", got " <> show a
    InfiniteType v t -> "[Error] " <> "Infinite type: " <> show v <> " ~ " <> show t
    ArityMismatch e g -> "[Error] " <> "Arity mismatch: expected " <> show e <> " arg(s), got " <> show g
    NoRuleMatched e -> "[Error] " <> "No typing rule matched for → " <> show e
    ReductionLimit t -> "[Error] " <> "Reduction limit: " <> show t <> " does not stop reducing"
    StuckSubstitution t -> "[Error] " <> "Stuck substitution: " <> show t <> " (its body is not known)"
    CannotInfer e -> "[Error] " <> "Cannot infer the type of " <> show e <> ": its rule only checks it against a known type"
    ScopeEscape x t -> "[Error] " <> "Scope error: " <> x <> " escapes its binder in " <> show t
    CustomErr msg -> "[Error] " <> msg

-- | One failed check during inference, with where and why it happened.
data Failure = Failure
  { failure'err :: Err,
    -- | Path from the inferred expression to the subterm at fault: child
    --   indices, so @[1, 0]@ is the first argument of the second argument.
    --   @[]@ is the expression itself.
    failure'path :: [Int],
    -- | The premises being checked when it failed, innermost first.
    failure'context :: [Frame]
  }
  deriving (Eq, Show)

-- | A premise of a rule being checked: which rule, which premise (with the
--   types known at the time), and the actual subterm it was checking.
data Frame = Frame
  { frame'rule :: String,
    frame'premise :: Premise,
    frame'subject :: Expr
  }
  deriving (Eq, Show)

-- | How serious a 'Diagnostic' is. Errors reject the definition.
data Severity = SevError | SevWarning
  deriving (Eq, Show)

-- | A problem found in a definition, e.g. an undeclared type.
data Diagnostic = Diagnostic
  { diag'severity :: Severity,
    diag'message :: String
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Derivations

--------------------------------------------------------------------------------

-- | Why a node of a derivation holds.
data Justification
  = -- | By a typing rule (its name).
    ByRule String
  | -- | By a declaration @expr C : T@.
    ByDeclaration
  | -- | By a hypothesis in scope (e.g. a lambda's parameter).
    ByAssumption
  deriving (Eq, Show)

-- | A derivation tree: the proof that an expression has a type, built by
--   inference. Each node is a judgment @e : T@, justified by a rule (with
--   one sub-derivation per premise), a declaration, or an assumption.
data Derivation = Derivation
  { deriv'expr :: Expr,
    deriv'type :: Type,
    deriv'by :: Justification,
    -- | Hypotheses this judgment is made under, introduced by the premise
    --   that this node proves (e.g. @x : a@ in @x : a ⊢ body : b@).
    deriv'assumptions :: [(String, Scheme)],
    deriv'premises :: [Derivation]
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Expectations

--------------------------------------------------------------------------------

-- | What a program expects: @check e : T@, @fails e@ or @find T@.
data Expectation
  = -- | Inference succeeds with this type (up to renaming of type variables).
    ExpectType Expr Type
  | -- | Inference fails.
    ExpectFailure Expr
  | -- | Some closed term has this type (proof search), at most the given
    --   number of rules deep: @find T N@.
    ExpectInhabited Type Int
  deriving (Eq, Lift)

instance Show Expectation where
  show = \case
    ExpectType e t -> "check " <> show e <> " : " <> show t
    ExpectFailure e -> "fails " <> show e
    ExpectInhabited t d -> "find " <> show t <> " " <> show d

--------------------------------------------------------------------------------

-- | Metatheory tests

--------------------------------------------------------------------------------

-- | A property of the whole type system that PAL can test on generated
--   terms (see "Interpreters.Common.Metatheory").
data MetaProperty
  = -- | Reduction keeps the type: if @e : T@ and @e@ reduces to @e'@ in one
    --   step, then @e' : T@.
    Preservation
  | -- | A well-typed term that does not reduce is a value.
    Progress
  deriving (Eq, Show, Lift)

-- | A test statement: @test preservation@ or @test progress@, with the
--   number of terms to generate.
data TestSpec = TestSpec
  { testSpec'property :: MetaProperty,
    testSpec'terms :: Int
  }
  deriving (Eq, Lift)

instance Show TestSpec where
  show (TestSpec p n) = "test " <> propertyName p <> " " <> show n

-- | The name of a property, as it is written in a program.
propertyName :: MetaProperty -> String
propertyName = \case
  Preservation -> "preservation"
  Progress -> "progress"

-- | The result of a test.
data TestResult = TestResult
  { test'spec :: TestSpec,
    -- | The well-typed terms that were checked.
    test'checked :: Int,
    -- | The smallest counterexample, if any.
    test'counterexample :: Maybe Counterexample
  }
  deriving (Eq, Show)

-- | Why a property does not hold.
data Counterexample
  = -- | @e : T@ reduces to @e'@, which has the given type (not as general
    --   as @T@) or an error.
    NotPreserved Expr Type Expr (Either Err Type)
  | -- | @e : T@ does not reduce, and it is not a value.
    Stuck Expr Type
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | PAL effect: the core DSL for building and running type systems

--------------------------------------------------------------------------------

-- | The 'PAL' effect defines the primitive operations available in the PAL DSL.
--   These correspond to user-facing actions that modify or query the typing
--   context:
--
--     * 'DefineType' — add a new type declaration to the context.
--     * 'DefineExpr' — register a new expression with its declared type.
--     * 'DefineRule' — introduce a new typing rule.
--     * 'DefineReduction' — introduce a new reduction (definitional equality).
--     * 'DefineTerm' — define a name for a term (@def Name := body@).
--     * 'Infer'      — perform type inference for a given expression.
--     * 'Expect'     — check an expectation (@check e : T@ or @fails e@);
--                      'True' when it is met.
--
--   A definition that uses an undeclared type, or a malformed rule, is
--   rejected: it is not added to the context.
--
--   These constructors are interpreted by the PAL interpreter(s),
--   which handle the state and error effects.
data PAL m a where
  DefineType :: TypeDecl -> PAL m ()
  DefineExpr :: ExprDecl -> PAL m ()
  DefineRule :: TypingRule -> PAL m ()
  DefineReduction :: Reduction -> PAL m ()
  DefineValue :: Term -> PAL m ()
  DefineTerm :: TermDef -> PAL m ()
  Infer :: Expr -> PAL m (Either Err Type)
  Expect :: Expectation -> PAL m Bool
  Test :: TestSpec -> PAL m Bool

-- | Generate convenient smart constructors (e.g. 'defineType', 'infer')
--   for use inside PAL programs.
makeSem ''PAL

-- | Generate lenses for record fields.
--   These allow convenient field access and updates when working with
--   complex PAL state (like nested contexts or rules).
makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''Ctx
makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''TypingRule
