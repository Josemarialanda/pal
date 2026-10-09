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
-- * **Type representation** ('Type') — algebraic and polymorphic types
--   expressed as constructors ('TCon') and variables ('TVar').
--
-- * **Expression representation** ('Expr') — the syntactic structure of
--   PAL programs, including variables and constructed expressions.
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

-- | Core type representation

--------------------------------------------------------------------------------

-- | Represents a type in a PAL program.
--   A type can either be:
--     * 'TVar' — a type variable (e.g. a polymorphic placeholder like "a")
--     * 'TCon' — a concrete type constructor, optionally parameterized
--       by other types (e.g. Num, Bool, List<Num>).
data Type
  = TCon String [Type]
  | TVar String
  deriving (Eq, Lift)

instance Show Type where
  show = \case
    TVar v -> v
    TCon name [] -> name
    TCon name ts ->
      name <> "<" <> intercalate ", " (fmap show ts) <> ">"

--------------------------------------------------------------------------------

-- | Expression representation

--------------------------------------------------------------------------------

-- | Represents an expression in a PAL program.
--   * 'EVar' represents a variable (e.g. "x", "y").
--   * 'ECon' represents a constructed expression or term, optionally applied
--     to subexpressions (e.g. Add(x, y), True, LitInt).
data Expr
  = EVar String
  | ECon String [Expr]
  deriving (Eq, Lift)

instance Show Expr where
  show = \case
    EVar v -> v
    ECon name [] -> name
    ECon name es ->
      name <> "(" <> intercalate ", " (fmap show es) <> ")"

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

-- | A single premise of a typing rule: a judgment @e : t@, optionally made
--   under hypotheses that bring variables into scope.
--
--   For example, the premise of a lambda rule
--
--   > x : a ⊢ body : b
--
--   says that @body@ has type @b@ when @x@ is assumed to have type @a@.
--   Hypothesis expressions must be pattern variables that are bound to
--   variables in the target expression (the binders).
data Premise = Premise
  { premise'hypotheses :: [Hypothesis],
    premise'judgment :: (Expr, Type)
  }
  deriving (Eq, Lift)

instance Show Premise where
  show (Premise [] j) = showJudgment j
  show (Premise hs j) = intercalate ", " (fmap show hs) <> " ⊢ " <> showJudgment j

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
showJudgment (e, t) = show e <> " : " <> show t

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
data TypingRule = TypingRule
  { typingRule'name :: String,
    typingRule'premises :: [Premise],
    typingRule'ruleConclusion :: (Expr, Type)
  }
  deriving (Eq, Lift)

instance Show TypingRule where
  show (TypingRule name premises conclusion) =
    unlines $
      [name <> ":"]
        <> fmap (("  " <>) . show) premises
        <> [ "—",
             "  " <> showJudgment conclusion
           ]

--------------------------------------------------------------------------------

-- | Context

--------------------------------------------------------------------------------

-- | The typing context ('Ctx') holds all information about the current language
--   being built and interpreted in a PAL program:
--
--     * 'ctx'types' — the known type constructors.
--     * 'ctx'exprs' — base expressions and their declared types.
--     * 'ctx'rules' — user-defined typing rules.
--     * 'ctx'env'   — local variable bindings (type schemes), introduced by rule hypotheses
--                     (e.g. a lambda's parameter) while checking a premise.
--
--   The context evolves as PAL actions are interpreted (e.g. when a new type
--   or rule is defined).
data Ctx = Ctx
  { ctx'types :: [TypeDecl],
    ctx'exprs :: [ExprDecl],
    ctx'rules :: [TypingRule],
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
        "Env:",
        indent (unlines (fmap showEnvBinding (M.toList ctx'env)))
      ]
    where
      showTypeName td = "- " <> show td
      showExprName ed = "- " <> show ed
      showRuleName tr = "- " <> show tr
      showEnvBinding (k, v) = k <> " :: " <> show v
      indent = unlines . fmap ("  " <>) . lines

instance Semigroup Ctx where
  c1 <> c2 =
    Ctx
      { ctx'types = ctx'types c1 <> ctx'types c2,
        ctx'exprs = ctx'exprs c1 <> ctx'exprs c2,
        ctx'rules = ctx'rules c1 <> ctx'rules c2,
        ctx'env = ctx'env c1 <> ctx'env c2
      }

instance Monoid Ctx where
  mempty =
    Ctx
      { ctx'types = mempty,
        ctx'exprs = mempty,
        ctx'rules = mempty,
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
  | CustomErr String
  deriving (Eq)

instance Show Err where
  show = \case
    UnknownExpr s -> "[Error] " <> "Unknown expression → " <> s
    Mismatch e a -> "[Error] " <> "Type mismatch: expected " <> show e <> ", got " <> show a
    InfiniteType v t -> "[Error] " <> "Infinite type: " <> show v <> " ~ " <> show t
    ArityMismatch e g -> "[Error] " <> "Arity mismatch: expected " <> show e <> " arg(s), got " <> show g
    NoRuleMatched e -> "[Error] " <> "No typing rule matched for → " <> show e
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

-- | What a program expects of an expression: @check e : T@ or @fails e@.
data Expectation
  = -- | Inference succeeds with this type (up to renaming of type variables).
    ExpectType Expr Type
  | -- | Inference fails.
    ExpectFailure Expr
  deriving (Eq, Lift)

instance Show Expectation where
  show = \case
    ExpectType e t -> "check " <> show e <> " : " <> show t
    ExpectFailure e -> "fails " <> show e

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
  Infer :: Expr -> PAL m (Either Err Type)
  Expect :: Expectation -> PAL m Bool

-- | Generate convenient smart constructors (e.g. 'defineType', 'infer')
--   for use inside PAL programs.
makeSem ''PAL

-- | Generate lenses for record fields.
--   These allow convenient field access and updates when working with
--   complex PAL state (like nested contexts or rules).
makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''Ctx
makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''TypingRule
