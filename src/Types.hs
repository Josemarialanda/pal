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

-- | Represents a type declaration in a PAL program.
--   These represent base type definitions known to the language.
data TypeDecl = TypeDecl
  { typeDecl'name :: String,
    typeDecl'args :: [String]
  }
  deriving (Eq, Lift)

instance Show TypeDecl where
  show (TypeDecl n args) =
    "type " <> n <> concatMap (" " <>) args

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

-- | A 'TypingRule' encodes a single typing judgment in a PAL program.
--   Each rule consists of:
--     * A name identifying the rule (e.g. "Add", "If").
--     * A list of premises (each an expression and its required type).
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
    typingRule'premises :: [(Expr, Type)],
    typingRule'ruleConclusion :: (Expr, Type)
  }
  deriving (Eq, Lift)

instance Show TypingRule where
  show (TypingRule name premises conclusion) =
    unlines $
      [name <> ":"]
        <> fmap showPremise premises
        <> [ "—",
             showConclusion conclusion
           ]
    where
      showPremise (e, t) = "  " <> show e <> " : " <> show t
      showConclusion (e, t) = "  " <> show e <> " : " <> show t

--------------------------------------------------------------------------------

-- | Context

--------------------------------------------------------------------------------

-- | The typing context ('Ctx') holds all information about the current language
--   being built and interpreted in a PAL program:
--
--     * 'ctx'types' — the known type constructors.
--     * 'ctx'exprs' — base expressions and their declared types.
--     * 'ctx'rules' — user-defined typing rules.
--     * 'ctx'env'   — a mapping for local inference or substitutions.
--
--   The context evolves as PAL actions are interpreted (e.g. when a new type
--   or rule is defined).
data Ctx = Ctx
  { ctx'types :: [TypeDecl],
    ctx'exprs :: [ExprDecl],
    ctx'rules :: [TypingRule],
    ctx'env :: Map String Type
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
  | CustomErr String
  deriving (Eq)

instance Show Err where
  show = \case
    UnknownExpr s -> "[Error] " <> "Unknown expression → " <> s
    Mismatch e a -> "[Error] " <> "Type mismatch: expected " <> showType e <> ", got " <> showType a
    ArityMismatch e g -> "[Error] " <> "Arity mismatch: expected " <> show e <> " arg(s), got " <> show g
    NoRuleMatched e -> "[Error] " <> "No typing rule matched for → " <> showExpr e
    CustomErr msg -> "[Error] " <> msg
    where
      showType = \case
        TCon name [] -> name
        TCon name as -> name <> "<" <> unwords (fmap showType as) <> ">"
        TVar v -> v

      showExpr = \case
        ECon name [] -> name
        ECon name as -> name <> "(" <> intercalate ", " (fmap showExpr as) <> ")"
        EVar v -> v

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
--
--   These constructors are interpreted by the PAL interpreter(s),
--   which handle the state and error effects.
data PAL m a where
  DefineType :: TypeDecl -> PAL m ()
  DefineExpr :: ExprDecl -> PAL m ()
  DefineRule :: TypingRule -> PAL m ()
  Infer :: Expr -> PAL m (Either Err Type)

-- | Generate convenient smart constructors (e.g. 'defineType', 'infer')
--   for use inside PAL programs.
makeSem ''PAL

-- | Generate lenses for record fields.
--   These allow convenient field access and updates when working with
--   complex PAL state (like nested contexts or rules).
makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''Ctx
makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''TypingRule