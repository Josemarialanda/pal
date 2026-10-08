{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : PAL
-- Description : Entry point and demonstration of the PAL (Typechecker Sandbox) project.
--
-- This module defines the main executable interface for **PAL** —
-- a *typechecker sandbox* and experimental framework for defining, exploring,
-- and running type systems declaratively.
--
-- PAL programs can be expressed in **three equivalent forms**:
--
-- 1. **As direct code** — (`mainAsCode`).
-- 2. **As data** — via lists of 'PalAction' values (`mainAsData`).
-- 3. **As syntax** — via a quasiquoter (`mainAsDSL`), using `TemplateHaskell` parsing.
--
-- Each mode ultimately drives the same interpreter backend (`runInterpreterStdout`)
-- and demonstrates how PAL unifies *types as data*, *typing rules as logic*, and
-- *inference as an effectful computation*.
--
-- ---
-- ### Overview
--
-- PAL’s goal is to provide a “playground for type rules” —
-- you can declaratively define types, expressions, and inference rules,
-- then immediately test or visualize them.
--
-- Example (via quasiquoter syntax):
--
-- @
-- [palQuasiQuoter|
--   type Num
--   type Bool
--
--   expr LitInt : Num
--   expr True   : Bool
--   expr False  : Bool
--
--   rule Add:
--     x : Num
--     y : Num
--   ->
--     Add(x, y) : Num
--
--   infer Add(True, LitInt)
--   infer Add(LitInt)
--   infer Add(LitInt, LitInt)
-- |]
-- @
--
-- Running this with 'runInterpreterStdout' produces trace output showing
-- type inference steps and results for each expression.
--
-- ---
-- ### Interpreters
--
-- Each PAL interpreter implements a different “view” of evaluation:
--
-- * **Core interpreter** – pure and minimal, no I/O or tracing.
-- * **Debug interpreter** – uses 'Polysemy.Trace' for human-readable logs.
-- * **IO interpreter (planned)** – allows runtime interaction and stdin/stdout input.
--
-- ---
-- ### Implementation Notes
--
-- The PAL system is effect-based (via 'Polysemy'), using a small effect algebra:
--
-- @
-- data PAL m a where
--   DefineType :: TypeDecl -> PAL m ()
--   DefineExpr :: ExprDecl -> PAL m ()
--   DefineRule :: TypingRule -> PAL m ()
--   Infer      :: Expr -> PAL m (Either Err Type)
-- @
--
-- Each backend provides its own handler ('interpreter') for executing these effects.
module PAL where

import Control.Monad (foldM)
import Interpreters.Debug as Debug (runInterpreterStdout)
import Parser.Quasi (palQuasiQuoter)
import Polysemy (Members, Sem)
import Types
  ( Ctx,
    Err,
    Expr (..),
    ExprDecl (ExprDecl),
    PAL,
    Type (TCon),
    TypeDecl (TypeDecl),
    TypingRule (..),
    premise,
    defineExpr,
    defineRule,
    defineType,
    infer,
  )

------------------------------------------------------------

-- | Entrypoint: defaults to running the DSL-based PAL program.

------------------------------------------------------------
main :: IO ()
main = mainAsDSL

------------------------------------------------------------
-- 1. Running PAL as *Code*
------------------------------------------------------------

-- | Direct Haskell representation of a PAL program using its effect constructors.
--
-- This is the most explicit way to write PAL programs — you invoke the
-- primitives ('defineType', 'defineExpr', 'defineRule', 'infer') directly
-- within the 'Sem' monad.
mainAsCode :: IO ()
mainAsCode = either print print =<< Debug.runInterpreterStdout (mempty @Ctx) program
  where
    program :: (Members '[PAL] r) => Sem r (Either Err Type)
    program = do
      defineType $ TypeDecl "Num" []
      defineType (TypeDecl "Bool" [])

      defineExpr $ ExprDecl "LitInt" (TCon "Num" [])
      defineExpr $ ExprDecl "True" (TCon "Bool" [])
      defineExpr $ ExprDecl "False" (TCon "Bool" [])

      defineRule $
        TypingRule
          { typingRule'name = "Add",
            typingRule'premises =
              [ premise (EVar "x") (TCon "Num" []),
                premise (EVar "y") (TCon "Num" [])
              ],
            typingRule'ruleConclusion =
              (ECon "Add" [EVar "x", EVar "y"], TCon "Num" [])
          }

      _ <- infer $ ECon "Add" [ECon "True" [], ECon "LitInt" []] -- Type error
      _ <- infer $ ECon "Add" [ECon "LitInt" []] -- Arity mismatch
      infer $ ECon "Add" [ECon "LitInt" [], ECon "LitInt" []] -- OK

------------------------------------------------------------
-- 2. Running PAL as *Data*
------------------------------------------------------------

-- | Run a PAL program represented purely as a list of 'PalAction' values.
--
-- This model allows serializing or generating programs from external inputs,
-- like JSON or a UI editor.
mainAsData :: IO ()
mainAsData = either print print =<< Debug.runInterpreterStdout (mempty @Ctx) (runPalActions program)
  where
    program =
      [ ADefineType (TypeDecl "Num" []),
        ADefineType (TypeDecl "Bool" []),
        ADefineExpr (ExprDecl "LitInt" (TCon "Num" [])),
        ADefineExpr (ExprDecl "True" (TCon "Bool" [])),
        ADefineExpr (ExprDecl "False" (TCon "Bool" [])),
        ADefineRule $
          TypingRule
            { typingRule'name = "Add",
              typingRule'premises =
                [ premise (EVar "x") (TCon "Num" []),
                  premise (EVar "y") (TCon "Num" [])
                ],
              typingRule'ruleConclusion =
                (ECon "Add" [EVar "x", EVar "y"], TCon "Num" [])
            },
        AInfer (ECon "Add" [ECon "True" [], ECon "LitInt" []]),
        AInfer (ECon "Add" [ECon "LitInt" []]),
        AInfer (ECon "Add" [ECon "LitInt" [], ECon "LitInt" []])
      ]

-- | A data-level representation of PAL’s core DSL actions.
--
-- Each constructor mirrors one PAL effect operation and can be interpreted
-- via different backends (debug, pure, traced, etc.).
data PalAction
  = -- | Add a new base type.
    ADefineType TypeDecl
  | -- | Declare a new expression and its type.
    ADefineExpr ExprDecl
  | -- | Introduce a new typing rule.
    ADefineRule TypingRule
  | -- | Run inference on a given expression.
    AInfer Expr
  deriving (Show)

-- | Execute a list of 'PalAction's in order.
--
-- The result is that of the last 'AInfer' (or @Unit@ if there is none).
runPalActions :: (Members '[PAL] r) => [PalAction] -> Sem r (Either Err Type)
runPalActions = flip foldM (Right (TCon "Unit" [])) $ \acc -> \case
  ADefineType td -> defineType td >> pure acc
  ADefineExpr ed -> defineExpr ed >> pure acc
  ADefineRule tr -> defineRule tr >> pure acc
  AInfer e -> infer e

------------------------------------------------------------
-- 3. Running PAL as *DSL* (via Quasiquotes)
------------------------------------------------------------

-- | A more natural syntax for PAL programs, enabled by the `palQuasiQuoter`.
--
-- This form feels like writing a standalone mini-language,
-- but compiles down to the same 'Sem' actions under the hood.
mainAsDSL :: IO ()
mainAsDSL = either print print =<< Debug.runInterpreterStdout mempty program
  where
    program :: (Members '[PAL] r) => Sem r (Either Err Type)
    program =
      [palQuasiQuoter|
        type Num
        type Bool

        expr LitInt : Num
        expr True   : Bool
        expr False  : Bool

        rule Add:
          x : Num
          y : Num
        ->
          Add(x, y) : Num

        infer Add(True, LitInt)
        infer Add(LitInt)
        infer Add(LitInt, LitInt)
      |]

------------------------------------------------------------
-- 4. A polymorphic example: Simply Typed Lambda Calculus
------------------------------------------------------------

-- | Run the 'stlc' example, which uses type variables in its rules.
mainStlc :: IO ()
mainStlc = either print print =<< Debug.runInterpreterStdout mempty stlc

-- | A minimal STLC fragment. Lowercase type names in rules (@a@, @b@) are
-- type variables, solved by unification. The 'Lam' rule's premise
-- @x : a |- body : b@ checks the body with the parameter @x@ in scope.
stlc :: (Members '[PAL] r) => Sem r (Either Err Type)
stlc = [palQuasiQuoter|
-- Simply Typed Lambda Calculus (minimal)

-- Base type
type Bool

-- Function type constructor (a -> b)
type Arrow

-- Core forms (as constructors)
expr True  : Bool
expr False : Bool
expr If    : Arrow
expr Lam   : Arrow
expr App   : Arrow

-- If: Bool-guarded branch with matching branch types
rule If:
  cond : Bool
  then : a
  else : a
->
  If(cond, then, else) : a

-- Application: (a -> b) applied to a gives b
rule App:
  f : Arrow<a, b>
  x : a
->
  App(f, x) : b

-- Lambda: λx. body has type a -> b if body : b when x : a
rule Lam:
  x : a |- body : b
->
  Lam(x, body) : Arrow<a, b>

-- ======================================
-- Inference examples
-- ======================================

-- 1. Simple boolean expression
infer If(True, False, True)                        -- Bool

-- 2. Nested conditionals
infer If(True, If(False, True, False), True)       -- Bool

-- 3. Identity function (λx. x)
infer Lam(x, x)                                 -- Arrow<a, a>

-- 4. Constant function (λx. True)
infer Lam(x, True)                              -- Arrow<a, Bool>

-- 5. Applying identity to a boolean
infer App(Lam(x, x), True)                      -- Bool

-- 6. Higher-order: (λx. λy. x)
infer Lam(x, Lam(y, x))                         -- Arrow<a, Arrow<b, a>>

-- 7. Applying a higher-order function:
-- (λf. f True) (λx. x)
infer App(Lam(f, App(f, True)), Lam(x, x))      -- Bool

-- 8. Nested application: ((λx. x) (λy. y))
infer App(Lam(x, x), Lam(y, y))                 -- Arrow<a, a>
|]
