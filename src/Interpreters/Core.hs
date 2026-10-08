-- |
-- Module      : Interpreters.Core
-- Description : Pure, side-effect-free interpreter for PAL programs
--
-- This module defines the **core interpreter** for the PAL DSL — a minimal,
-- pure backend that executes PAL programs without any tracing or IO effects.
--
-- It provides the simplest possible evaluation pipeline:
--   * maintains a mutable typing context ('Ctx') using the 'State' effect,
--   * executes each 'PAL' operation in order,
--   * and returns either a successful inferred type or a type error ('Err').
module Interpreters.Core where

import qualified Interpreters.Common.Actions as Actions
import Polysemy (Members, Sem, interpret, run)
import Polysemy.State (State, evalState, get)
import Types
  ( Ctx,
    Err,
    PAL (..),
    Type,
  )

--------------------------------------------------------------------------------

-- | Entry point for running a PAL program

--------------------------------------------------------------------------------

-- | Execute a PAL program in a pure context, producing either an error or a type.
--
--   This function initializes the typing context ('Ctx'), runs the interpreter,
--   and extracts the final result using 'run'.
runInterpreter ::
  Ctx ->
  Sem [PAL, State Ctx] (Either Err Type) ->
  Either Err Type
runInterpreter ctx =
  run -- Collapse the Polysemy stack into a pure value
    . evalState ctx -- Initialize and thread the mutable context
    . interpreter -- Interpret PAL effects into state transitions

--------------------------------------------------------------------------------

-- | PAL effect interpreter

--------------------------------------------------------------------------------

-- | Core interpreter for the 'PAL' effect.
--
--   Each constructor of the 'PAL' DSL corresponds to an action
--   implemented in "Interpreters.Common.Actions":
--
--   * 'DefineType' — register a new type declaration.
--   * 'DefineExpr' — define a new expression and its type.
--   * 'DefineRule' — add a new typing rule to the context.
--   * 'Infer'      — perform type inference for a given expression.
interpreter ::
  (Members '[State Ctx] r) =>
  Sem (PAL ': r) (Either Err Type) ->
  Sem r (Either Err Type)
interpreter = interpret $ \case
  DefineType td -> Actions.defineType td
  DefineExpr ed -> Actions.defineExpr ed
  DefineRule tr -> Actions.defineRule tr
  Infer e -> Actions.infer e <$> get
