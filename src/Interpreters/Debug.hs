{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Interpreters.Debug
-- Description : Debug and trace interpreters for PAL programs
--
-- This module provides **debugging-oriented interpreters** for PAL programs.
--
-- Unlike pure interpreters, these variants emit trace information to help visualize
-- the evaluation process — making them ideal for learning, debugging, or explaining
-- type inference behavior in a REPL or terminal.
--
-- Two main interpreters are defined:
--
-- * 'runInterpreterStdout' — runs a PAL program and streams traces directly
--   to stdout via 'traceToStdout'.
--
-- * 'runInterpreterTraceList' — executes a PAL program and returns a tuple of
--   all trace messages collected during execution, along with the final result.
--
-- Both interpreters share the same core 'interpreter', which interprets the
-- 'PAL' effect by:
--   - mutating the typing context ('Ctx'),
--   - recording trace messages,
--   - and invoking semantic operations defined in "Interpreters.Common.Actions".
--
-- These interpreters are meant for *inspection* and *understanding*, not
-- performance-critical evaluation.
module Interpreters.Debug where

import qualified Interpreters.Common.Actions as Actions
import Polysemy (Embed, Members, Sem, interpret, run, runM)
import Polysemy.State (State, evalState, get)
import Polysemy.Trace (Trace, runTraceList, trace, traceToStdout)
import Types (Ctx, Err, PAL (..), Type)

--------------------------------------------------------------------------------

-- | Stdout interpreter

--------------------------------------------------------------------------------

-- | Run a PAL program, printing trace messages to the terminal.
--
--   This interpreter uses 'traceToStdout' to stream debug information in real time.
--   It is useful for observing the evolution of the typing context ('Ctx') and
--   type inference decisions as the program executes.
runInterpreterStdout ::
  Ctx ->
  Sem '[PAL, State Ctx, Trace, Embed IO] (Either Err Type) ->
  IO (Either Err Type)
runInterpreterStdout ctx =
  runM -- Run in 'IO'
    . traceToStdout -- Print traces live to stdout
    . evalState ctx -- Initialize context state
    . interpreter -- Interpret PAL actions

--------------------------------------------------------------------------------

-- | Pure trace-list interpreter

--------------------------------------------------------------------------------

-- | Run a PAL program and collect all trace messages in-memory.
--
--   Unlike 'runInterpreterStdout', this version is **pure** and returns all
--   traces as a list, allowing post-processing or structured testing.
runInterpreterTraceList ::
  Ctx ->
  Sem '[PAL, State Ctx, Trace] (Either Err Type) ->
  ([String], Either Err Type)
runInterpreterTraceList ctx =
  run
    . runTraceList -- Collect traces as a list
    . evalState ctx -- Initialize context state
    . interpreter -- Interpret PAL actions

--------------------------------------------------------------------------------

-- | Core PAL interpreter (debug version)

--------------------------------------------------------------------------------

-- | The core PAL interpreter used by all debug variants.
--
--   Each PAL effect is handled by invoking the corresponding semantic function
--   from "Interpreters.Common.Actions" while logging its behavior using 'Trace'.
--
--   For example, when 'DefineRule' is encountered, the interpreter:
--     1. logs the action being performed,
--     2. applies 'Actions.defineRule',
--     3. logs the updated context,
--     4. and continues execution.
--
--   This interpreter exposes the full trace of:
--     * rule/expr/type definitions,
--     * inference steps,
--     * and the final context state.
interpreter ::
  (Members '[State Ctx, Trace] r) =>
  Sem (PAL ': r) (Either Err Type) ->
  Sem r (Either Err Type)
interpreter = interpret $ \case
  DefineType td -> do
    trace $ "[PAL] Defining type: " <> show td
    Actions.defineType td
    ctx <- get @Ctx
    trace $ show ctx

  DefineExpr ed -> do
    trace $ "[PAL] Defining expression: " <> show ed
    Actions.defineExpr ed
    ctx <- get @Ctx
    trace $ show ctx

  DefineRule tr -> do
    trace $ "[PAL] Defining rule: " <> show tr
    Actions.defineRule tr
    ctx <- get @Ctx
    trace $ show ctx

  Infer e -> do
    trace $ "[PAL] Inferring type for expression: " <> show e
    ctx <- get
    let r = Actions.infer e ctx
    trace $
      case r of
        Right t -> "[PAL] ✓ " <> show e <> " :: " <> show t
        Left err -> "[PAL] ✗ " <> show e <> " -> " <> show err
    trace $ show ctx
    pure r
