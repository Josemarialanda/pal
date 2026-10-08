-- |
-- Module      : Interpreters.IO
-- Description : IO interpreter for PAL programs
--
-- This module defines the **IO interpreter**: it runs PAL programs in 'IO' and
-- prints one line per inference result to stdout, without the context dumps
-- of "Interpreters.Debug".
--
-- Unlike the other interpreters, it can also return the final typing context
-- ('runInterpreterIOWithCtx'), so a context can be carried from one program
-- into the next. The @pal@ command uses this to run files and to power its
-- interactive REPL ("Repl").
--
-- Example output:
--
-- > ✓ Add(LitInt, LitInt) :: Num
-- > ✗ Add(True, LitInt) -> [Error] Type mismatch: expected Num, got Bool
module Interpreters.IO
  ( -- * Options
    IOOptions (..),
    defaultIOOptions,

    -- * Running programs
    runInterpreterIO,
    runInterpreterIOWithCtx,
    runActionsIO,

    -- * The interpreter
    interpreter,
    formatResult,
  )
where

import Control.Monad (when)
import Data.Maybe (catMaybes)
import qualified Interpreters.Common.Actions as Actions
import Polysemy (Embed, Members, Sem, embed, interpret, runM)
import Polysemy.State (State, get, runState)
import Program (PalAction, runPalAction)
import Types (Ctx, Err, Expr, PAL (..), Type, typingRule'name)

--------------------------------------------------------------------------------

-- | Options

--------------------------------------------------------------------------------

-- | What the IO interpreter prints.
newtype IOOptions = IOOptions
  { -- | Also print a line for every definition (@defined type Num@, …),
    --   not just inference results. The REPL turns this on.
    ioEchoDefinitions :: Bool
  }

-- | Print inference results only.
defaultIOOptions :: IOOptions
defaultIOOptions = IOOptions {ioEchoDefinitions = False}

--------------------------------------------------------------------------------

-- | Running programs

--------------------------------------------------------------------------------

-- | Run a PAL program in 'IO', printing inference results as they happen.
runInterpreterIO ::
  IOOptions ->
  Ctx ->
  Sem '[PAL, State Ctx, Embed IO] a ->
  IO a
runInterpreterIO opts ctx = fmap snd . runInterpreterIOWithCtx opts ctx

-- | Like 'runInterpreterIO', but also return the final typing context.
runInterpreterIOWithCtx ::
  IOOptions ->
  Ctx ->
  Sem '[PAL, State Ctx, Embed IO] a ->
  IO (Ctx, a)
runInterpreterIOWithCtx opts ctx =
  runM -- Run in 'IO'
    . runState ctx -- Thread the context and return its final value
    . interpreter opts -- Interpret PAL actions

-- | Run a list of 'PalAction's, returning the final context and every
--   inference result in order.
runActionsIO :: IOOptions -> Ctx -> [PalAction] -> IO (Ctx, [Either Err Type])
runActionsIO opts ctx actions =
  fmap catMaybes <$> runInterpreterIOWithCtx opts ctx (traverse runPalAction actions)

--------------------------------------------------------------------------------

-- | PAL effect interpreter

--------------------------------------------------------------------------------

-- | Interpret the 'PAL' effect, printing to stdout.
--
--   Definitions update the context (and are echoed if 'ioEchoDefinitions' is
--   set); each 'Infer' prints its result using 'formatResult'.
interpreter ::
  (Members '[State Ctx, Embed IO] r) =>
  IOOptions ->
  Sem (PAL ': r) a ->
  Sem r a
interpreter opts = interpret $ \case
  DefineType td -> do
    Actions.defineType td
    echo ("defined " <> show td)
  DefineExpr ed -> do
    Actions.defineExpr ed
    echo ("defined expr " <> show ed)
  DefineRule tr -> do
    Actions.defineRule tr
    echo ("defined rule " <> typingRule'name tr)
  Infer e -> do
    r <- Actions.infer e <$> get
    embed (putStrLn (formatResult e r))
    pure r
  where
    echo :: (Members '[Embed IO] r') => String -> Sem r' ()
    echo msg = when (ioEchoDefinitions opts) (embed (putStrLn msg))

-- | Format an inference result as a single line.
formatResult :: Expr -> Either Err Type -> String
formatResult e = \case
  Right t -> "✓ " <> show e <> " :: " <> show t
  Left err -> "✗ " <> show e <> " -> " <> show err
