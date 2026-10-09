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

import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Interpreters.Common.Actions as Actions
import Polysemy (Embed, Members, Sem, embed, interpret, runM)
import Polysemy.State (State, get, runState)
import Pretty (Style, plain, prettyResult)
import Program (Outcome (..), PalAction (..), runPalAction)
import Report (Report (..), defaultReport, reportOutcome)
import Types (Ctx, Derivation (..), Err, Expr, Failure (..), PAL (..), Type)

--------------------------------------------------------------------------------

-- | Options

--------------------------------------------------------------------------------

-- | What the IO interpreter prints, and how.
data IOOptions = IOOptions
  { -- | Also print a line for every definition (@defined type Num@, …),
    --   not just inference results. The REPL turns this on.
    ioEchoDefinitions :: Bool,
    -- | Colors and text attributes (see "Pretty"). With 'plain', output is
    --   the one-line format shown in the documentation.
    ioStyle :: Style
  }

-- | Print inference results only, without colors.
defaultIOOptions :: IOOptions
defaultIOOptions = IOOptions {ioEchoDefinitions = False, ioStyle = plain}

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
--   set); each 'Infer' prints its result, styled with 'ioStyle'.
interpreter ::
  (Members '[State Ctx, Embed IO] r) =>
  IOOptions ->
  Sem (PAL ': r) a ->
  Sem r a
interpreter opts = interpret $ \case
  DefineType td -> define (ADefineType td) (Actions.defineType td)
  DefineExpr ed -> define (ADefineExpr ed) (Actions.defineExpr ed)
  DefineRule tr -> define (ADefineRule tr) (Actions.defineRule tr)
  Infer e -> do
    result <- (`Actions.inferDetailed` e) <$> get
    say (AInfer e) (Inferred result)
    pure (either (Left . failure'err . NE.head) (Right . deriv'type) result)
  Expect x -> do
    (diagnostics, result, met) <- (`Actions.checkExpectation` x) <$> get
    say (AExpect x) (Expected x diagnostics result met)
    pure met
  where
    report = defaultReport {report'style = ioStyle opts, report'echoDefinitions = ioEchoDefinitions opts}
    say :: (Members '[Embed IO] r') => PalAction -> Outcome -> Sem r' ()
    say action outcome = embed (mapM_ putStrLn (reportOutcome report Nothing action outcome))
    define action act = do
      diagnostics <- act
      say action (if Actions.hasErrors diagnostics then Rejected diagnostics else Defined diagnostics)

-- | Format an inference result as a single plain line.
formatResult :: Expr -> Either Err Type -> String
formatResult = prettyResult plain
