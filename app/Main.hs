-- |
-- Module      : Main
-- Description : The @pal@ command: run .pal files or start a REPL.
--
-- > pal                    start an interactive REPL
-- > pal FILE...            run .pal files (in order, sharing one context)
-- > pal -i FILE...         run the files, then start a REPL with their context
-- > pal --derivations …    draw the derivation tree under each result
-- > pal --no-color …       never use colors (also: NO_COLOR=1)
--
-- When running files, the exit code is 1 if a file fails to parse, a
-- definition is rejected, an @infer@ fails, or a @check@ / @fails@ is not
-- met, so @pal@ can be used to check programs in scripts or CI.
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, unless)
import Data.Maybe (fromMaybe)
import Pretty (Attr (..), Style, detectStyle, failure, paint, plain, prettyParseError)
import Program (Outcome (..), Step (..), loadPalFileLocated, runSteps, succeeded)
import Repl (runRepl)
import Report (Report (..), Source (..), defaultReport, reportStep)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStr, hSetEncoding, stderr, stdin, stdout, utf8)
import Text.Read (readMaybe)
import Types (Ctx)

-- | Output settings from the command line.
data Options = Options
  { outStyle :: Style,
    errStyle :: Style,
    -- | Draw derivation trees under successful results.
    derivations :: Bool,
    -- | Terminal width, for derivation trees.
    width :: Int
  }

main :: IO ()
main = do
  -- PAL output uses symbols such as ✓, ✗ and ⊢.
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  args <- getArgs
  let noColor = "--no-color" `elem` args
      style h = if noColor then pure plain else detectStyle h
  columns <- (>>= readMaybe) <$> lookupEnv "COLUMNS"
  opts <- Options <$> style stdout <*> style stderr <*> pure ("--derivations" `elem` args) <*> pure (fromMaybe 100 columns)
  case filter (`notElem` ["--no-color", "--derivations"]) args of
    [] -> runRepl (outStyle opts) mempty
    [flag] | flag `elem` ["-h", "--help"] -> putStr usage
    (flag : files) | flag `elem` ["-i", "--interactive"] -> do
      (ctx, _) <- runFiles opts files
      runRepl (outStyle opts) ctx
    files@(f : _) | take 1 f /= "-" -> do
      (_, ok) <- runFiles opts files
      unless ok exitFailure
    _ -> hPutStr stderr usage >> exitFailure

-- | Run files in order in one shared context, printing every result. Stops
--   at the first file that fails to parse. Returns the final context and
--   whether everything succeeded: no rejected definitions, no failed
--   @infer@, and every @check@ and @fails@ met.
runFiles :: Options -> [FilePath] -> IO (Ctx, Bool)
runFiles opts files = do
  (ctx, steps) <- foldM step (mempty, []) files
  let expectations = [met | Step _ (Expected _ _ _ met) <- steps]
      unmet = length (filter not expectations)
  unless (null expectations) . putStrLn $
    if unmet == 0
      then paint (outStyle opts) [Bold, Green] (count (length expectations) "check" <> " passed")
      else paint (outStyle opts) [Bold, Red] (show unmet <> " of " <> count (length expectations) "check" <> " failed")
  pure (ctx, all (succeeded . step'outcome) steps)
  where
    step (ctx, done) file =
      try (loadPalFileLocated file) >>= \case
        Left (e :: IOException) -> die (failure (errStyle opts) ("cannot read " <> file <> ": " <> show e) <> "\n")
        Right (_, Left err) -> die (prettyParseError (errStyle opts) err)
        Right (src, Right actions) -> do
          let (ctx', steps) = runSteps ctx actions
              report =
                defaultReport
                  { report'style = outStyle opts,
                    report'source = Just (Source file src),
                    report'derivations = derivations opts,
                    report'width = width opts
                  }
          mapM_ (mapM_ putStrLn . reportStep report) steps
          pure (ctx', done <> steps)
    die msg = hPutStr stderr msg >> exitFailure
    count n word = show n <> " " <> word <> (if n == 1 then "" else "s")

usage :: String
usage =
  unlines
    [ "Usage:",
      "  pal                 start an interactive REPL",
      "  pal FILE...         run .pal files (in order, sharing one context)",
      "  pal -i FILE...      run the files, then start a REPL with their context",
      "",
      "Options:",
      "  --derivations       draw the derivation tree under each successful result",
      "  --no-color          plain output (colors are also off when NO_COLOR is set",
      "                      or output is not a terminal)",
      "",
      "When running files, the exit code is 1 if a file fails to parse, a",
      "definition is rejected, an infer fails, or a check or fails is not met."
    ]
