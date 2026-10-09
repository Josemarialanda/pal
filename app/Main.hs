-- |
-- Module      : Main
-- Description : The @pal@ command: run .pal files or start a REPL.
--
-- > pal                    start an interactive REPL
-- > pal FILE...            run .pal files (in order, sharing one context)
-- > pal -i FILE...         run the files, then start a REPL with their context
-- > pal --no-color …       never use colors (also: NO_COLOR=1)
--
-- When running files, the exit code is 1 if a file fails to parse or any
-- inference fails, so @pal@ can be used to check programs in scripts or CI.
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, unless)
import Data.Either (isLeft)
import Interpreters.IO (IOOptions (..), defaultIOOptions, runActionsIO)
import Pretty (Style, detectStyle, failure, plain, prettyParseError)
import Program (loadPalFile)
import Repl (runRepl)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hSetEncoding, stderr, stdin, stdout, utf8)
import Types (Ctx)

-- | Output styles for stdout and stderr.
data Styles = Styles {outStyle :: Style, errStyle :: Style}

main :: IO ()
main = do
  -- PAL output uses symbols such as ✓, ✗ and ⊢.
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  args <- getArgs
  let noColor = "--no-color" `elem` args
      style h = if noColor then pure plain else detectStyle h
  styles <- Styles <$> style stdout <*> style stderr
  case filter (/= "--no-color") args of
    [] -> runRepl (outStyle styles) mempty
    [flag] | flag `elem` ["-h", "--help"] -> putStr usage
    (flag : files) | flag `elem` ["-i", "--interactive"] -> do
      (ctx, _) <- runFiles styles files
      runRepl (outStyle styles) ctx
    files@(f : _) | take 1 f /= "-" -> do
      (_, ok) <- runFiles styles files
      unless ok exitFailure
    _ -> hPutStr stderr usage >> exitFailure

-- | Run files in order in one shared context. Stops at the first file that
--   fails to parse. Returns the final context and whether every inference
--   succeeded.
runFiles :: Styles -> [FilePath] -> IO (Ctx, Bool)
runFiles styles = foldM step (mempty, True)
  where
    step (ctx, ok) file =
      try (loadPalFile file) >>= \case
        Left (e :: IOException) -> die (failure (errStyle styles) ("cannot read " <> file <> ": " <> show e) <> "\n")
        Right (Left err) -> die (prettyParseError (errStyle styles) err)
        Right (Right actions) -> do
          (ctx', results) <- runActionsIO defaultIOOptions {ioStyle = outStyle styles} ctx actions
          pure (ctx', ok && not (any isLeft results))
    die msg = hPutStr stderr msg >> exitFailure

usage :: String
usage =
  unlines
    [ "Usage:",
      "  pal                 start an interactive REPL",
      "  pal FILE...         run .pal files (in order, sharing one context)",
      "  pal -i FILE...      run the files, then start a REPL with their context",
      "",
      "Options:",
      "  --no-color          plain output (colors are also off when NO_COLOR is set",
      "                      or output is not a terminal)",
      "",
      "When running files, the exit code is 1 if a file fails to parse",
      "or any inference fails."
    ]
