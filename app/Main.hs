-- |
-- Module      : Main
-- Description : The @pal@ command: run .pal files or start a REPL.
--
-- > pal                    start an interactive REPL
-- > pal FILE...            run .pal files (in order, sharing one context)
-- > pal -i FILE...         run the files, then start a REPL with their context
--
-- When running files, the exit code is 1 if a file fails to parse or any
-- inference fails, so @pal@ can be used to check programs in scripts or CI.
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, unless)
import Data.Either (isLeft)
import Interpreters.IO (defaultIOOptions, runActionsIO)
import Program (loadPalFile)
import Repl (runRepl)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, hSetEncoding, stderr, stdin, stdout, utf8)
import Types (Ctx)

main :: IO ()
main = do
  -- PAL output uses symbols such as ✓, ✗ and ⊢.
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  getArgs >>= \case
    [] -> runRepl mempty
    [flag] | flag `elem` ["-h", "--help"] -> putStr usage
    (flag : files) | flag `elem` ["-i", "--interactive"] -> do
      (ctx, _) <- runFiles files
      runRepl ctx
    files@(f : _) | take 1 f /= "-" -> do
      (_, ok) <- runFiles files
      unless ok exitFailure
    _ -> hPutStr stderr usage >> exitFailure

-- | Run files in order in one shared context. Stops at the first file that
--   fails to parse. Returns the final context and whether every inference
--   succeeded.
runFiles :: [FilePath] -> IO (Ctx, Bool)
runFiles = foldM step (mempty, True)
  where
    step (ctx, ok) file =
      try (loadPalFile file) >>= \case
        Left (e :: IOException) -> hPutStrLn stderr ("cannot read " <> file <> ": " <> show e) >> exitFailure
        Right (Left err) -> hPutStr stderr err >> exitFailure
        Right (Right actions) -> do
          (ctx', results) <- runActionsIO defaultIOOptions ctx actions
          pure (ctx', ok && not (any isLeft results))

usage :: String
usage =
  unlines
    [ "Usage:",
      "  pal                 start an interactive REPL",
      "  pal FILE...         run .pal files (in order, sharing one context)",
      "  pal -i FILE...      run the files, then start a REPL with their context",
      "",
      "When running files, the exit code is 1 if a file fails to parse",
      "or any inference fails."
    ]
