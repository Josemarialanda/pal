-- |
-- Module      : Repl
-- Description : Interactive PAL session on stdin/stdout
--
-- A read–eval–print loop built on the IO interpreter ("Interpreters.IO").
-- Each input is parsed as PAL statements and run against a context that
-- persists for the whole session:
--
-- > pal> type Bool
-- > defined type Bool
-- > pal> expr True : Bool
-- > defined expr True : Bool
-- > pal> rule Not:
-- > ...>   x : Bool
-- > ...> ->
-- > ...>   Not(x) : Bool
-- > defined rule Not
-- > pal> Not(True)
-- > ✓ Not(True) :: Bool
--
-- Input that is not finished yet (like the rule above) continues on the next
-- line; a blank line ends it early. A bare expression is shorthand for
-- @infer@. Lines starting with @:@ are commands; see 'helpText'.
module Repl
  ( runRepl,
    ReplInput (..),
    parseInput,
    helpText,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum, isSpace)
import Data.List (dropWhileEnd)
import qualified Data.List.NonEmpty as NE
import Interpreters.IO (IOOptions (..), defaultIOOptions, runActionsIO)
import Parser.Parser (pExprApp, palProgram, sc)
import Program (PalAction (..), fromStmt, loadPalFile)
import System.Console.Haskeline
  ( InputT,
    Settings (..),
    defaultSettings,
    getInputLine,
    handleInterrupt,
    outputStrLn,
    runInputT,
    withInterrupt,
  )
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdin)
import Text.Megaparsec (ParseErrorBundle (..), eof, errorBundlePretty, errorOffset, parse)
import Types (Ctx)

-- | Start a REPL with the given initial context.
--
--   Line editing uses Haskeline: ←/→ move the cursor, ↑/↓ browse history,
--   Ctrl-R searches it, Tab completes file names (for @:load@), and Ctrl-C
--   discards the current input. In a terminal, history is saved to
--   @~/.pal_history@.
--
--   Prompts, the banner and the history file are only used when stdin is a
--   terminal, so a script can also be piped in: @pal < script.pal@.
runRepl :: Ctx -> IO ()
runRepl ctx0 = do
  interactive <- hIsTerminalDevice stdin
  settings <- replSettings interactive
  runInputT settings $ withInterrupt $ do
    when interactive $
      outputStrLn "PAL REPL — type :help for commands, :quit to exit."
    loop interactive (ctx0, "")

-- | Haskeline settings: default key bindings and file name completion, plus
--   a history file in interactive sessions.
replSettings :: Bool -> IO (Settings IO)
replSettings interactive = do
  home <- lookupEnv "HOME"
  let history = if interactive then (<> "/.pal_history") <$> home else Nothing
  pure defaultSettings {historyFile = history}

-- | The main loop. The state is the context and the lines of an unfinished
--   input. Ctrl-C (at the prompt or while a step runs) discards the
--   unfinished input and keeps the context.
loop :: Bool -> (Ctx, String) -> InputT IO ()
loop interactive st@(ctx, _) = do
  next <- handleInterrupt (Just (ctx, "") <$ outputStrLn "Interrupted.") (step interactive st)
  maybe (pure ()) (loop interactive) next

-- | Read and handle one line. Returns the next state, or 'Nothing' to quit.
step :: Bool -> (Ctx, String) -> InputT IO (Maybe (Ctx, String))
step interactive (ctx, buffer) =
  getInputLine prompt >>= \case
    Nothing -> do
      -- End of input: run whatever is left, reporting it if it is incomplete.
      unless (all isSpace buffer) $ void (liftIO (evalInput True ctx buffer))
      pure Nothing
    Just line -> case trim line of
      ':' : cmd | null buffer -> fmap (,"") <$> liftIO (command ctx (words cmd))
      "" | not (null buffer) -> do
        -- A blank line ends an unfinished input.
        ctx' <- liftIO (evalInput True ctx buffer)
        pure (Just (ctx', ""))
      _ -> do
        let buffer' = buffer <> line <> "\n"
        case parseInput buffer' of
          Incomplete _ -> pure (Just (ctx, buffer'))
          _ -> Just . (,"") <$> liftIO (evalInput False ctx buffer')
  where
    prompt
      | not interactive = ""
      | null buffer = "pal> "
      | otherwise = "...> "

-- | Parse and run an input. With @force@, an incomplete input is an error.
evalInput :: Bool -> Ctx -> String -> IO Ctx
evalInput force ctx src = case parseInput src of
  Complete actions -> fst <$> runActionsIO replOptions ctx actions
  Incomplete err | force -> ctx <$ putStr err
  Incomplete _ -> pure ctx
  Invalid err -> ctx <$ putStr err

-- | The REPL echoes definitions so every input gets a response.
replOptions :: IOOptions
replOptions = defaultIOOptions {ioEchoDefinitions = True}

--------------------------------------------------------------------------------

-- | Parsing input

--------------------------------------------------------------------------------

-- | The result of parsing (possibly partial) REPL input.
data ReplInput
  = -- | Ready to run.
    Complete [PalAction]
  | -- | The input stopped early (e.g. a rule without its conclusion yet);
    --   more lines may complete it. Carries the error to show if not.
    Incomplete String
  | -- | A syntax error that more input cannot fix.
    Invalid String

-- | Parse REPL input: PAL statements, or a single bare expression to infer.
parseInput :: String -> ReplInput
parseInput src =
  case parse palProgram "<input>" src of
    Right stmts -> Complete (fmap fromStmt stmts)
    Left progErr
      | startsWithKeyword -> classify progErr
      | otherwise -> case parse (sc *> pExprApp <* eof) "<input>" src of
          Right e -> Complete [AInfer e]
          Left exprErr
            | atEnd exprErr -> Incomplete (errorBundlePretty exprErr)
            | otherwise -> classify progErr
  where
    classify err
      | atEnd err = Incomplete (errorBundlePretty err)
      | otherwise = Invalid (errorBundlePretty err)
    atEnd err = errorOffset (NE.head (bundleErrors err)) >= length src
    startsWithKeyword =
      takeWhile isAlphaNum (dropWhile isSpace src) `elem` ["type", "expr", "rule", "infer"]

--------------------------------------------------------------------------------

-- | Commands

--------------------------------------------------------------------------------

-- | Run a @:command@. Returns the new context, or 'Nothing' to quit.
command :: Ctx -> [String] -> IO (Maybe Ctx)
command ctx = \case
  [c] | c `elem` ["q", "quit"] -> pure Nothing
  [c] | c `elem` ["h", "help", "?"] -> Just ctx <$ putStr helpText
  ["ctx"] -> Just ctx <$ putStr (show ctx)
  ["reset"] -> Just mempty <$ putStrLn "context cleared"
  ("load" : files@(_ : _)) -> Just <$> loadFiles ctx files
  _ -> Just ctx <$ putStrLn "unknown command; type :help for a list"

-- | Load @.pal@ files into the context, printing their inference results.
loadFiles :: Ctx -> [FilePath] -> IO Ctx
loadFiles ctx [] = pure ctx
loadFiles ctx (file : rest) = do
  loaded <- try (loadPalFile file)
  case loaded of
    Left (e :: IOException) -> ctx <$ putStrLn ("cannot read " <> file <> ": " <> show e)
    Right (Left err) -> ctx <$ putStr err
    Right (Right actions) -> do
      (ctx', _) <- runActionsIO defaultIOOptions ctx actions
      putStrLn ("loaded " <> file)
      loadFiles ctx' rest

-- | The text shown by @:help@.
helpText :: String
helpText =
  unlines
    [ "Enter PAL statements (type, expr, rule, infer) or a bare expression to infer.",
      "Unfinished input continues on the next line; a blank line ends it.",
      "",
      "Commands:",
      "  :load FILE...   run .pal files in the current context",
      "  :ctx            show the current context",
      "  :reset          clear the context",
      "  :help           show this help",
      "  :quit           exit (or Ctrl-D)",
      "",
      "Keys: ←/→ move the cursor, ↑/↓ browse history, Ctrl-R search history,",
      "      Tab complete file names, Ctrl-C discard the current input."
    ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
