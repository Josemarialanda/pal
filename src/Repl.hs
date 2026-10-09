-- |
-- Module      : Repl
-- Description : Interactive PAL session on stdin/stdout
--
-- A read–eval–print loop built on the IO interpreter ("Interpreters.IO").
-- Each input is parsed as PAL statements and run against a context that
-- persists for the whole session:
--
-- > pal❯ type Bool
-- > defined type Bool
-- > pal❯ expr True : Bool
-- > defined expr True : Bool
-- > pal❯ rule Not:
-- >    ┆   x : Bool
-- >    ┆ ->
-- >    ┆   Not(x) : Bool
-- > defined rule Not
-- > pal❯ Not(True)
-- > ✓ Not(True) :: Bool
--
-- Input that is not finished yet (like the rule above) continues on the next
-- line; a blank line ends it early. A bare expression is shorthand for
-- @infer@. Lines starting with @:@ are commands; see 'helpText'.
--
-- In a terminal, output is colored and highlighted (see "Pretty"): rules are
-- drawn as inference rules and errors get their own line.
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
import Pretty
  ( Attr (..),
    Style (..),
    box,
    failure,
    note,
    paint,
    paintPrompt,
    prettyCtx,
    prettyParseError,
    success,
  )
import Program (PalAction (..), fromStmt, loadPalFile)
import System.Console.Haskeline
  ( InputT,
    Settings (..),
    defaultSettings,
    getInputLine,
    handleInterrupt,
    outputStr,
    outputStrLn,
    runInputT,
    withInterrupt,
  )
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdin)
import Text.Megaparsec (ParseErrorBundle (..), eof, errorBundlePretty, errorOffset, parse)
import Types (Ctx)

-- | How the REPL talks to the user.
data ReplEnv = ReplEnv
  { -- | stdin is a terminal: show the banner and prompts, keep history.
    reInteractive :: Bool,
    -- | Colors and text attributes for output.
    reStyle :: Style
  }

-- | Start a REPL with the given output style and initial context.
--
--   Line editing uses Haskeline: ←/→ move the cursor, ↑/↓ browse history,
--   Ctrl-R searches it, Tab completes file names (for @:load@), and Ctrl-C
--   discards the current input. In a terminal, history is saved to
--   @~/.pal_history@.
--
--   Prompts, the banner and the history file are only used when stdin is a
--   terminal, so a script can also be piped in: @pal < script.pal@.
runRepl :: Style -> Ctx -> IO ()
runRepl st ctx0 = do
  interactive <- hIsTerminalDevice stdin
  settings <- replSettings interactive
  let env = ReplEnv interactive st
  runInputT settings $ withInterrupt $ do
    when interactive $ outputStr (banner st)
    loop env (ctx0, "")

-- | The greeting shown when the REPL starts.
banner :: Style -> String
banner st
  | styleEnabled st =
      box
        st
        [ paint st [Bold, Magenta] "λ PAL" <> note st "  ·  a typechecker sandbox",
          note st "  " <> cmd ":help" <> note st " for commands  ·  " <> cmd ":quit" <> note st " or Ctrl-D to exit"
        ]
  | otherwise = "PAL REPL — type :help for commands, :quit to exit.\n"
  where
    cmd = paint st [Cyan]

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
loop :: ReplEnv -> (Ctx, String) -> InputT IO ()
loop env st@(ctx, _) = do
  next <- handleInterrupt (Just (ctx, "") <$ outputStrLn (note (reStyle env) "Interrupted.")) (step env st)
  maybe (pure ()) (loop env) next

-- | Read and handle one line. Returns the next state, or 'Nothing' to quit.
step :: ReplEnv -> (Ctx, String) -> InputT IO (Maybe (Ctx, String))
step env (ctx, buffer) =
  getInputLine prompt >>= \case
    Nothing -> do
      -- End of input: run whatever is left, reporting it if it is incomplete.
      unless (all isSpace buffer) $ void (liftIO (evalInput st True ctx buffer))
      pure Nothing
    Just line -> case trim line of
      ':' : cmd | null buffer -> fmap (,"") <$> liftIO (command st ctx (words cmd))
      "" | not (null buffer) -> do
        -- A blank line ends an unfinished input.
        ctx' <- liftIO (evalInput st True ctx buffer)
        pure (Just (ctx', ""))
      _ -> do
        let buffer' = buffer <> line <> "\n"
        case parseInput buffer' of
          Incomplete _ -> pure (Just (ctx, buffer'))
          _ -> Just . (,"") <$> liftIO (evalInput st False ctx buffer')
  where
    st = reStyle env
    -- Both prompts are five columns wide, so continued lines line up.
    prompt
      | not (reInteractive env) = ""
      | null buffer = paintPrompt st [Bold, Magenta] "pal" <> paintPrompt st [Bold, Cyan] "❯" <> " "
      | otherwise = paintPrompt st [Dim] "   ┆ "

-- | Parse and run an input. With @force@, an incomplete input is an error.
evalInput :: Style -> Bool -> Ctx -> String -> IO Ctx
evalInput st force ctx src = case parseInput src of
  Complete actions -> fst <$> runActionsIO (replOptions st) ctx actions
  Incomplete err | force -> ctx <$ putStr (prettyParseError st err)
  Incomplete _ -> pure ctx
  Invalid err -> ctx <$ putStr (prettyParseError st err)

-- | The REPL echoes definitions so every input gets a response.
replOptions :: Style -> IOOptions
replOptions st = defaultIOOptions {ioEchoDefinitions = True, ioStyle = st}

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
command :: Style -> Ctx -> [String] -> IO (Maybe Ctx)
command st ctx = \case
  [c] | c `elem` ["q", "quit"] -> pure Nothing
  [c] | c `elem` ["h", "help", "?"] -> Just ctx <$ putStr (helpText st)
  ["ctx"] -> Just ctx <$ putStr (if styleEnabled st then prettyCtx st ctx else show ctx)
  ["reset"] -> Just mempty <$ putStrLn (note st "context cleared")
  ("load" : files@(_ : _)) -> Just <$> loadFiles st ctx files
  _ -> Just ctx <$ putStrLn (failure st "unknown command; type :help for a list")

-- | Load @.pal@ files into the context, printing their inference results.
loadFiles :: Style -> Ctx -> [FilePath] -> IO Ctx
loadFiles _ ctx [] = pure ctx
loadFiles st ctx (file : rest) = do
  loaded <- try (loadPalFile file)
  case loaded of
    Left (e :: IOException) -> ctx <$ putStrLn (failure st ("cannot read " <> file <> ": " <> show e))
    Right (Left err) -> ctx <$ putStr (prettyParseError st err)
    Right (Right actions) -> do
      (ctx', _) <- runActionsIO defaultIOOptions {ioStyle = st} ctx actions
      putStrLn (success st "loaded " <> file)
      loadFiles st ctx' rest

-- | The text shown by @:help@.
helpText :: Style -> String
helpText st =
  unlines
    [ "Enter PAL statements (" <> kws <> ") or a bare expression to infer.",
      "Unfinished input continues on the next line; a blank line ends it.",
      "",
      heading "Commands:",
      "  " <> cmd ":load FILE..." <> "   run .pal files in the current context",
      "  " <> cmd ":ctx" <> "            show the current context",
      "  " <> cmd ":reset" <> "          clear the context",
      "  " <> cmd ":help" <> "           show this help",
      "  " <> cmd ":quit" <> "           exit (or Ctrl-D)",
      "",
      heading "Keys:" <> " " <> key "←/→" <> " move the cursor, " <> key "↑/↓" <> " browse history, " <> key "Ctrl-R" <> " search history,",
      "      " <> key "Tab" <> " complete file names, " <> key "Ctrl-C" <> " discard the current input."
    ]
  where
    heading = paint st [Bold]
    cmd = paint st [Cyan]
    key = paint st [Yellow]
    kws = intercalateComma (fmap (paint st [Bold, Blue]) ["type", "expr", "rule", "infer"])
    intercalateComma = foldr1 (\a b -> a <> ", " <> b)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
