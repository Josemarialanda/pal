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
import Control.Monad (unless, when)
import Data.Char (isAlphaNum, isSpace)
import Data.List (dropWhileEnd)
import qualified Data.List.NonEmpty as NE
import Interpreters.IO (IOOptions (..), defaultIOOptions, runActionsIO)
import Parser.Parser (pExprApp, palProgram, sc)
import Program (PalAction (..), fromStmt, loadPalFile)
import System.IO (hFlush, hIsTerminalDevice, isEOF, stdin, stdout)
import Text.Megaparsec (ParseErrorBundle (..), eof, errorBundlePretty, errorOffset, parse)
import Types (Ctx)

-- | Start a REPL with the given initial context.
--
--   Prompts and the banner are only shown when stdin is a terminal, so a
--   script can also be piped in: @pal < script.pal@.
runRepl :: Ctx -> IO ()
runRepl ctx0 = do
  interactive <- hIsTerminalDevice stdin
  when interactive $
    putStrLn "PAL REPL — type :help for commands, :quit to exit."
  loop interactive ctx0 ""

-- | The main loop. @buffer@ holds the lines of an unfinished input.
loop :: Bool -> Ctx -> String -> IO ()
loop interactive ctx buffer = do
  when interactive $ do
    putStr (if null buffer then "pal> " else "...> ")
    hFlush stdout
  done <- isEOF
  if done
    then do
      -- Run whatever is left, reporting it if it is incomplete.
      unless (all isSpace buffer) $ () <$ evalInput True ctx buffer
      when interactive (putStrLn "")
    else do
      line <- getLine
      let trimmed = trim line
      case trimmed of
        ':' : cmd | null buffer -> do
          next <- command ctx (words cmd)
          maybe (pure ()) (\ctx' -> loop interactive ctx' "") next
        "" | not (null buffer) -> do
          -- A blank line ends an unfinished input.
          ctx' <- evalInput True ctx buffer
          loop interactive ctx' ""
        _ -> do
          let buffer' = buffer <> line <> "\n"
          case parseInput buffer' of
            Incomplete _ -> loop interactive ctx buffer'
            _ -> evalInput False ctx buffer' >>= \ctx' -> loop interactive ctx' ""

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
      "  :quit           exit (or Ctrl-D)"
    ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
