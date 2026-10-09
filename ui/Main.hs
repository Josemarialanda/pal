-- |
-- Module      : Main
-- Description : @pal-ui@: the PAL web UI, served on localhost
--
-- > pal-ui                 serve on http://127.0.0.1:7337 and open a browser
-- > pal-ui --port N        use port N (0 picks any free port)
-- > pal-ui --no-open       don't open a browser, just print the URL
--
-- The page is embedded in the executable at
-- compile time (see "Ui.Embed"), so the binary is self-contained.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Control.Monad (void, when)
import Network.Socket (Socket)
import Pretty (Attr (..), detectStyle, note, paint)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import System.Info (os)
import System.Process (StdStream (..), createProcess, proc, std_err, std_in, std_out, waitForProcess)
import Text.Read (readMaybe)
import Ui.Api (app)
import Ui.Http (listenLocal, serve)

data Options = Options
  { optPort :: Maybe Int,
    optOpen :: Bool
  }

-- | The port used when none is given. If it is taken, any free port is used.
defaultPort :: Int
defaultPort = 7337

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  -- Show the URL immediately, even when stdout is a file or a pipe.
  hSetBuffering stdout LineBuffering
  args <- getArgs
  when (any (`elem` ["-h", "--help"]) args) (putStr usage >> exitSuccess)
  opts <- either (\msg -> hPutStrLn stderr msg >> exitFailure) pure (parseArgs (Options Nothing True) args)
  (sock, port) <- bindPort (optPort opts)
  let url = "http://127.0.0.1:" <> show port <> "/"
  st <- detectStyle stdout
  putStrLn (paint st [Bold, Magenta] "λ PAL UI" <> " running at " <> paint st [Bold, Cyan] url)
  putStrLn (note st "Press Ctrl-C to stop.")
  when (optOpen opts) . void . forkIO $ openBrowser url
  serve sock (pure . app port)

parseArgs :: Options -> [String] -> Either String Options
parseArgs opts = \case
  [] -> Right opts
  "--port" : n : rest | Just p <- readMaybe n, p >= 0, p < 65536 -> parseArgs opts {optPort = Just p} rest
  "--no-open" : rest -> parseArgs opts {optOpen = False} rest
  arg : _ -> Left ("unknown argument: " <> arg <> "\n\n" <> usage)

usage :: String
usage =
  unlines
    [ "Usage: pal-ui [--port N] [--no-open]",
      "",
      "Serve the PAL web UI on http://127.0.0.1 and open it in a browser.",
      "",
      "  --port N     listen on port N (default " <> show defaultPort <> "; 0 = any free port)",
      "  --no-open    don't open a browser, just print the URL"
    ]

-- | Bind the requested port. Without one, try the default and fall back to
--   any free port if it is taken.
bindPort :: Maybe Int -> IO (Socket, Int)
bindPort = \case
  Just port ->
    try (listenLocal port) >>= \case
      Right bound -> pure bound
      Left (e :: IOException) -> do
        hPutStrLn stderr ("pal-ui: cannot listen on port " <> show port <> ": " <> show e)
        exitFailure
  Nothing ->
    try (listenLocal defaultPort) >>= \case
      Right bound -> pure bound
      Left (_ :: IOException) -> listenLocal 0

-- | Open the URL in the default browser. Failure is not fatal: the URL has
--   already been printed (e.g. on a headless machine or over SSH).
openBrowser :: String -> IO ()
openBrowser url = do
  let (cmd, args) = case os of
        "darwin" -> ("open", [url])
        "mingw32" -> ("cmd", ["/c", "start", "", url])
        _ -> ("xdg-open", [url])
  result <- try $ do
    (_, _, _, ph) <- createProcess (proc cmd args) {std_in = NoStream, std_out = NoStream, std_err = NoStream}
    waitForProcess ph
  case result of
    Right ExitSuccess -> pure ()
    Right (ExitFailure _) -> couldNotOpen
    Left (_ :: IOException) -> couldNotOpen
  where
    couldNotOpen = hPutStrLn stderr "Couldn't open a browser; open the URL above manually."
