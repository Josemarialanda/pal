-- |
-- Module      : Ui.Http
-- Description : A minimal HTTP/1.1 server for localhost
--
-- Just enough HTTP for a single-user tool talking to a browser on the same
-- machine: one request per connection (@Connection: close@), requests with
-- a @Content-Length@ body, size limits, and a read timeout. It binds to
-- @127.0.0.1@ only, so it is never reachable from the network.
module Ui.Http
  ( Request (..),
    Response (..),
    listenLocal,
    serve,
    header,
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracketOnError, finally, try)
import Control.Monad (forever, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data Request = Request
  { reqMethod :: String,
    -- | The path, without any query string.
    reqPath :: String,
    -- | Header names are lower-cased.
    reqHeaders :: [(String, String)],
    reqBody :: BS.ByteString
  }

data Response = Response
  { resStatus :: Int,
    resContentType :: String,
    resHeaders :: [(String, String)],
    resBody :: BS.ByteString
  }

-- | Look up a request header (by lower-case name).
header :: String -> Request -> Maybe String
header name = lookup name . reqHeaders

-- | Listen on @127.0.0.1@. Port 0 lets the OS pick a free port.
--   Returns the socket and the port actually bound.
listenLocal :: Int -> IO (Socket, Int)
listenLocal port = do
  let hints = defaultHints {addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
  addrs <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
  addr <- case addrs of
    a : _ -> pure a
    [] -> ioError (userError "cannot resolve 127.0.0.1")
  bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 64
    bound <- socketPort sock
    pure (sock, fromIntegral bound)

-- | Accept connections forever, handling each in its own thread.
serve :: Socket -> (Request -> IO Response) -> IO ()
serve sock handler = forever $ do
  (conn, _) <- accept sock
  void . forkIO $ handleConnection conn handler `finally` close conn

handleConnection :: Socket -> (Request -> IO Response) -> IO ()
handleConnection conn handler = do
  request <- timeout (10 * 1000000) (readRequest conn)
  response <- case request of
    Nothing -> pure (plainText 408 "request timeout")
    Just (Left status) -> pure (plainText status (statusText status))
    Just (Right req) ->
      try (handler req) >>= \case
        Left (e :: SomeException) -> pure (plainText 500 ("internal error: " <> show e))
        Right res -> pure res
  sendAll conn (render response)

-- | Limits on request size.
maxHeaderBytes, maxBodyBytes :: Int
maxHeaderBytes = 64 * 1024
maxBodyBytes = 1024 * 1024

-- | Read and parse one request, or fail with an HTTP status code.
readRequest :: Socket -> IO (Either Int Request)
readRequest conn = readHead BS.empty
  where
    readHead buf
      | (headBytes, rest) <- BS.breakSubstring "\r\n\r\n" buf,
        not (BS.null rest) =
          parseHead headBytes (BS.drop 4 rest)
      | BS.length buf > maxHeaderBytes = pure (Left 431)
      | otherwise = do
          chunk <- recv conn 4096
          if BS.null chunk then pure (Left 400) else readHead (buf <> chunk)

    parseHead headBytes bodyStart =
      case lines (filter (/= '\r') (BC.unpack headBytes)) of
        requestLine : headerLines
          | [method, target, _version] <- words requestLine -> do
              let headers = fmap parseHeader headerLines
                  len = fromMaybe 0 (lookup "content-length" headers >>= readMaybe)
              if len > maxBodyBytes
                then pure (Left 413)
                else do
                  body <- readBody len bodyStart
                  pure $ case body of
                    Nothing -> Left 400
                    Just b -> Right (Request method (takeWhile (/= '?') target) headers b)
        _ -> pure (Left 400)

    parseHeader l =
      let (name, value) = break (== ':') l
       in (fmap toLower name, trim (drop 1 value))

    readBody len buf
      | BS.length buf >= len = pure (Just (BS.take len buf))
      | otherwise = do
          chunk <- recv conn (min 65536 (len - BS.length buf))
          if BS.null chunk then pure Nothing else readBody len (buf <> chunk)

    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Serialise a response. Header values are ASCII; the body is raw bytes.
render :: Response -> BS.ByteString
render (Response status contentType extra body) =
  BC.pack (statusLine <> concatMap line headers <> "\r\n") <> body
  where
    statusLine = "HTTP/1.1 " <> show status <> " " <> statusText status <> "\r\n"
    line (k, v) = k <> ": " <> v <> "\r\n"
    headers =
      [ ("Content-Type", contentType),
        ("Content-Length", show (BS.length body)),
        ("Connection", "close"),
        ("Cache-Control", "no-store"),
        ("X-Content-Type-Options", "nosniff")
      ]
        <> extra

plainText :: Int -> String -> Response
plainText status msg = Response status "text/plain; charset=utf-8" [] (BC.pack msg)

statusText :: Int -> String
statusText = \case
  200 -> "OK"
  400 -> "Bad Request"
  403 -> "Forbidden"
  404 -> "Not Found"
  405 -> "Method Not Allowed"
  408 -> "Request Timeout"
  413 -> "Payload Too Large"
  431 -> "Request Header Fields Too Large"
  500 -> "Internal Server Error"
  _ -> "Unknown"
