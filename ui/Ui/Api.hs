-- |
-- Module      : Ui.Api
-- Description : Routes and JSON API for the PAL web UI
--
-- > GET  /               the page (embedded at compile time, with the themes)
-- > POST /api/run        PAL source in the body → results as JSON
--
-- Every run starts from an empty context and checks the whole program, like
-- @pal FILE@. The server keeps no state between requests.
--
-- Requests are only accepted with a @Host@ of @127.0.0.1:PORT@ or
-- @localhost:PORT@, so other websites cannot reach the server through DNS
-- rebinding.
module Ui.Api (app, page, runSource) where

import Data.List (stripPrefix)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Interpreters.Core (runInterpreterWithCtx)
import Parser.Parser (palProgram)
import Program (PalAction (..), fromStmt, runPalAction)
import Text.Megaparsec (ParseErrorBundle (..), errorBundlePretty, errorOffset, parse, parseErrorTextPretty)
import Types
  ( Ctx,
    Err,
    Expr,
    ExprDecl (..),
    Hypothesis (..),
    Premise (..),
    Type,
    TypeDecl (..),
    TypingRule (..),
  )
import Ui.Embed (embedFile)
import Ui.Http (Request (..), Response (..), header)
import Ui.Json (Json (..), encode, object)
import Ui.Themes (embedThemes)

-- | The page, embedded from @ui/static/index.html@.
indexHtml :: String
indexHtml = $(embedFile "ui/static/index.html")

-- | The colour schemes from @ui/themes@, as JSON (see "Ui.Themes").
themesJson :: String
themesJson = $(embedThemes "ui/themes")

-- | The page as served: 'indexHtml' with the themes inlined in place of its
--   @/*THEMES*/[]@ placeholder. @<@ is escaped so the data can't end the
--   @<script>@ it sits in.
page :: String
page = T.unpack (T.replace "/*THEMES*/[]" (T.replace "<" "\\u003c" (T.pack themesJson)) (T.pack indexHtml))

-- | Handle a request. The port is needed to check the @Host@ header.
app :: Int -> Request -> Response
app port req
  | not hostAllowed = text 403 "forbidden: unexpected Host header"
  | otherwise = case (reqMethod req, reqPath req) of
      ("GET", "/") -> html page
      ("GET", "/index.html") -> html page
      ("POST", "/api/run") -> case decodeUtf8' (reqBody req) of
        Left _ -> text 400 "request body must be UTF-8"
        Right src -> json 200 (runSource (T.unpack src))
      (_, path)
        | path `elem` ["/", "/index.html", "/api/run"] -> text 405 "method not allowed"
        | otherwise -> text 404 "not found"
  where
    hostAllowed = header "host" req `elem` fmap Just [h <> ":" <> show port | h <- ["127.0.0.1", "localhost"]]

--------------------------------------------------------------------------------

-- | Responses

--------------------------------------------------------------------------------

utf8 :: String -> Response -> Response
utf8 body res = res {resBody = encodeUtf8 (T.pack body)}

html :: String -> Response
html body =
  utf8 body $
    Response
      200
      "text/html; charset=utf-8"
      -- Everything is inline and same-origin; nothing is loaded from elsewhere.
      [("Content-Security-Policy", "default-src 'self'; script-src 'unsafe-inline'; style-src 'unsafe-inline'; img-src 'self' data:")]
      ""

json :: Int -> Json -> Response
json status value = utf8 (encode value) (Response status "application/json; charset=utf-8" [] "")

text :: Int -> String -> Response
text status msg = utf8 msg (Response status "text/plain; charset=utf-8" [] "")

--------------------------------------------------------------------------------

-- | JSON

--------------------------------------------------------------------------------

-- | Parse and run a program, reporting every statement in order.
--
-- > {"ok": true,  "items": [{"kind": "type" | "expr" | "rule" | "infer", …}, …]}
-- > {"ok": false, "error": {"line": 3, "column": 5, "message": "…", "pretty": "…"}}
runSource :: String -> Json
runSource src = case parse palProgram "input" src of
  Left bundle -> object [("ok", JBool False), ("error", parseError bundle)]
  Right stmts -> object [("ok", JBool True), ("items", JArray (steps mempty (fmap fromStmt stmts)))]
  where
    steps :: Ctx -> [PalAction] -> [Json]
    steps _ [] = []
    steps ctx (a : rest) =
      let (ctx', result) = runInterpreterWithCtx ctx (runPalAction a)
       in item a result : steps ctx' rest

    parseError bundle =
      let err = NE.head (bundleErrors bundle)
          before = take (errorOffset err) src
       in object
            [ ("line", JNumber (1 + length (filter (== '\n') before))),
              ("column", JNumber (1 + length (takeWhile (/= '\n') (reverse before)))),
              ("message", JString (trimEnd (parseErrorTextPretty err))),
              ("pretty", JString (errorBundlePretty bundle))
            ]

-- | One statement and its outcome.
item :: PalAction -> Maybe (Either Err Type) -> Json
item action result = case (action, result) of
  (ADefineType (TypeDecl n args), _) ->
    object [("kind", JString "type"), ("name", JString n), ("args", JArray (fmap JString args))]
  (ADefineExpr (ExprDecl n t), _) ->
    object [("kind", JString "expr"), ("name", JString n), ("type", showJ t)]
  (ADefineRule tr, _) ->
    object [("kind", JString "rule"), ("rule", rule tr)]
  (AInfer e, Just (Right t)) ->
    object [("kind", JString "infer"), ("expr", showJ e), ("ok", JBool True), ("type", showJ t)]
  (AInfer e, Just (Left err)) ->
    object [("kind", JString "infer"), ("expr", showJ e), ("ok", JBool False), ("error", JString (errMessage err))]
  (AInfer e, Nothing) ->
    object [("kind", JString "infer"), ("expr", showJ e), ("ok", JBool False), ("error", JString "no result")]

-- | A rule, with each part as text (the page highlights the syntax).
rule :: TypingRule -> Json
rule (TypingRule name premises conclusion) =
  object
    [ ("name", JString name),
      ("premises", JArray (fmap premise premises)),
      ("conclusion", judgment conclusion)
    ]
  where
    premise (Premise hyps j) =
      object [("hypotheses", JArray (fmap hypothesis hyps)), ("judgment", judgment j)]
    hypothesis (Hypothesis e t g) =
      object [("expr", showJ e), ("type", showJ t), ("gen", JBool g)]
    judgment :: (Expr, Type) -> Json
    judgment (e, t) = object [("expr", showJ e), ("type", showJ t)]

showJ :: (Show a) => a -> Json
showJ = JString . show

-- | An error message without the @[Error] @ prefix used in the terminal.
errMessage :: Err -> String
errMessage err = let s = show err in fromMaybe s (stripPrefix "[Error] " s)

trimEnd :: String -> String
trimEnd = reverse . dropWhile (`elem` ['\n', ' ']) . reverse
