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
import Parser.Parser (palProgramLocated)
import Parser.Types (Located (..), Span (..), spanAt)
import Program (Outcome (..), PalAction (..), Step (..), fromStmt, runSteps)
import Report (derivationLatex)
import Text.Megaparsec (ParseErrorBundle (..), errorBundlePretty, errorOffset, parse, parseErrorTextPretty)
import Types
  ( Derivation (..),
    Diagnostic (..),
    Err,
    Expectation (..),
    Expr,
    ExprDecl (..),
    Failure (..),
    Frame (..),
    Hypothesis (..),
    Justification (..),
    Premise (..),
    Severity (..),
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
-- > {"ok": true,  "items": [{"kind": "type" | "expr" | "rule" | "infer" | "check" | "fails", …}, …]}
-- > {"ok": false, "error": {"line": 3, "column": 5, "message": "…", "pretty": "…"}}
--
-- Every item has @start@, the offset of its statement in the source.
-- Definitions have @rejected@ and @diagnostics@. Inferences and expectations
-- have @ok@ and, on success, @type@, @derivation@ and @latex@; on failure,
-- @error@ (the first message) and @errors@ (each with the @start@/@end@ of
-- the subterm at fault and the premises being checked).
runSource :: String -> Json
runSource src = case parse palProgramLocated "input" src of
  Left bundle -> object [("ok", JBool False), ("error", parseError bundle)]
  Right stmts ->
    let (_, steps) = runSteps mempty (fmap (fmap fromStmt) stmts)
     in object [("ok", JBool True), ("items", JArray (fmap item steps))]
  where
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
item :: Step -> Json
item (Step located outcome) = object (("start", JNumber (loc'start located)) : fields)
  where
    fields = case (loc'value located, outcome) of
      (ADefineType (TypeDecl n args), _) ->
        [("kind", JString "type"), ("name", JString n), ("args", JArray (fmap JString args))] <> definition
      (ADefineExpr (ExprDecl n t), _) ->
        [("kind", JString "expr"), ("name", JString n), ("type", showJ t)] <> definition
      (ADefineRule tr, _) ->
        [("kind", JString "rule"), ("rule", rule tr)] <> definition
      (AInfer e, Inferred result) ->
        [("kind", JString "infer"), ("expr", showJ e)] <> inference result
      (AExpect x@(ExpectType e t), Expected _ diagnostics result met) ->
        [("kind", JString "check"), ("expr", showJ e), ("expected", showJ t), ("text", showJ x)]
          <> withoutOk (inference result)
          <> [("ok", JBool met), ("diagnostics", JArray (fmap diagnostic diagnostics))]
      (AExpect x@(ExpectFailure e), Expected _ _ result met) ->
        [("kind", JString "fails"), ("expr", showJ e), ("text", showJ x)] <> withoutOk (inference result) <> [("ok", JBool met)]
      (action, _) -> [("kind", JString "unknown"), ("text", JString (show action))]

    definition = case outcome of
      Rejected diagnostics -> [("rejected", JBool True), ("diagnostics", JArray (fmap diagnostic diagnostics))]
      Defined diagnostics -> [("rejected", JBool False), ("diagnostics", JArray (fmap diagnostic diagnostics))]
      _ -> []

    -- "ok" is whether inference succeeded; expectations replace it with
    -- whether they are met.
    withoutOk = filter ((/= "ok") . fst)
    inference = \case
      Right d ->
        [ ("ok", JBool True),
          ("type", showJ (deriv'type d)),
          ("derivation", derivation d),
          ("latex", JString (derivationLatex d))
        ]
      Left failures ->
        [ ("ok", JBool False),
          ("error", JString (errMessage (failure'err (NE.head failures)))),
          ("errors", JArray (fmap failureJson (NE.toList failures)))
        ]

    failureJson (Failure err path frames) =
      object $
        [("message", JString (errMessage err))]
          <> maybe [] (\spans -> let Span s e = spanAt path spans in [("start", JNumber s), ("end", JNumber e)]) (loc'exprSpans located)
          <> [("context", JArray (fmap frame frames))]

    frame (Frame ruleName premise subject) =
      object [("rule", JString ruleName), ("premise", showJ premise), ("subject", showJ subject)]

    diagnostic (Diagnostic severity msg) =
      object [("severity", JString (if severity == SevError then "error" else "warning")), ("message", JString msg)]

-- | A derivation tree as nested JSON, each part as text.
derivation :: Derivation -> Json
derivation (Derivation e t by assumptions premises) =
  object
    [ ("expr", showJ e),
      ("type", showJ t),
      ("by", JString byText),
      ("rule", maybe JNull JString ruleName),
      ("assumptions", JArray [object [("var", JString v), ("type", showJ s)] | (v, s) <- assumptions]),
      ("premises", JArray (fmap derivation premises))
    ]
  where
    (byText, ruleName) = case by of
      ByRule name -> ("rule", Just name)
      ByDeclaration -> ("declaration", Nothing)
      ByAssumption -> ("assumption", Nothing)

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
