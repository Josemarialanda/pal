-- |
-- Module      : Ui.Json
-- Description : A minimal JSON encoder
--
-- The UI only ever /sends/ JSON, and only small, fixed-shape values, so a
-- tiny encoder is enough (and avoids a dependency on aeson).
module Ui.Json (Json (..), encode, object) where

import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)

data Json
  = JString String
  | JNumber Int
  | JBool Bool
  | JNull
  | JArray [Json]
  | JObject [(String, Json)]

-- | Build an object from key/value pairs.
object :: [(String, Json)] -> Json
object = JObject

-- | Render JSON as compact text.
encode :: Json -> String
encode = \case
  JString s -> quote s
  JNumber n -> show n
  JBool b -> if b then "true" else "false"
  JNull -> "null"
  JArray xs -> "[" <> intercalate "," (fmap encode xs) <> "]"
  JObject kvs -> "{" <> intercalate "," [quote k <> ":" <> encode v | (k, v) <- kvs] <> "}"

-- | A JSON string literal, escaping quotes, backslashes and control characters.
quote :: String -> String
quote s = "\"" <> concatMap esc s <> "\""
  where
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      c
        | ord c < 0x20 -> "\\u" <> pad (showHex (ord c) "")
        | otherwise -> [c]
    pad h = replicate (4 - length h) '0' <> h
