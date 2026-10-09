-- |
-- Module      : Ui.Themes
-- Description : Embed the Tinted Theming colour schemes at compile time
--
-- @ui/themes@ holds the scheme files from
-- <https://github.com/tinted-theming/schemes>, one directory per system
-- (@base16@, @base24@, @tinted8@). 'embedThemes' reads them all while
-- compiling and turns them into one JSON array for the page:
--
-- > [["base16/gruvbox-dark-hard", "Gruvbox dark, hard", "dark", "1d20213c3836…"], …]
--
-- that is @[id, name, variant, colours]@, where @colours@ is the sixteen
-- base16 slots @base00@–@base0F@ as 6-digit hex, concatenated. base24
-- schemes keep their first sixteen slots; tinted8 schemes (ANSI-style
-- palettes) are mapped onto base16 slots by 'tinted8'.
--
-- The scheme files are simple enough (flat @key: "value"@ pairs, one level
-- of nesting) that a small line-based reader suffices, so there is no YAML
-- dependency. A scheme that can't be read fails the build.
module Ui.Themes (embedThemes) where

import Control.Monad (filterM, forM)
import Data.Char (isSpace, toLower)
import Data.List (sortOn)
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift, makeRelativeToProject)
import Numeric (readHex, showHex)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO (IOMode (ReadMode), hGetContents', hSetEncoding, utf8, withFile)
import Ui.Json (Json (..), encode)

data Theme = Theme
  { themeId :: String,
    themeName :: String,
    themeVariant :: String,
    themeColours :: [String] -- 16 × 6-digit hex, no @#@
  }

-- | Every scheme under a directory (relative to the project root), as a
--   JSON 'String' expression. Schemes are sorted by system, then name.
embedThemes :: FilePath -> Q Exp
embedThemes dir = do
  root <- makeRelativeToProject dir
  files <- runIO (schemeFiles root)
  mapM_ (addDependentFile . snd) files
  themes <- runIO . forM files $ \(system, path) -> do
    fields <- readFields <$> readUtf8 path
    either (fail . ((path <> ": ") <>)) pure (theme system (takeBaseName path) fields)
  lift (encode (JArray (fmap json (sortOn (\t -> (takeWhile (/= '/') t.themeId, map toLower t.themeName)) themes))))
  where
    json t = JArray [JString t.themeId, JString t.themeName, JString t.themeVariant, JString (concat t.themeColours)]

-- | @(system, path)@ for each @.yaml@/@.yml@ file in each subdirectory.
schemeFiles :: FilePath -> IO [(String, FilePath)]
schemeFiles root = do
  systems <- filterM (doesDirectoryExist . (root </>)) =<< listDirectory root
  fmap concat . forM systems $ \system -> do
    names <- listDirectory (root </> system)
    pure [(system, root </> system </> n) | n <- names, takeExtension n `elem` [".yaml", ".yml"]]

readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \h -> hSetEncoding h utf8 >> hGetContents' h

--------------------------------------------------------------------------------

-- | Schemes

--------------------------------------------------------------------------------

theme :: String -> String -> [(String, String)] -> Either String Theme
theme system slug fields = do
  colours <- if system == "tinted8" then tinted8 fields else traverse (colour . slot) [0 .. 15 :: Int]
  name <- maybe (Left "no name") Right (lookup "name" fields `orElse` lookup "scheme.name" fields `orElse` familyStyle)
  pure (Theme (system <> "/" <> slug) name variant colours)
  where
    variant = if lookup "variant" fields == Just "light" then "light" else "dark"
    slot i = "palette.base0" <> map toLower (showHex i "")
    colour key = maybe (Left ("missing " <> key)) hex (lookup key fields)
    familyStyle = (\f s -> f <> " " <> s) <$> lookup "scheme.family" fields <*> lookup "scheme.style" fields

-- | Map a tinted8 palette (black, white, red, … and optional @ui.*@ colours)
--   onto base16 slots. Background shades between the background and
--   foreground are blended when the scheme doesn't give them.
tinted8 :: [(String, String)] -> Either String [String]
tinted8 fields = do
  black <- need "palette.black"
  white <- need "palette.white"
  let dark = lookup "variant" fields /= Just "light"
  bg <- opt "ui.global.background.normal" (if dark then black else white)
  fg <- opt "ui.global.foreground.normal" (if dark then white else black)
  bg2 <- opt "ui.global.background.dark" (mix bg fg 0.07)
  border <- opt "ui.border.normal" (mix bg fg 0.16)
  fgDim <- opt "ui.global.foreground.dark" (mix bg fg 0.65)
  accents <-
    traverse (need . ("palette." <>)) ["red", "orange", "yellow", "green", "cyan", "blue", "magenta"]
  gray <- need "palette.gray"
  brown <- opt "palette.red-dim" =<< need "palette.red"
  pure ([bg, bg2, border, gray, fgDim, fg, fg, fg] <> accents <> [brown])
  where
    need key = maybe (Left ("missing " <> key)) hex (lookup key fields)
    opt key def = maybe (Right def) hex (lookup key fields)

-- | Normalise @#RRGGBB@ to lowercase @rrggbb@.
hex :: String -> Either String String
hex = \case
  '#' : ds | length ds == 6, [(_, "")] <- readHex @Int ds -> Right (map toLower ds)
  s -> Left ("not a colour: " <> s)

-- | Blend two colours: @t = 0@ is the first, @t = 1@ the second.
mix :: String -> String -> Double -> String
mix a b t = concat (zipWith channel (channels a) (channels b))
  where
    channels s = [byte (take 2 (drop i s)) | i <- [0, 2, 4]]
    byte ds = case readHex @Int ds of [(n, "")] -> n; _ -> 0
    channel x y = pad (showHex (round (fromIntegral x + t * fromIntegral (y - x)) :: Int) "")
    pad s = replicate (2 - length s) '0' <> s

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

--------------------------------------------------------------------------------

-- | Reading scheme files

--------------------------------------------------------------------------------

-- | Flatten a scheme file into @(key, value)@ pairs. Keys under a top-level
--   section are prefixed with it (@palette.base00@, @scheme.name@); keys
--   are lowercased. Comments and keys without a value are dropped.
readFields :: String -> [(String, String)]
readFields = go "" . lines
  where
    go _ [] = []
    go section (l : ls)
      | all isSpace l || "#" == take 1 (dropWhile isSpace l) = go section ls
      | (k, ':' : rest) <- break (== ':') (dropWhile isSpace l) =
          let key = map toLower (trim k)
              indented = take 1 l == " "
              full = if indented && not (null section) then section <> "." <> key else key
              section' = if indented then section else key
           in case value (dropWhile isSpace rest) of
                "" -> go section' ls
                v -> (full, v) : go section' ls
      | otherwise = go section ls

-- | A scalar: double- or single-quoted, or plain up to a @ #@ comment.
value :: String -> String
value = \case
  '"' : s -> dq s
  '\'' : s -> takeWhile (/= '\'') s
  s -> trim (plain s)
  where
    dq = \case
      '\\' : c : s -> c : dq s
      '"' : _ -> ""
      c : s -> c : dq s
      [] -> ""
    plain = \case
      c : '#' : _ | isSpace c -> ""
      c : s -> c : plain s
      [] -> ""

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
