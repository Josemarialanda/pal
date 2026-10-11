-- |
-- Module      : Ui.Embed
-- Description : Embed files into the executable at compile time
--
-- Template Haskell splices that read files while compiling and turn their
-- contents into string literals, so @pal-ui@ is a single self-contained
-- binary. The files are registered with 'addDependentFile', so editing one
-- triggers a rebuild.
module Ui.Embed (embedFile, embedExamples) where

import Data.Char (isUpper)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift, makeRelativeToProject)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO (IOMode (ReadMode), hGetContents', hSetEncoding, utf8, withFile)
import Ui.Json (Json (..), encode)

embedFile :: FilePath -> Q Exp
embedFile path = do
  abs' <- makeRelativeToProject path
  addDependentFile abs'
  runIO (readUtf8 abs') >>= lift

-- | The example programs in a directory, as a JSON array of
--   @[name, title, source]@. The title is the first comment line. The names
--   in the list come first, in its order; the other programs follow in
--   alphabetical order. @broken.pal@ (a parse error on purpose) is left out.
embedExamples :: FilePath -> [String] -> Q Exp
embedExamples dir order = do
  root <- makeRelativeToProject dir
  names <- runIO (listDirectory root)
  let programs = [takeBaseName n | n <- names, takeExtension n == ".pal", takeBaseName n /= "broken"]
      position n = (fromMaybe (length order) (lookup n (zip order [0 :: Int ..])), n)
  examples <- mapM (example root) (sortOn position programs)
  lift (encode (JArray examples))
  where
    example root name = do
      let path = root </> name <> ".pal"
      addDependentFile path
      source <- runIO (readUtf8 path)
      pure (JArray [JString name, JString (title source), JString source])
    -- The first sentence of the first comment paragraph.
    title source =
      firstSentence (0 :: Int) . unwords . fmap (drop 3) . takeWhile ("-- " `isPrefixOf`) . dropWhile (not . ("-- " `isPrefixOf`)) $
        lines source
    -- A sentence ends at a colon after a short label (like "Mini-ML:"),
    -- or at a full stop followed by the end or
    -- by a capital letter (not at "ch. 8").
    firstSentence n = \case
      [] -> ""
      ':' : _ | n >= 20 -> ""
      '.' : rest | null rest || startsSentence rest -> ""
      c : rest -> c : firstSentence (n + 1) rest
    startsSentence = \case
      ' ' : c : _ -> isUpper c
      _ -> False

readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \h -> hSetEncoding h utf8 >> hGetContents' h
