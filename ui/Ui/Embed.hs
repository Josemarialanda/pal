-- |
-- Module      : Ui.Embed
-- Description : Embed files into the executable at compile time
--
-- A Template Haskell splice that reads a file while compiling and turns its
-- contents into a string literal, so @pal-ui@ is a single self-contained
-- binary. The file is registered with 'addDependentFile', so editing it
-- triggers a rebuild.
module Ui.Embed (embedFile) where

import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift, makeRelativeToProject)
import System.IO (IOMode (ReadMode), hGetContents', hSetEncoding, utf8, withFile)

-- | The contents of a file (UTF-8), as a 'String' expression.
--   The path is relative to the project root.
embedFile :: FilePath -> Q Exp
embedFile path = do
  abs' <- makeRelativeToProject path
  addDependentFile abs'
  runIO (readUtf8 abs') >>= lift

readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \h -> hSetEncoding h utf8 >> hGetContents' h
