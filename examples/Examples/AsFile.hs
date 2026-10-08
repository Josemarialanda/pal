-- |
-- Module      : Examples.AsFile
-- Description : PAL programs loaded from @.pal@ source files at runtime.
--
-- Uses the same parser as the quasiquoter, but at runtime: syntax errors are
-- reported when the file is loaded, not at compile time.
module Examples.AsFile (examples, loadPalFile) where

import Examples.Types (Example (..), Program (..))
import PAL (PalAction (..), runPalActions)
import Parser.Parser (palProgram)
import Parser.Types (PalStmt (..))
import System.FilePath (takeBaseName, (</>))
import Text.Megaparsec (errorBundlePretty, parse)

-- | Directory holding the example @.pal@ files, relative to the project root.
programsDir :: FilePath
programsDir = "examples" </> "programs"

examples :: [Example]
examples =
  [ fileExample "stlc.pal" "STLC with Lam, Let and App, loaded from a .pal file",
    fileExample "broken.pal" "A file with a syntax error, to show runtime parse errors"
  ]
  where
    fileExample file description =
      Example
        ("file/" <> takeBaseName file)
        description
        (loadPalFile (programsDir </> file))

-- | Read and parse a @.pal@ file into a runnable 'Program'.
loadPalFile :: FilePath -> IO (Either String Program)
loadPalFile path = do
  src <- readFile path
  pure $ case parse palProgram path src of
    Left err -> Left (errorBundlePretty err)
    Right stmts -> Right (Program (runPalActions (fmap toAction stmts)))

-- | Parsed statements map one-to-one onto 'PalAction's.
toAction :: PalStmt -> PalAction
toAction = \case
  SType td -> ADefineType td
  SExpr ed -> ADefineExpr ed
  SRule tr -> ADefineRule tr
  SInfer e -> AInfer e
