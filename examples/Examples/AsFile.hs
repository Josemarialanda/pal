-- |
-- Module      : Examples.AsFile
-- Description : PAL programs loaded from @.pal@ source files at runtime.
--
-- Uses the same parser as the quasiquoter, but at runtime: syntax errors are
-- reported when the file is loaded, not at compile time. The loading itself
-- ('loadPalFile') lives in the library's "Program" module, which the @pal@
-- command also uses.
module Examples.AsFile (examples) where

import Examples.Types (Example (..), Program (..))
import Program (loadPalFile, runPalActions)
import System.FilePath (takeBaseName, (</>))

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
        (fmap (\actions -> Program (runPalActions actions)) <$> loadPalFile (programsDir </> file))
