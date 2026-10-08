-- |
-- Module      : Program
-- Description : PAL programs as data, and loading them from PAL source.
--
-- A PAL program can always be reduced to a list of 'PalAction's. This module
-- defines that representation, runs it through the 'PAL' effect, and parses
-- PAL source text (from a string or a @.pal@ file) into it at runtime.
--
-- The quasiquoter ("Parser.Quasi") parses at compile time instead; both share
-- the same parser ('palProgram').
module Program
  ( -- * Programs as data
    PalAction (..),
    runPalAction,
    runPalActions,

    -- * Loading PAL source at runtime
    fromStmt,
    parsePalSource,
    loadPalFile,
  )
where

import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Parser.Parser (palProgram)
import Parser.Types (PalStmt (..))
import Polysemy (Member, Sem)
import Text.Megaparsec (errorBundlePretty, parse)
import Types
  ( Err,
    Expr,
    ExprDecl,
    PAL,
    Type (TCon),
    TypeDecl,
    TypingRule,
    defineExpr,
    defineRule,
    defineType,
    infer,
  )

--------------------------------------------------------------------------------

-- | Programs as data

--------------------------------------------------------------------------------

-- | A data-level representation of PAL’s core DSL actions.
--
-- Each constructor mirrors one PAL effect operation and can be interpreted
-- via different backends (core, debug, IO, …).
data PalAction
  = -- | Add a new base type.
    ADefineType TypeDecl
  | -- | Declare a new expression and its type.
    ADefineExpr ExprDecl
  | -- | Introduce a new typing rule.
    ADefineRule TypingRule
  | -- | Run inference on a given expression.
    AInfer Expr
  deriving (Show)

-- | Execute one 'PalAction'. Returns the inference result for 'AInfer',
--   and 'Nothing' for definitions.
runPalAction :: (Member PAL r) => PalAction -> Sem r (Maybe (Either Err Type))
runPalAction = \case
  ADefineType td -> Nothing <$ defineType td
  ADefineExpr ed -> Nothing <$ defineExpr ed
  ADefineRule tr -> Nothing <$ defineRule tr
  AInfer e -> Just <$> infer e

-- | Execute a list of 'PalAction's in order.
--
-- The result is that of the last 'AInfer' (or @Unit@ if there is none).
runPalActions :: (Member PAL r) => [PalAction] -> Sem r (Either Err Type)
runPalActions = foldM (\acc a -> fromMaybe acc <$> runPalAction a) (Right (TCon "Unit" []))

--------------------------------------------------------------------------------

-- | Loading PAL source at runtime

--------------------------------------------------------------------------------

-- | Parsed statements map one-to-one onto 'PalAction's.
fromStmt :: PalStmt -> PalAction
fromStmt = \case
  SType td -> ADefineType td
  SExpr ed -> ADefineExpr ed
  SRule tr -> ADefineRule tr
  SInfer e -> AInfer e

-- | Parse PAL source text. The first argument names the source in error
--   messages (usually a file path).
parsePalSource :: String -> String -> Either String [PalAction]
parsePalSource name src =
  either (Left . errorBundlePretty) (Right . fmap fromStmt) (parse palProgram name src)

-- | Read and parse a @.pal@ file.
loadPalFile :: FilePath -> IO (Either String [PalAction])
loadPalFile path = parsePalSource path <$> readFile path
