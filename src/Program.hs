-- |
-- Module      : Program
-- Description : PAL programs as data, loading them from PAL source, and
--               running them step by step.
--
-- A PAL program can always be reduced to a list of 'PalAction's. This module
-- defines that representation, runs it through the 'PAL' effect, and parses
-- PAL source text (from a string or a @.pal@ file) into it at runtime.
--
-- 'runSteps' runs a parsed program purely and reports a detailed 'Outcome'
-- for every statement (derivations, every failure with its location and
-- context, rejected definitions). The @pal@ command, its REPL and @pal-ui@
-- all use it, so they report the same things.
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
    parsePalSourceLocated,
    loadPalFile,
    loadPalFileLocated,

    -- * Running step by step
    Step (..),
    Outcome (..),
    runSteps,
    succeeded,
  )
where

import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import qualified Interpreters.Common.Actions as Actions
import Parser.Parser (palProgram, palProgramLocated)
import Parser.Types (Located (..), PalStmt (..))
import Polysemy (Member, Sem, run)
import Polysemy.State (runState)
import System.IO (IOMode (ReadMode), hGetContents', hSetEncoding, utf8, withFile)
import Text.Megaparsec (errorBundlePretty, parse)
import Types
  ( Ctx,
    Derivation,
    Diagnostic,
    Err,
    Expectation (..),
    Expr,
    ExprDecl,
    Failure,
    PAL,
    Type (TCon),
    TypeDecl,
    TypingRule,
    defineExpr,
    defineRule,
    defineType,
    expect,
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
  | -- | Check an expectation (@check e : T@ or @fails e@).
    AExpect Expectation
  deriving (Show)

-- | Execute one 'PalAction'. Returns the inference result for 'AInfer',
--   and 'Nothing' for definitions and expectations.
runPalAction :: (Member PAL r) => PalAction -> Sem r (Maybe (Either Err Type))
runPalAction = \case
  ADefineType td -> Nothing <$ defineType td
  ADefineExpr ed -> Nothing <$ defineExpr ed
  ADefineRule tr -> Nothing <$ defineRule tr
  AInfer e -> Just <$> infer e
  AExpect x -> Nothing <$ expect x

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
  SCheck e t -> AExpect (ExpectType e t)
  SFails e -> AExpect (ExpectFailure e)

-- | Parse PAL source text. The first argument names the source in error
--   messages (usually a file path).
parsePalSource :: String -> String -> Either String [PalAction]
parsePalSource name src =
  either (Left . errorBundlePretty) (Right . fmap fromStmt) (parse palProgram name src)

-- | Like 'parsePalSource', keeping where each statement is in the source.
parsePalSourceLocated :: String -> String -> Either String [Located PalAction]
parsePalSourceLocated name src =
  either (Left . errorBundlePretty) (Right . fmap (fmap fromStmt)) (parse palProgramLocated name src)

-- | Read and parse a @.pal@ file.
loadPalFile :: FilePath -> IO (Either String [PalAction])
loadPalFile path = parsePalSource path <$> readUtf8 path

-- | Read and parse a @.pal@ file, returning its source and located actions.
loadPalFileLocated :: FilePath -> IO (String, Either String [Located PalAction])
loadPalFileLocated path = do
  src <- readUtf8 path
  pure (src, parsePalSourceLocated path src)

readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \h -> hSetEncoding h utf8 >> hGetContents' h

--------------------------------------------------------------------------------

-- | Running step by step

--------------------------------------------------------------------------------

-- | What happened when a statement ran.
data Outcome
  = -- | A definition was added (possibly with warnings).
    Defined [Diagnostic]
  | -- | A definition was rejected; at least one diagnostic is an error.
    Rejected [Diagnostic]
  | -- | The result of @infer@: a derivation, or every failure.
    Inferred (Either (NonEmpty Failure) Derivation)
  | -- | The result of @check@ / @fails@: diagnostics about the expected type,
    --   the inference result, and whether the expectation is met.
    Expected Expectation [Diagnostic] (Either (NonEmpty Failure) Derivation) Bool

-- | A statement, where it is in the source, and its outcome.
data Step = Step
  { step'source :: Located PalAction,
    step'outcome :: Outcome
  }

-- | Whether a step succeeded: definitions that were added, inferences that
--   typecheck, and expectations that are met.
succeeded :: Outcome -> Bool
succeeded = \case
  Defined _ -> True
  Rejected _ -> False
  Inferred r -> either (const False) (const True) r
  Expected _ _ _ met -> met

-- | Run a program from a context, statement by statement, reporting each
--   outcome. Pure: nothing is printed. Returns the final context.
runSteps :: Ctx -> [Located PalAction] -> (Ctx, [Step])
runSteps ctx [] = (ctx, [])
runSteps ctx (located : rest) =
  let (ctx', outcome) = runStep ctx (loc'value located)
      (final, steps) = runSteps ctx' rest
   in (final, Step located outcome : steps)

runStep :: Ctx -> PalAction -> (Ctx, Outcome)
runStep ctx = \case
  ADefineType td -> define (Actions.defineType td)
  ADefineExpr ed -> define (Actions.defineExpr ed)
  ADefineRule tr -> define (Actions.defineRule tr)
  AInfer e -> (ctx, Inferred (Actions.inferDetailed ctx e))
  AExpect x ->
    let (diagnostics, result, met) = Actions.checkExpectation ctx x
     in (ctx, Expected x diagnostics result met)
  where
    define action =
      let (ctx', diagnostics) = run (runState ctx action)
       in (ctx', if Actions.hasErrors diagnostics then Rejected diagnostics else Defined diagnostics)
