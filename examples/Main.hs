-- |
-- Module      : Main
-- Description : Runner for the PAL examples.
--
-- Usage:
--
-- > pal-examples [--trace] [--list] [NAME | GROUP ...]
--
-- With no names, every example runs. A group (@code@, @data@, @dsl@, @file@)
-- runs every example whose name starts with @GROUP/@.
--
-- By default only each inference result is printed. With @--trace@ the Debug
-- interpreter prints every step along with the full context.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.List (isPrefixOf, partition)
import qualified Examples.AsCode as AsCode
import qualified Examples.AsDSL as AsDSL
import qualified Examples.AsData as AsData
import qualified Examples.AsFile as AsFile
import Examples.Types (Example (..), Program (..))
import qualified Interpreters.Debug as Debug
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Types (Ctx)

allExamples :: [Example]
allExamples = AsCode.examples <> AsData.examples <> AsDSL.examples <> AsFile.examples

main :: IO ()
main = do
  (flags, names) <- partition ("--" `isPrefixOf`) <$> getArgs
  let unknownFlags = filter (`notElem` ["--trace", "--list"]) flags
  unless (null unknownFlags) $ do
    putStrLn ("Unknown flag(s): " <> unwords unknownFlags)
    exitFailure
  if "--list" `elem` flags
    then forM_ allExamples $ \ex -> putStrLn (pad 16 (exName ex) <> exDescription ex)
    else do
      selected <- selectExamples names
      forM_ selected (runExample ("--trace" `elem` flags))

-- | Pick the examples matching the given names or groups (all if none given).
selectExamples :: [String] -> IO [Example]
selectExamples [] = pure allExamples
selectExamples names = do
  let matches n ex = exName ex == n || (n <> "/") `isPrefixOf` exName ex
      unknown = [n | n <- names, not (any (matches n) allExamples)]
  unless (null unknown) $ do
    putStrLn ("Unknown example(s): " <> unwords unknown <> " (see --list)")
    exitFailure
  pure [ex | ex <- allExamples, any (`matches` ex) names]

runExample :: Bool -> Example -> IO ()
runExample traceAll ex = do
  putStrLn ("━━ " <> exName ex <> " ━━ " <> exDescription ex)
  loaded <- exLoad ex
  case loaded of
    Left err -> putStrLn ("[load error]\n" <> err)
    Right (Program program)
      | traceAll -> do
          result <- Debug.runInterpreterStdout (mempty @Ctx) program
          putStrLn ("result: " <> either show show result)
      | otherwise -> do
          let (traces, _) = Debug.runInterpreterTraceList (mempty @Ctx) program
          mapM_ putStrLn (filter isResultLine traces)
  putStrLn ""
  where
    isResultLine l = any (`isPrefixOf` l) ["[PAL] ✓", "[PAL] ✗"]

pad :: Int -> String -> String
pad n s = s <> replicate (n - length s) ' '
