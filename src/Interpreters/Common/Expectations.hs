-- |
-- Module      : Interpreters.Common.Expectations
-- Description : Checking @check@, @fails@ and @find@ statements
--
-- An expectation states what a program expects: a type for an expression
-- (@check e : T@), a type error (@fails e@), or a term of a type
-- (@find T@, see "Interpreters.Common.Search").
module Interpreters.Common.Expectations
  ( checkExpectation,
  )
where

import Data.Either (isLeft, isRight)
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Interpreters.Common.Actions
  ( checkDetailed,
    equivalent,
    errorDiag,
    hasErrors,
    inferDetailed,
    normalForm,
    validateType,
  )
import Interpreters.Common.Search (findTerm, notFoundMessage)
import qualified Types

-- | Check an expectation. Returns diagnostics about the expected type (an
--   undeclared type makes the expectation unmet), the inference result, and
--   whether the expectation is met.
--
--   @check e : T@ compares the inferred type with @T@ up to renaming of type
--   variables, so @check Lam(x, x) : Arrow<b, b>@ is met by @Arrow<a, a>@.
--   If the type of @e@ cannot be inferred because a rule only checks (a
--   @<=@ conclusion), @e@ is checked against @T@ instead.
--
--   @find T@ is met when proof search finds a term of type @T@; the result
--   is the derivation of that term.
checkExpectation ::
  Types.Ctx ->
  Types.Expectation ->
  ([Types.Diagnostic], Either (NonEmpty Types.Failure) Types.Derivation, Bool)
checkExpectation ctx = \case
  Types.ExpectType e t ->
    let diagnostics = validateType ctx t <> either (\err -> [errorDiag (errText err)]) (const []) (normalForm ctx t)
        inferred = inferDetailed ctx e
        needsChecking = either (any (isCannotInfer . Types.failure'err)) (const False) inferred
        result = if needsChecking then checkDetailed ctx e t else inferred
        errText err = let s = show err in fromMaybe s (stripPrefix "[Error] " s)
        met
          | hasErrors diagnostics = False
          | needsChecking = isRight result
          | otherwise = either (const False) (equivalent ctx t . Types.deriv'type) result
     in (diagnostics, result, met)
  Types.ExpectFailure e ->
    let result = inferDetailed ctx e
     in ([], result, isLeft result)
  Types.ExpectInhabited t depth ->
    let diagnostics = validateType ctx t
     in case findTerm ctx depth t of
          Right e -> (diagnostics, checkDetailed ctx e t, not (hasErrors diagnostics))
          Left why ->
            let notFound = Types.CustomErr (notFoundMessage why)
             in (diagnostics, Left (pure (Types.Failure notFound [] [])), False)
  where
    isCannotInfer = \case
      Types.CannotInfer _ -> True
      _ -> False
