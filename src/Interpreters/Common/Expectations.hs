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
    deriveRelation,
    equivalent,
    errorDiag,
    hasErrors,
    inferDetailed,
    normalForm,
    relationRules,
    runDetailed,
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
  -- For a relation (like @fails Sub(Bool, Nat)@), the judgment must not be
  -- derivable; for an expression, it must not have a type.
  Types.ExpectFailure e
    | isRelationTerm e ->
        let result = derive e
         in ([], result, isLeft result)
    | otherwise ->
        let result = inferDetailed ctx e
         in ([], result, isLeft result)
  Types.ExpectHolds j ->
    let diagnostics = [errorDiag ("unknown relation " <> show j <> ": no rule concludes a judgment of this form") | not (isRelationTerm j)]
        result = derive j
     in (diagnostics, result, not (hasErrors diagnostics) && isRight result)
  Types.ExpectInhabited t depth ->
    let diagnostics = validateType ctx t
     in case findTerm ctx depth t of
          Right e -> (diagnostics, checkDetailed ctx e t, not (hasErrors diagnostics))
          Left why ->
            let notFound = Types.CustomErr (notFoundMessage why)
             in (diagnostics, Left (pure (Types.Failure notFound [] [])), False)
  where
    -- A term whose head is a relation of the context.
    isRelationTerm t = maybe False (not . null . uncurry (relationRules ctx)) (Types.relationName t)
    derive j = runDetailed ctx ((Types.relationSubject,) <$> deriveRelation ctx j)
    isCannotInfer = \case
      Types.CannotInfer _ -> True
      _ -> False
