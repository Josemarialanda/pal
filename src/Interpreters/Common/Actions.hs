-- |
-- Module      : Interpreters.Common.Actions
-- Description : Core semantic actions shared by all PAL interpreters
--
-- This module defines the **core semantic actions** that power the PAL DSL.
-- Each function here corresponds to a fundamental operation within PAL — defining types,
-- expressions, and rules, and performing type inference based on the current context ('Ctx').
--
-- These actions are shared across all interpreter variants:
--
-- * "Interpreters.Core" — pure, minimal interpreters.
-- * "Interpreters.Debug" — traced interpreters that log execution.
--
-- All these interpreters delegate to the functions in this module to perform
-- the actual typechecking logic.  This separation keeps the **semantics**
-- independent of the **execution environment** (pure, IO, traced, etc.).
module Interpreters.Common.Actions where

import Control.Monad (foldM, forM_, when)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Polysemy (Member, Sem)
import Polysemy.State (State, modify)
import qualified Types

--------------------------------------------------------------------------------

-- | Definition actions (context mutation)

--------------------------------------------------------------------------------

-- | Define a new type declaration in the current context.
--
--   Adds a new 'TypeDecl' to 'ctx'types'.
defineType :: (Member (State Types.Ctx) r) => Types.TypeDecl -> Sem r ()
defineType td =
  modify $ \ctx -> ctx {Types.ctx'types = td : Types.ctx'types ctx}

-- | Define a new expression and its associated type.
--
--   Adds a new 'ExprDecl' to 'ctx'exprs'.
defineExpr :: (Member (State Types.Ctx) r) => Types.ExprDecl -> Sem r ()
defineExpr ed =
  modify $ \ctx -> ctx {Types.ctx'exprs = ed : Types.ctx'exprs ctx}

-- | Define a new typing rule for inference.
--
--   Adds a 'TypingRule' to 'ctx'rules'.
defineRule :: (Member (State Types.Ctx) r) => Types.TypingRule -> Sem r ()
defineRule tr =
  modify $ \ctx -> ctx {Types.ctx'rules = tr : Types.ctx'rules ctx}

--------------------------------------------------------------------------------

-- | Expression lookup and inference

--------------------------------------------------------------------------------

-- | Look up the declared type of an expression by name, if it exists.
--
--   Returns 'Nothing' if the expression is not defined in the context.
lookupExprType :: Types.Ctx -> String -> Maybe Types.Type
lookupExprType ctx name =
  listToMaybe [t | Types.ExprDecl n t <- Types.ctx'exprs ctx, n == name]

-- | Perform type inference for an expression in the given context.
--
--   This is the heart of the PAL typechecking engine. It works as follows:
--
--   1. If the expression has a known declared type (and no arguments),
--      it returns that type immediately.
--   2. Otherwise, it searches for a matching typing rule (via 'matchRule')
--      and applies it (via 'applyRule').
--   3. Fails with 'UnknownExpr' if the expression is unrecognized.
infer :: Types.Expr -> Types.Ctx -> Either Types.Err Types.Type
infer e@(Types.ECon name args) ctx = case lookupExprType ctx name of
  Just t | null args -> Right t
  Just t             -> Right t  -- constructors with zero args are base cases
  Nothing -> do
    rule <- matchRule ctx e
    applyRule ctx e rule
infer (Types.EVar v) ctx = maybe (Left (Types.UnknownExpr v)) Right (lookupExprType ctx v)
--------------------------------------------------------------------------------

-- | Rule matching and validation

--------------------------------------------------------------------------------

-- | Attempt to find a typing rule in the context that matches a given expression.
--
--   The process checks:
--     * that the rule’s conclusion name matches the expression,
--     * and that the number of arguments (arity) matches.
--
--   Returns the matching rule, or a descriptive error such as
--   'ArityMismatch' or 'NoRuleMatched'.
matchRule :: Types.Ctx -> Types.Expr -> Either Types.Err Types.TypingRule
matchRule ctx (Types.ECon name args) = case rulesWithSameName' of
  [] -> Left (Types.UnknownExpr name)
  _ -> case find (\r -> arity r == argArity) rulesWithSameName' of
    Just r -> Right r
    Nothing -> case listToMaybe (fmap arity rulesWithSameName') of
      Just k -> Left (Types.ArityMismatch k argArity)
      Nothing -> Left (Types.NoRuleMatched (Types.ECon name args))
  where
    argArity = length args
    rulesWithSameName' = rulesWithSameName ctx name
matchRule _ e = Left (Types.NoRuleMatched e)

-- | Get all typing rules in the context that have the same constructor name.
rulesWithSameName :: Types.Ctx -> String -> [Types.TypingRule]
rulesWithSameName ctx name = filter match (Types.ctx'rules ctx)
  where
    match r = case fst (Types.typingRule'ruleConclusion r) of
      Types.ECon n _ -> n == name
      _ -> False

-- | Compute the arity (number of parameters) of a rule’s conclusion.
arity :: Types.TypingRule -> Int
arity r = case fst (Types.typingRule'ruleConclusion r) of
  Types.ECon _ ps -> length ps
  _ -> 0

--------------------------------------------------------------------------------

-- | Rule application and substitution

--------------------------------------------------------------------------------

-- | Attempt to match a rule’s conclusion pattern against a target expression.
--
--   If successful, returns an environment mapping pattern variables to
--   actual expressions. If the pattern does not match, returns a descriptive
--   error (such as 'ArityMismatch' or 'NoRuleMatched').
matchConclusion :: Types.Expr -> Types.Expr -> Either Types.Err (Map String Types.Expr)
matchConclusion = go M.empty
  where
    go env (Types.ECon pn ps) (Types.ECon en es)
      | pn == en && length ps == length es = foldM (\acc (p, e) -> go acc p e) env (zip ps es)
      | pn == en = Left (Types.ArityMismatch (length ps) (length es))
      | otherwise = Left (Types.NoRuleMatched (Types.ECon en es))
    go env (Types.EVar var) e = case M.lookup var env of
      Nothing -> Right (M.insert var e env)
      Just ePrev ->
        if ePrev == e
          then Right env
          else Left (Types.CustomErr $ "inconsistent binding for " <> var)
    go _ p e = Left (Types.CustomErr $ "unsupported pattern: " <> show (p, e))

-- | Perform substitution of variables within an expression.
--
--   Given a mapping from variable names to expressions, replaces all
--   occurrences of those variables recursively.
substituteExpr :: Map String Types.Expr -> Types.Expr -> Types.Expr
substituteExpr env = \case
  Types.EVar v -> M.findWithDefault (Types.EVar v) v env
  Types.ECon n args -> Types.ECon n (fmap (substituteExpr env) args)

-- | Apply a typing rule to an expression, verifying all its premises.
--
--   This function:
--     1. Matches the rule’s conclusion pattern against the target expression.
--     2. Substitutes concrete expressions for variables in each premise.
--     3. Infers the type of each instantiated premise recursively.
--     4. Ensures that inferred types match the expected types.
--
--   If all premises succeed, the conclusion type is returned.
applyRule :: Types.Ctx -> Types.Expr -> Types.TypingRule -> Either Types.Err Types.Type
applyRule ctx target (Types.TypingRule _ premises (conclExpr, conclTy)) = do
  env <- matchConclusion conclExpr target
  forM_ premises $ \(pExpr, pTy) -> do
    let concretized = substituteExpr env pExpr
    tGot <- infer concretized ctx
    when (tGot /= pTy) (Left (Types.Mismatch pTy tGot))
  pure conclTy
