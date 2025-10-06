-- |
-- Module      : Parser.Types
-- Description : Core semantic actions shared by all PAL interpreters
--
-- This module defines the intermediate representation used by the PAL parser.
-- Each 'PalStmt' corresponds to a top-level statement in a PAL source file or
-- quasiquoted block (e.g. @[pal| ... |]@).
--
-- After parsing, a PAL program is represented as a list of 'PalStmt' values,
-- which can then be interpreted or compiled into effectful PAL actions
-- (e.g. 'defineType', 'defineExpr', 'defineRule', 'infer').
module Parser.Types where

import Types (Expr, ExprDecl, TypeDecl, TypingRule)

-- | A single top-level PAL statement.
--
-- The parser produces a sequence of these from user input.
data PalStmt
  = -- | A type declaration: @type Foo@
    SType TypeDecl
  | -- | An expression declaration: @expr Bar : Baz@
    SExpr ExprDecl
  | -- | A typing rule definition
    SRule TypingRule
  | -- | A type inference request
    SInfer Expr
  deriving (Show)
