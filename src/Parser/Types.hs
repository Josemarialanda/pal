-- |
-- Module      : Parser.Types
-- Description : Intermediate statement representation produced by the PAL parser
--
-- This module defines the intermediate representation used by the PAL parser.
-- Each 'PalStmt' corresponds to a top-level statement in a PAL source file or
-- quasiquoted block (e.g. @[palQuasiQuoter| ... |]@).
--
-- After parsing, a PAL program is represented as a list of 'PalStmt' values,
-- which can then be interpreted or compiled into effectful PAL actions
-- (e.g. 'defineType', 'defineExpr', 'defineRule', 'infer').
module Parser.Types where

import Types (Expr, ExprDecl, Type, TypeDecl, TypingRule)

-- | A single top-level PAL statement.
--
-- The parser produces a sequence of these from user input.
data PalStmt
  = -- | A type declaration: @type Foo@ or @type Foo\<a, b\>@
    SType TypeDecl
  | -- | An expression declaration: @expr Bar : Baz@
    SExpr ExprDecl
  | -- | A typing rule definition
    SRule TypingRule
  | -- | A type inference request: @infer e@
    SInfer Expr
  | -- | An expected type: @check e : T@
    SCheck Expr Type
  | -- | An expected failure: @fails e@
    SFails Expr
  deriving (Show)

-- | A range of the source, as character offsets (end exclusive).
data Span = Span {span'start :: Int, span'end :: Int}
  deriving (Eq, Show)

-- | The spans of an expression and, recursively, of its arguments, in the
--   same shape as the expression. Used to point at the subterm at fault.
data SpanTree = SpanTree Span [SpanTree]
  deriving (Eq, Show)

-- | A statement with where it starts in the source and, for statements
--   about an expression (@infer@, @check@, @fails@), the spans of that
--   expression.
data Located a = Located
  { loc'start :: Int,
    loc'exprSpans :: Maybe SpanTree,
    loc'value :: a
  }
  deriving (Show)

instance Functor Located where
  fmap f (Located s t a) = Located s t (f a)

-- | The span of the subterm at a path (child indices), as far as the tree
--   reaches.
spanAt :: [Int] -> SpanTree -> Span
spanAt path (SpanTree sp children) = case path of
  i : rest | i >= 0, child : _ <- drop i children -> spanAt rest child
  _ -> sp
