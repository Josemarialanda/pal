{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser.Parser
-- Description : Parser for the PAL language syntax.
--
-- This module implements the Megaparsec-based parser for the PAL DSL.
-- It converts raw PAL source code (or quasiquoted strings via @[pal| ... |]@)
-- into an abstract syntax tree of 'PalStmt' values, which can then be interpreted
-- by PAL's typechecker and inference engine.
--
-- The grammar supports type declarations, expression declarations,
-- typing rules, and inference queries. For example:
--
-- @
-- type Num
-- type Bool
--
-- expr LitInt : Num
-- expr True   : Bool
-- expr False  : Bool
--
-- rule Add:
--   x : Num
--   y : Num
-- ->
--   Add(x, y) : Num
--
-- infer Add(LitInt, LitInt)
-- @
--
-- The parser is designed to be used in the quasiquoter ('Parser.Quasi')
module Parser.Parser where

import Data.Char (isLower)
import Data.Maybe (listToMaybe)
import Data.Void (Void)
import Parser.Types (PalStmt (..))
import Text.Megaparsec
  ( Parsec,
    between,
    empty,
    many,
    manyTill,
    option,
    sepBy,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Types
  ( Expr (..),
    ExprDecl (ExprDecl),
    Type (TCon),
    TypeDecl (TypeDecl),
    TypingRule (TypingRule),
  )

-- | The base parser type for PAL programs.
-- Uses 'String' as the input stream and 'Void' for custom error components.
type Parser = Parsec Void String

------------------------------------------------------------
-- Lexing
------------------------------------------------------------

-- | Whitespace consumer used between tokens.
-- PAL syntax is whitespace-insensitive and ignores spaces and newlines
-- between top-level statements.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Parse a token and consume trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a fixed symbol (like punctuation or keywords)
-- and consume trailing whitespace.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parse an identifier.
--
-- Identifiers may contain letters, digits, and underscores.
-- By convention, identifiers starting with a lowercase letter
-- represent /variables/ ('EVar'), while those starting with an uppercase
-- letter represent /constructors/ ('ECon').
ident :: Parser String
ident = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

------------------------------------------------------------
-- Top-level PAL program
------------------------------------------------------------

-- | Parse an entire PAL program as a sequence of top-level statements.
--
-- Each statement is parsed as one of:
--
-- * @type@ declarations ('SType')
-- * @expr@ declarations ('SExpr')
-- * @rule@ definitions ('SRule')
-- * @infer@ expressions ('SInfer')
palProgram :: Parser [PalStmt]
palProgram = many (sc *> (pType <|> pExpr <|> pRule <|> pInfer))

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

-- | Parse a type declaration, e.g.:
--
-- > type Num
pType :: Parser PalStmt
pType = do
  _ <- symbol "type"
  n <- ident
  pure (SType (TypeDecl n []))

------------------------------------------------------------
-- Type parser
------------------------------------------------------------

-- | Parse a type constructor, possibly with parameters.
--
-- Examples:
--
-- > Num
-- > Bool
-- > List<Num>
-- > Pair<Num, Bool>
--
-- Produces nested 'TCon' terms.
pTypeExpr :: Parser Type
pTypeExpr = do
  name <- ident
  args <- option [] (angles (pTypeExpr `sepBy` symbol ","))
  pure (TCon name args)

------------------------------------------------------------
-- Expression declarations
------------------------------------------------------------

-- | Parse an expression declaration, e.g.:
--
-- > expr LitInt : Num
pExpr :: Parser PalStmt
pExpr = do
  _ <- symbol "expr"
  n <- ident
  _ <- symbol ":"
  SExpr . ExprDecl n <$> pTypeExpr

------------------------------------------------------------
-- Typing rules
------------------------------------------------------------

-- | Parse a typing rule, e.g.:
--
-- > rule Add:
-- >   x : Num
-- >   y : Num
-- > ->
-- >   Add(x, y) : Num
--
-- Produces a 'TypingRule' with premises and a conclusion.
pRule :: Parser PalStmt
pRule = do
  _ <- symbol "rule"
  name <- ident
  _ <- symbol ":"
  premises <- manyTill pPremise (symbol "->")
  SRule . TypingRule name premises <$> pConclusion

-- | Parse a single rule premise, e.g.:
--
-- > x : Num
pPremise :: Parser (Expr, Type)
pPremise = do
  v <- ident
  _ <- symbol ":"
  t <- pTypeExpr
  pure (EVar v, t)

-- | Parse the rule conclusion, e.g.:
--
-- > Add(x, y) : Num
pConclusion :: Parser (Expr, Type)
pConclusion = do
  e <- pExprApp
  _ <- symbol ":"
  t <- pTypeExpr
  pure (e, t)

------------------------------------------------------------
-- Expression inference
------------------------------------------------------------

-- | Parse an inference statement, e.g.:
--
-- > infer Add(LitInt, LitInt)
--
-- Produces an 'SInfer' statement representing a type query.
pInfer :: Parser PalStmt
pInfer = do
  _ <- symbol "infer"
  SInfer <$> pExprApp

------------------------------------------------------------
-- Expression applications
------------------------------------------------------------

-- | Parse an expression application, e.g.:
--
-- > Add(x, y)
--
-- or an atomic expression like:
--
-- > LitInt
-- > x
--
-- Uses the convention:
-- * lowercase identifiers → 'EVar'
-- * uppercase identifiers → 'ECon'
pExprApp :: Parser Expr
pExprApp = do
  name <- ident
  args <- parens (pExprApp `sepBy` symbol ",") <|> pure []
  pure $
    case listToMaybe name of
      Just c
        | null args && isLower c -> EVar name
        | otherwise -> ECon name args
      Nothing -> ECon name args -- defensive; 'ident' never yields ""

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

-- | Parse parentheses, e.g. @(a, b)@
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse angle brackets, e.g. @<Num, Bool>@
angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")
