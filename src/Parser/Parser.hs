{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser.Parser
-- Description : Parser for the PAL language syntax.
--
-- This module implements the Megaparsec-based parser for the PAL DSL.
-- It converts raw PAL source code (or quasiquoted strings via @[palQuasiQuoter| ... |]@)
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
    eof,
    many,
    manyTill,
    notFollowedBy,
    option,
    sepBy,
    sepBy1,
    try,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Types
  ( Expr (..),
    ExprDecl (ExprDecl),
    Premise (..),
    Type (TCon, TVar),
    TypeDecl (TypeDecl),
    TypingRule (TypingRule),
  )

-- | The base parser type for PAL programs.
-- Uses 'String' as the input stream and 'Void' for custom error components.
type Parser = Parsec Void String

------------------------------------------------------------
-- Lexing
------------------------------------------------------------

-- | Space consumer: skips whitespace and line ('--') comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

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
ident = lexeme ((:) <$> letterChar <*> many identChar)

-- | Characters allowed after the first letter of an identifier.
identChar :: Parser Char
identChar = alphaNumChar <|> char '_'

-- | Parse a reserved keyword, ensuring it is not the prefix of a longer
-- identifier (so @typeFoo@ is not read as @type Foo@).
keyword :: String -> Parser String
keyword w = lexeme (try (string w <* notFollowedBy identChar))

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
--
-- The whole input must be consumed; unrecognised input is a parse error.
palProgram :: Parser [PalStmt]
palProgram = sc *> many (pType <|> pExpr <|> pRule <|> pInfer) <* eof

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

-- | Parse a type declaration, e.g.:
--
-- > type Num
pType :: Parser PalStmt
pType = do
  _ <- keyword "type"
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
-- Produces nested 'TCon' terms. A lowercase identifier without type
-- arguments (e.g. @a@) is a type variable ('TVar').
pTypeExpr :: Parser Type
pTypeExpr = do
  name <- ident
  args <- option [] (angles (pTypeExpr `sepBy` symbol ","))
  pure $
    case listToMaybe name of
      Just c | null args && isLower c -> TVar name
      _ -> TCon name args

------------------------------------------------------------
-- Expression declarations
------------------------------------------------------------

-- | Parse an expression declaration, e.g.:
--
-- > expr LitInt : Num
pExpr :: Parser PalStmt
pExpr = do
  _ <- keyword "expr"
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
  _ <- keyword "rule"
  name <- ident
  _ <- symbol ":"
  premises <- manyTill pPremise (symbol "->")
  SRule . TypingRule name premises <$> pConclusion

-- | Parse a single rule premise, either a plain judgment or one made under
-- hypotheses (@⊢@ may also be written @|-@), e.g.:
--
-- > x : Num
-- > x : a |- body : b
-- > x : a, y : b ⊢ body : c
pPremise :: Parser Premise
pPremise = do
  bindings <- pBinding `sepBy1` symbol ","
  hasHypotheses <- option False (True <$ (symbol "|-" <|> symbol "⊢"))
  if hasHypotheses
    then Premise bindings <$> pBinding
    else case bindings of
      [judgment] -> pure (Premise [] judgment)
      _ -> fail "expected '|-' after a list of hypotheses"

-- | Parse a variable with its type, e.g.:
--
-- > x : Num
pBinding :: Parser (Expr, Type)
pBinding = do
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
  _ <- keyword "infer"
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
