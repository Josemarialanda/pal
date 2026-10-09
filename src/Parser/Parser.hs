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
import Parser.Types (Located (..), PalStmt (..), Span (..), SpanTree (..))
import Text.Megaparsec
  ( Parsec,
    between,
    empty,
    eof,
    getOffset,
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
    Hypothesis (..),
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
-- * @check@ expectations ('SCheck') and @fails@ expectations ('SFails')
--
-- The whole input must be consumed; unrecognised input is a parse error.
palProgram :: Parser [PalStmt]
palProgram = fmap loc'value <$> palProgramLocated

-- | Like 'palProgram', also recording where each statement starts and the
--   spans of the expression of each @infer@, @check@ and @fails@.
palProgramLocated :: Parser [Located PalStmt]
palProgramLocated = sc *> many located <* eof
  where
    located = do
      start <- getOffset
      (stmt, spans) <- statement
      pure (Located start spans stmt)
    statement =
      ((,Nothing) <$> (pType <|> pExpr <|> pRule))
        <|> pInfer
        <|> pCheck
        <|> pFails

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

-- | Parse a type declaration, with its parameters if it takes any, e.g.:
--
-- > type Num
-- > type Arrow<a, b>
pType :: Parser PalStmt
pType = do
  _ <- keyword "type"
  n <- ident
  params <- option [] (angles (ident `sepBy` symbol ","))
  pure (SType (TypeDecl n params))

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
--
-- A hypothesis may be marked @gen@ to generalise its type (let-polymorphism):
--
-- > x : gen a |- body : b
pPremise :: Parser Premise
pPremise = do
  hyps <- pHypothesis `sepBy1` symbol ","
  hasHypotheses <- option False (True <$ (symbol "|-" <|> symbol "⊢"))
  if hasHypotheses
    then Premise hyps <$> pBinding
    else case hyps of
      [Hypothesis e t False] -> pure (Premise [] (e, t))
      [_] -> fail "'gen' is only allowed in hypotheses (before '|-')"
      _ -> fail "expected '|-' after a list of hypotheses"

-- | Parse a hypothesis: a variable with its type, optionally marked @gen@.
--
-- > x : a
-- > x : gen a
pHypothesis :: Parser Hypothesis
pHypothesis = do
  v <- ident
  _ <- symbol ":"
  generalize <- option False (True <$ keyword "gen")
  Hypothesis (EVar v) <$> pTypeExpr <*> pure generalize

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
pInfer :: Parser (PalStmt, Maybe SpanTree)
pInfer = do
  _ <- keyword "infer"
  (e, spans) <- pExprAppLocated
  pure (SInfer e, Just spans)

-- | Parse an expected type, e.g.:
--
-- > check Lam(x, x) : Arrow<a, a>
pCheck :: Parser (PalStmt, Maybe SpanTree)
pCheck = do
  _ <- keyword "check"
  (e, spans) <- pExprAppLocated
  _ <- symbol ":"
  t <- pTypeExpr
  pure (SCheck e t, Just spans)

-- | Parse an expected failure, e.g.:
--
-- > fails Add(True, LitInt)
pFails :: Parser (PalStmt, Maybe SpanTree)
pFails = do
  _ <- keyword "fails"
  (e, spans) <- pExprAppLocated
  pure (SFails e, Just spans)

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
pExprApp = fst <$> pExprAppLocated

-- | Like 'pExprApp', also returning the span of the expression and of each
--   argument (excluding trailing whitespace and comments).
pExprAppLocated :: Parser (Expr, SpanTree)
pExprAppLocated = do
  start <- getOffset
  name <- (:) <$> letterChar <*> many identChar
  nameEnd <- getOffset
  sc
  (args, end) <- option ([], nameEnd) $ do
    _ <- symbol "("
    args <- pExprAppLocated `sepBy` symbol ","
    _ <- string ")"
    end <- getOffset
    sc
    pure (args, end)
  let expr = case listToMaybe name of
        Just c
          | null args && isLower c -> EVar name
          | otherwise -> ECon name (fmap fst args)
        Nothing -> ECon name (fmap fst args) -- defensive; identifiers are never empty
  pure (expr, SpanTree (Span start end) (fmap snd args))

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

-- | Parse parentheses, e.g. @(a, b)@
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse angle brackets, e.g. @<Num, Bool>@
angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")
