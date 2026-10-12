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
import Interpreters.Common.Search (defaultDepth)
import Parser.Data (expandData)
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
    oneOf,
    option,
    optional,
    sepBy,
    sepBy1,
    some,
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
  ( Brackets (..),
    Expr,
    ExprDecl (ExprDecl),
    Hypothesis (..),
    MetaProperty (..),
    Mode (..),
    Premise (..),
    Reduction (Reduction),
    Term (..),
    TermDef (..),
    TestSpec (..),
    Type,
    TypeDecl (TypeDecl),
    TypingRule (..),
    Usage (..),
    isRelation,
    operatorChars,
    premise'share,
    relationSubject,
    pattern EVar,
    pattern PremiseWith,
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
-- * @reduce@ definitions ('SReduce')
-- * @value@ patterns ('SValue') and @test@ statements ('STest')
-- * @find@ statements ('SFind')
-- * @holds@ statements ('SHolds')
-- * @def@ definitions ('SDef')
-- * @infer@ expressions ('SInfer')
-- * @check@ expectations ('SCheck') and @fails@ expectations ('SFails')
--
-- The whole input must be consumed; unrecognised input is a parse error.
palProgram :: Parser [PalStmt]
palProgram = fmap loc'value <$> palProgramLocated

-- | Like 'palProgram', also recording where each statement starts and the
--   spans of the expression of each @infer@, @check@ and @fails@.
palProgramLocated :: Parser [Located PalStmt]
palProgramLocated = sc *> (concat <$> many located) <* eof
  where
    -- A statement, or the statements that a data declaration stands for
    -- (all at the place of the declaration).
    located = do
      start <- getOffset
      fmap (\(stmt, spans) -> Located start spans stmt) <$> statements
    statements =
      (fmap (,Nothing) <$> pData)
        <|> (pure <$> statement)
    statement =
      ((,Nothing) <$> (pType <|> pExpr <|> pRule <|> pReduce <|> pDef <|> pValue <|> pTest <|> pFind <|> pHolds))
        <|> pInfer
        <|> pCheck
        <|> pFails

------------------------------------------------------------
-- Data declarations
------------------------------------------------------------

-- | Parse a declaration of an inductive type; it stands for several
-- statements (see "Parser.Data"), e.g.:
--
-- > data Nat = Zero | Succ(Nat)
-- > data List<a> = Nil | Cons(a, List<a>)
pData :: Parser [PalStmt]
pData = do
  _ <- keyword "data"
  name <- upper "the name of a data type"
  params <- option [] (angles (ident `sepBy` symbol ","))
  _ <- symbol "="
  constructors <- pConstructor `sepBy1` symbol "|"
  pure (expandData name params constructors)
  where
    pConstructor = do
      c <- upper "a constructor"
      fields <- option [] (parens (pTypeExpr `sepBy` symbol ","))
      pure (c, fields)
    upper what = do
      n <- ident
      case n of
        c : _ | isLower c -> fail (what <> " starts with an uppercase letter")
        _ -> pure n

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

-- | Parse a type. A type is a term (see 'pExprApp'), so it can mention
-- expressions, e.g.:
--
-- > Num
-- > List<Num>
-- > Pair<Num, Bool>
-- > Vec(Succ(n), a)
-- > u[x := a]
--
-- A lowercase identifier without arguments (e.g. @a@) is a type variable.
pTypeExpr :: Parser Type
pTypeExpr = pExprApp

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
  (\(mode, conclusion) -> SRule (TypingRuleWith name premises conclusion mode)) <$> pConclusion

------------------------------------------------------------
-- Reductions
------------------------------------------------------------

-- | Parse a reduction, e.g.:
--
-- > reduce Beta: App(Lam(x, t, body), a) ~> body[x := a]
pReduce :: Parser PalStmt
pReduce = do
  _ <- keyword "reduce"
  name <- ident
  _ <- symbol ":"
  lhs <- pExprApp
  _ <- symbol "~>"
  SReduce . Reduction name lhs <$> pExprApp

------------------------------------------------------------
-- Values and tests
------------------------------------------------------------

-- | Parse a value pattern, for the progress test, e.g.:
--
-- > value Succ(n)
pValue :: Parser PalStmt
pValue = keyword "value" *> (SValue <$> pExprApp)

-- | Parse a test of the type system, with the number of terms to generate
-- (5000 if it is not given), e.g.:
--
-- > test preservation
-- > test progress 5000
pTest :: Parser PalStmt
pTest = do
  _ <- keyword "test"
  property <- (Preservation <$ keyword "preservation") <|> (Progress <$ keyword "progress")
  n <- option defaultTestTerms (lexeme L.decimal)
  pure (STest (TestSpec property n))

-- | The number of terms that a test generates if the program does not say.
defaultTestTerms :: Int
defaultTestTerms = 5000

-- | Parse a proof search, e.g.:
--
-- > find Implies<a, a>
-- > find Implies<a, a> 8
--
-- The number is the depth of the search (see "Interpreters.Common.Search").
pFind :: Parser PalStmt
pFind = do
  _ <- keyword "find"
  t <- pTypeExpr
  SFind t <$> option defaultDepth (lexeme L.decimal)

-- | Parse a definition, e.g.:
--
-- > def Two := Succ(Succ(Zero))
-- > def Double(n) := Plus(n, n)
pDef :: Parser PalStmt
pDef = do
  _ <- keyword "def"
  name <- ident
  case name of
    c : _ | isLower c -> fail "the name of a definition starts with an uppercase letter, like a constructor"
    _ -> pure ()
  params <- option [] (parens (pParam `sepBy` symbol ","))
  _ <- symbol ":="
  SDef . TermDef name params <$> pExprApp
  where
    pParam = do
      p <- ident
      case p of
        c : _ | isLower c -> pure p
        _ -> fail "a parameter of a definition starts with a lowercase letter, like a variable"

-- | Parse a single rule premise, either a plain judgment or one made under
-- hypotheses (@⊢@ may also be written @|-@), e.g.:
--
-- > x : Num
-- > x : a |- body : b
-- > x : a, y : b ⊢ body : c
-- > a <: b
-- > Sub(a, b)
--
-- A hypothesis may be marked @gen@ to generalise its type (let-polymorphism):
--
-- > x : gen a |- body : b
pPremise :: Parser Premise
pPremise = do
  shared <- option False (True <$ keyword "share")
  items <- pItem `sepBy1` symbol ","
  hasHypotheses <- option False (True <$ (symbol "|-" <|> symbol "⊢"))
  p <-
    if hasHypotheses
      then do
        hyps <- traverse hypothesis items
        (m, j) <- pItem >>= premiseJudgment
        pure (PremiseWith m hyps j)
      else case items of
        [item] -> (\(m, j) -> PremiseWith m [] j) <$> premiseJudgment item
        _ -> fail "expected '|-' after a list of hypotheses"
  pure p {premise'share = shared}
  where
    hypothesis = \case
      ItemTyping Plain e@(EVar _) t g u -> pure (HypothesisWith e t g u)
      _ -> fail "a hypothesis is written x : T (with ':')"
    premiseJudgment item = do
      (m, j@(e, _)) <- plainItem item
      case e of
        EVar _ -> pure (m, j)
        _ | isRelation j -> pure (m, j)
        _ -> fail "the subject of a premise is a variable of the conclusion, like x : T"

-- | One judgment of a premise or a conclusion: a typing judgment (@e : T@,
--   @e => T@, @e <= T@; in a hypothesis maybe with @gen@, @lin@ or @aff@),
--   or a relation (@a <: b@, or @Sub(a, b)@ with a prefix name).
data Item
  = ItemTyping Mode Expr Type Bool Usage
  | ItemRelation Term

-- | A judgment without the marks of hypotheses (@gen@, @lin@, @aff@).
plainItem :: Item -> Parser (Mode, (Expr, Type))
plainItem = \case
  ItemTyping m e t False Unrestricted -> pure (m, (e, t))
  ItemTyping {} -> fail "'gen', 'lin' and 'aff' are only allowed in hypotheses (before '|-')"
  ItemRelation r -> pure (Plain, (relationSubject, r))

pItem :: Parser Item
pItem = do
  t <- pExprApp
  symbolOrNothing <- optional pJudgmentSymbol
  case symbolOrNothing of
    Nothing -> case t of
      Con {} -> pure (ItemRelation t)
      _ -> fail "expected a judgment, like x : T or a <: b"
    Just (Left m) -> do
      generalize <- if m == Plain then option False (True <$ keyword "gen") else pure False
      usage <- if m == Plain then option Unrestricted ((Linear <$ keyword "lin") <|> (Affine <$ keyword "aff")) else pure Unrestricted
      (\ty -> ItemTyping m t ty generalize usage) <$> pTypeExpr
    Just (Right op) -> (\r -> ItemRelation (Con Parens op [t, r])) <$> pExprApp

-- | The symbol of a judgment: a mode (@:@, @=>@, @<=@), or the operator of a
--   relation (made of symbols, like @<:@ or @::@). The symbols of the
--   language itself (@->@, @~>@, @:=@, @|-@) are not operators.
pJudgmentSymbol :: Parser (Either Mode String)
pJudgmentSymbol = try $ do
  op <- some (oneOf operatorChars)
  result <- case op of
    ":" -> pure (Left Plain)
    "=>" -> pure (Left Synthesis)
    "<=" -> pure (Left Checking)
    _
      | op `elem` ["->", "~>", ":=", "|-"] -> fail ("unexpected " <> op)
      | otherwise -> pure (Right op)
  sc
  pure result

-- | Parse the rule conclusion with its mode, e.g.:
--
-- > Add(x, y) : Num
-- > Lam(x, body) <= Arrow<a, b>
-- > Arrow<a1, b1> <: Arrow<a2, b2>
pConclusion :: Parser (Mode, (Expr, Type))
pConclusion = pItem >>= plainItem

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
  -- An infix relation, like fails Bool <: Nat.
  relation <- optional $ do
    op <- pJudgmentSymbol >>= either (const (fail "expected a relation operator, like <:")) pure
    (\r -> Con Parens op [e, r]) <$> pExprApp
  pure (maybe (SFails e, Just spans) (\r -> (SFails r, Nothing)) relation)

-- | Parse an expectation that a relation judgment can be derived, e.g.:
--
-- > holds Arrow<Top, Nat> <: Arrow<Nat, Top>
-- > holds Sub(Nat, Top)
pHolds :: Parser PalStmt
pHolds = do
  _ <- keyword "holds"
  pItem >>= \case
    ItemRelation r -> pure (SHolds r)
    ItemTyping {} -> fail "holds takes a relation, like a <: b; use check for e : T"

------------------------------------------------------------
-- Expression applications
------------------------------------------------------------

-- | Parse a term (an expression or a type), e.g.:
--
-- > Add(x, y)
-- > List<Num>
--
-- or an atomic term like:
--
-- > LitInt
-- > x
--
-- The arguments go in @(…)@ or @\<…\>@; both give the same term. A term can
-- end with substitutions, e.g. @u[x := a]@.
--
-- Uses the convention:
-- * lowercase identifiers → 'Var'
-- * uppercase identifiers → 'Con'
pExprApp :: Parser Expr
pExprApp = fst <$> pExprAppLocated

-- | Like 'pExprApp', also returning the span of the term and of each
--   argument (excluding trailing whitespace and comments).
pExprAppLocated :: Parser (Expr, SpanTree)
pExprAppLocated = do
  start <- getOffset
  name <- (:) <$> letterChar <*> many identChar
  nameEnd <- getOffset
  sc
  (brackets, args, end) <- option (Parens, [], nameEnd) (arguments Parens "(" ")" <|> arguments Angles "<" ">")
  let term = case listToMaybe name of
        Just c
          | null args && isLower c -> Var name
          | otherwise -> Con brackets name (fmap fst args)
        Nothing -> Con brackets name (fmap fst args) -- defensive; identifiers are never empty
  substitutions start (term, SpanTree (Span start end) (fmap snd args))
  where
    arguments brackets open close = do
      -- `<` must not start an operator, like `<=` (a checking judgment) or `<:`.
      _ <- try (string open <* notFollowedBy (oneOf operatorChars)) <* sc
      args <- pExprAppLocated `sepBy` symbol ","
      _ <- string close
      end <- getOffset
      sc
      pure (brackets, args, end)
    -- Postfix substitutions: @t[x := e]@, @t[x := e][y := f]@, …
    substitutions start located@(body, _) =
      option located $ do
        _ <- symbol "["
        var <- pExprApp
        _ <- symbol ":="
        replacement <- pExprApp
        _ <- string "]"
        end <- getOffset
        sc
        substitutions start (Subst body var replacement, SpanTree (Span start end) [])

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

-- | Parse parentheses, e.g. @(a, b)@
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse angle brackets, e.g. @<Num, Bool>@
angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")
