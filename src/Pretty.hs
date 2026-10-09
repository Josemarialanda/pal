-- |
-- Module      : Pretty
-- Description : Terminal styling for PAL output (ANSI colors and text attributes)
--
-- Renders PAL types, expressions, rules, errors and contexts with syntax
-- highlighting, using plain ANSI escape sequences (no dependencies).
--
-- Every renderer takes a 'Style'. With 'plain' the output is exactly the
-- unstyled text used elsewhere (e.g. the 'Show' instances), so piped output,
-- scripts and documentation stay unchanged. 'detectStyle' only enables
-- colors for a terminal, and honours @NO_COLOR@ and @TERM=dumb@.
--
-- Palette:
--
-- * keywords (@type@, @rule@, @gen@, …) — bold blue
-- * type constructors — cyan; type variables — italic magenta
-- * expression constructors — bold; variables — italic
-- * punctuation and secondary text — dim
-- * success — green; errors — red
module Pretty
  ( -- * Styles
    Style (..),
    plain,
    colored,
    detectStyle,

    -- * Primitives
    Attr (..),
    paint,
    paintPrompt,
    visibleLength,

    -- * PAL syntax
    prettyType,
    prettyExpr,
    prettyRule,
    prettyCtx,

    -- * Messages
    prettyResult,
    prettyErr,
    prettyDefinedType,
    prettyDefinedExpr,
    prettyDefinedRule,
    prettyParseError,
    note,
    failure,
    success,
    box,
  )
where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate, isPrefixOf)
import System.Environment (lookupEnv)
import System.IO (Handle, hIsTerminalDevice)
import Types
  ( Ctx (..),
    Err (..),
    Expr (..),
    ExprDecl (..),
    Hypothesis (..),
    Premise (..),
    Type (..),
    TypeDecl (..),
    TypingRule (..),
  )

--------------------------------------------------------------------------------

-- | Styles

--------------------------------------------------------------------------------

-- | Whether output is styled with ANSI escape sequences.
newtype Style = Style {styleEnabled :: Bool}

-- | No styling: renderers produce plain text.
plain :: Style
plain = Style False

-- | ANSI colors and text attributes.
colored :: Style
colored = Style True

-- | Enable styling only if the handle is a terminal, @NO_COLOR@ is unset or
--   empty (<https://no-color.org>), and @TERM@ is not @dumb@.
detectStyle :: Handle -> IO Style
detectStyle h = do
  tty <- hIsTerminalDevice h
  noColor <- lookupEnv "NO_COLOR"
  term <- lookupEnv "TERM"
  pure (Style (tty && maybe True null noColor && term /= Just "dumb"))

--------------------------------------------------------------------------------

-- | Primitives

--------------------------------------------------------------------------------

-- | Text attributes (SGR codes). Terminals cannot change the font family,
--   but they can change its weight and slant.
data Attr = Bold | Dim | Italic | Underline | Red | Green | Yellow | Blue | Magenta | Cyan

sgr :: Attr -> String
sgr = \case
  Bold -> "1"
  Dim -> "2"
  Italic -> "3"
  Underline -> "4"
  Red -> "31"
  Green -> "32"
  Yellow -> "33"
  Blue -> "34"
  Magenta -> "35"
  Cyan -> "36"

escape :: [Attr] -> String
escape as = "\ESC[" <> intercalate ";" (fmap sgr as) <> "m"

reset :: String
reset = "\ESC[0m"

-- | Apply attributes to a string (a no-op for 'plain').
paint :: Style -> [Attr] -> String -> String
paint (Style False) _ s = s
paint _ [] s = s
paint _ as s = escape as <> s <> reset

-- | Like 'paint', for a Haskeline prompt: each escape sequence is followed
--   by @\\STX@, which tells Haskeline it takes up no columns.
paintPrompt :: Style -> [Attr] -> String -> String
paintPrompt (Style False) _ s = s
paintPrompt _ [] s = s
paintPrompt _ as s = escape as <> "\STX" <> s <> reset <> "\STX"

-- | Remove escape sequences.
stripAnsi :: String -> String
stripAnsi = \case
  '\ESC' : '[' : rest -> stripAnsi (drop 1 (dropWhile (/= 'm') rest))
  '\STX' : rest -> stripAnsi rest
  c : rest -> c : stripAnsi rest
  [] -> []

-- | The number of columns a (possibly styled) string occupies.
visibleLength :: String -> Int
visibleLength = length . stripAnsi

--------------------------------------------------------------------------------

-- | PAL syntax

--------------------------------------------------------------------------------

keyword, punct, note, failure, success :: Style -> String -> String
keyword st = paint st [Bold, Blue]
punct st = paint st [Dim]

-- | Secondary text.
note st = paint st [Dim]

-- | Error text.
failure st = paint st [Red]

-- | Success text.
success st = paint st [Green]

-- | A type, e.g. @Arrow\<a, Bool\>@. Plain output matches 'show'.
prettyType :: Style -> Type -> String
prettyType st = \case
  TVar v -> paint st [Italic, Magenta] v
  TCon n [] -> paint st [Cyan] n
  TCon n ts ->
    paint st [Cyan] n
      <> punct st "<"
      <> intercalate (punct st ", ") (fmap (prettyType st) ts)
      <> punct st ">"

-- | An expression, e.g. @App(f, True)@. Plain output matches 'show'.
prettyExpr :: Style -> Expr -> String
prettyExpr st = \case
  EVar v -> paint st [Italic] v
  ECon n [] -> paint st [Bold] n
  ECon n es ->
    paint st [Bold] n
      <> punct st "("
      <> intercalate (punct st ", ") (fmap (prettyExpr st) es)
      <> punct st ")"

prettyJudgment :: Style -> (Expr, Type) -> String
prettyJudgment st (e, t) = prettyExpr st e <> punct st " : " <> prettyType st t

prettyPremise :: Style -> Premise -> String
prettyPremise st (Premise [] j) = prettyJudgment st j
prettyPremise st (Premise hs j) =
  intercalate (punct st ", ") (fmap hypothesis hs) <> punct st " ⊢ " <> prettyJudgment st j
  where
    hypothesis (Hypothesis e t g) =
      prettyExpr st e <> punct st " : " <> (if g then keyword st "gen" <> " " else "") <> prettyType st t

-- | A typing rule drawn as an inference rule: premises above a bar, the
--   conclusion below, and the rule's name beside the bar.
--
-- >   x : Bool     y : Bool
-- >  ─────────────────────── And
-- >      And(x, y) : Bool
prettyRule :: Style -> TypingRule -> [String]
prettyRule st (TypingRule name premises conclusion) =
  [center above | not (null premises)]
    <> [punct st (replicate width '─') <> " " <> paint st [Italic, Yellow] name, center below]
  where
    above = intercalate "     " (fmap (prettyPremise st) premises)
    below = prettyJudgment st conclusion
    width = 2 + max (visibleLength above) (visibleLength below)
    center s = replicate ((width - visibleLength s) `div` 2) ' ' <> s

-- | The context, grouped by kind, in definition order.
prettyCtx :: Style -> Ctx -> String
prettyCtx st Ctx {..} =
  unlines $
    [heading "Types", "  " <> orNone (intercalate (punct st ", ") (fmap typeName (reverse ctx'types))), ""]
      <> [heading "Expressions"]
      <> orNoneLines (fmap (\(ExprDecl n t) -> "  " <> prettyJudgment st (ECon n [], t)) (reverse ctx'exprs))
      <> ["", heading "Rules"]
      <> orNoneLines (intercalate [""] (fmap (fmap ("  " <>) . prettyRule st) (reverse ctx'rules)))
  where
    heading = paint st [Bold, Underline]
    typeName (TypeDecl n args) = unwords (paint st [Cyan] n : fmap (paint st [Italic, Magenta]) args)
    orNone s = if null s then note st "(none)" else s
    orNoneLines ls = if null ls then ["  " <> note st "(none)"] else ls

--------------------------------------------------------------------------------

-- | Messages

--------------------------------------------------------------------------------

-- | An inference result.
--
--   Plain: @✓ e :: T@ or @✗ e -> [Error] …@ on one line (the format used in
--   the docs). Styled: highlighted, with the error on its own line.
prettyResult :: Style -> Expr -> Either Err Type -> String
prettyResult st e = \case
  Right t
    | styled -> mark [Bold, Green] "✓" <> " " <> prettyExpr st e <> punct st " :: " <> prettyType st t
    | otherwise -> "✓ " <> show e <> " :: " <> show t
  Left err
    | styled -> mark [Bold, Red] "✗" <> " " <> prettyExpr st e <> "\n" <> punct st "  ╰─ " <> prettyErr st err
    | otherwise -> "✗ " <> show e <> " -> " <> show err
  where
    styled = styleEnabled st
    mark = paint st

-- | An error message with highlighted types and expressions.
prettyErr :: Style -> Err -> String
prettyErr st err
  | not (styleEnabled st) = show err
  | otherwise = case err of
      UnknownExpr s -> label "unknown expression" <> " " <> paint st [Bold] s
      Mismatch e a -> label "type mismatch" <> ": expected " <> prettyType st e <> ", got " <> prettyType st a
      ArityMismatch e g -> label "arity mismatch" <> ": expected " <> show e <> " argument(s), got " <> show g
      NoRuleMatched e -> label "no typing rule matches" <> " " <> prettyExpr st e
      InfiniteType v t -> label "infinite type" <> ": " <> prettyType st v <> punct st " ~ " <> prettyType st t
      CustomErr msg -> label msg
  where
    label = paint st [Bold, Red]

prettyDefinedType :: Style -> TypeDecl -> String
prettyDefinedType st td@(TypeDecl n args)
  | styleEnabled st =
      note st "defined " <> keyword st "type" <> " " <> unwords (paint st [Cyan] n : fmap (paint st [Italic, Magenta]) args)
  | otherwise = "defined " <> show td

prettyDefinedExpr :: Style -> ExprDecl -> String
prettyDefinedExpr st ed@(ExprDecl n t)
  | styleEnabled st = note st "defined " <> keyword st "expr" <> " " <> prettyJudgment st (ECon n [], t)
  | otherwise = "defined expr " <> show ed

-- | Styled, the confirmation is followed by the rule drawn as an inference rule.
prettyDefinedRule :: Style -> TypingRule -> String
prettyDefinedRule st tr
  | styleEnabled st =
      intercalate "\n" $
        (note st "defined " <> keyword st "rule" <> " " <> paint st [Bold] (typingRule'name tr))
          : fmap ("    " <>) (prettyRule st tr)
  | otherwise = "defined rule " <> typingRule'name tr

-- | Highlight a Megaparsec error report: the location in bold, the source
--   gutter dimmed, the caret and messages in color. Plain: unchanged.
prettyParseError :: Style -> String -> String
prettyParseError st report
  | not (styleEnabled st) = report
  | otherwise = unlines (fmap line (lines report))
  where
    line l
      | isLocation l = paint st [Bold] l
      | (gutter, '|' : rest) <- break (== '|') l,
        all (\c -> isSpace c || isDigit c) gutter =
          punct st (gutter <> "|") <> if all (\c -> c == ' ' || c == '^') rest then paint st [Bold, Red] rest else rest
      | "unexpected" `isPrefixOf` l = paint st [Red] l
      | "expecting" `isPrefixOf` l = paint st [Yellow] l
      | otherwise = paint st [Red] l
    isLocation l = not (null l) && last l == ':' && any isDigit l && ' ' `notElem` take 1 l

-- | Draw lines of text in a rounded box. Plain: the lines unchanged.
box :: Style -> [String] -> String
box st ls
  | not (styleEnabled st) = unlines ls
  | otherwise =
      unlines $
        [frame ("╭" <> replicate (width + 2) '─' <> "╮")]
          <> fmap (\l -> frame "│ " <> l <> replicate (width - visibleLength l) ' ' <> frame " │") ls
          <> [frame ("╰" <> replicate (width + 2) '─' <> "╯")]
  where
    width = maximum (0 : fmap visibleLength ls)
    frame = punct st
