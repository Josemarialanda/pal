-- |
-- Module      : Report
-- Description : Reporting outcomes: results, failures in context, derivation trees
--
-- Renders the 'Outcome' of each statement for the terminal (built on the
-- styling in "Pretty"), plus derivation trees as text or LaTeX.
--
-- With the 'plain' style, the first line of every result is the one-line
-- format used throughout the docs (@✓ e :: T@, @✗ e -> [Error] …@); any
-- details follow on indented lines.
module Report
  ( -- * Options
    Report (..),
    Source (..),
    defaultReport,

    -- * Outcomes
    reportStep,
    reportOutcome,
    reportDiagnostics,
    reportFailure,

    -- * Derivations
    prettyDerivation,
    derivationLatex,
    derivationLatexDocument,

    -- * Locations
    lineColumn,
  )
where

import Data.Char (isAlphaNum)
import Data.List (dropWhileEnd, intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Parser.Types (Located (..), Span (..), spanAt)
import Pretty
  ( Attr (..),
    Style (..),
    failure,
    note,
    paint,
    plain,
    prettyDefinedExpr,
    prettyDefinedRule,
    prettyDefinedType,
    prettyErr,
    prettyExpr,
    prettyJudgment,
    prettyPremise,
    prettyResult,
    prettyType,
    visibleLength,
  )
import Program (Outcome (..), PalAction (..), Step (..))
import Types
  ( Derivation (..),
    Diagnostic (..),
    Expectation (..),
    Expr (..),
    Failure (..),
    Frame (..),
    Justification (..),
    Scheme (..),
    Severity (..),
    Type (..),
    TypeDecl (..),
    TypingRule (..),
  )

--------------------------------------------------------------------------------

-- | Options

--------------------------------------------------------------------------------

-- | The program text that statements came from, to point into it.
data Source = Source
  { -- | Shown in locations, e.g. a file path or @input@.
    source'name :: String,
    source'text :: String
  }

-- | How to report outcomes.
data Report = Report
  { report'style :: Style,
    -- | Where statements came from, for locations and source excerpts.
    report'source :: Maybe Source,
    -- | Confirm successful definitions (@defined rule Not@), as the REPL does.
    report'echoDefinitions :: Bool,
    -- | Draw the derivation tree under each successful result.
    report'derivations :: Bool,
    -- | Width available for derivation trees; wider ones become outlines.
    report'width :: Int
  }

-- | Plain output, no source, no echo, no derivations, 100 columns.
defaultReport :: Report
defaultReport = Report plain Nothing False False 100

--------------------------------------------------------------------------------

-- | Outcomes

--------------------------------------------------------------------------------

-- | Report one step: the statement's outcome, located in the source.
reportStep :: Report -> Step -> [String]
reportStep r (Step located outcome) = reportOutcome r (Just located) (loc'value located) outcome

-- | Report an outcome. The located statement, when known, gives source
--   locations for errors.
reportOutcome :: Report -> Maybe (Located PalAction) -> PalAction -> Outcome -> [String]
reportOutcome r located action = \case
  Defined diagnostics ->
    [definedLine | report'echoDefinitions r] <> reportDiagnostics st diagnostics
  Rejected diagnostics ->
    rejectedLine diagnostics <> fmap ("    " <>) (statementLocation r located)
  Inferred result -> case (action, result) of
    (AInfer e, Right d) -> prettyResult st e (Right (deriv'type d)) : derivation d
    (AInfer e, Left failures) -> failureLines e failures
    _ -> []
  Expected x diagnostics result met -> expectation x diagnostics result met
  where
    st = report'style r

    definedLine = case action of
      ADefineType td -> prettyDefinedType st td
      ADefineExpr ed -> prettyDefinedExpr st ed
      ADefineRule tr -> prettyDefinedRule st tr
      _ -> ""

    rejectedLine diagnostics =
      let errors = [msg | Diagnostic SevError msg <- diagnostics]
          prefix = mark False <> " " <> what <> " " <> failure st "rejected"
       in case errors of
            [msg] -> [prefix <> ": " <> msg]
            msgs -> (prefix <> ":") : fmap ("    • " <>) msgs

    what = case action of
      ADefineType (TypeDecl n _) -> keyword "type" <> " " <> n
      ADefineExpr ed -> keyword "expr" <> " " <> takeWhile (/= ' ') (show ed)
      ADefineRule tr -> keyword "rule" <> " " <> typingRule'name tr
      _ -> ""

    keyword = paint st [Bold, Blue]

    -- The first line keeps the one-line format; details follow.
    failureLines e (f :| rest) =
      prettyResult st e (Left (failure'err f))
        : reportFailure r located f
          <> concatMap (\g -> ("    and: " <> errLine (failure'err g)) : fmap ("  " <>) (reportFailure r located g)) rest

    expectation x diagnostics result met = case (x, result) of
      (ExpectType e t, _)
        | not (null [() | Diagnostic SevError _ <- diagnostics]) ->
            (mark False <> " " <> checkText e t <> dash <> intercalate "; " [m | Diagnostic SevError m <- diagnostics]) : located'
      (ExpectType e t, Right d)
        | met -> mark True <> " " <> checkText e t : derivation d
        | otherwise -> (mark False <> " " <> checkText e t <> dash <> "inferred " <> prettyType st (deriv'type d)) : located'
      (ExpectType e t, Left (f :| rest)) ->
        (mark False <> " " <> checkText e t <> dash <> prettyErr st (failure'err f))
          : reportFailure r located f
            <> concatMap (\g -> ("    and: " <> errLine (failure'err g)) : fmap ("  " <>) (reportFailure r located g)) rest
      (ExpectFailure e, Left (f :| _)) ->
        [mark True <> " " <> failsText e <> note st ("  (" <> errText (failure'err f) <> ")")]
      (ExpectFailure e, Right d) ->
        (mark False <> " " <> failsText e <> dash <> "but " <> prettyExpr st e <> punct " :: " <> prettyType st (deriv'type d)) : located'

    checkText e t = keyword "check" <> " " <> prettyExpr st e <> punct " : " <> prettyType st t
    failsText e = keyword "fails" <> " " <> prettyExpr st e
    errText err = let s = show err in fromMaybe s (dropPrefix "[Error] " s)
    -- An error on its own line: without the "[Error] " prefix when plain.
    errLine err = if styleEnabled st then prettyErr st err else errText err
    dash = punct " — "
    -- Where an unmet expectation is, when there is no subterm to point at.
    located' = fmap ("    " <>) (statementLocation r located)
    punct = paint st [Dim]
    mark ok
      | ok = paint st [Bold, Green] "✓"
      | otherwise = paint st [Bold, Red] "✗"

    derivation d
      | report'derivations r = fmap ("    " <>) (prettyDerivation st (report'width r - 4) d)
      | otherwise = []

-- | Diagnostics on their own lines: @⚠ …@ for warnings, @✗ …@ for errors.
reportDiagnostics :: Style -> [Diagnostic] -> [String]
reportDiagnostics st = fmap $ \case
  Diagnostic SevWarning msg -> paint st [Bold, Yellow] "⚠" <> " " <> msg
  Diagnostic SevError msg -> paint st [Bold, Red] "✗" <> " " <> msg

-- | The details of a failure, indented: where (with a source excerpt, when
--   the source is known) and the premises being checked, innermost first.
reportFailure :: Report -> Maybe (Located PalAction) -> Failure -> [String]
reportFailure r located (Failure _ path frames) =
  fmap ("    " <>) (location <> context)
  where
    st = report'style r
    location = case (report'source r, located >>= loc'exprSpans) of
      (Just src, Just spans) -> excerpt st src (spanAt path spans)
      _ -> []
    context = fmap frameLine (take 4 frames) <> ["…" | length frames > 4]
    frameLine (Frame rule premise subject) =
      note st "in rule "
        <> paint st [Italic, Yellow] rule
        <> note st ", premise "
        <> prettyPremise st premise
        <> note st "  (checking "
        <> prettyExpr st subject
        <> note st ")"

-- | Where a statement starts, for definitions (which have no expression).
statementLocation :: Report -> Maybe (Located PalAction) -> [String]
statementLocation r located = case (report'source r, located) of
  (Just src, Just l) ->
    let (line, col) = lineColumn (source'text src) (loc'start l)
     in [note (report'style r) ("at " <> source'name src <> ":" <> show line <> ":" <> show col)]
  _ -> []

-- | A megaparsec-style excerpt: the location, the source line, and carets
--   under the span.
excerpt :: Style -> Source -> Span -> [String]
excerpt st (Source name text) (Span start end) =
  [ note st ("at " <> name <> ":" <> show line <> ":" <> show col),
    gutter0,
    gutter (show line) <> sourceLine,
    gutter0 <> " " <> replicate (col - 1) ' ' <> paint st [Bold, Red] (replicate width '^')
  ]
  where
    (line, col) = lineColumn text start
    sourceLine = takeWhile (/= '\n') (drop (start - col + 1) text)
    width = max 1 (min (end - start) (length sourceLine - col + 1))
    numberWidth = length (show line)
    gutter n = paint st [Dim] (replicate (numberWidth - length n) ' ' <> n <> " | ")
    gutter0 = paint st [Dim] (replicate numberWidth ' ' <> " |")

-- | 1-based line and column of an offset.
lineColumn :: String -> Int -> (Int, Int)
lineColumn text offset =
  let before = take offset text
   in (1 + length (filter (== '\n') before), 1 + length (takeWhile (/= '\n') (reverse before)))

dropPrefix :: String -> String -> Maybe String
dropPrefix p s = if take (length p) s == p then Just (drop (length p) s) else Nothing

--------------------------------------------------------------------------------

-- | Derivation trees

--------------------------------------------------------------------------------

-- | A derivation drawn as stacked inference rules: each rule's premises side
--   by side above a bar, labelled with the rule's name, and its conclusion
--   below. Assumptions (variables in scope) are leaves without a bar.
--
--   If the tree is wider than the given width, it is drawn as an indented
--   outline instead.
prettyDerivation :: Style -> Int -> Derivation -> [String]
prettyDerivation st maxWidth d
  | blockWidth tree <= maxWidth = fmap (dropWhileEnd (== ' ')) tree
  | otherwise = outline st d
  where
    tree = treeBlock st d

-- | The judgment at a node, with the hypotheses it is made under.
nodeJudgment :: Style -> Derivation -> String
nodeJudgment st (Derivation e t _ assumptions _) =
  hypotheses <> prettyJudgment st (e, t)
  where
    hypotheses
      | null assumptions = ""
      | otherwise = intercalate (paint st [Dim] ", ") (fmap assumption assumptions) <> paint st [Magenta] " ⊢ "
    assumption (v, scheme) = prettyExpr st (EVar v) <> paint st [Dim] " : " <> prettyScheme st scheme

prettyScheme :: Style -> Scheme -> String
prettyScheme st (Forall [] t) = prettyType st t
prettyScheme st (Forall vs t) = "∀" <> unwords (fmap (paint st [Italic, Magenta]) vs) <> ". " <> prettyType st t

ruleLabel :: Justification -> Maybe String
ruleLabel = \case
  ByRule name -> Just name
  ByDeclaration -> Just "expr"
  ByAssumption -> Nothing

treeBlock :: Style -> Derivation -> [String]
treeBlock st d@(Derivation _ _ by _ premises) = case ruleLabel by of
  -- An assumption: just the judgment.
  Nothing -> [conclusion]
  Just label ->
    let above = sideBySide (fmap (treeBlock st) premises)
        width = maximum (2 : visibleLength conclusion + 2 : fmap visibleLength above)
        bar = paint st [Dim] (replicate width '─') <> " " <> paint st [Italic, Yellow] label
     in fmap (center width) above <> [bar, center width conclusion]
  where
    conclusion = nodeJudgment st d
    center w s = replicate ((w - visibleLength s) `div` 2) ' ' <> s

-- | Place blocks side by side, bottom-aligned, four spaces apart.
sideBySide :: [[String]] -> [String]
sideBySide [] = []
sideBySide blocks = foldr1 (zipWith (\a b -> a <> "    " <> b)) (fmap pad blocks)
  where
    height = maximum (fmap length blocks)
    pad block =
      let w = blockWidth block
       in replicate (height - length block) (replicate w ' ') <> fmap (\l -> l <> replicate (w - visibleLength l) ' ') block

blockWidth :: [String] -> Int
blockWidth = maximum . (0 :) . fmap visibleLength

-- | The derivation as an indented outline, for trees too wide to draw.
outline :: Style -> Derivation -> [String]
outline st = go "" ""
  where
    go first rest d@(Derivation _ _ by _ premises) =
      (first <> nodeJudgment st d <> justification by)
        : concat (zipWith child [1 :: Int ..] premises)
      where
        child i p
          | i == length premises = go (rest <> "└─ ") (rest <> "   ") p
          | otherwise = go (rest <> "├─ ") (rest <> "│  ") p
    justification = \case
      ByRule name -> paint st [Dim] "   by " <> paint st [Italic, Yellow] name
      ByDeclaration -> paint st [Dim] "   by declaration"
      ByAssumption -> paint st [Dim] "   by assumption"

--------------------------------------------------------------------------------

-- | LaTeX

--------------------------------------------------------------------------------

-- | A derivation as nested @mathpartir@ @\\inferrule*@ commands.
derivationLatex :: Derivation -> String
derivationLatex d@(Derivation _ _ by _ premises) = case ruleLabel by of
  Nothing -> judgmentLatex d
  Just label ->
    "\\inferrule*[right="
      <> escape label
      <> "]{"
      <> intercalate " \\\\ " (fmap derivationLatex premises)
      <> "}{"
      <> judgmentLatex d
      <> "}"

-- | A complete, compilable LaTeX document showing the derivation.
derivationLatexDocument :: Derivation -> String
derivationLatexDocument d =
  unlines
    [ "\\documentclass{article}",
      "\\usepackage{mathpartir}",
      "% Use the full page width: mathpartir puts premises side by side only",
      "% while they fit, and wraps them onto separate lines otherwise.",
      "\\setlength{\\textwidth}{\\paperwidth}",
      "\\addtolength{\\textwidth}{-2cm}",
      "\\setlength{\\oddsidemargin}{-1in}",
      "\\addtolength{\\oddsidemargin}{1cm}",
      "\\pagestyle{empty}",
      "\\begin{document}",
      "\\begin{mathpar}",
      derivationLatex d,
      "\\end{mathpar}",
      "\\end{document}"
    ]

judgmentLatex :: Derivation -> String
judgmentLatex (Derivation e t _ assumptions _) =
  hypotheses <> exprLatex e <> " : " <> typeLatex t
  where
    hypotheses
      | null assumptions = ""
      | otherwise = intercalate ", " [ident v <> " : " <> schemeLatex s | (v, s) <- assumptions] <> " \\vdash "
    schemeLatex (Forall [] st) = typeLatex st
    schemeLatex (Forall vs st) = "\\forall " <> unwords (fmap ident vs) <> ".\\, " <> typeLatex st

exprLatex :: Expr -> String
exprLatex = \case
  EVar v -> ident v
  ECon n [] -> latexName n
  ECon n es -> latexName n <> "(" <> intercalate ", " (fmap exprLatex es) <> ")"

typeLatex :: Type -> String
typeLatex = \case
  TVar v -> ident v
  TCon n [] -> latexName n
  TCon n ts -> latexName n <> "\\langle " <> intercalate ", " (fmap typeLatex ts) <> "\\rangle"

-- | A variable: single letters stay italic, longer names are upright.
ident :: String -> String
ident [c] = [c]
ident v = "\\mathit{" <> escape v <> "}"

-- | A constructor or rule name, upright.
latexName :: String -> String
latexName n = "\\mathsf{" <> escape n <> "}"

escape :: String -> String
escape = concatMap $ \c -> if isAlphaNum c then [c] else if c == '_' then "\\_" else [c]
