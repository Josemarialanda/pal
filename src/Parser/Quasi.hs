{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module      : Parser.Quasi
-- Description : Quasiquoter for embedding PAL programs directly into Haskell code.
--
-- This module defines the @[pal| ... |]@ quasiquoter, which allows PAL programs to be
-- written inline within Haskell source files.
--
-- The quasiquoter parses the PAL DSL syntax into a list of 'PalStmt' values (via
-- 'palProgram'), and then generates Template Haskell code that executes the
-- corresponding PAL actions (such as 'T.defineType', 'T.defineExpr', etc.) in order.
--
-- For example:
--
-- @
-- program :: Members '[PAL] r => Sem r (Either Err Type)
-- program = [pal|
--   type Num
--   type Bool
--
--   expr LitInt : Num
--   expr True   : Bool
--   expr False  : Bool
--
--   rule Add:
--     x : Num
--     y : Num
--   ->
--     Add(x, y) : Num
--
--   infer Add(LitInt, LitInt)
-- |]
-- @
--
-- This is equivalent to writing:
--
-- @
-- program = do
--   defineType (T.TypeDecl "Num" [])
--   defineType (T.TypeDecl "Bool" [])
--   defineExpr (T.ExprDecl "LitInt" (T.TCon "Num" []))
--   defineExpr (T.ExprDecl "True" (T.TCon "Bool" []))
--   defineExpr (T.ExprDecl "False" (T.TCon "Bool" []))
--   defineRule
--     (TypingRule
--        "Add"
--        [(EVar "x", TCon "Num" []), (EVar "y", TCon "Num" [])]
--        (ECon "Add" [EVar "x", EVar "y"], TCon "Num" []))
--   infer (ECon "Add" [ECon "LitInt" [], ECon "LitInt" []])
-- @
--
-- The @[pal| ... |]@ syntax provides a readable, declarative way to define type systems
-- and run type inference directly in code, without manually writing Haskell calls.
module Parser.Quasi where

import Language.Haskell.TH (Exp, Q, Stmt, doE, noBindS)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Parser.Parser (palProgram)
import Parser.Types (PalStmt (..))
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Types as T

-- | The @[pal| ... |]@ quasiquoter for embedding PAL programs directly
--   within Haskell code.
--
-- This quasiquoter supports only expression contexts (via 'quoteExp').
-- Pattern, type, and declaration contexts are not supported.
palQuasiQuoter :: QuasiQuoter
palQuasiQuoter =
  QuasiQuoter
    { quoteExp = parsePAL,
      quotePat = error "pal quasiquoter: patterns not supported",
      quoteType = error "pal quasiquoter: types not supported",
      quoteDec = error "pal quasiquoter: declarations not supported"
    }

-- | Parse PAL source code from a quasiquote into Template Haskell expressions.
--
-- On success, the parser produces a list of 'PalStmt' which is then translated
-- into a Haskell @do@ block of PAL DSL commands using 'genPAL'.
--
-- On failure, it produces a compile-time error with a Megaparsec diagnostic.
parsePAL :: String -> Q Exp
parsePAL src = case parse palProgram "<pal quasiquoter>" src of
  Left err -> fail (errorBundlePretty err)
  Right stmts -> genPAL stmts

-- | Generate a @do@ block that sequentially executes each parsed 'PalStmt'.
--
-- All but the last statement are treated as declarations ('defineType', 'defineExpr',
-- 'defineRule'). The final statement is returned as the overall result, usually an
-- inference action returning @Either Err Type@.
genPAL :: [PalStmt] -> Q Exp
genPAL [] = [|T.infer (T.ECon "Unit" [])|]
genPAL stmts = doE (fmap genStmtInit (init stmts) <> [genStmtLast (last stmts)])

-- | Generate a Template Haskell statement for all but the last 'PalStmt'.
--
-- These statements are executed for their side effects (defining types, expressions,
-- or rules) and do not return a value.
genStmtInit :: PalStmt -> Q Stmt
genStmtInit = \case
  SType td -> noBindS [|T.defineType td|]
  SExpr ed -> noBindS [|T.defineExpr ed|]
  SRule tr -> noBindS [|T.defineRule tr|]
  SInfer e -> noBindS [|T.infer e|]

-- | Generate the final Template Haskell statement, whose result becomes the result
--   of the entire quasiquoted expression.
--
-- This ensures that the resulting expression has the type:
--
-- > Members '[PAL] r => Sem r (Either Err Type)
--
-- so it can be interpreted using a PAL interpreter.
genStmtLast :: PalStmt -> Q Stmt
genStmtLast = \case
  SType td -> noBindS [|T.defineType td >> T.infer (T.ECon "Unit" [])|]
  SExpr ed -> noBindS [|T.defineExpr ed >> T.infer (T.ECon "Unit" [])|]
  SRule tr -> noBindS [|T.defineRule tr >> T.infer (T.ECon "Unit" [])|]
  SInfer e -> noBindS [|T.infer e|]
