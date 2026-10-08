-- |
-- Module      : Examples.AsData
-- Description : PAL programs built as plain data ('PalAction' lists).
--
-- Because a program is just a list, it can be generated, transformed or
-- loaded from elsewhere before being run with 'runPalActions'.
module Examples.AsData (examples) where

import Examples.Types (Example, pureExample)
import PAL (PalAction (..), runPalActions)
import Types
  ( Expr (..),
    ExprDecl (..),
    Type (..),
    TypeDecl (..),
    TypingRule (..),
    premise,
  )

examples :: [Example]
examples =
  [ pureExample
      "data/pairs"
      "Polymorphic pairs (Pair, Fst, Snd) as a list of PalAction values"
      (runPalActions pairs),
    pureExample
      "data/generated"
      "A program generated in Haskell: Add chains of increasing depth"
      (runPalActions generated)
  ]

num, bool :: Type
num = TCon "Num" []
bool = TCon "Bool" []

lit :: String -> Expr
lit n = ECon n []

-- | Base declarations shared by both examples.
prelude :: [PalAction]
prelude =
  [ ADefineType (TypeDecl "Num" []),
    ADefineType (TypeDecl "Bool" []),
    ADefineExpr (ExprDecl "LitInt" num),
    ADefineExpr (ExprDecl "True" bool)
  ]

pairs :: [PalAction]
pairs =
  prelude
    <> [ ADefineType (TypeDecl "Pair" ["a", "b"]),
         ADefineRule $
           TypingRule
             "Pair"
             [premise (EVar "x") (TVar "a"), premise (EVar "y") (TVar "b")]
             (ECon "Pair" [EVar "x", EVar "y"], pairOf (TVar "a") (TVar "b")),
         ADefineRule $
           TypingRule
             "Fst"
             [premise (EVar "p") (pairOf (TVar "a") (TVar "b"))]
             (ECon "Fst" [EVar "p"], TVar "a"),
         ADefineRule $
           TypingRule
             "Snd"
             [premise (EVar "p") (pairOf (TVar "a") (TVar "b"))]
             (ECon "Snd" [EVar "p"], TVar "b"),
         AInfer (ECon "Pair" [lit "LitInt", lit "True"]), -- Pair<Num, Bool>
         AInfer (ECon "Fst" [ECon "Pair" [lit "LitInt", lit "True"]]), -- Num
         AInfer (ECon "Snd" [ECon "Pair" [lit "LitInt", lit "True"]]), -- Bool
         AInfer (ECon "Snd" [ECon "Pair" [lit "True", ECon "Pair" [lit "LitInt", lit "LitInt"]]]), -- Pair<Num, Num>
         AInfer (ECon "Fst" [lit "LitInt"]) -- ✗ not a pair
       ]
  where
    pairOf a b = TCon "Pair" [a, b]

-- | Rules are data too: define Add once, then generate inference queries.
generated :: [PalAction]
generated =
  prelude
    <> [ ADefineRule $
           TypingRule
             "Add"
             [premise (EVar "x") num, premise (EVar "y") num]
             (ECon "Add" [EVar "x", EVar "y"], num)
       ]
    <> fmap (AInfer . addChain) [1, 2, 3]
    <> [AInfer (ECon "Add" [addChain 2, lit "True"])] -- ✗ Bool deep inside
  where
    -- addChain n = Add(LitInt, Add(LitInt, ... LitInt))
    addChain :: Int -> Expr
    addChain 0 = lit "LitInt"
    addChain n = ECon "Add" [lit "LitInt", addChain (n - 1)]
