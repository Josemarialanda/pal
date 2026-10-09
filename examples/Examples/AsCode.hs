-- |
-- Module      : Examples.AsCode
-- Description : PAL programs written directly as Haskell code.
--
-- The most explicit style: call the 'PAL' effect's smart constructors
-- ('defineType', 'defineExpr', 'defineRule', 'infer') inside 'Sem'.
module Examples.AsCode (examples) where

import Examples.Types (Example, pureExample)
import Polysemy (Member, Sem)
import Types
  ( Err,
    Expr (..),
    ExprDecl (..),
    PAL,
    Premise (..),
    Type (..),
    TypeDecl (..),
    TypingRule (..),
    defineExpr,
    defineRule,
    defineType,
    hyp,
    infer,
    premise,
  )

examples :: [Example]
examples =
  [ pureExample
      "code/arith"
      "Numbers and booleans with a polymorphic If, written as Haskell code"
      arith,
    pureExample
      "code/lambda"
      "Lambdas via a premise with a hypothesis (x : a ⊢ body : b), as Haskell code"
      lambda
  ]

-- | Small constructors to keep the program readable.
num, bool :: Type
num = TCon "Num" []
bool = TCon "Bool" []

con :: String -> [Expr] -> Expr
con = ECon

lit :: String -> Expr
lit n = ECon n []

arith :: (Member PAL r) => Sem r (Either Err Type)
arith = do
  defineType (TypeDecl "Num" [])
  defineType (TypeDecl "Bool" [])

  defineExpr (ExprDecl "Zero" num)
  defineExpr (ExprDecl "One" num)
  defineExpr (ExprDecl "True" bool)
  defineExpr (ExprDecl "False" bool)

  -- Add(x, y) : Num  given  x : Num, y : Num
  defineRule $
    TypingRule
      "Add"
      [premise (EVar "x") num, premise (EVar "y") num]
      (con "Add" [EVar "x", EVar "y"], num)

  -- IsZero(n) : Bool  given  n : Num
  defineRule $
    TypingRule
      "IsZero"
      [premise (EVar "n") num]
      (con "IsZero" [EVar "n"], bool)

  -- If(c, t, e) : a  given  c : Bool, t : a, e : a   ('a' is a type variable)
  defineRule $
    TypingRule
      "If"
      [premise (EVar "c") bool, premise (EVar "t") (TVar "a"), premise (EVar "e") (TVar "a")]
      (con "If" [EVar "c", EVar "t", EVar "e"], TVar "a")

  _ <- infer $ con "Add" [lit "One", con "Add" [lit "One", lit "Zero"]] -- Num
  _ <- infer $ con "IsZero" [con "Add" [lit "One", lit "One"]] -- Bool
  _ <- infer $ con "If" [con "IsZero" [lit "Zero"], lit "One", lit "Zero"] -- Num
  _ <- infer $ con "If" [lit "True", lit "One", lit "False"] -- ✗ branches differ
  _ <- infer $ con "If" [lit "One", lit "One", lit "Zero"] -- ✗ condition not Bool
  infer $ con "Mul" [lit "One", lit "One"] -- ✗ unknown expression

lambda :: (Member PAL r) => Sem r (Either Err Type)
lambda = do
  defineType (TypeDecl "Bool" [])
  defineType (TypeDecl "Arrow" ["a", "b"])
  defineExpr (ExprDecl "True" bool)

  -- Lam(x, body) : Arrow<a, b>  given  x : a ⊢ body : b
  defineRule $
    TypingRule
      "Lam"
      [Premise [hyp (EVar "x") (TVar "a")] (EVar "body", TVar "b")]
      (con "Lam" [EVar "x", EVar "body"], arrow (TVar "a") (TVar "b"))

  -- App(f, x) : b  given  f : Arrow<a, b>, x : a
  defineRule $
    TypingRule
      "App"
      [premise (EVar "f") (arrow (TVar "a") (TVar "b")), premise (EVar "x") (TVar "a")]
      (con "App" [EVar "f", EVar "x"], TVar "b")

  _ <- infer $ con "Lam" [EVar "x", EVar "x"] -- Arrow<a, a>
  _ <- infer $ con "Lam" [EVar "x", lit "True"] -- Arrow<a, Bool>
  _ <- infer $ con "Lam" [EVar "x", con "Lam" [EVar "y", EVar "x"]] -- Arrow<a, Arrow<b, a>>
  _ <- infer $ con "App" [con "Lam" [EVar "x", EVar "x"], lit "True"] -- Bool
  infer $ con "App" [lit "True", lit "True"] -- ✗ True is not a function
  where
    arrow a b = TCon "Arrow" [a, b]
