{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Examples.AsDSL
-- Description : PAL programs written in PAL syntax via the quasiquoter.
--
-- The source is parsed at compile time, so syntax errors are compile errors.
module Examples.AsDSL (examples) where

import Examples.Types (Example, pureExample)
import Parser.Quasi (palQuasiQuoter)
import Polysemy (Member, Sem)
import Types (Err, PAL, Type)

examples :: [Example]
examples =
  [ pureExample
      "dsl/logic"
      "Boolean connectives written in PAL syntax"
      logic,
    pureExample
      "dsl/maybe"
      "An optional type with a type parameter: Just, FromMaybe"
      maybeEx
  ]

logic :: (Member PAL r) => Sem r (Either Err Type)
logic =
  [palQuasiQuoter|
    type Bool
    type Num

    expr True   : Bool
    expr False  : Bool
    expr LitInt : Num

    rule Not:
      x : Bool
    ->
      Not(x) : Bool

    rule And:
      x : Bool
      y : Bool
    ->
      And(x, y) : Bool

    rule Or:
      x : Bool
      y : Bool
    ->
      Or(x, y) : Bool

    infer And(True, Not(False))           -- Bool
    infer Or(And(True, False), Not(True)) -- Bool
    infer Not(True, False)                -- ✗ arity mismatch
    infer Or(True, LitInt)                -- ✗ Num is not Bool
  |]

maybeEx :: (Member PAL r) => Sem r (Either Err Type)
maybeEx =
  [palQuasiQuoter|
    type Num
    type Bool
    type Maybe<a>

    expr LitInt  : Num
    expr True    : Bool

    -- A polymorphic constant: each use gets its own fresh 'a'.
    expr Nothing : Maybe<a>

    -- Wrap a value of any type.
    rule Just:
      x : a
    ->
      Just(x) : Maybe<a>

    -- Unwrap with a default; the default and the contents must agree.
    rule FromMaybe:
      d : a
      m : Maybe<a>
    ->
      FromMaybe(d, m) : a

    infer Just(LitInt)                    -- Maybe<Num>
    infer Just(Just(True))                -- Maybe<Maybe<Bool>>
    infer FromMaybe(LitInt, Just(LitInt)) -- Num
    infer Nothing                         -- Maybe<a>
    infer FromMaybe(True, Nothing)        -- Bool
    infer FromMaybe(LitInt, Just(True))   -- ✗ default is Num, contents Bool
  |]
