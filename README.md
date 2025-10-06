# PAL — Typechecker Sandbox

**PAL** is a *typechecker sandbox* and experimental framework for defining, exploring,
and running type systems declaratively.

It lets you design and test new type systems like *STLC*, *MiniHaskell*, or *Hindley–Milner*
as embedded DSLs — directly in Haskell.

---

## Overview

PAL programs can be expressed in **three equivalent forms**:

1. **As direct code** — (`mainAsCode`)
2. **As data** — via lists of `PalAction` values (`mainAsData`)
3. **As syntax** — via a quasiquoter (`mainAsDSL`), using Template Haskell parsing

Each mode ultimately drives the same interpreter backend (`runInterpreterStdout`)
and demonstrates how PAL unifies:

* **Types as data**
* **Typing rules as logic**
* **Inference as an effectful computation**

---

## Goal

PAL’s goal is to provide a “**playground for type rules**” —
you can declaratively define types, expressions, and inference rules,
then immediately test or visualize them.

### Example (Quasiquote Syntax)

```haskell
[palQuasiQuoter|
  type Num
  type Bool

  expr LitInt : Num
  expr True   : Bool
  expr False  : Bool

  rule Add:
    x : Num
    y : Num
  ->
    Add(x, y) : Num

  infer Add(True, LitInt)
  infer Add(LitInt)
  infer Add(LitInt, LitInt)
|]
```

Running this with `runInterpreterStdout` produces trace output showing
type inference steps and results for each expression.

---

## Interpreters

Each PAL interpreter implements a different “view” of evaluation:

| Interpreter      | Description                                     |
| ---------------- | ----------------------------------------------- |
| **Core**         | Pure and minimal, no I/O or tracing             |
| **Debug**        | Uses `Polysemy.Trace` for human-readable logs   |
| **IO (planned)** | Will allow runtime interaction via stdin/stdout |

---

## Implementation Notes

The PAL system is effect-based (via [`Polysemy`](https://hackage.haskell.org/package/polysemy)),
using a small effect algebra:

```haskell
data PAL m a where
  DefineType :: TypeDecl -> PAL m ()
  DefineExpr :: ExprDecl -> PAL m ()
  DefineRule :: TypingRule -> PAL m ()
  Infer      :: Expr -> PAL m (Either Err Type)
```

Each backend provides its own handler (`interpreter`) for executing these effects.

---

## Usage Examples

### 1. Running PAL as **Code**

Direct Haskell representation of a PAL program using its effect constructors.
This is the most explicit way to write PAL programs — you invoke the primitives
(`defineType`, `defineExpr`, `defineRule`, `infer`) directly within the `Sem` monad.

```haskell
mainAsCode :: IO ()
mainAsCode = either print print =<< Debug.runInterpreterStdout (mempty @Ctx) program
  where
    program :: (Members '[PAL] r) => Sem r (Either Err Type)
    program = do
      defineType $ TypeDecl "Num" []
      defineType (TypeDecl "Bool" [])

      defineExpr $ ExprDecl "LitInt" (TCon "Num" [])
      defineExpr $ ExprDecl "True" (TCon "Bool" [])
      defineExpr $ ExprDecl "False" (TCon "Bool" [])

      defineRule $
        TypingRule
          { typingRule'name = "Add",
            typingRule'premises =
              [ (EVar "x", TCon "Num" []),
                (EVar "y", TCon "Num" [])
              ],
            typingRule'ruleConclusion =
              (ECon "Add" [EVar "x", EVar "y"], TCon "Num" [])
          }

      _ <- infer $ ECon "Add" [ECon "True" [], ECon "LitInt" []] -- Type error
      _ <- infer $ ECon "Add" [ECon "LitInt" []]                 -- Arity mismatch
      infer $ ECon "Add" [ECon "LitInt" [], ECon "LitInt" []]    -- OK
```

---

### 2. Running PAL as **Data**

Run a PAL program represented purely as a list of `PalAction` values.
This model allows serializing or generating programs from external inputs,
like JSON or a UI editor.

```haskell
mainAsData :: IO ()
mainAsData = either print print =<< Debug.runInterpreterStdout (mempty @Ctx) (pal program)
  where
    pal = flip foldM (Right (TCon "Unit" [])) $ \acc -> \case
      ADefineType td -> defineType td >> pure acc
      ADefineExpr ed -> defineExpr ed >> pure acc
      ADefineRule tr -> defineRule tr >> pure acc
      AInfer e       -> infer e

    program =
      [ ADefineType (TypeDecl "Num" []),
        ADefineType (TypeDecl "Bool" []),
        ADefineExpr (ExprDecl "LitInt" (TCon "Num" [])),
        ADefineExpr (ExprDecl "True" (TCon "Bool" [])),
        ADefineExpr (ExprDecl "False" (TCon "Bool" [])),
        ADefineRule $
          TypingRule
            { typingRule'name = "Add",
              typingRule'premises =
                [ (EVar "x", TCon "Num" []),
                  (EVar "y", TCon "Num" [])
                ],
              typingRule'ruleConclusion =
                (ECon "Add" [EVar "x", EVar "y"], TCon "Num" [])
            },
        AInfer (ECon "Add" [ECon "True" [], ECon "LitInt" []]),
        AInfer (ECon "Add" [ECon "LitInt" []]),
        AInfer (ECon "Add" [ECon "LitInt" [], ECon "LitInt" []])
      ]
```

#### Data Type

```haskell
data PalAction
  = ADefineType TypeDecl   -- Add a new base type.
  | ADefineExpr ExprDecl   -- Declare a new expression and its type.
  | ADefineRule TypingRule -- Introduce a new typing rule.
  | AInfer Expr            -- Run inference on a given expression.
  deriving (Show)
```

---

### 3. Running PAL as **DSL** (via Quasiquotes)

A more natural syntax for PAL programs, enabled by the `palQuasiQuoter`.
This form feels like writing a standalone mini-language but compiles
down to the same `Sem` actions under the hood.

```haskell
mainAsDSL :: IO ()
mainAsDSL = either print print =<< Debug.runInterpreterStdout mempty program
  where
    program :: (Members '[PAL] r) => Sem r (Either Err Type)
    program =
      [palQuasiQuoter|
        type Num
        type Bool

        expr LitInt : Num
        expr True   : Bool
        expr False  : Bool

        rule Add:
          x : Num
          y : Num
        ->
          Add(x, y) : Num

        infer Add(True, LitInt)
        infer Add(LitInt)
        infer Add(LitInt, LitInt)
      |]
```

---

## Example Output

When running any of the three versions of the program —
**Code**, **Data**, or **DSL (Quasiquote)** — 
you will get identical results.

Below is the full trace of what the debug interpreter prints:

(The **Core interpreter**, in contrast, would only return the final inferred type (`Num`))

```
[PAL] Defining type: type Num
=== Context ===
Types:
  - type Num

Expressions:

Rules:

Env:


[PAL] Defining type: type Bool
=== Context ===
Types:
  - type Bool
  - type Num

Expressions:

Rules:

Env:


[PAL] Defining expression: LitInt : Num
=== Context ===
Types:
  - type Bool
  - type Num

Expressions:
  - LitInt : Num

Rules:

Env:


[PAL] Defining expression: True : Bool
=== Context ===
Types:
  - type Bool
  - type Num

Expressions:
  - True : Bool
  - LitInt : Num

Rules:

Env:


[PAL] Defining expression: False : Bool
=== Context ===
Types:
  - type Bool
  - type Num

Expressions:
  - False : Bool
  - True : Bool
  - LitInt : Num

Rules:

Env:


[PAL] Defining rule: Add:
  x : Num
  y : Num
—
  Add(x, y) : Num

=== Context ===
Types:
  - type Bool
  - type Num

Expressions:
  - False : Bool
  - True : Bool
  - LitInt : Num

Rules:
  - Add:
    x : Num
    y : Num
  —
    Add(x, y) : Num

Env:


[PAL] Inferring type for expression: Add(True, LitInt)
[PAL] ✗ Add(True, LitInt) -> [Error] Type mismatch: expected Num, got Bool
=== Context ===
Types:
  - type Bool
  - type Num

Expressions:
  - False : Bool
  - True : Bool
  - LitInt : Num

Rules:
  - Add:
    x : Num
    y : Num
  —
    Add(x, y) : Num

Env:


[PAL] Inferring type for expression: Add(LitInt)
[PAL] ✗ Add(LitInt) -> [Error] Arity mismatch: expected 2 arg(s), got 1
=== Context ===
Types:
  - type Bool
  - type Num

Expressions:
  - False : Bool
  - True : Bool
  - LitInt : Num

Rules:
  - Add:
    x : Num
    y : Num
  —
    Add(x, y) : Num

Env:


[PAL] Inferring type for expression: Add(LitInt, LitInt)
[PAL] ✓ Add(LitInt, LitInt) :: Num
=== Context ===
Types:
  - type Bool
  - type Num

Expressions:
  - False : Bool
  - True : Bool
  - LitInt : Num

Rules:
  - Add:
    x : Num
    y : Num
  —
    Add(x, y) : Num

Env:


Num
```

### 🪶 Notes

* The **Debug interpreter** (`runInterpreterStdout`) prints both the action (`DefineType`, `Infer`, etc.)
  and the resulting **context state** after each step.
* The **Core interpreter** (pure version) performs the same inference logic,
  but returns only the final result (e.g., `Right (TCon "Num" [])`).
* The `Env` section remains empty for now — it’s reserved for future goodies =)