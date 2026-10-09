# PAL — Typechecker Sandbox

**PAL** is a *typechecker sandbox* and experimental framework for defining, exploring,
and running type systems declaratively.

It lets you design and test new type systems like *STLC*, *MiniHaskell*, or *Hindley–Milner*
as embedded DSLs — directly in Haskell.

> 📘 **New to PAL or to type systems?** Start with the [**Tutorial**](TUTORIAL.md):
> it walks through every example step by step, with diagrams, and explains the
> theory along the way (inference rules, unification, typing contexts).

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

| Interpreter | Module                | Description                                                        |
| ----------- | --------------------- | ------------------------------------------------------------------ |
| **Core**    | `Interpreters.Core`   | Pure and minimal, no I/O or tracing                                |
| **Debug**   | `Interpreters.Debug`  | Uses `Polysemy.Trace` for human-readable logs, with context dumps  |
| **IO**      | `Interpreters.IO`     | Runs in `IO` and prints one line per result. Powers the `pal` command and its REPL |

---

## The `pal` Command (IO Interpreter)

The `pal` executable runs PAL source directly. You don't need to write any
Haskell.

The quickest way in is the `pal-repl` command, provided by the dev shell
(`nix develop`, or automatically with direnv). It builds `pal` if needed, then
starts the REPL:

```sh
pal-repl                               # empty REPL
pal-repl examples/programs/stlc.pal    # load files first, then REPL
```

`pal-repl` runs from your current directory, so relative file paths work
from anywhere. To run files without a REPL, or to pass other options, call
`pal` through cabal:

```sh
cabal run -v0 pal -- FILE.pal        # run a file
cabal run -v0 pal                    # start the interactive REPL
cabal run -v0 pal -- -i FILE.pal     # run a file, then open a REPL with its context
cabal run -v0 pal -- --no-color …    # same, without colours
```

Use `cabal install exe:pal` to put `pal` on your `PATH`. The rest of this
section uses the plain `pal` name.

### Running files

```sh
$ pal examples/programs/stlc.pal
✓ App(Not, True) :: Bool
✓ Lam(x, x) :: Arrow<a, a>
…
✗ Lam(x, App(x, x)) -> [Error] Infinite type: a ~ Arrow<a, b>
```

* Each `infer` prints one line: `✓ expr :: type` or `✗ expr -> error`.
* Several files run **in order in one shared context**, so you can split a
  language over files: `pal prelude.pal program.pal`.
* The exit code is **1** if a file fails to parse or any inference fails, and
  0 otherwise. This makes `pal` usable as a checker in scripts and CI.

### The REPL

```text
$ pal
╭──────────────────────────────────────────────────╮
│ λ PAL  ·  a typechecker sandbox                  │
│   :help for commands  ·  :quit or Ctrl-D to exit │
╰──────────────────────────────────────────────────╯
pal❯ type Bool
defined type Bool
pal❯ expr True : Bool
defined expr True : Bool
pal❯ rule Not:
   ┆   x : Bool
   ┆ ->
   ┆   Not(x) : Bool
defined rule Not
       x : Bool
    ─────────────── Not
     Not(x) : Bool
pal❯ Not(Not(True))
✓ Not(Not(True)) :: Bool
pal❯ Not(Zero)
✗ Not(Zero)
  ╰─ unknown expression Zero
pal❯ :quit
```

* **Colours and highlighting.** In a terminal, output is syntax-highlighted
  with ANSI colours and text styles: keywords in bold blue, type constructors
  in cyan, type variables in italic magenta, constructors in bold, variables
  in italic, punctuation dimmed, `✓` in green and `✗` in red. Each rule you
  define is drawn as an **inference rule**, with its premises above a bar and
  its conclusion below. Errors get their own line. Colours are turned off
  when output isn't a terminal, when the
  [`NO_COLOR`](https://no-color.org) environment variable is set, when
  `TERM=dumb`, or with `pal --no-color`. Output is then the plain one-line
  format shown elsewhere in this README.
* The context persists for the whole session. Every statement builds on the
  ones before it.
* **Multi-line input:** if the input isn't finished yet (a rule without its
  conclusion, an unclosed `(`), the prompt changes to `┆` and the input
  continues on the next line. A blank line ends it early and shows the
  syntax error.
* **A bare expression** such as `Not(True)` is shorthand for `infer Not(True)`.
* **Line editing** (via [Haskeline](https://hackage.haskell.org/package/haskeline),
  the same library GHCi uses):

  | Key                | Effect                                              |
  | ------------------ | --------------------------------------------------- |
  | ← / →              | Move the cursor within the line                     |
  | ↑ / ↓              | Browse previous inputs                              |
  | Ctrl-R             | Search history                                      |
  | Tab                | Complete file names (handy for `:load`)             |
  | Ctrl-A / Ctrl-E    | Jump to the start / end of the line                 |
  | Ctrl-C             | Discard the current (possibly multi-line) input     |

  History is saved to `~/.pal_history`, so it carries over between sessions.
* Commands:

  | Command          | Effect                                     |
  | ---------------- | ------------------------------------------ |
  | `:load FILE...`  | Run `.pal` files in the current context    |
  | `:ctx`           | Show the context, rules drawn as inference rules |
  | `:reset`         | Clear the context                          |
  | `:help`          | List the commands                          |
  | `:quit` / Ctrl-D | Exit                                       |

* If stdin is not a terminal, the banner, prompts and history file are left
  out, so you can pipe a session in: `pal < session.txt`.

### Using the IO interpreter from Haskell

```haskell
import Interpreters.IO (defaultIOOptions, runInterpreterIO, runInterpreterIOWithCtx)

main :: IO ()
main = do
  -- Prints "✓ …" / "✗ …" for every infer, then returns the last result.
  _ <- runInterpreterIO defaultIOOptions mempty program

  -- Also returns the final context, so it can be reused by a later program.
  (ctx, _) <- runInterpreterIOWithCtx defaultIOOptions mempty program
  _ <- runInterpreterIO defaultIOOptions ctx anotherProgram
  pure ()
```

`IOOptions { ioEchoDefinitions = True }` also prints a line for every
definition, which is what the REPL uses. `Program.loadPalFile` parses a `.pal`
file into `[PalAction]`, and `Interpreters.IO.runActionsIO` runs such a list
and returns every inference result.

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
              [ premise (EVar "x") (TCon "Num" []),
                premise (EVar "y") (TCon "Num" [])
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
                [ premise (EVar "x") (TCon "Num" []),
                  premise (EVar "y") (TCon "Num" [])
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

Defined in [`src/Program.hs`](src/Program.hs) (and re-exported by `PAL`), along
with `runPalActions` and `loadPalFile`, which parses a `.pal` file into this form.

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

### 4. Type Variables

In a rule, a lowercase type name (`a`, `b`, …) is a **type variable**.
Each time a rule is applied, its variables get fresh names. They are then
solved by **unification** against the types inferred for the premises, and
the solution is substituted into the conclusion type. Unsolved variables are
reported as `a`, `b`, … (e.g. `Lam(x, x) : Arrow<a, a>`). A declared type can
be polymorphic too: with `expr Nothing : Maybe<a>`, every use of `Nothing`
gets its own `a`.

```haskell
[palQuasiQuoter|
  type Bool
  expr True  : Bool
  expr False : Bool

  rule If:
    cond : Bool
    then : a
    else : a
  ->
    If(cond, then, else) : a

  infer If(True, False, True)   -- Bool
|]
```

### 5. Binders (Hypothetical Premises)

A premise can be checked **under hypotheses**, written `hyp, … |- e : T`
(`⊢` works too). The hypotheses bring variables into scope while that
premise is checked, which is how `Lam`, `Let` and other binding forms are
typed:

```haskell
[palQuasiQuoter|
  type Bool
  type Arrow
  expr True : Bool

  rule Lam:
    x : a |- body : b
  ->
    Lam(x, body) : Arrow<a, b>

  rule App:
    f : Arrow<a, b>
    x : a
  ->
    App(f, x) : b

  infer Lam(x, x)                          -- Arrow<a, a>
  infer Lam(x, Lam(y, x))                  -- Arrow<a, Arrow<b, a>>
  infer App(Lam(f, App(f, True)), Lam(x, x)) -- Bool
|]
```

A hypothesis variable (`x` above) must match a variable in the expression
being checked: in `Lam(y, …)`, `y` is bound. Inner binders shadow outer
ones. Bound variables live in `ctx'env` only while their premise is checked,
so the `Env` shown in Debug traces stays empty.

In Haskell, a rule's premises are `Premise` values. `premise e t` builds a
plain judgment, and `Premise [hyp (EVar "x") (TVar "a")] (EVar "body", TVar "b")`
builds one with a hypothesis.

`mainStlc` in `src/PAL.hs` runs a small STLC fragment built this way.

### 6. Let-Polymorphism (`gen`)

By default, a variable bound by a hypothesis has **one monomorphic type**.
That's what a lambda's parameter needs. Mark a hypothesis with `gen` to
**generalise** it instead: the variable gets its type quantified over every
type variable not free in the surrounding context, and each use instantiates
it afresh. This is full Hindley–Milner `let`:

```haskell
[palQuasiQuoter|
  rule Let:
    value : a
    x : gen a |- body : b
  ->
    Let(x, value, body) : b

  -- (with Lam, App and Pair rules as above)
  infer Let(id, Lam(x, x), Pair(App(id, True), App(id, LitInt)))  -- Pair<Bool, Num>
|]
```

* The premise that fixes `a` (`value : a`) must come before the `gen`
  hypothesis, because premises are checked in order.
* Only type variables **not free in the context** are generalised. In
  `Lam(y, Let(z, y, …))`, `z` has `y`'s type, so it stays monomorphic.
* In Haskell, use `genHyp (EVar "x") (TVar "a")` in place of `hyp`.

[`examples/programs/hm.pal`](examples/programs/hm.pal) compares `Let` with a
monomorphic `MonoLet` side by side.

---

## Examples

The [`examples/`](examples/) folder contains runnable programs, one module for
each way of writing a PAL program:

| Group  | Style                                         | Source                                                        |
| ------ | --------------------------------------------- | ------------------------------------------------------------- |
| `code` | Haskell code calling `defineType`, `infer`, … | [`Examples/AsCode.hs`](examples/Examples/AsCode.hs)           |
| `data` | Lists of `PalAction` values (incl. generated) | [`Examples/AsData.hs`](examples/Examples/AsData.hs)           |
| `dsl`  | PAL syntax via `palQuasiQuoter`               | [`Examples/AsDSL.hs`](examples/Examples/AsDSL.hs)             |
| `file` | `.pal` files parsed at runtime                | [`programs/`](examples/programs/), loaded by `Examples/AsFile.hs` |

The `.pal` files double as a small catalogue of type systems:

| File | Type system |
| ---- | ----------- |
| [`arith.pal`](examples/programs/arith.pal) | Typed arithmetic (TAPL ch. 8): booleans, naturals, `If` |
| [`stlc.pal`](examples/programs/stlc.pal) | Simply typed λ-calculus: `Lam`, `App`, `Let`, `If` |
| [`stlc-ext.pal`](examples/programs/stlc-ext.pal) | STLC + unit, products, sums (`Case`) and recursion (`Fix`) |
| [`lists.pal`](examples/programs/lists.pal) | Polymorphic lists: `Nil`, `Cons`, `Head`, `Fold` |
| [`hm.pal`](examples/programs/hm.pal) | Hindley–Milner with let-polymorphism (`x : gen a`) |
| [`logic.pal`](examples/programs/logic.pal) | Propositional logic via Curry–Howard: `infer` a proof, get its theorem |
| [`broken.pal`](examples/programs/broken.pal) | A deliberate syntax error |

Run them with the `run-examples` dev-shell command:

```sh
run-examples                    # every example, inference results only
run-examples dsl                # one group: code | data | dsl | file
run-examples data/pairs         # a single example
run-examples --trace dsl/maybe  # full Debug trace with context dumps
run-examples --list             # list all examples
```

You can also run the `.pal` files directly with the [`pal` command](#the-pal-command-io-interpreter)
(`cabal run -v0 pal -- examples/programs/stlc.pal`), or load them into the
REPL with `pal-repl examples/programs/stlc.pal`.

For a guided, step-by-step explanation of each example, including derivation
trees and unification traces, see [TUTORIAL.md](TUTORIAL.md).

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
* The **IO interpreter** (`runInterpreterIO`, used by the `pal` command) prints just
  one line per inference result, with no context dumps.
* The `Env` section of a trace stays empty. Variables bound by rule hypotheses
  (e.g. a lambda’s parameter) live there only while their premise is checked.
