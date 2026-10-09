# PAL — a typechecker sandbox

Define a type system as a handful of inference rules, then ask PAL to infer types with it.

```haskell
type Bool
expr True : Bool

rule Not:
  x : Bool
->
  Not(x) : Bool

infer Not(Not(True))   -- ✓ Bool
infer Not(True, True)  -- ✗ Arity mismatch: expected 1 arg(s), got 2
```

New to type systems? **[Read the tutorial →](TUTORIAL.md)**

---

## Quick start

```sh
nix develop                              # or let direnv load the shell

pal                                      # interactive REPL
pal FILE.pal                             # typecheck a file
pal -i examples/programs/stlc.pal        # REPL with a file preloaded
pal-ui                                   # web UI, opens in your browser
run-examples                             # run every bundled example
```

The dev shell's `pal` and `pal-ui` rebuild from your working tree first, so they always run your latest code.

`pal` exits with **1** if any `infer` fails, so it works as a checker in scripts and CI.

## The language

| Statement                           | Meaning                                         |
| ----------------------------------- | ----------------------------------------------- |
| `type T`                            | Declare a type name (informational only)        |
| `expr C : T`                        | Axiom: constant `C` has type `T`                |
| `rule Name:` *premises* `->` *conclusion* | Inference rule                            |
| `infer e`                           | Infer the type of `e`                           |

Premises come in four shapes:

```haskell
x : T                    -- plain:       x must have type T
x : A |- body : B        -- hypothesis:  check body with x : A in scope (⊢ also works)
x : A, y : B |- e : C    -- several hypotheses
x : gen A |- body : B    -- generalised: x is polymorphic in body (let-polymorphism)
```

**Naming:** `Uppercase` is a constructor or concrete type (`Add(x, y)`, `Maybe<Num>`). `lowercase` is a variable or type variable (`x`, `a`). Comments start with `--`.

**Errors:**

| Error                    | Cause                                                  |
| ------------------------ | ------------------------------------------------------ |
| `Type mismatch`          | Two different types had to be equal                    |
| `Arity mismatch`         | A rule exists, but for a different number of arguments |
| `Unknown expression`     | No rule, no `expr`, and not a bound variable           |
| `Infinite type`          | A type would contain itself (`a ~ Arrow<a, b>`)        |
| `No typing rule matched` | A constant was given arguments                         |

## The REPL

```text
pal❯ rule Not:
   ┆   x : Bool
   ┆ ->
   ┆   Not(x) : Bool
defined rule Not
       x : Bool
    ─────────────── Not
     Not(x) : Bool
pal❯ Not(True)
✓ Not(True) :: Bool
```

- A bare expression means `infer`.
- Unfinished input continues on a `┆` line. A blank line or Ctrl-C abandons it.
- History, ↑/↓ and Ctrl-R work as in GHCi. History is saved to `~/.pal_history`.

| Command         | Effect                          |
| --------------- | ------------------------------- |
| `:load FILE...` | Run files in the current context |
| `:ctx`          | Show the context                |
| `:reset`        | Clear the context               |
| `:help` `:quit` | Help / exit (also Ctrl-D)       |

`pal` flags: `-i FILE...` runs files and then opens a REPL with their context, and `--no-color` turns off colour (as do `NO_COLOR` and piped output).

## The web UI

`pal-ui` serves an editor on `http://127.0.0.1:7337` and opens it in your browser. It checks your program as you type:

- The editor highlights PAL syntax. Syntax errors are marked on their line, and clicking the message jumps to it.
- Results are highlighted like the REPL's, and rules are drawn as inference rules.
- Your program is saved in the browser between visits.

| Flag        | Effect                                                     |
| ----------- | ---------------------------------------------------------- |
| `--port N`  | Use port `N` (default 7337; any free port if it's taken)   |
| `--no-open` | Don't open a browser, just print the URL                   |

The page is compiled into the binary, so `pal-ui` is a single self-contained executable. It only listens on `127.0.0.1` and rejects requests for any other host name, so other machines and other websites can't reach it.

A public copy runs at <https://pal-ui.vercel.app> (from `main`) and <https://pal-ui-testing.vercel.app> (from `testing`). Each push to those branches redeploys it: [`.github/workflows/deploy.yml`](.github/workflows/deploy.yml) builds `pal-ui` with Nix and uploads it to Vercel, where a small function ([`deploy/vercel/`](deploy/vercel/)) runs it behind `/api/run`.

## Examples

[`examples/programs/`](examples/programs/) is a small catalogue of type systems:

| File                                             | System                                         |
| ------------------------------------------------ | ---------------------------------------------- |
| [`arith.pal`](examples/programs/arith.pal)       | Typed arithmetic (TAPL ch. 8)                  |
| [`stlc.pal`](examples/programs/stlc.pal)         | Simply typed λ-calculus                        |
| [`stlc-ext.pal`](examples/programs/stlc-ext.pal) | STLC + unit, products, sums, `Fix`             |
| [`lists.pal`](examples/programs/lists.pal)       | Polymorphic lists with `Fold`                  |
| [`hm.pal`](examples/programs/hm.pal)             | Hindley–Milner (let-polymorphism)              |
| [`logic.pal`](examples/programs/logic.pal)       | Propositional logic via Curry–Howard           |

```sh
run-examples --list              # everything, including the Haskell examples
run-examples file/hm             # one example
run-examples --trace dsl/maybe   # every step, with context dumps
```

## Using PAL from Haskell

A PAL program is a [Polysemy](https://hackage.haskell.org/package/polysemy) effect with four operations:

```haskell
data PAL m a where
  DefineType :: TypeDecl   -> PAL m ()
  DefineExpr :: ExprDecl   -> PAL m ()
  DefineRule :: TypingRule -> PAL m ()
  Infer      :: Expr       -> PAL m (Either Err Type)
```

You can write programs three ways. All of them run on the same engine:

| Style      | Looks like                                  | Example                                             |
| ---------- | ------------------------------------------- | --------------------------------------------------- |
| Code       | `defineRule (TypingRule "Add" …)`           | [`AsCode.hs`](examples/Examples/AsCode.hs)          |
| Data       | `[ADefineType …, AInfer …]` (`PalAction`)   | [`AsData.hs`](examples/Examples/AsData.hs)          |
| Quasiquote | `[palQuasiQuoter\| rule Add: … \|]`         | [`AsDSL.hs`](examples/Examples/AsDSL.hs)            |

Run a program with one of three interpreters:

| Interpreter          | Output                                  |
| -------------------- | --------------------------------------- |
| `Interpreters.Core`  | None (pure). Returns the last result    |
| `Interpreters.Debug` | Every step plus the full context        |
| `Interpreters.IO`    | One line per result, and returns the final context (used by `pal`) |

`Program.loadPalFile` parses a `.pal` file into `[PalAction]`.

## Development

| Command                   | Does                                       |
| ------------------------- | ------------------------------------------ |
| `pal`                     | Build and run `pal`                        |
| `pal-ui`                  | Build and run the web UI                   |
| `format` / `nix fmt`      | Format Haskell sources with ormolu         |
| `format --check`          | Fail if anything is unformatted            |
| `run-examples`            | Build and run the examples                 |

| Path                                                                       | Contents                         |
| -------------------------------------------------------------------------- | -------------------------------- |
| [`src/Types.hs`](src/Types.hs)                                             | Types, terms, rules, the effect  |
| [`src/Interpreters/Common/Actions.hs`](src/Interpreters/Common/Actions.hs) | Inference and unification        |
| [`src/Parser/`](src/Parser/)                                               | Syntax and quasiquoter           |
| [`src/Repl.hs`](src/Repl.hs), [`app/Main.hs`](app/Main.hs)                 | REPL and the `pal` command       |
| [`ui/`](ui/)                                                                | The web UI: [`ui/static/index.html`](ui/static/index.html) (the page) and the server it is embedded in |
