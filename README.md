# PAL — a typechecker sandbox

Define a type system as a handful of inference rules, then ask PAL to infer types with it.

```haskell
type Bool
expr True : Bool

rule Not:
  x : Bool
->
  Not(x) : Bool

infer Not(Not(True))          -- ✓ Bool
check Not(True) : Bool        -- ✓ the type you expected
fails Not(True, True)         -- ✓ a type error, as expected
```

New to type systems? **[Read the tutorial →](TUTORIAL.md)**

---

## Quick start

```sh
nix develop                              # or let direnv load the shell

pal                                      # interactive REPL
pal FILE.pal                             # typecheck a file
pal --derivations FILE.pal               # … and draw each derivation tree
pal -i examples/programs/stlc.pal        # REPL with a file preloaded
pal-ui                                   # web UI, opens in your browser
run-examples                             # run every bundled example
run-tests                                # run the regression tests
```

The dev shell's `pal` and `pal-ui` rebuild from your working tree first, so they always run your latest code.

`pal` exits with **1** if a definition is rejected, an `infer` fails, or a `check` / `fails` isn't met, so a `.pal` file doubles as a test suite in scripts and CI.

## The language

| Statement                           | Meaning                                                   |
| ----------------------------------- | --------------------------------------------------------- |
| `type T`, `type T<a, b>`            | Declare a type, with its parameters                       |
| `data T<a> = C1 \| C2(a, T<a>)`      | An inductive type: its type, constructors, recursor `RecT`, reductions and values |
| `expr C : T`                        | Axiom: constant `C` has type `T`                          |
| `rule Name:` *premises* `->` *conclusion* | Inference rule                                      |
| `reduce Name: lhs ~> rhs`           | Reduction: a term that matches `lhs` is equal to `rhs`    |
| `def Name := e`, `def Name(x, y) := e` | Definition: a name for `e` (with parameters), with the inferred type, that reduces to `e` |
| `infer e`                           | Infer the type of `e`                                     |
| `check e : T`                       | Expect `e` to have type `T` (type variables may be renamed) |
| `fails e`                           | Expect `e` to be a type error                             |
| `find T`, `find T N`                | Proof search: find a term of type `T`, at most `N` rules deep (default 6) |
| `holds J`                           | Expect the relation judgment `J` (like `a <: b`) to be derivable; `fails J` expects the opposite |
| `value P`                           | Declare a pattern of values, e.g. `value Succ(n)` (for `test progress`) |
| `test preservation [N]`             | Test that reduction keeps the type, on N generated terms (default 5000) |
| `test progress [N]`                 | Test that a well-typed term that does not reduce is a value |

Premises come in these shapes:

```haskell
x : T                    -- plain:       x must have type T
x : A |- body : B        -- hypothesis:  check body with x : A in scope (⊢ also works)
x : A, y : B |- e : C    -- several hypotheses
x : gen A |- body : B    -- generalised: x is polymorphic in body (let-polymorphism)
x : lin A |- body : B    -- linear: body uses x exactly once
x : aff A |- body : B    -- affine: body uses x at most once
share t : A              -- shares the variables in scope with the next share premises
```

**Bidirectional typing.** A judgment can have a direction: `e => T` infers (synthesises) the type of `e`, and `e <= T` checks `e` against a known `T`. A plain `e : T` premise checks too. For a rule without a direction, checking falls back to inference, so plain rules work as before.

```haskell
rule Lam:                        rule App:
  x : a |- body <= b               f => Arrow<a, b>
->                                 x <= a
  Lam(x, body) <= Arrow<a, b>    ->
                                   App(f, x) => b
```

- A rule with a `<=` conclusion is used only when the expected type is known. PAL unifies the expected type with the conclusion type first, then checks the premises.
- Where a type must be inferred (`infer e`, or a `=>` premise), such a term gives `Cannot infer the type of Lam(x, x)`.
- `check e : T` checks `e` against `T` when its type cannot be inferred. [`tests/pass/bidirectional.pal`](tests/pass/bidirectional.pal) is a complete example, with an annotation rule `Ann(e, t) => t`.

**Naming:** `Uppercase` is a constructor or concrete type (`Add(x, y)`, `Maybe<Num>`). `lowercase` is a variable or type variable (`x`, `a`). Comments start with `--`.

**Dependent types.** Expressions and types are the same terms. `C(…)` and `C<…>` are equal, and PAL shows each term with the brackets that you wrote. Thus a type can mention an expression, and rules can check types:

```haskell
rule Replicate:                 rule Pi:
  n : Nat                         t : Type
  x : a                           x : t |- u : Type
->                              ->
  Replicate(n, x) : Vec(n, a)     Pi(x, t, u) : Type

rule App:                       reduce Beta: App(Lam(x, t, body), a) ~> body[x := a]
  f : Pi(x, t, u)
  a : t
->
  App(f, a) : u[x := a]
```

- In a rule, a variable of the conclusion (`n` above) stands for the matched expression, also in the types. Each other lowercase variable is a type variable.
- `u[x := a]` is `u` with `a` in place of `x`. PAL applies it when it knows `u`.
- PAL gets the binders from the rules. The premise `x : t |- u : Type` of the rule for `Pi(x, t, u)` tells PAL that `x` is bound in `u`. Substitution does not capture variables, and types are equal up to the names of bound variables.
- Types are equal up to the `reduce` statements. PAL stops a reduction after 10 000 steps.
- `def Two := Succ(Succ(Zero))` gives a name to a term. PAL infers the type of the body, adds `Two` as a constant of that type, and adds the reduction `Two ~> Succ(Succ(Zero))`. So a definition can also name a type, such as `def Vec2 := Vec(Two, Nat)`.
- A definition can have parameters: `def Double(n) := Plus(n, n)`. Each parameter gets the type that the body needs (here `n : Nat`), and PAL adds a typing rule and the reduction `Double(n) ~> Plus(n, n)`. The result type can mention a parameter: `def Rep(n) := Replicate(n, Zero)` gives `Rep(n) : Vec(n, Nat)`.
- A unification with `u[x := a]`, where `u` is still unknown, is solved if `a` is a variable that `u` cannot mention (pattern unification): `u[x := y] = Vec(y, Nat)` gives `u := Vec(x, Nat)`. Otherwise it waits until a later premise solves `u`. If `u` stays unknown, PAL reports a stuck substitution. For example, a rule can check its body with its own binder `y` and give the type `Pi(x, t, u)` through the premise `y : t |- body : u[x := y]`.
- **Implicit arguments:** a variable that appears only in the types of a rule is an unknown that unification solves. So `IApp(f) : u[a := m]` (with `f : Pi(a, Type, u)`) applies `f` to a type `m` that PAL finds from the context: `App(IApp(Id), Zero) : Nat`.
- A variable cannot escape its binder. For example, in a rule with the conclusion `Weird(x, t, body) : u` and the premise `x : t |- body : u`, the type `u` cannot mention `x`, because `u` is also used outside `x`. Write `u[x := …]` to remove `x`.
- Limits: the checks of termination and confluence give warnings, not proofs (see below), and PAL accepts `Type : Type`.

[`lambda-pi.pal`](examples/programs/lambda-pi.pal) is a complete example.

**Metatheory tests.** `test preservation` and `test progress` check two classic properties of your type system:

- PAL generates random closed terms from the constructors of your `expr` declarations and rules, and keeps the ones that have a type. It takes most arguments from the terms that it already found, chosen so that their types fit the premises.
- A term that can only be checked (a `<=` rule, like an unannotated `Lam(x, x)`) is checked against an unknown type, so such terms are tested too. An argument that a rule uses as a type (like `t` in `Ann(e, t) => t`) gets one of the types that PAL already found.
- **Preservation:** each one-step reduct of a term `e : T` must have a type that is at least as general as `T`.
- **Progress:** each term that does not reduce must match a `value` pattern.
- PAL reports the smallest counterexample. The generator has a fixed seed, so the result is the same every time.

```text
✗ test preservation — Lam(x, Fst(Pair(False, x))) : Arrow<a, Bool> reduces to Lam(x, x) : Arrow<a, a>
✗ test progress — Not(False) : Bool does not reduce, and no value pattern matches it
```

A test that passes is evidence, not a proof: it checks only the terms that PAL generated.

**Proof search.** `find T` looks for a closed term of type `T`. With types as propositions ([`logic.pal`](examples/programs/logic.pal)), the term is a proof:

```text
✓ find Implies<And<a, Or<b, c>>, Or<And<a, b>, And<a, c>>> — Lam(x, Case(Snd(x), y, Inl(Pair(Fst(x), y)), z, Inr(Pair(Fst(x), z))))
✗ find Or<a, Implies<a, False>> — no term of this type found (the search goes 6 rules deep)
```

- The search works backwards from `T`: it tries the variables in scope, the constants, and the rules whose conclusion type unifies with the goal. The premises become new goals.
- It looks only for normal terms. An elimination rule (a rule whose conclusion type is a variable, like `Fst(p) : a`) is applied only to a variable, a constant or another elimination.
- It deepens step by step up to 6 rules (or `N` with `find T N`), so it finds a small term first. PAL checks the term with the type checker before it shows it. A search also stops after 200 000 steps, and then it tells how deep it went.
- If the search finds nothing, that is not a proof that no term exists.

**Definitions are checked.** A definition is rejected (with a reason, and `pal` exits 1) if it uses a type that wasn't declared, gives a type the wrong number of arguments, or is a rule whose premise uses a variable that its conclusion doesn't bind. In a type, a constructor is declared by `type`, by `expr` or by the conclusion of a rule:

```text
✗ expr Not rejected: undeclared type Bol (did you mean Bool?)
✗ expr Id rejected: Arrow takes 2 type arguments, but is given 1
✗ rule Weird rejected: premise variable z does not appear in the conclusion W(x)
```

**Other judgment forms.** A rule whose conclusion is not `e : T` defines a *relation*: subtyping, kinding, type equality, effects and so on. Write it with an infix operator made of symbols (`a <: b`, `t :: k`, `a == b`) or with a prefix name (`Sub(a, b)`, `Le(m, n)`):

```haskell
rule SubTop:          rule SubArrow:
->                      a2 <: a1
  a <: Top              b1 <: b2
                      ->
                        Arrow<a1, b1> <: Arrow<a2, b2>

rule App:
  f : Arrow<a, b>
  x : c
  c <: a
->
  App(f, x) : b
```

- A typing rule can have relation premises (like `c <: a` above), and a rule for a relation can have relation premises (only those).
- PAL derives a relation judgment by searching backwards through the rules of the relation, like Prolog: in order of definition, with backtracking, at most 64 rules deep. If it finds no derivation, the error is `Cannot derive Bool <: Int`.
- `holds J` expects that `J` can be derived, and `fails J` expects that it cannot.
- Limit: a relation does not see the variables in scope (the typing context), so a rule like kinding of a type variable from the context cannot be written yet.

[`subtyping.pal`](examples/programs/subtyping.pal) is a complete example.

**Linear and affine types.** A hypothesis `x : lin A` must be used exactly once in its premise, and `x : aff A` at most once. PAL counts the uses:

- Premises split the variables in scope: the uses of a linear variable in the premises of a rule add up, so `Pair(x, x)` uses `x` twice. This is the context splitting of linear logic.
- Premises marked `share` that are next to each other use the same variables: of them, the one with the most uses counts. Use it for rules whose premises are alternatives, like the branches of `If`.
- The error is, for example, `Usage error: x is linear, so it must be used once, but it is used 2 times`.

[`linear.pal`](examples/programs/linear.pal) is a complete example.

**Data types.** `data List<a> = Nil | Cons(a, List<a>)` stands for the statements that you would write for an inductive type:

- the type `type List<a>`, and the constructors: `expr Nil : List<a>` and a rule for `Cons`;
- the recursor `RecList(l, nil, cons_1, cons_2, cons_rec2, cons)`: for each constructor a branch, with the fields in scope and, for each field of the type itself, the result of the recursion on it (`cons_rec2`). It does case analysis and structural recursion, so `Lam(l, RecList(l, Zero, h, t, r, Succ(r)))` is the length of a list;
- a reduction for each constructor, e.g. `RecList(Nil, …) ~> nil`, and a `value` pattern for each constructor (for `test progress`).

`:ctx` shows the generated definitions. If one of them is rejected (for example, a field type that is not declared), the definitions that need it are rejected too; the first error names the cause.

**Some problems are warnings.** The definition is still added:

- PAL uses only the newest rule for a constructor and a number of arguments (in each direction), so it warns when a new rule hides an older one.
- **Confluence:** where two reductions apply to the same term (a critical pair), PAL reduces both results to normal form. If the normal forms differ, the result depends on which reduction PAL uses.
- **Termination:** a reduction must call its own constructor only on smaller arguments (from left to right, the first argument that changes must be a part of the old one), and the constructors of reductions must not call each other in a cycle. PAL checks only this first-order recursion: a substitution on the right side (like β-reduction) is not checked, and a cycle that does stop (like `Even`/`Odd`) still gets a warning.

```text
⚠ rule IsTrueAgain hides rule IsTrue: both are for IsTrue with 1 argument, and PAL uses only the newest
⚠ reductions IsTrueTrue and IsTrueAny are not confluent: IsTrue(True) reduces to False and to True
⚠ reduction Forever may not terminate: IsTrue(b) on the right side has arguments that are not smaller than in IsTrue(b)
```

**Errors** point at the part of the expression at fault and list the premises being checked. Every independent error is reported, not just the first:

```text
✗ Two(True, Nope) -> [Error] Type mismatch: expected Num, got Bool
    at tests/fail/errors.pal:29:11
       |
    29 | infer Two(True, Nope)                 -- two independent errors
       |           ^^^^
    in rule Two, premise x : Num  (checking True)
    and: Unknown expression → Nope
      at tests/fail/errors.pal:29:17
         |
      29 | infer Two(True, Nope)                 -- two independent errors
         |                 ^^^^
      in rule Two, premise y : Num  (checking Nope)
```

| Error                    | Cause                                                  |
| ------------------------ | ------------------------------------------------------ |
| `Type mismatch`          | Two different types had to be equal                    |
| `Arity mismatch`         | A rule exists, but for a different number of arguments |
| `Unknown expression`     | No rule, no `expr`, and not a bound variable           |
| `Infinite type`          | A type would contain itself (`a ~ Arrow<a, b>`)        |
| `No typing rule matched` | A constant was given arguments                         |
| `Stuck substitution`     | In `u[x := e]`, `u` is still unknown                   |
| `Reduction limit`        | The reductions of a term do not stop                   |
| `Cannot infer`           | A term that only checks (a `<=` rule) is where a type must be inferred |
| `Scope error`            | A variable would be used outside its binder            |
| `Cannot derive`          | No rule derives a relation judgment, like `Bool <: Int` |
| `Usage error`            | A linear variable is not used once, or an affine one more than once |

**Derivations.** On success PAL has built a derivation tree, the proof that the expression has its type. `pal --derivations`, the REPL's `:derive` and the web UI draw it:

```text
          x : Bool ⊢ x : Bool
    ─────────────────────────────── Lam    ───────────── expr
     Lam(x, x) : Arrow<Bool, Bool>          True : Bool
    ───────────────────────────────────────────────────────── App
                   App(Lam(x, x), True) : Bool
```

`:latex` (and the web UI's *Copy LaTeX*) gives the same tree as a [`mathpartir`](https://ctan.org/pkg/mathpartir) document.

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
| `:derive EXPR`  | Draw the derivation tree of `EXPR` |
| `:latex EXPR`   | The same, as a LaTeX document   |
| `:load FILE...` | Run files in the current context |
| `:ctx`          | Show the context                |
| `:reset`        | Clear the context               |
| `:help` `:quit` | Help / exit (also Ctrl-D)       |

`pal` flags: `-i FILE...` runs files and then opens a REPL with their context, `--derivations` draws the derivation tree under each result, and `--no-color` turns off colour (as do `NO_COLOR` and piped output).

## The web UI

`pal-ui` serves an editor on `http://127.0.0.1:7337` and opens it in your browser. It checks your program as you type:

- The editor highlights PAL syntax. Syntax errors are marked on their line, the part of an expression at fault is underlined, and clicking an error jumps to it.
- Results are highlighted like the REPL's, rules are drawn as inference rules, and rejected definitions and unmet `check` / `fails` are flagged.
- Open **derivation** under a result to see its derivation tree, or copy it as LaTeX.
- Your program is saved in the browser between visits.
- The **Examples** menu loads any program from [`examples/programs/`](examples/programs/).
- **Share** copies a link that opens your program. The program is in the link itself (after `#`), so the server never sees it.
- `test` statements are slow, so the page does not run them while you type. Press **Run tests** to run them.
- The theme menu offers every [Tinted Theming](https://github.com/tinted-theming/schemes) colour scheme (base16, base24 and tinted8, vendored in [`ui/themes/`](ui/themes/)).

| Flag        | Effect                                                       |
| ----------- | ------------------------------------------------------------ |
| `--port N`  | Use port `N` (default 7337; any free port if it's taken)     |
| `--no-open` | Don't open a browser, just print the URL                     |
| `--page`    | Print the page (themes inlined) and exit, for static hosting |

The page and the colour schemes are compiled into the binary, so `pal-ui` is a single self-contained executable. It only listens on `127.0.0.1` and rejects requests for any other host name, so other machines and other websites can't reach it.

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
| [`lambda-pi.pal`](examples/programs/lambda-pi.pal) | Dependent types (λΠ) with `Vec` and β-reduction |
| [`subtyping.pal`](examples/programs/subtyping.pal) | Subtyping with a relation `a <: b` |
| [`linear.pal`](examples/programs/linear.pal) | Linear and affine types (`lin`, `aff`, `share`) |

```sh
run-examples --list              # everything, including the Haskell examples
run-examples file/hm             # one example
run-examples --trace dsl/maybe   # every step, with context dumps
```

## Using PAL from Haskell

A PAL program is a [Polysemy](https://hackage.haskell.org/package/polysemy) effect with nine operations:

```haskell
data PAL m a where
  DefineType      :: TypeDecl    -> PAL m ()
  DefineExpr      :: ExprDecl    -> PAL m ()
  DefineRule      :: TypingRule  -> PAL m ()
  DefineReduction :: Reduction   -> PAL m ()
  DefineValue     :: Term        -> PAL m ()             -- value P
  DefineTerm      :: TermDef     -> PAL m ()             -- def Name := e
  Infer           :: Expr        -> PAL m (Either Err Type)
  Expect          :: Expectation -> PAL m Bool           -- check e : T / fails e / find T
  Test            :: TestSpec    -> PAL m Bool           -- test preservation / progress
```

`Expr` and `Type` are both names for `Term` (`Var`, `Con`, `Subst`). The pattern synonyms `ECon`/`EVar` and `TCon`/`TVar` build a constructor with `(…)` or `<…>` brackets. `TypingRule` and `Premise` are pattern synonyms for a rule and a premise with plain judgments (`:`); `TypingRuleWith` and `PremiseWith` also take a `Mode` (`Plain`, `Synthesis`, `Checking`).

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

`Program.loadPalFile` parses a `.pal` file into `[PalAction]`. For the full detail (derivations, every error with its location and context, rejected definitions), `Program.runSteps` runs located statements purely and returns an `Outcome` per statement; [`Report`](src/Report.hs) renders outcomes and derivations (text or LaTeX). `pal`, its REPL and `pal-ui` all use these.

## Development

| Command                   | Does                                       |
| ------------------------- | ------------------------------------------ |
| `pal`                     | Build and run `pal`                        |
| `pal-ui`                  | Build and run the web UI                   |
| `format` / `nix fmt`      | Format Haskell sources with ormolu         |
| `format --check`          | Fail if anything is unformatted            |
| `run-examples`            | Build and run the examples                 |
| `run-tests`               | Run the regression tests ([`tests/`](tests/)) and the property tests of the engine |
| `run-tests --accept`      | Also rewrite `tests/fail/*.out` from the current output |
| `nix flake check`         | Run the tests against a sandboxed build    |

| Path                                                                       | Contents                         |
| -------------------------------------------------------------------------- | -------------------------------- |
| [`src/Types.hs`](src/Types.hs)                                             | Types, terms, rules, the effect  |
| [`src/Interpreters/Common/`](src/Interpreters/Common/) | The engine: `Terms` (variables, binders, substitution), `Reduce` (reductions), `InferMonad`, `Unify`, `Inference` (inference and checking with the rules) and `Definitions` (definitions and their checks); `Actions` re-exports them |
| | Also `Expectations` (check, fails, find), `Search` (proof search) and `Metatheory` (preservation and progress tests) |
| [`tests/properties/`](tests/properties/) | Property tests of the engine (QuickCheck) |
| [`src/Program.hs`](src/Program.hs), [`src/Report.hs`](src/Report.hs)       | Running statements step by step; reporting outcomes |
| [`tests/`](tests/)                                                           | `pass/` must succeed; `fail/` must fail with the output in its `.out` file |
| [`src/Parser/`](src/Parser/)                                               | Syntax and quasiquoter           |
| [`src/Repl.hs`](src/Repl.hs), [`app/Main.hs`](app/Main.hs)                 | REPL and the `pal` command       |
| [`ui/`](ui/)                                                                | The web UI: [`ui/static/index.html`](ui/static/index.html) (the page) and the server it is embedded in |
