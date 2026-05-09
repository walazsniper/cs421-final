# Core Interpreter

A Haskell interpreter for **Core**, a small functional intermediate language
from *Implementing Functional Languages: A Tutorial* (Peyton Jones & Lester).
Built as the CS 421 final project.

## What Core looks like

A program is a list of supercombinator declarations separated by `;`.
Execution starts at `main`, which must take no arguments.

```
maybe ::= Just num | None ;
fromMaybe d m = case m of
    <1> x -> x ;
    <2> -> d
;
main = fromMaybe 0 (Just 42)
```

The language supports:
- Numbers, variables, lambdas (`\x . body`), application
- `let` and `letrec` with the usual scoping
- Algebraic data types, compiled to `Pack{tag, arity}` constructors
- `case` for pattern matching on constructors
- Binary operators: `+ - * /`, comparators (`< <= == ~= >= >`), `&`, `|`

The full grammar lives in `../docs/core.org`.

## Running

Build once, then pipe a Core program in on stdin:

```
stack build
stack run < ../core-files/parser-tests/19-call-precedence.core
```

You'll see:

```
Welcome to your Core interpreter!

Enter your program here and hit ^D when done.
"216"
```

The quoted string is the value of `main`. You can also run interactively
(type a program, hit `^D` when done).

## How it works

The pipeline is read → parse → evaluate → print:

1. **Parse** (`src/Parse.hs`) — Parsec turns source into a `HashMap Name Decl`.
   Algebraic type declarations are stripped at parse time and their
   constructors registered in user state with fresh tags, so `Just`, `None`,
   etc. become `EPack tag arity` nodes in the AST.
2. **Evaluate** (`src/Interp.hs`) — a value-based interpreter. Closures
   (`VClos`), constructor values (`VPack`), and primitive operators
   (`VPrim`) all share the same currying machinery in `apply`. `letrec` and
   top-level mutual recursion both tie the knot using Haskell's laziness.
3. **Print** (`app/Main.hs`) — evaluates `main`'s body in the global env and
   prints the resulting value.

## Project layout

```
src/Types.hs   AST, Value type, Env type
src/Parse.hs   Parsec-based parser
src/Interp.hs  evaluator, apply, binop primitives
app/Main.hs    read-parse-eval-print driver
```

Sample programs are in `../core-files/` and `../core-files/parser-tests/`.
