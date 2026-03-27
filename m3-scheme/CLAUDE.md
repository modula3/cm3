# CLAUDE.md — m3-scheme (MScheme)

MScheme is a Scheme interpreter embedded in Modula-3, with an
ahead-of-time compiler that translates Scheme to Modula-3 source.

## Package layout

| Package | What it is |
|---|---|
| `mscheme/` | Core interpreter (Scheme.T, primitives, reader, environment) |
| `mscheme-interactive/` | Interactive REPL binary (`mscheme`) with readline |
| `modula3scheme/` | SchemeM3.T — interpreter with M3 library support (file I/O, etc.) |
| `scheme-lib/` | Standard library `.scm` files (display, struct, mbe, pregexp, etc.) |
| `schemecompiler/` | Ahead-of-time Scheme→M3 compiler (compiler.scm + scheme.tmpl) |
| `sstubgen/` | Tool that generates Scheme bindings from M3 interfaces |
| `mscheme-doc/` | LaTeX documentation and test suites |

## Building

```sh
cd cm3
scripts/do-pkg.sh buildship mscheme modula3scheme scheme-lib schemecompiler mscheme-interactive sstubgen
```

Or build everything:
```sh
scripts/do-cm3-min.sh buildship   # m3core + libm3
scripts/do-pkg.sh buildship mscheme modula3scheme scheme-lib schemecompiler mscheme-interactive
```

## Running the interpreter

```sh
install/pkg/mscheme-interactive/ARM64_DARWIN/mscheme          # REPL
install/pkg/mscheme-interactive/ARM64_DARWIN/mscheme foo.scm  # load file then REPL
```

## Compiling Scheme to Modula-3

### In your m3makefile

```
import ("schemecompiler")

scheme ("mylib")                        % compiles mylib.scm → mylib.i3 + mylib.m3
scheme ("my-utils")                     % hyphens → underscores: my_utils.i3/.m3
LinkCompiledSchemeAs ("MyCompiled")      % MUST come after all scheme() calls

implementation ("Main")
program ("myprog")
```

### In your Main.m3

You **must** import the linker module to force initialization:

```modula3
(* Force compiled Scheme modules to be linked and registered *)
IMPORT MyCompiled; <*NOWARN*>
```

Without this import, the compiled modules are dead code — linked but
never initialized. The interpreter will load `.scm` files from disk
as if no compilation occurred.

### Important rules

- **Unique names**: Each package that compiles Scheme must use a
  different name in `LinkCompiledSchemeAs`. If two packages both
  use `LinkCompiledScheme()` (which defaults to "SchemeCompiled"),
  you get a duplicate-unit error at link time.
- **Hyphens**: `scheme("fold-constants")` generates M3 module
  `fold_constants`. The compiler registers both `"fold-constants"`
  and `"fold_constants"` for runtime lookup.
- **Bootstrap**: If `mscheme` is not yet built, `scheme()` silently
  becomes a no-op. The `.scm` files load via the interpreter at
  runtime as usual.

### Bypassing compiled code

Set `MSCHEME_INTERPRETED=1` to force interpreter mode at runtime:

```sh
env MSCHEME_INTERPRETED=1 ./myprog -scm
```

This disables `SchemeCompiledRegistry.Lookup`, so all `(load ...)`
calls fall through to the interpreter even when compiled modules are
linked in. Useful for debugging or testing interpreter vs compiled
behavior.

## Compiler details

The compiler (`schemecompiler/src/compiler.scm`) is a Level 1
Scheme-to-M3 compiler. It:

- Compiles `define` forms with `lambda` bodies to M3 procedures
- Handles: `if`, `cond`, `case`, `let`/`let*`/`letrec`, `begin`,
  `do`, `and`/`or`/`not`, `set!`, closures, rest params, self-tail-calls
- **Punts** (falls back to interpreter) for: `quasiquote`, `call/cc`,
  `eval`, `unwind-protect`, `define-macro`
- Each compiled `.scm` generates an `Install` procedure that registers
  compiled procedures in the Scheme environment
- The `SchemeCompiledRegistry` maps filenames to Install procedures;
  `Scheme.LoadFile` checks the registry before looking on disk

### Number literal formatting

The compiler emits M3 number literals. M3 LONGREAL syntax requires
`digits.digitsd[sign]digits` (e.g., `1.0d-6`). The helpers
`number-to-m3-longreal` and `number-to-m3-integer` in compiler.scm
handle the conversion from Scheme's `number->string` output.

## sstubgen

Generates Scheme stubs from M3 interfaces, allowing Scheme code to
create and manipulate M3 objects:

```
SchemeStubs ("Foo")          % generates FooSchemeStubs.m3
ExportSchemeStubs ("pkg")    % aggregates all stubs for this package
importSchemeStubs ()         % imports stubs from dependencies
```

## Test suites

```sh
cd m3-scheme/schemecompiler/numtest
ARM64_DARWIN/numtest src/test-numeric-tower.scm   # 347 tests
ARM64_DARWIN/numtest src/test-examples.scm        # 180 tests
ARM64_DARWIN/numtest src/test-r4rs-gaps.scm       # 97 tests
```

The numtest binary links compiled versions of these test files, so
they test both compiled and interpreted code paths (compiled defines
run as M3 procedures; punted defines run through the interpreter).

## Key source files

| File | Purpose |
|---|---|
| `mscheme/src/Scheme.m3` | Core interpreter: eval, LoadFile, environment |
| `mscheme/src/SchemeCompiledRegistry.m3` | Compiled module registry |
| `mscheme/src/SchemePrimitive.m3` | Built-in primitive procedures |
| `mscheme/src/SchemeInt.m3` | Exact integer type (wraps Mpz.T) |
| `mscheme/src/SchemeLongReal.m3` | Inexact number type (LONGREAL) |
| `schemecompiler/src/compiler.scm` | The Scheme→M3 compiler |
| `schemecompiler/src/scheme.tmpl` | Quake template: scheme(), LinkCompiledSchemeAs() |
| `sstubgen/src/sstubgen.scm` | Stub generator |
