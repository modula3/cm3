# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

ESC/Simplify is a first-order theorem prover for program verification, originally developed at DEC/Compaq SRC as part of the Extended Static Checker (ESC) system. It is written in Modula-3 and lives within the CM3 repository but is **not** included in the standard CM3 build scripts (`pkginfo.txt`). It must be built independently.

Reference: Detlefs, Nelson, and Saxe, "A Theorem Prover for Program Checking," Compaq SRC Research Report 178 (2002).

## Build

Requires a working CM3 compiler installation. See the parent `CLAUDE.md` for CM3 bootstrap instructions.

Build the three packages in dependency order:
```
cd ESC/Simplify/cgi-utils/src && cm3 -build && cm3 -ship
cd ESC/Simplify/prover/src && cm3 -build && cm3 -ship
cd ESC/Simplify/simplify/src && cm3 -build && cm3 -ship
```

The Simplify executable is built as a standalone binary via `build_standalone()`.

Dependencies (imported via m3makefile):
- **prover** library: libm3, set, table-list, digraph
- **simplify** program: libm3, ui, cgi-utils, mtex, prover

## Tests

```
cd ESC/Simplify/simplify/test
make test OUTPUTDIR=<target>    # e.g., AMD64_DARWIN
```

The `OUTPUTDIR` variable must match the CM3 target directory (e.g., `AMD64_DARWIN`, `AMD64_LINUX`). It selects the Simplify binary at `../$(OUTPUTDIR)/Simplify`.

The test harness runs Simplify against multiple formula suites (valid, invalid, ESC-specific, ordering, predicate definitions), compares output against `test.ans`, and reports diffs. Only conclusion differences (Valid/Invalid/Unknown) are flagged as errors; satisfying-context differences are expected and tolerated.

Performance tests: `cd simplify/test/perf && ./runtests` (requires `ESCTOOLS_ROOT` and `OUTPUTDIR` env vars).

## Architecture

### Package Structure

```
ESC/Simplify/
├── prover/src/       # Core prover library (75 modules)
│   └── gens/         # Generated modules (sequences, tables, digraphs via m3quake)
├── simplify/src/     # CLI driver (single module: Simplify.m3)
│   └── test/         # Test suites and oracle (test.ans)
└── cgi-utils/src/    # CGI/HTML utilities (HTMLUtils, CGITrans)
```

### Prover Pipeline

Input formulas use S-expression syntax with quantifiers, arithmetic, and equality:
```
(FORALL (x) (PATS (f x)) (EQ (f (f x)) x))
```

The pipeline:
1. **Simplify.m3** — CLI entry point; reads formulas from stdin/file, manages axiom stack (BG_PUSH/BG_POP), calls Prover
2. **Prover** — Master interface; maintains axiom sets, background predicate conjunction with Push/Pop scoping
3. **PredSx / Clause** — Formula representation and CNF conversion (Skolemization, clause generation)
4. **MatchingRule / Match** — Pattern-driven quantifier instantiation using user-supplied PATS/NOPATS hints
5. **Satisfy** — Main backtracking search with case splitting, resource limits, and heuristics
6. **Decision procedures** (coordinated via Context):
   - **Enode** — Egraph with congruence closure for uninterpreted functions
   - **Simplex** — Linear arithmetic over integers/rationals
   - **Orders** — Partial order constraints
   - **AF** — Atomic formula registry with three-valued truth tracking

Output: Valid (no counterexample found), Invalid (with counterexample context and labels), or Unknown (resource limits hit).

### Key Design Patterns

- **Resettable state via undo stacks**: All theories (Enode, Simplex, AF, Context) support Push/Pop for backtracking during search.
- **AF.T as shared currency**: All decision procedures register atomic formulas through AF with unique IDs and truth-value tracking.
- **Lazy activation**: Enodes are not eagerly interned; activation is deferred and controlled by assertion.
- **Resource-bounded search**: Hard limits on time (`PROVER_KILL_TIME`), iterations (`PROVER_KILL_ITER`), and matching depth (`PROVER_MATCH_DEPTH`) ensure termination.

### Axiom Files

Bundled in `prover/src/` as resources (compiled into the binary via `bundle("Axioms")`):
- `def.ax` — Default axioms (select/store, ordering)
- `esc.ax` / `esc0.ax` — ESC axiom sets

Test-specific axiom files live in `simplify/test/` (escv1.ax, escv2.ax, escv3.ax, necula1.ax, saxe1.ax).

## Environment Variables

The prover reads many `PROVER_*` environment variables at startup. Key ones:

| Variable | Default | Effect |
|---|---|---|
| `PROVER_KILL_TIME` | 10^9 | Max seconds per conjecture |
| `PROVER_KILL_ITER` | MAXINT | Max search iterations per conjecture |
| `PROVER_MATCH_DEPTH` | 3 | Max non-unit matching depth |
| `PROVER_MAX_FNUR` | 100 | Max restricted non-unit matching rounds |
| `PROVER_SUBSUMPTION` | NonProxy | None / NonProxy / All |
| `PROVER_CLAUSE_SHUFFLE` | Reverse | None / Reverse / Rand |
| `PROVER_NO_PLUNGE` | (unset) | Disable depth-1 plunging |
| `PROVER_LIT_SPLIT` | (unset) | Enable literal splitting heuristic |
| `PROVER_CC_LIMIT` | 1 | Max counterexample contexts shown |

See `Prover.i3` for the full list (~40 variables with documentation).
