# TODO: ex_stack backtrace on uncaught exceptions

## Problem

When an M3 program built with `M3_USE_STACK_WALKER = TRUE` (zero-cost C++
EH path) throws an exception that no `EXCEPT` clause handles, the only
diagnostic is:

```
libc++abi: terminating due to uncaught exception of type _M3Exc
```

This tells us the runtime is in `ex_stack` mode and that `_M3Exc` (the
single C++ exception type carrying every M3 exception) was thrown
unhandled. It does **not** tell us:

- which M3 exception identity was raised (e.g. `RuntimeError.NarrowFailed`,
  user `Foo`),
- the exception value/payload, if any,
- where it was raised from (no source file/line, no symbolized frames),
- whether it was an explicit `RAISE`, an implicit NARROW/NIL/range/index
  check failure, or a runtime-internal abort.

Result: debugging an unhandled raise requires bisection with prints —
each iteration a full rebuild — and you can't even tell the bug *kind*
until you've narrowed it to a single statement.

## Suggested fix

Install a `std::set_terminate` handler in `ex_stack` startup that runs
*before* `abort()` and emits a useful diagnostic. On the C++ side this
hooks the same path libc++abi already uses to print the
"terminating due to..." line, but with full access to the in-flight
exception object.

Outline:

1. **Install handler** in `RTStackC.c` (one of the per-target files) at
   first entry, e.g. from `RTStack__Init` or via a constructor.

2. **Recover the in-flight `_M3Exc`** via the C++ ABI:
   `__cxxabiv1::__cxa_current_exception_type()` and
   `__cxxabiv1::__cxa_get_globals()->caughtExceptions` — or, more
   portably, rethrow inside a `try { throw; } catch (_M3Exc& e) { ... }`
   block from the terminate handler, which is the standard idiom.

3. **Print the M3 exception identity.** `_M3Exc.act` carries the
   `ActivationRecord` whose `info` points at the M3 exception
   descriptor. The descriptor's name (already a symbol the linker
   resolved) is what `RAISE Foo` users see — print that. If the
   exception carries a value of a printable type, attempt to render it
   (best-effort; fall back to `<value>`).

4. **Walk and symbolize the stack.** `ex_stack` already depends on
   libunwind; the same primitives (`unw_init_local`, `unw_step`,
   `unw_get_proc_name`, `unw_get_proc_info`) work here. On Darwin these
   are part of `libSystem.B.dylib` — no extra link flags. Symbolize
   each frame via `dladdr` (already used by the runtime) or
   `backtrace_symbols`.

5. **Then `abort()`** so cores still drop and signal-aware tooling
   (lldb, crash reporter) still triggers.

Even *just* steps 1–3 (exception identity, no backtrace) would handle
most cases — most M3 unhandled raises are diagnosed once you know which
exception tag fired.

## Scope

- **`ex_stack` only.** The `setjmp`/`longjmp` path (`ex_frame`) has no
  unwinder access; uncaught exceptions there go through
  `RTException.DefaultBackstop` which already has more context.
- **Touch points:** `m3-libs/m3core/src/runtime/<TARGET>/RTStackC.c`
  for each `HAS_STACK_WALKER` target (currently `AMD64_LINUX`,
  `AMD64_DARWIN`, `ARM64_DARWIN`).
- **No IR / front-end / config changes.** Pure runtime-library work.

## Why it's high-leverage

Every front-end / compiler-internals debug session that hits an
unhandled M3 exception today loses several rebuild cycles to print
bisection. A one-line "exception is `RuntimeError.NarrowFailed` at
`Formal.m3:124` (`Formal.Split+0x14`)" output would collapse most of
those sessions to a single run. Came up directly while wiring the MSIR
builder into m3front.
