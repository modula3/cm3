# Native C++ Exception Handling for ARM64_DARWIN, AMD64_DARWIN, and AMD64_LINUX (`ex_stack`)

This document describes the native stack-unwinding exception handling
implementation added for the Darwin ports and AMD64_LINUX, explains
the changes made to the compiler and runtime, and documents the
bootstrap process required to transition from the old `ex_frame`
(setjmp/longjmp) implementation to the new `ex_stack` (C++
throw/catch) implementation.

---

## Background

CM3 has historically used a portable "frame-linked" exception handling
mechanism (`ex_frame`) in which:

- Each `TRY` block pushes a `jmp_buf` onto a per-thread singly-linked
  list.
- `RAISE` walks that list, calling `longjmp` to jump to the matching
  handler.
- `m3front` compiles `TRY/EXCEPT` via `Compile2`, which emits `setjmp`
  calls and the associated frame setup/teardown.

This approach is fully portable but has non-trivial overhead on every
`TRY` entry (a `setjmp` call) and does not interact with C++
destructors or other language runtimes sharing the same stack.

The `ex_stack` implementation replaces this with **C++ zero-cost
exception handling**:

- `RAISE` calls `RTStack__ThrowM3Exc`, a C++ function that executes
  `throw _M3Exc{act}`, putting the M3 activation record into a C++
  exception.
- The C++ runtime unwinds the stack using platform DWARF/EH tables,
  landing in `catch (_M3Exc& _m3exc)` clauses that `M3C.m3` generates
  around every call site inside a `TRY` block.
- `m3front` compiles `TRY/EXCEPT` via `Compile1`, which emits
  `CG.Start_try()` / `CG.End_try()` / `CG.Invoke_direct()` /
  `CG.Landing_pad()` instead of setjmp.

The result is that `TRY` blocks have zero overhead when no exception
is raised; cost is only incurred during unwinding.

---

## Requirement: C Backend (`M3_BACKEND_MODE = "C"`)

The `ex_stack` EH model **requires the C backend**.

`Compile1` in `m3front` emits `CG.Start_try()`, `CG.End_try()`,
`CG.Invoke_direct()`, and `CG.Landing_pad()`.  These operations are
implemented only in `M3C.m3` (the C code generator).  They cause
`M3C.m3` to emit C++ `try { … } catch (_M3Exc& _m3exc) { … }` blocks,
which the platform C++ compiler (clang/g++) then lowers to DWARF EH
tables.

The native x86 assembly backend (`M3_BACKEND_MODE = "3"`, used by
`M3x86`) does **not** implement these operations.  Enabling
`M3_USE_STACK_WALKER` with a native backend would crash the compiler
at code-generation time on any `TRY` statement.

**Always pair `M3_USE_STACK_WALKER = TRUE` with `M3_BACKEND_MODE =
"C"`.**

Both Darwin targets discussed here use `M3_BACKEND_MODE = "C"`, as
does `AMD64_LINUX`.

---

## Files Changed

### Runtime — `m3-libs/m3core`

#### `src/runtime/ARM64_DARWIN/RTMachine.i3`
Declares `Has_stack_walker = TRUE` and the extended `FrameInfo` record
that holds libunwind cursor state (`cursor`, `start_ip`, `end_ip`,
`lsda`, `persFn`, `landingPad`, `excRef`, `tTypeIndex`).  The
`Has_stack_walker` constant is hardcoded `TRUE` for `ARM64_DARWIN`,
meaning the runtime uses C++ throw/catch support when `ex_stack` is
selected.

#### `src/runtime/ARM64_DARWIN/RTStackC.c`
Implements all stack-walking primitives using Apple's built-in
libunwind (part of `libSystem.B.dylib`; no extra link flags needed):

- `RTStack__CurFrame` — captures the current unwinder cursor.
- `RTStack__PrevFrame` — steps one frame up the stack.
- `RTStack__Unwind` — resumes execution at a specific landing pad
  (used in the manual-unwind path; not used by the C++ throw path).
- `RTStack__ThrowM3Exc(void* act)` — the key entry point.  Defined
  outside `extern "C"` so the C++ compiler's full EH machinery is
  engaged:

  ```c
  struct _M3Exc { void* act; };

  extern "C" void RTStack__ThrowM3Exc(void* act) {
      throw _M3Exc{act};
  }
  ```

  The `_M3Exc` struct must be declared in the same translation unit
  that contains the `catch (_M3Exc& …)` clauses.  `M3C.m3` emits the
  struct definition at the top of every generated `.c` file.

#### `src/runtime/AMD64_DARWIN/RTMachine.i3`
The `AMD64_DARWIN` port adds the corresponding x86_64 macOS runtime
description.  Like the ARM64 version, it declares `Has_stack_walker =
TRUE` and uses the same `FrameInfo` layout for libunwind state.

#### `src/runtime/AMD64_DARWIN/RTStackC.c`
Implements the same stack-walking primitives and `RTStack__ThrowM3Exc`
entry point for `AMD64_DARWIN`.

The implementation is effectively identical to the ARM64 file: Apple's
libunwind API on x86_64 macOS is the same interface used on arm64, and
the exception throw path is the same C++ `throw _M3Exc{act}` model.

All five verification tests pass under Rosetta 2.

#### `src/runtime/ex_stack/RTExStack.m3`
Implements `RTException.Raise` and `RTException.ResumeRaise` for the
`ex_stack` runtime.  Key points:

- The `RaiseActivation` record must be **heap-allocated** before
  throwing, because the C++ unwinder destroys the original stack frame
  before landing in the catch clause.  A stack-allocated `act` would
  dangle:

  ```modula3
  excRef := NEW(REF RaiseActivation);
  excRef^ := act;
  RTStack.ThrowM3Exc(ADR(excRef^));
  ```

- `RTStack.ThrowM3Exc` never returns; the call to `InvokeBackstop`
  below it is an unreachable safety net only.

- The assertion `<*ASSERT RTStack.Has_walker*>` at module
  initialisation verifies that `ex_stack` is never accidentally linked
  into a build that has `Has_stack_walker = FALSE`.

#### `src/runtime/AMD64_LINUX/RTStackC.c`
Previously had the libunwind frame-walking primitives but was missing
the C++ throw entry point.  Added `struct _M3Exc { void* act; }`
(outside the `extern "C"` block) and `RTStack__ThrowM3Exc` (after the
closing `extern "C"` brace), identical in structure to the Darwin
versions.

#### `src/runtime/m3makefile`
The `HAS_STACK_WALKER` quake table controls which targets can use
`ex_stack`:

```quake
readonly HAS_STACK_WALKER = {
  "AMD64_LINUX"   : TRUE,
  "AMD64_DARWIN"  : TRUE,
  "ARM64_DARWIN"  : TRUE,
}
```

When `M3_USE_STACK_WALKER = TRUE` is set in the platform config
**and** the target appears in `HAS_STACK_WALKER`,
`include_dir("ex_stack")` and `include_dir(TARGET)` are used instead
of `include_dir("ex_frame")`.

---

### Compiler front-end — `m3-sys/m3front`

The `Compile1` code path (enabled by `Target.Has_stack_walker`, set
from the `M3_USE_STACK_WALKER` config variable) required three
correctness fixes.

#### `src/stmts/TryStmt.m3`
**Problem**: The original `Compile1` passed `last := TRUE` to
`CompileHandler1` for the final handler in a `TRY/EXCEPT` chain.  With
`last = TRUE` the type check was skipped, so the catch clause accepted
*any* M3 exception — silently routing unmatched exceptions (e.g.,
`OSError.E`) to the wrong handler (e.g., `Thread.Alerted`).

**Fix**:
1. Always pass `last := FALSE` to `CompileHandler1`, ensuring every
   handler emits an explicit type check.
2. Add a mandatory `ELSE` branch at the end of the handler chain.
   When no handler matches, the `ELSE` branch calls
   `RTHooks.ResumeRaiseEx` to re-throw the exception so enclosing
   `TRY` blocks or the module-body backstop can handle it.

Without this fix, misrouted exceptions produced "Fatal Error:
interrupted" instead of the correct runtime error.

#### `src/stmts/TryFinStmt.m3`
**Problem**: `CG.End_try()` was placed *after* `CG.Set_label(l+1)` —
putting the FINALLY body *inside* the try region.  Any exception
raised in the FINALLY block (including the re-raise of the original
exception) would be caught by the same `catch` clause, causing
infinite recursion.

**Fix**: Move `CG.End_try()` to just *before* `CG.Set_label(l+1)` so
the FINALLY body executes outside the try region.

#### `src/stmts/LockStmt.m3`
**Problem**: The same `End_try` placement issue in the `LOCK`
statement's `Compile1` path.  The mutex-release code and any exception
handler ran inside the try region.

**Fix**: Move `CG.End_try()` before `CG.Set_label(lab+1)` so the
lock-release and handler code are outside the try region.

---

### Compiler back-end — `m3-sys/m3back`

#### `src/M3C.m3`
**Problem**: `RTStack.ThrowM3Exc` is declared `<*EXTERNAL*>` with C
calling convention and returns void.  Without special treatment the C
code generator did not mark it as `[[noreturn]]`, so the C compiler
emitted spurious "control reaches end of non-void function" warnings
and potentially generated incorrect code for the callers of
`RTExStack.Raise`.

**Fix**: Detect `RTStack__ThrowM3Exc` by name in `import_procedure` and set
`proc.no_return := TRUE`, causing `M3C.m3` to emit the
`__attribute__((noreturn))` (or `[[noreturn]]`) annotation in the generated
C prototype.

The relevant fields and code added to `M3C.m3`:

```modula3
(* in the T record fields, ~line 96 *)
RTStack_ThrowM3Exc_id : M3ID.T;

(* in begin_unit, ~line 3155 *)
self.RTStack_ThrowM3Exc_id := M3ID.Add("RTStack__ThrowM3Exc");

(* in import_procedure, ~line 6096 *)
ELSIF name = self.RTStack_ThrowM3Exc_id THEN
  proc.no_return := TRUE;
  no_return(self);
```

The `start_try`, `end_try`, `invoke_direct`, `invoke_indirect`, and
`landing_pad` procedures in `M3C.m3` emit the C++ try/catch structure.
`invoke_direct` replaces `call_direct` inside a try region: instead of
a plain C function call it emits a C++ `try { f(…); goto next; } catch
(_M3Exc& _m3exc) { goto handler; }` pattern.

---

### Config — `m3-sys/cminstall/src/config/ARM64_DARWIN` and `AMD64_DARWIN`

Both Darwin targets use the same config requirements for `ex_stack`:

```quake
M3_BACKEND_MODE = "C"        % must be before includes
M3_USE_STACK_WALKER = TRUE   % must be before includes

% ... include target/common config here ...

% Keep Rosetta-based bootstrap builds single-threaded.
M3_PARALLEL_BACK = 1

% libunwind is part of libSystem.B.dylib on macOS — no extra link flags.
SYSTEM_LIBS{"UNWIND"} = []
```

`M3_BACKEND_MODE` and `M3_USE_STACK_WALKER` must be set **before** the
target config includes so the guards in the common Darwin config files
see the correct values.

`SYSTEM_LIBS{"UNWIND"} = []` must be set unconditionally (outside any
`if defined("M3_USE_STACK_WALKER")` guard) so that the `import_sys_lib
("UNWIND")` call in `m3core/src/runtime/m3makefile` does not fail with
a "does not define a value for UNWIND" error.

On `AMD64_DARWIN`, `M3_PARALLEL_BACK = 1` must come **after** the
includes, because `AMD64.common` resets it to 20.

---

## Bootstrap Process

Transitioning from `ex_frame` to `ex_stack` cannot be done in a single
step because the old compiler binary (statically linked with the old
m3back) cannot compile the new `RTExStack.m3`.  A two-phase bootstrap
is required.

### Phase 1 — Rebuild with `ex_frame` (setjmp), using fixed m3back

**Goal**: produce a new `cm3` binary whose statically-linked `m3back`
contains all our fixes (noreturn detection, `start_try`/`end_try`
generation), but is still linked against the safe `ex_frame` runtime.

1. Temporarily disable `M3_USE_STACK_WALKER` in the platform config.
   This causes `m3core/src/runtime/m3makefile` to select `ex_frame`;
   the new `RTExStack.m3` (which imports `RTStack.ThrowM3Exc`) is
   never compiled, avoiding the assertion in the old binary's m3back.

2. Using the Stage 3 backup binary:
   ```
   cd m3-libs/m3core   && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-libs/libm3    && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3middle  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3linker  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3front   && cm3 -clean && cm3 -build && cm3 -ship   # includes TryStmt/LockStmt/TryFinStmt fixes
   cd m3-sys/m3quake   && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3objfile && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3back    && cm3 -clean && cm3 -build && cm3 -ship   # includes noreturn fix
   cd m3-sys/cm3       && cm3 -clean && cm3 -build && cm3 -ship
   ```
   The resulting `cm3` binary has all fixes compiled in and links against
   the `ex_frame` runtime — it is fully functional for Phase 2.

3. Install the new binary as the active `cm3`.

### Why the Stage 3 binary cannot compile the new `RTExStack.m3` directly

The Stage 3 backup binary has an old `m3back` statically linked that
contains an assertion:

```
<* ASSERT NOT proc.is_RTException_Raise *>
```

This fires because `RTException.Raise` (declared in `RTExStack.m3`) is
detected as `is_RTException_Raise` in `declare_procedure`.  In the old
`ex_frame` implementation, `Raise` had a setjmp-based body that the
old m3back knew how to handle.  The new `ex_stack` body — which calls
`RTStack.ThrowM3Exc` inside a `RAISES ANY` scope — triggers a
different code-generation path in old m3back that trips the assertion.

The Phase 1 bootstrap sidesteps this by never asking the old m3back to
compile `RTExStack.m3` with `M3_USE_STACK_WALKER` active.  Once the
Phase 1 binary is built, its internal m3back has the fixed code and
does not contain the problematic assertion.

### Phase 2 — Rebuild with `ex_stack` (C++ throw), using Phase 1 binary

**Goal**: produce a fully `ex_stack` `cm3` binary.

1. Re-enable `M3_USE_STACK_WALKER = TRUE` in the platform config.

2. Using the Phase 1 binary (built above), rebuild in the same order:
   ```
   cd m3-libs/m3core   && cm3 -clean && cm3 -build && cm3 -ship   # ex_stack runtime
   cd m3-libs/libm3    && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3middle  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3linker  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3front   && cm3 -clean && cm3 -build && cm3 -ship   # Compile1 path active
   cd m3-sys/m3quake   && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3objfile && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3back    && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/cm3       && cm3 -clean && cm3 -build && cm3 -ship
   ```
   Because the Phase 1 binary has the fixed m3back (with noreturn
   detection), it compiles `RTExStack.m3` and the `RTStack.ThrowM3Exc`
   import without any assertion failures.

3. Install the resulting `cm3` binary.  It now uses C++ zero-cost EH
   throughout.

### Verification

A simple sanity test after Phase 2:

```modula3
MODULE Main;
IMPORT IO, OSError, FileRd, Thread;
EXCEPTION LocalExc(TEXT);

BEGIN
  (* Basic raise/catch *)
  TRY RAISE LocalExc("hello") EXCEPT LocalExc(m) => IO.Put("caught: " & m & "\n") END;

  (* Cross-exception dispatch — must NOT catch wrong type *)
  TRY
    TRY RAISE LocalExc("x") EXCEPT OSError.E => IO.Put("FAIL\n") END;
  EXCEPT LocalExc => IO.Put("reraise ok\n") END;

  (* FINALLY must run even when an exception propagates through *)
  VAR ran := FALSE;
  BEGIN
    TRY TRY RAISE LocalExc("f") FINALLY ran := TRUE END EXCEPT LocalExc => END;
    IF ran THEN IO.Put("finally ok\n") ELSE IO.Put("FAIL\n") END;
  END;

  (* LOCK must release the mutex when an exception escapes *)
  VAR mu := NEW(MUTEX);
  BEGIN
    TRY LOCK mu DO RAISE LocalExc("lock") END EXCEPT LocalExc => IO.Put("lock ok\n") END;
  END;

  (* Thread.Alerted must be caught by the correct handler *)
  TRY
    Thread.Alert(Thread.Self()); Thread.AlertPause(0.0d0);
  EXCEPT Thread.Alerted => IO.Put("alerted ok\n") END;
END Main.
```

These five checks all pass on `ARM64_DARWIN`, and also on
`AMD64_DARWIN` under Rosetta 2.

---

## Portability Notes

### AMD64_LINUX

`AMD64_LINUX` is now fully ported.

Previously it had `Has_stack_walker = TRUE` in its `RTMachine.i3` and
was listed in `HAS_STACK_WALKER`, but was missing
`RTStack__ThrowM3Exc` and the config wiring needed to activate
ex_stack.

The completed work:

1. Added `struct _M3Exc { void* act; }` (outside `extern "C"`) and
   `RTStack__ThrowM3Exc` (after the closing `extern "C"` brace) to
   `src/runtime/AMD64_LINUX/RTStackC.c`
2. In `m3-sys/cminstall/src/config/AMD64_LINUX`: enabled
   `M3_USE_STACK_WALKER = TRUE`, added a guard that sets
   `M3_BACKEND_MODE = "C"` before the includes, and a post-include
   guard that overrides `SYSTEM_CC` to `g++ -x c++` and sets
   `SYSTEM_LIBS{"UNWIND"}`

**Prerequisite:** `libunwind-dev` must be installed:

```sh
apt-get install libunwind-dev    # Debian/Ubuntu
dnf install libunwind-devel      # Fedora/RHEL
```

The config structure uses a single toggle:

```quake
% Comment out to revert to the native GCC backend (mode "3").
M3_USE_STACK_WALKER = TRUE

if defined("M3_USE_STACK_WALKER")
  M3_BACKEND_MODE = "C"          % must be before includes
end

include("AMD64.common")
include("Linux.common")

if defined("M3_USE_STACK_WALKER")
  % M3C.m3-generated files and RTStackC.c both use C++ EH constructs.
  SYSTEM_CC = "g++ -g -m64 -fPIC -x c++"
  SYSTEM_LIBS{"UNWIND"} = [ "-lunwind-x86_64", "-lunwind" ]
end
```

Note that `SYSTEM_LIBS{"UNWIND"}` does **not** need to be set outside
the guard on Linux (unlike Darwin) because `import_sys_lib("UNWIND")`
in `m3makefile` is itself inside the `if M3_USE_STACK_WALKER` block;
the UNWIND entry is only looked up when ex_stack is active.

### AMD64_DARWIN

`AMD64_DARWIN` is now fully ported.

The completed work includes:

1. `m3-libs/m3core/src/runtime/AMD64_DARWIN/RTMachine.i3`
2. `m3-libs/m3core/src/runtime/AMD64_DARWIN/RTStackC.c`
3. `m3-libs/m3core/src/runtime/AMD64_DARWIN/m3makefile`
4. adding `"AMD64_DARWIN" : TRUE` to `HAS_STACK_WALKER`
5. setting `M3_BACKEND_MODE = "C"` and `M3_USE_STACK_WALKER = TRUE` in
   the `AMD64_DARWIN` config

All five verification tests pass under Rosetta 2.

### Other targets

Any target using `M3_BACKEND_MODE = "C"` with a C++ compiler that
supports DWARF EH is a candidate.  The native x86 backend
(`M3_BACKEND_MODE = "3"`) cannot be used.
