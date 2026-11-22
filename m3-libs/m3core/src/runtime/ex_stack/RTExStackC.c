/* The target-independent part of native C++ exception handling (ex_stack).
   A Modula-3 raise is a C++ throw of _M3Exc carrying the RT0.RaiseActivation;
   the "catch (_M3Exc& ...)" clauses that M3C generates dispatch it.  Only the
   stack dump (RTExStack.PrintStack) needs a target's stack walker
   (RTStackC.c in the target's directory); raising does not. */

#ifndef __cplusplus
#error ex_stack needs C sources compiled as C++ (SYSTEM_CC with -x c++)
#endif

/* _M3Exc is the C++ exception type thrown by RTStack__ThrowM3Exc and caught
   by the "catch (_M3Exc& _m3exc)" clauses that M3C.m3 generates in each
   translation unit.  It must be a proper C++ struct (not inside extern "C")
   so that C++ EH type-matching works correctly.  The identical definition
   appears in the M3C.m3 preamble for generated translation units; ODR is
   satisfied because the layout is the same everywhere. */
struct _M3Exc { void* act; };

/* RTStack__ThrowM3Exc is defined outside any extern "C" block so that the
   C++ compiler's exception-handling machinery is fully engaged.  The
   extern "C" on the definition gives the symbol C linkage so that M3 code
   can call it via the EXTERNAL pragma in RTStack.i3. */
extern "C" void RTStack__ThrowM3Exc(void* act) {
  throw _M3Exc{act};
}
