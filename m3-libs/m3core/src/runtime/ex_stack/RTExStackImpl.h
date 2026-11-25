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

/* An _M3Exc that no handler catches reaches std::terminate, which by default
   prints "terminating due to uncaught exception of type _M3Exc".  RTExStack's
   module body installs a terminate handler that recovers the escaping _M3Exc
   by re-throwing it and hands its activation to RTException.ReportUnhandled,
   which prints the usual "Unhandled exception" report and aborts.  It is
   installed by an explicit call, not by a C++ static initializer, whose
   running is less dependable (e.g. under Visual C++ link-time code
   generation). */
#include <exception>
#include <stdlib.h>
extern "C" void RTException__ReportUnhandled(void* act);

static void _m3_terminate_handler(void) {
  try { throw; }
  catch (_M3Exc& e) { RTException__ReportUnhandled(e.act); }
  catch (...) { }
  abort();
}

extern "C" void RTStack__InstallTerminateHandler(void) {
  std::set_terminate(_m3_terminate_handler);
}
