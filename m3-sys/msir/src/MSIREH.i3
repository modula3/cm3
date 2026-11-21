(* MSIREH -- lowering of EH scopes (D26) to the C++ EH form MSIRToLLVM emits.

   Rewrites a procedure whose blocks carry EH scopes into one with no scopes,
   in which exceptions flow through invoke / landingpad / resume and calls of
   the runtime's re-raise hook, as m3front built them before D26:
   - a call in a protected block becomes an invoke unwinding to its scope's
     landing pad, a catch-all (so phase 1 of the Itanium search stops in this
     frame);
   - the landing pad only peeks at the M3 activation
     (__cxa_get_exception_ptr); a handler claims the exception with
     __cxa_begin_catch on entry and releases it with __cxa_end_catch on every
     way out;
   - an unmatched exception is re-raised by RTHooks.ResumeRaiseEx, invoked
     to the enclosing landing pad in the same frame (resume would skip it),
     or resumed when there is none;
   - FINALLY code runs from a selector: each way into it stores a code, and
     end_finally dispatches on it to continue the exit, return the saved
     value, or re-raise the saved activation.

   The output contains invoke, landingpad, extractvalue and resume, which
   only this pass may produce (MSIRVerifier rejects them in front-end MSIR). *)

INTERFACE MSIREH;

IMPORT MSIR;

PROCEDURE LowerProc (m: MSIR.Module;  p: MSIR.Proc);
PROCEDURE LowerModule (m: MSIR.Module);

END MSIREH.
