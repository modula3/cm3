(* Copyright 1996, Critical Mass, Inc.   All rights reserved. *)

UNSAFE MODULE RTSignal;

IMPORT RTMachInfo, RTMisc, (** RTProcess,**)  ThreadContext, ThreadF;
IMPORT WinBase, WinDef, WinNT;

VAR
  old_filter: WinBase.PTOP_LEVEL_EXCEPTION_FILTER := NIL;

PROCEDURE InstallHandlers () =
  BEGIN
    old_filter := WinBase.SetUnhandledExceptionFilter (RuntimeFilter);
  END InstallHandlers;

PROCEDURE RestoreHandlers () =
  BEGIN
    EVAL WinBase.SetUnhandledExceptionFilter (old_filter);
  END RestoreHandlers;

PROCEDURE RuntimeFilter (info: WinNT.PEXCEPTION_POINTERS): WinDef.LONG =
  VAR
    desc := info.ExceptionRecord;
    err  := desc.ExceptionCode;
    pc   := LOOPHOLE (desc.ExceptionAddress, INTEGER);
    ctxt := LOOPHOLE (info.ContextRecord, ThreadContext.PCONTEXT);
  BEGIN
    ThreadF.SuspendOthers ();
    FOR i := FIRST (SysErrs) TO LAST (SysErrs) DO
      IF (SysErrs[i].err = err) THEN
        RTMisc.ReportErrorPC (pc, SysErrs[i].msg);
        IF (ctxt # NIL) AND (ctxt.Ebp # 0) THEN
          RTMachInfo.DumpStack (LOOPHOLE (pc, ADDRESS),
                                LOOPHOLE (ctxt.Ebp, ADDRESS));
        END;
        EXIT;
      END;
    END;
    ThreadF.ResumeOthers ();
(***
    IF (old_filter # NIL)
      THEN RETURN old_filter (info);
      ELSE RTProcess.Crash (NIL);
    END;
***)
    RETURN 0; (* == EXCEPTION_CONTINUE_SEARCH in EXCPT.H *)
  END RuntimeFilter;

TYPE XX = RECORD err: INTEGER;  msg: TEXT;  END;
CONST SysErrs = ARRAY OF XX {
  XX{WinBase.EXCEPTION_ACCESS_VIOLATION     , "Illegal memory access"},
  XX{WinBase.EXCEPTION_ARRAY_BOUNDS_EXCEEDED, "Array bounds violation"},
  XX{WinBase.EXCEPTION_FLT_DENORMAL_OPERAND , "Denormalized floating-point operand"},
  XX{WinBase.EXCEPTION_FLT_DIVIDE_BY_ZERO   , "Floating-point divide by zero"},
  XX{WinBase.EXCEPTION_FLT_INEXACT_RESULT   , "Inexact floating-point result"},
  XX{WinBase.EXCEPTION_FLT_INVALID_OPERATION, "Invalid floating-point operation"},
  XX{WinBase.EXCEPTION_FLT_OVERFLOW         , "Floating-point overflow"},
  XX{WinBase.EXCEPTION_FLT_STACK_CHECK      , "Floating-point stack check"},
  XX{WinBase.EXCEPTION_FLT_UNDERFLOW        , "Floating-point underflow"},
  XX{WinBase.EXCEPTION_INT_DIVIDE_BY_ZERO   , "Integer divide by zero"},
  XX{WinBase.EXCEPTION_INT_OVERFLOW         , "Integer overflow"},
  XX{WinBase.EXCEPTION_PRIV_INSTRUCTION     , "Privileged instruction attempted"},
  XX{WinBase.CONTROL_C_EXIT                 , "Control-C interrupt"}
  };

BEGIN
END RTSignal.
