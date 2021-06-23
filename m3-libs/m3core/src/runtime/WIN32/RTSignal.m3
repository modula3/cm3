(* Copyright 1996-2000, Critical Mass, Inc.   All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE RTSignal;

IMPORT RT0, RTMachInfo, RTError, RTException, RTProcess, RuntimeError;
IMPORT WinBase, WinCon, WinDef, WinNT, ThreadContext, Thread, RTThread;

TYPE
  RTE = RuntimeError.T;

VAR
  old_filter: WinBase.PTOP_LEVEL_EXCEPTION_FILTER := NIL;

PROCEDURE InstallHandlers () =
  BEGIN
    old_filter := WinBase.SetUnhandledExceptionFilter (RuntimeFilter);
    RTProcess.RegisterInterruptSetup (InstallInterruptHandler,
                                      RestoreInterruptHandler);
  END InstallHandlers;

PROCEDURE RestoreHandlers () =
  BEGIN
    EVAL WinBase.SetUnhandledExceptionFilter (old_filter);
  END RestoreHandlers;

<*WINAPI*>
PROCEDURE RuntimeFilter (info: WinNT.PEXCEPTION_POINTERS): WinDef.LONG =
  VAR
    desc := info.ExceptionRecord;
    err  := desc.ExceptionCode;
    pc   := LOOPHOLE (desc.ExceptionAddress, INTEGER);
    ctxt := LOOPHOLE (info.ContextRecord, ThreadContext.PCONTEXT);
  BEGIN
    IF (err = WinBase.CONTROL_C_EXIT) THEN
      VAR h := RTProcess.OnInterrupt (NIL); BEGIN
        IF (h # NIL) THEN
          EVAL RTProcess.OnInterrupt (h); (* reinstall the handler *)
          h (); (* invoke the user's CTL-C handler *)
          RETURN -1; (* == EXCEPTION_CONTINUE_EXECUTION in EXCPT.H *)
        END;
      END;
    END;

    (* try converting the system exception into an M3 exception *)

    (* look for a known error... *)
    FOR i := FIRST (SysErrs) TO LAST (SysErrs) DO
      IF (SysErrs[i].err = err) THEN
        IF Raise (SysErrs[i].rte, err, pc, ctxt) THEN
          RETURN -1; (* == EXCEPTION_CONTINUE_EXECUTION in EXCPT.H *)
        END;
        EXIT;
      END;
    END;

    (* try reporting a mysterious error *)
    IF Raise (RTE.Unknown, err, pc, ctxt) THEN
      RETURN -1; (* == EXCEPTION_CONTINUE_EXECUTION in EXCPT.H *)
    END;

    (* Otherwise, just dump our guts... *)
    RTThread.SuspendOthers ();
    FOR i := FIRST (SysErrs) TO LAST (SysErrs) DO
      IF (SysErrs[i].err = err) THEN
        RTError.ReportPC (pc, SysErrs[i].msg);
        IF (ctxt # NIL) AND (ctxt.Ebp # 0) THEN
          RTMachInfo.DumpStack (LOOPHOLE (pc, ADDRESS),
                                LOOPHOLE (ctxt.Ebp, ADDRESS));
        END;
        EXIT;
      END;
    END;
    RTThread.ResumeOthers ();
(***
    IF (old_filter # NIL)
      THEN RETURN old_filter (info);
      ELSE RTProcess.Crash (NIL);
    END;
***)
    RETURN 0; (* == EXCEPTION_CONTINUE_SEARCH in EXCPT.H *)
  END RuntimeFilter;

PROCEDURE Raise (rte: RTE;  err, pc: INTEGER;
                 ctxt: ThreadContext.PCONTEXT): BOOLEAN =
(* Alter the thread's stack and PC so that when it resumes, it looks
   like it magically called "SystemError (rte, err, pc)" *)
  TYPE Frame = RECORD ret_pc, rte, err, pc: INTEGER; END;
  VAR f: UNTRACED REF Frame;
  BEGIN
    IF (ctxt = NIL) THEN RETURN FALSE; END;

    f := LOOPHOLE (ctxt.Esp - ADRSIZE (Frame), UNTRACED REF Frame);
    IF WinBase.IsBadWritePtr (f, BYTESIZE(f^)) # 0 THEN
      (* it doesn't look like we can push the arguments on the stack *)
      RETURN FALSE;
    END;

    f.ret_pc := pc;
    f.rte    := ORD (rte);
    f.err    := err;
    f.pc     := pc;

    ctxt.Esp := LOOPHOLE (f, INTEGER);
    ctxt.Eip := LOOPHOLE (SystemError, INTEGER);

    RETURN TRUE;
  END Raise;

<*CALLBACK*>  (* so the fake call doesn't need to pop its arguments *)
PROCEDURE SystemError (rte: INTEGER;  err: INTEGER;  pc: ADDRESS) RAISES ANY =
  VAR a: RT0.RaiseActivation;
  BEGIN
    a.exception   := RuntimeError.Self ();
    a.arg         := LOOPHOLE (rte, RT0.ExceptionArg);
    a.module      := NIL;
    a.line        := 0;
    a.pc          := pc;
    a.info0       := LOOPHOLE (err, ADDRESS);
    a.info1       := NIL;
    a.un_except   := NIL;
    a.un_arg      := NIL;
    RTException.Raise (a);
  END SystemError;


TYPE XX = RECORD err: INTEGER;  msg: TEXT;  rte: RTE;  END;
CONST SysErrs = ARRAY OF XX {
  XX{WinBase.EXCEPTION_ACCESS_VIOLATION,
     "Illegal memory access",                 RTE.BadMemoryReference},
  XX{WinBase.EXCEPTION_ARRAY_BOUNDS_EXCEEDED,
     "Array bounds violation",                RTE.SubscriptOutOfRange },
  XX{WinBase.EXCEPTION_FLT_DENORMAL_OPERAND,
     "Denormalized floating-point operand",   RTE.FloatInvalid},
  XX{WinBase.EXCEPTION_FLT_DIVIDE_BY_ZERO,
     "Floating-point divide by zero",         RTE.FloatDivideByZero},
  XX{WinBase.EXCEPTION_FLT_INEXACT_RESULT,
     "Inexact floating-point result",         RTE.FloatInexact},
  XX{WinBase.EXCEPTION_FLT_INVALID_OPERATION,
     "Invalid floating-point operation",      RTE.SystemError},
  XX{WinBase.EXCEPTION_FLT_OVERFLOW,
     "Floating-point overflow",               RTE.FloatOverflow},
  XX{WinBase.EXCEPTION_FLT_STACK_CHECK,
     "Floating-point stack check",            RTE.SystemError},
  XX{WinBase.EXCEPTION_FLT_UNDERFLOW,
     "Floating-point underflow",              RTE.FloatUnderflow},
  XX{WinBase.EXCEPTION_INT_DIVIDE_BY_ZERO,
     "Integer divide by zero",                RTE.IntegerDivideByZero},
  XX{WinBase.EXCEPTION_INT_OVERFLOW,
     "Integer overflow",                      RTE.IntegerOverflow},
  XX{WinBase.EXCEPTION_PRIV_INSTRUCTION,
     "Privileged instruction attempted",      RTE.PrivilegedInstruction},
  XX{WinBase.CONTROL_C_EXIT,
     "Control-C interrupt",                   RTE.SystemError }
  };

(*----------------------------------------------- Control C handling ---*)

TYPE
  CtlCClosure = Thread.Closure OBJECT OVERRIDES apply := ControlCMonitor; END;

VAR
  control_c_count   : INTEGER      := 0;
  control_c_signal  : WinNT.HANDLE := NIL;
  control_c_monitor : Thread.T     := NIL;

PROCEDURE InstallInterruptHandler () =
  BEGIN
    IF (control_c_monitor = NIL) THEN
      control_c_signal := WinBase.CreateSemaphore (NIL, 0, 1, NIL);
      control_c_monitor := Thread.Fork (NEW (CtlCClosure));
    END;
    EVAL WinCon.SetConsoleCtrlHandler (ConsoleHandler, 1 (* Add := TRUE *));
  END InstallInterruptHandler;

PROCEDURE RestoreInterruptHandler () =
  BEGIN
    EVAL WinCon.SetConsoleCtrlHandler (ConsoleHandler, 0 (* Add := FALSE *));
  END RestoreInterruptHandler;

<*WINAPI*>
PROCEDURE ConsoleHandler (signal: WinDef.DWORD): WinDef.BOOL =
  VAR prevCount: WinDef.LONG;
  BEGIN
    IF (signal = WinCon.CTRL_C_EVENT) THEN
      INC (control_c_count);
      EVAL WinBase.ReleaseSemaphore (control_c_signal, 1, ADR(prevCount));
      RETURN 1; (* TRUE => handled *)
    END;
    RETURN 0; (* FALSE => not handled *)
  END ConsoleHandler;

PROCEDURE ControlCMonitor (<*UNUSED*> cl: Thread.Closure): REFANY =
  VAR cnt, new_cnt: INTEGER;  h: RTProcess.InterruptHandler;
  BEGIN
    cnt := control_c_count;
    LOOP
      EVAL WinBase.WaitForSingleObject (control_c_signal, WinBase.INFINITE);
      new_cnt := control_c_count;
      IF (new_cnt # cnt) THEN
        h := RTProcess.OnInterrupt (NIL);
        IF (h # NIL) THEN
          EVAL RTProcess.OnInterrupt (h); (* reinstall the handler *)
          h (); (* invoke the user's CTL-C handler *)
        END;
        new_cnt := cnt;
      END;
    END;
  END ControlCMonitor;

BEGIN
END RTSignal.
