(* Copyright 1996, Critical Mass, Inc.   All rights reserved. *)

UNSAFE MODULE RTHeapDep;

IMPORT RTHeapRep, RTMachine, (**RTOS,**) WinBase, WinDef, WinNT, Word;

VAR
  initialized := FALSE;
  old_handler : WinBase.PTOP_LEVEL_EXCEPTION_FILTER := NIL;

PROCEDURE Protect (p: Page;  n: CARDINAL;  readable, writable: BOOLEAN) =
  TYPE ZZ = ARRAY BOOLEAN OF WinDef.DWORD;
  CONST
    Access = ARRAY BOOLEAN OF ZZ {   (*write=FALSE*)       (*write=TRUE*)
               (*read=FALSE*) ZZ { WinNT.PAGE_NOACCESS, WinNT.PAGE_NOACCESS  },
               (*read=TRUE*)  ZZ { WinNT.PAGE_READONLY, WinNT.PAGE_READWRITE }};
  VAR old_access: WinDef.DWORD;
  BEGIN
    IF NOT initialized THEN
      old_handler := WinBase.SetUnhandledExceptionFilter (FaultHandler);
      initialized := TRUE;
    END;
    IF WinBase.VirtualProtect (p * BytesPerPage, n * BytesPerPage,
                               Access[readable][writable],
                               ADR (old_access)) = 0 THEN
      <*ASSERT FALSE*>
    END;
  END Protect;

<*WINAPI*>
PROCEDURE FaultHandler (info: WinNT.PEXCEPTION_POINTERS): WinDef.LONG =
  VAR
    desc := info.ExceptionRecord;
    err  := desc.ExceptionCode;
    (*** pc   := LOOPHOLE (desc.ExceptionAddress, INTEGER); ***)
    (*** is_write : BOOLEAN := FALSE; **)
    bad_addr : ADDRESS := NIL;
  BEGIN
    IF (err = WinBase.EXCEPTION_ACCESS_VIOLATION) THEN
      IF (desc.NumberParameters >= 2) THEN
        (*** is_write := desc.ExceptionInformation [0] # 0;  ***)
        bad_addr := LOOPHOLE (desc.ExceptionInformation[1], ADDRESS);
        IF RTHeapRep.Fault (bad_addr)  (* make it readable *)
          AND RTHeapRep.Fault (bad_addr) THEN (* make it writable *)
          RETURN -1; (* == EXCEPTION_CONTINUE_EXECUTION in EXCPT.H *)
        END;
      END;
    END;
    IF (old_handler # NIL)
      THEN RETURN old_handler (info);
      ELSE RETURN 0; (* == EXCEPTION_CONTINUE_SEARCH in EXCPT.H *)
    END;
  END FaultHandler;

(*----------------------------------------- Timers for tuning the collector ---*)

VAR
  time_used : REAL := 0.0;  (* seconds *)
  last_tick : WinDef.DWORD := WinBase.GetTickCount ();

PROCEDURE TimeUsed (): REAL =
  (* NOTE: we're supposed to return the process time, not the system
     time.  But, Win95 doesn't support GetProcessTimes().... *)
  CONST
    H = FLOAT(LAST(INTEGER)) + 1.0;
    H2 = H * 0.002;
  VAR
    tick := WinBase.GetTickCount ();  (* milliseconds *)
    diff := Word.Minus (tick, last_tick);
  BEGIN
    IF (diff >= 0) THEN
      time_used := time_used + (FLOAT (diff) * 0.001);
    ELSE
      time_used := time_used + H2 + (FLOAT (diff) * 0.001);
    END;
    last_tick := tick;
    RETURN time_used;
  END TimeUsed;

PROCEDURE VMFaultTime (): REAL =
  BEGIN
    RETURN 0.010;                (* guess 10ms to handle a page fault *)
  END VMFaultTime;

(*---------------------------------- M3 => external DLL parameter validation ---*)

(***
VAR in_check := 0;
***)

PROCEDURE CheckArgs (mask: INTEGER;  <*UNUSED*> ret_addr: ADDRESS;  args: INTEGER) =
  VAR
    arg_ptr: UNTRACED REF INTEGER := ADR (args);
    arg, p, min_heap, max_heap: INTEGER;
  BEGIN
    (***
    IF in_check > 0 THEN RETURN; END;
    INC (in_check);
    ***)

    (*** RTOS.LockHeap (); --- can't cause it makes a system call with
                               an check address parameter => infinite recusion ***)
      min_heap := RTHeapRep.p0 * BytesPerPage;
      max_heap := RTHeapRep.p1 * BytesPerPage;
      WHILE (mask # 0) DO
        IF Word.And (mask, 1) # 0 THEN (* check this arg *)
          arg := arg_ptr^;
          IF (min_heap <= arg) AND (arg < max_heap) THEN
            (* the pointer might be pointing to a heap page! *)
            p := Word.RightShift ((arg - min_heap), LogBytesPerPage);
            WITH z = RTHeapRep.desc [p] DO
              IF (z.protected) AND (z.space # RTHeapRep.Space.Unallocated) THEN
                (* it looks like a bad page => pretend we dereferenced the arg *)
                EVAL RTHeapRep.Fault (LOOPHOLE (arg, ADDRESS))  (* make readable *)
                 AND RTHeapRep.Fault (LOOPHOLE (arg, ADDRESS)); (* make writable *)
              END;
            END;
          END;
        END;
        mask := Word.RightShift (mask, 1);
        INC (arg_ptr, ADRSIZE (ADDRESS));
      END;
    (*** RTOS.UnlockHeap (); ***)

    (***
    DEC(in_check);
    ***)
  END CheckArgs;

BEGIN
  RTMachine.m3_arg_check := LOOPHOLE (CheckArgs, ADDRESS);
  (* let the stub routines that call external DLLs know where
     the parameter validation routine is hiding... *)
END RTHeapDep.
