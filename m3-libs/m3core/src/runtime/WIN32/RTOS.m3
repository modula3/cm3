(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Tue Nov 29 14:41:33 PST 1994 by kalsow     *)
(*      modified on Mon Feb 15 16:17:00 PST 1993 by mjordan    *)

UNSAFE MODULE RTOS;

IMPORT RTMachInfo, RTSignal, Thread, ThreadF;
IMPORT WinBase, WinNT, WinCon, WinDef, Word;

(*--------------------------------------------------- process termination ---*)

PROCEDURE Exit (n: INTEGER) =
  BEGIN
    WinBase.ExitProcess (n);
  END Exit;

PROCEDURE Crash () =
  CONST Magic = 1 * ADRSIZE (INTEGER);  (* == offset of "fp" in this frame *)
  VAR fp: ADDRESS := ADR (fp) + Magic;  (* == my frame pointer *)
  BEGIN
    ThreadF.SuspendOthers ();
    RTMachInfo.DumpStack (LOOPHOLE (Crash, ADDRESS), fp);
    RTSignal.RestoreHandlers (); (* so we really do crash... *)
    WinBase.DebugBreak ();
    WinBase.FatalExit (-1);
  END Crash;

(*********************
PROCEDURE Crash () =
  VAR ptr := LOOPHOLE(-99, UNTRACED REF INTEGER);
  BEGIN
    ThreadF.SuspendOthers ();
    ptr^ := 99; (* try to get to the debugger... *)
    WinBase.FatalExit (-1);
  END Crash;
*********************)

(*------------------------------------------------------------- allocator ---*)

(******
PROCEDURE GetMemory (size: INTEGER): ADDRESS =
  (* Return the address of "size" bytes of unused storage *)
  BEGIN
    RETURN WinBase.VirtualAlloc (NIL, size, WinNT.MEM_COMMIT,
                                 WinNT.PAGE_READWRITE);
  END GetMemory;
******)

(******
PROCEDURE GetMemory (size: INTEGER): ADDRESS =
  (* Return the address of "size" bytes of unused storage *)
  BEGIN
    RETURN LOOPHOLE(WinBase.LocalAlloc(WinBase.LMEM_FIXED, size), ADDRESS);
  END GetMemory;
******)

VAR
  reserved_mem  : BOOLEAN := FALSE;
  next_reserved : ADDRESS := NIL;
  page_size     : INTEGER := 8192;
  page_mask     : INTEGER := 8191;

PROCEDURE GetMemory (size: INTEGER): ADDRESS =
  (* Return the address of "size" bytes of unused storage *)
  VAR mem: ADDRESS;
  BEGIN
    IF NOT reserved_mem THEN InitMemory (); END;

    size := Word.And (size + page_size - 1, page_mask);

    IF next_reserved # NIL THEN
      (* try getting some memory from our reserved chunk *)
      mem := WinBase.VirtualAlloc (next_reserved, size, WinNT.MEM_COMMIT,
                                   WinNT.PAGE_READWRITE);
      IF (mem # NIL) THEN
        next_reserved := mem + size;
        RETURN mem;
      END;
      next_reserved := NIL; (* give up on the reserved space *)
    END;

    (* our reserved address space is broken or all used up... *)
    RETURN WinBase.VirtualAlloc (NIL, size, WinNT.MEM_COMMIT,
                                 WinNT.PAGE_READWRITE);
  END GetMemory;

PROCEDURE InitMemory () =
  CONST Reserve = 64 * 1024 * 1024; (* 64MByte *)
  VAR info: WinBase.SYSTEM_INFO;
  BEGIN
    reserved_mem := TRUE;

    (* try to reserve a large contiguous chunk of memory *)
    next_reserved := WinBase.VirtualAlloc (NIL, Reserve,
                       WinNT.MEM_RESERVE, WinNT.PAGE_NOACCESS);

    (* grab the system's page size *)
    WinBase.GetSystemInfo (ADR (info));
    page_size := info.dwPageSize;
    page_mask := Word.Not (page_size - 1);
    (* ASSERT page_size = 2^k for some k *)
  END InitMemory;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector.  This is the Windows/NT version, and depends on the NT
   thread implementation. *)

(* LockHeap() enters a critical section; the same thread may enter the
   critical section multiple times.  It could be written at user level
   as:

| VAR
|   mutex    : MUTEX            := NEW(MUTEX);
|   condition: Thread.Condition := NEW(Thread.Condition);
|   thread   : Thread.T         := NIL;
|   count    : CARDINAL         := 0;

| PROCEDURE LockHeap () =
|   BEGIN
|     LOCK mutex DO
|       IF count = 0 THEN
|         thread := Thread.Self();
|         INC(count);
|       ELSIF thread = Thread.Self() THEN
|         INC(count);
|       ELSE
|         Thread.Wait(mutex, condition);
|       END;
|     END;
|   END LockHeap;

   However, it must be possible to call it from anywhere in the
   collector. *)

(* UnlockHeap() leaves the critical section.  It could be written at user
   level as:

| PROCEDURE UnlockHeap () =
|   BEGIN
|     LOCK mutex DO DEC(count); END;
|     IF count = 0 THEN Thread.Signal(condition); END;
|   END UnlockHeap;

   However, it must be possible to call it from anywhere inside the
   collector. *)


VAR
  cs        : WinBase.LPCRITICAL_SECTION := NIL;
  csstorage : WinNT.RTL_CRITICAL_SECTION;
  lock_cnt  := 0;      (* LL = cs *)
  do_signal := FALSE;  (* LL = cs *)
  mutex     := NEW(MUTEX);
  condition := NEW(Thread.Condition);

PROCEDURE LockHeap () =
  BEGIN
    IF (cs = NIL) THEN
      cs := ADR(csstorage);
      WinBase.InitializeCriticalSection(cs);
    END;
    WinBase.EnterCriticalSection(cs);
    INC(lock_cnt);
  END LockHeap;

PROCEDURE UnlockHeap () =
  VAR sig := FALSE;
  BEGIN
    DEC(lock_cnt);
    IF (lock_cnt = 0) AND (do_signal) THEN sig := TRUE; do_signal := FALSE; END;
    WinBase.LeaveCriticalSection(cs);
    IF (sig) THEN Thread.Broadcast(condition); END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  (* LL = 0 *)
  BEGIN
    LOCK mutex DO Thread.Wait(mutex, condition); END;
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  (* LL = RT0u.inCritical *)
  BEGIN
    do_signal := TRUE;
  END BroadcastHeap;

(*------------------------------------------------------------------- I/O ---*)

VAR
  ready  := FALSE;
  stderr : WinDef.HANDLE;
  (* Perhaps we should explicitly open CONERR$ et al,
     in case of redirection by parent? *)

PROCEDURE Write (a: ADDRESS;  n: INTEGER) =
  VAR nWritten: INTEGER;
  BEGIN
    IF NOT ready THEN
      EVAL WinCon.AllocConsole(); (* make sure we've got one! *)
      stderr := WinBase.GetStdHandle(WinBase.STD_ERROR_HANDLE);
      ready := TRUE;
    END;
    EVAL WinBase.WriteFile(stderr, a, n, ADR(nWritten), NIL);
  END Write;

BEGIN
END RTOS.
