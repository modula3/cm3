(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Nov 18 16:00:55 PST 1994 by kalsow     *)
(*      modified on Wed Jan 27 22:49:37 PST 1993 by mjordan    *)

UNSAFE MODULE RTOS;

IMPORT Unix, Uuio, Cstdlib, RT0u, Thread, Umman, Word;

(*--------------------------------------------------- process termination ---*)

PROCEDURE Exit (n: INTEGER) =
  BEGIN
    Unix.exit (n);
  END Exit;

PROCEDURE Crash () =
  BEGIN
    Cstdlib.abort ();
    LOOP END; (* wait *)
  END Crash;

(*------------------------------------------------------------- allocator ---*)

PROCEDURE GetMemory (size: INTEGER): ADDRESS =
  (* Return the address of "size" bytes of unused storage *)
  BEGIN
    WITH addr = LOOPHOLE(0, ADDRESS),
         prot = Word.Or(Umman.PROT_READ, Umman.PROT_WRITE),
         flags = Word.Or(Umman.MAP_ANON, Umman.MAP_PRIVATE) DO
      RETURN Umman.mmap(addr, size, prot, flags, -1, 0);
    END;
  END GetMemory;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector.  This is the Ultrix version, and depends on the Ultrix
   user-level thread implementation. *)

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
  lock_cnt  := 0;      (* LL = RT0u.inCritical *)
  do_signal := FALSE;  (* LL = RT0u.inCritical *)
  mutex     := NEW(MUTEX);
  condition := NEW(Thread.Condition);

PROCEDURE LockHeap () =
  BEGIN
    INC(RT0u.inCritical);
    INC(lock_cnt);
  END LockHeap;

PROCEDURE UnlockHeap () =
  VAR sig := FALSE;
  BEGIN
    DEC(lock_cnt);
    IF (lock_cnt = 0) AND (do_signal) THEN sig := TRUE; do_signal := FALSE; END;
    DEC(RT0u.inCritical);
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

PROCEDURE Write (a: ADDRESS;  n: INTEGER) =
  BEGIN
    EVAL Uuio.write (2, a, n);
  END Write;

BEGIN
END RTOS.
