(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE MODULE ThreadWin32 EXPORTS
Thread, ThreadF, Scheduler, RTThread, RTOS, RTHooks, ThreadWin32;

IMPORT RTError, WinGDI, RTParams, FloatMode, RuntimeError;
IMPORT MutexRep, RTHeapRep, RTCollectorSRC, RTIO;
IMPORT ThreadEvent, RTPerfTool, RTProcess, ThreadDebug;
FROM Compiler IMPORT ThisFile, ThisLine;
FROM WinNT IMPORT LONG, HANDLE, DWORD, UINT32;
FROM WinBase IMPORT WaitForSingleObject, INFINITE,
    CloseHandle, CreateThread, ResumeThread, Sleep,
    SuspendThread, GetThreadContext, SetLastError, GetLastError,
    CREATE_SUSPENDED, GetCurrentThreadId, InterlockedCompareExchange,
    InterlockedExchange, InterlockedIncrement, InterlockedDecrement,
    SetEvent, WAIT_OBJECT_0, CreateEvent, WaitForMultipleObjects;
FROM ThreadContext IMPORT PCONTEXT;
FROM WinNT IMPORT MemoryBarrier;

(*----------------------------------------- Exceptions, types and globals ---*)

VAR
  default_stack: DWORD := 8192;

REVEAL
  Mutex = MutexRep.Public BRANDED "MUTEX Win32-1.0" OBJECT
      lock: LockRE_t := NIL;
      held: BOOLEAN := FALSE; (* LL = self.cs *) (* Because critical sections are thread re-entrant *)
    OVERRIDES
      acquire := LockMutex;
      release := UnlockMutex;
    END;

  Condition = BRANDED "Thread.Condition Win32-1.0" OBJECT
      waiters: T := NIL; (* LL = giant; List of threads waiting on this CV. *)
    END;

  T = MUTEX BRANDED "Thread.T Win32-1.0" OBJECT
      act: Activation := NIL;       (* LL = Self(); live (untraced) thread data *)
      closure: Closure := NIL;      (* our work and its result *)
      result: REFANY := NIL;        (* our work and its result *)
      join: Condition := NIL;       (* LL = Self(); wait here to join *)
      waitingOn: Condition := NIL;  (* LL = giant; CV that we're blocked on *)
      nextWaiter: T := NIL;         (* LL = giant; queue of threads waiting on the same CV *)
      joined: BOOLEAN := FALSE;     (* LL = Self(); "Join" or "AlertJoin" has already returned *)
    END;

  REVEAL Activation = UNTRACED BRANDED REF RECORD
      frame: ADDRESS := NIL;            (* exception handling support; this field is accessed MANY times
                                        so perhaps therefore should be first *)
      next, prev: Activation := NIL;    (* LL = activeMu; global doubly-linked, circular list of all active threads *)
      handle: HANDLE := NIL;            (* thread handle in Windows *)
      stackStart: ADDRESS := NIL;       (* stack bounds for use by GC *)
      stackEnd: ADDRESS := NIL;         (* stack bounds for use by GC *)
      slot := 0;                        (* LL = slotLock;  index into global array of active, slotted threads *)
      suspendCount := 0;                (* LL = activeLock *)
      context: PCONTEXT := NIL;         (* registers of suspended thread *)
      stackPointer: ADDRESS := NIL;     (* context->Esp etc. (processor dependent) *)
      waitEvent: HANDLE := NIL;         (* event for blocking during "Wait", "AlertWait", "AlertPause", etc. *)
      alertEvent: HANDLE := NIL;        (* event for blocking during "Wait", "AlertWait", "AlertPause", etc. *)
      heapState: RTHeapRep.ThreadState; (* thread state *)
      floatState: FloatMode.ThreadState; (* thread state *)
    END;

(*----------------------------------------------------------------- Mutex ---*)
(* Note: {Unlock,Lock}Mutex are the routines called directly by
   the compiler.  Acquire and Release are the routines exported through
   the Thread interface *)
         
PROCEDURE Acquire (m: Mutex) =
  BEGIN
    m.acquire ();
  END Acquire;

PROCEDURE Release (m: Mutex) =
  BEGIN
    m.release ();
  END Release;

PROCEDURE LockGiant() =
BEGIN
  LockRE(giantLock);
END LockGiant;

PROCEDURE UnlockGiant() =
BEGIN
  UnlockRE(giantLock);
END UnlockGiant;

PROCEDURE CleanMutex (r: REFANY) =
  VAR m := NARROW(r, Mutex);
  BEGIN
    DeleteLockRE(m.lock);
    m.lock := NIL;
  END CleanMutex;

PROCEDURE InitMutex (VAR m: LockRE_t; root: REFANY;
                     Clean: PROCEDURE(root: REFANY)) =
  BEGIN
    IF InitMutexC(m) # 0 THEN (* We won the race and succeeded. *)
        RTHeapRep.RegisterFinalCleanup (root, Clean);
    ELSIF m = NIL THEN (* We failed and nobody else succeeded at about the same time. *)
        RuntimeError.Raise(RuntimeError.T.OutOfMemory);
    END;
  END InitMutex;

PROCEDURE LockMutex (m: Mutex) =
  BEGIN
    IF perfOn THEN PerfChanged(State.locking) END;
    IF m.lock = NIL THEN InitMutex(m.lock, m, CleanMutex) END;
    LockRE(m.lock);
    IF m.held THEN Die(ThisLine(), "attempt to lock mutex already locked by self") END;
    m.held := TRUE;
    IF perfOn THEN PerfRunning() END;
  END LockMutex;

PROCEDURE UnlockMutex(m: Mutex) =
  BEGIN
    IF NOT m.held THEN Die(ThisLine(), "attempt to release an unlocked mutex") END;
    m.held := FALSE;
    UnlockRE(m.lock);
  END UnlockMutex;

(**********
PROCEDURE DumpSlots () =
  VAR
    me := GetActivation();
  BEGIN
    RTIO.PutText ("me = ");
    RTIO.PutAddr (me);
    RTIO.PutText ("  slot = ");
    RTIO.PutInt  (me.slot);
    RTIO.PutText ("  self = ");
    RTIO.PutAddr (LOOPHOLE (slots[me.slot], ADDRESS));
    RTIO.PutText ("\r\n");
    FOR i := 1 TO InterlockedRead(n_slotted) DO
      RTIO.PutText (" slot = ");
      RTIO.PutInt  (i);
      RTIO.PutText ("  thr = ");
      RTIO.PutAddr (LOOPHOLE (slots[i], ADDRESS));
      RTIO.PutText ("  act = ");
      RTIO.PutAddr (slots[i].act);
      RTIO.PutText ("\r\n");
    END;
  END DumpSlots;
**********)

(*---------------------------------------- Condition variables and Alerts ---*)

PROCEDURE InnerWait(m: Mutex; c: Condition; act: Activation; self: T;
                    alertable: BOOLEAN) RAISES {Alerted} =
    (* LL = m on entry and exit, but not for the duration *)
  VAR alerted: BOOLEAN;
      (* The order of the handles is important. See WaitForMultipleObjects. *)
      handles := ARRAY [0..1] OF HANDLE {act.waitEvent, act.alertEvent};
  BEGIN

    IF DEBUG THEN ThreadDebug.InnerWait(m, c, self); END;

    (* CONSIDER: if alertable, do one "quick" up-front check before
     * taking the giant lock. The thing is, the lock is only held briefly
     * and checking for alert is a kernel call, AND the giant lock
     * will be going away as well.
     *)

    LockGiant();
      <* ASSERT self.waitingOn = NIL *>
      <* ASSERT self.nextWaiter = NIL *>
      self.waitingOn := c;
      self.nextWaiter := c.waiters;
      c.waiters := self;
    UnlockGiant();

    m.release();

    IF perfOn THEN PerfChanged(State.waiting) END;

    alerted := (WaitForMultipleObjects(1 + ORD(alertable), ADR(handles[0]), 0,
                INFINITE) = (WAIT_OBJECT_0 + 1));

    (* "IF alerted" is repeated in order to reduce the lock size. *)
    IF alerted THEN
      self.waitingOn := NIL;
      self.nextWaiter := NIL;
    ELSE
      <* ASSERT self.waitingOn = NIL *>
      <* ASSERT self.nextWaiter = NIL *>
    END;

    m.acquire();

    IF alerted THEN
      RAISE Alerted;
    END;

  END InnerWait;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF DEBUG THEN ThreadDebug.AlertWait(m, c); END;
    IF self = NIL THEN Die(ThisLine(), "AlertWait called from non-Modula-3 thread") END;
    <* ASSERT self.waitingOn = NIL *>
    <* ASSERT self.nextWaiter = NIL *>
    InnerWait(m, c, self.act, self, alertable := TRUE);
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  <*FATAL Alerted*>
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF DEBUG THEN ThreadDebug.Wait(m, c); END;
    IF self = NIL THEN Die(ThisLine(), "Wait called from non-Modula-3 thread") END;
    <* ASSERT self.waitingOn = NIL *>
    <* ASSERT self.nextWaiter = NIL *>
    InnerWait(m, c, self.act, self, alertable := FALSE);
  END Wait;

PROCEDURE DequeueHead(c: Condition) =
  (* LL = giant *)
  VAR t: T;
      act: Activation;
  BEGIN
    IF DEBUG THEN ThreadDebug.DequeueHead(c); END;
    t := c.waiters;
    c.waiters := t.nextWaiter;
    act := t.act;
    t.nextWaiter := NIL;
    t.waitingOn := NIL;
    IF SetEvent(act.waitEvent) = 0 THEN
      Choke(ThisLine());
    END;
  END DequeueHead;

PROCEDURE Signal (c: Condition) =
  BEGIN
    IF DEBUG THEN ThreadDebug.Signal(c); END;
    LockGiant();
    IF c.waiters # NIL THEN DequeueHead(c) END;
    UnlockGiant();
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  BEGIN
    IF DEBUG THEN ThreadDebug.Broadcast(c); END;
    LockGiant();
    WHILE c.waiters # NIL DO DequeueHead(c) END;
    UnlockGiant();
  END Broadcast;

PROCEDURE Alert(t: T) =
  VAR prev, next: T;
  BEGIN
    IF DEBUG THEN ThreadDebug.Alert(t); END;
    IF t = NIL THEN Die(ThisLine(), "Alert called from non-Modula-3 thread") END;
    LockGiant();
      (* Dequeue from any CV *)
      IF t.waitingOn # NIL THEN
        next := t.waitingOn.waiters; prev := NIL;
        WHILE next # t DO
          <* ASSERT(next#NIL) *>
          prev := next; next := next.nextWaiter;
        END;
        IF prev = NIL THEN
          t.waitingOn.waiters := t.nextWaiter
        ELSE
          prev.nextWaiter := t.nextWaiter;
        END;
        t.nextWaiter := NIL;
        t.waitingOn := NIL;
      END;
    UnlockGiant();
    IF SetEvent(t.act.alertEvent) = 0 THEN
      Choke(ThisLine());
    END;
  END Alert;

PROCEDURE TestAlert(): BOOLEAN =
  VAR self := Self();
      act: Activation;
      result := FALSE;
  BEGIN
    IF DEBUG THEN ThreadDebug.TestAlert(); END;
    IF self = NIL THEN
      (* Not created by Fork; not alertable *)
    ELSE
      act := self.act;
      <* ASSERT self.waitingOn = NIL *>
      <* ASSERT self.nextWaiter = NIL *>
      result := WaitForSingleObject(act.alertEvent, 0) = WAIT_OBJECT_0;
      <* ASSERT self.waitingOn = NIL *>
      <* ASSERT self.nextWaiter = NIL *>
    END;
    RETURN result;
  END TestAlert;

(*------------------------------------------------------------------ Self ---*)

VAR (* LL = slotLock *)
  n_slotted: LONG := 0;
  next_slot := 1;
  slots     : REF ARRAY OF T;  (* NOTE: we don't use slots[0]. *)

PROCEDURE Self (): T =
  (* If not the initial thread and not created by Fork, returns NIL *)
  (* LL = 0 *)
  VAR
    me := GetActivation();
    t: T;
  BEGIN
    IF me = NIL THEN RETURN NIL; END;
    t := slots[me.slot];
    IF t.act # me THEN Die (ThisLine(), "thread with bad slot!"); END;
    RETURN t;
  END Self;

PROCEDURE AssignSlot (t: T) =
  (* LL = 0, cause we allocate stuff with NEW! *)
  VAR n: CARDINAL;  old_slots, new_slots: REF ARRAY OF T;
      retry := TRUE;
  BEGIN
    LockRE(slotLock);
      WHILE retry DO
        retry := FALSE;

        (* make sure we have room to register this guy *)
        IF slots = NIL THEN
          UnlockRE(slotLock);
            slots := NEW (REF ARRAY OF T, 20);
          LockRE(slotLock);
        END;
        IF InterlockedRead(n_slotted) >= LAST (slots^) THEN
          old_slots := slots;
          n := NUMBER (old_slots^);
          UnlockRE(slotLock);
            new_slots := NEW (REF ARRAY OF T, n+n);
          LockRE(slotLock);
          IF old_slots = slots THEN
            (* we won any races that may have occurred. *)
            SUBARRAY (new_slots^, 0, n) := slots^;
            MemoryBarrier(); (* finish filling in new_slots before writing to global slots
                                so that slots can be read without a lock *)
            slots := new_slots;
          ELSIF InterlockedRead(n_slotted) < LAST (slots^) THEN
            (* we lost a race while allocating a new slot table,
               and the new table has room for us. *)
          ELSE
            (* ouch, the new table is full too!   Bail out and retry *)
            retry := TRUE;
          END;
        END;
      END;
     
      (* look for an empty slot *)
      WHILE (slots [next_slot] # NIL) DO
        INC (next_slot);
        IF next_slot >= NUMBER (slots^) THEN next_slot := 1; END;
      END;

      EVAL InterlockedIncrement(ADR(n_slotted));
      t.act.slot := next_slot;
      slots [next_slot] := t;

    UnlockRE(slotLock);
  END AssignSlot;

PROCEDURE FreeSlot (t: T; act: Activation) =
  (* LL = 0 *)
  BEGIN
    EVAL InterlockedDecrement(ADR(n_slotted));
    WITH z = slots [act.slot] DO
      IF z # t THEN Die (ThisLine(), "unslotted thread!"); END;
      z := NIL; (* need write this carefully? I don't think so. *)
    END;
    act.slot := 0;
  END FreeSlot;

(*------------------------------------------------------------ Fork, Join ---*)

VAR (* LL=activeMu *)
  allThreads: Activation := NIL;  (* global list of active threads *)

PROCEDURE CreateT (act: Activation): T =
  (* LL = 0, because allocating a traced reference may cause
     the allocator to start a collection which will call "SuspendOthers"
     which will try to acquire "activeMu". *)
  VAR t := NEW(T, act := act);
  BEGIN
    act.context := NewContext();
    IF act.context = NIL THEN
      RuntimeError.Raise(RuntimeError.T.OutOfMemory);
    END;
    act.waitEvent := CreateEvent(NIL, 0, 0, NIL);
    act.alertEvent := CreateEvent(NIL, 0, 0, NIL);
    IF act.waitEvent = NIL OR act.alertEvent = NIL THEN
      RuntimeError.Raise(RuntimeError.T.SystemError);
    END;
    t.join     := NEW(Condition);
    AssignSlot (t);
    RETURN t;
  END CreateT;

PROCEDURE DeleteActivation(VAR act: Activation) =
  VAR error: UINT32;
BEGIN
  IF act # NIL THEN
    error := GetLastError();
    DeleteContext(act.context);
    IF act.waitEvent # NIL THEN
      EVAL CloseHandle(act.waitEvent);
    END;
    IF act.alertEvent # NIL THEN
      EVAL CloseHandle(act.alertEvent);
    END;
    IF act.handle # NIL THEN
      EVAL CloseHandle(act.handle);
    END;
    DISPOSE(act);
    act := NIL;
    SetLastError(error);
  END;
END DeleteActivation;

<*WINAPI*>
PROCEDURE ThreadBase (param: ADDRESS): DWORD =
  VAR
    me: Activation := param;
    self: T := NIL;
  BEGIN
    SetActivation (me);
    (* We need to establish this binding before this thread touches any
       traced references.  Otherwise, it may trigger a heap page fault,
       which would call SuspendOthers, which requires an Activation. *)

    IF DEBUG THEN
      RTIO.PutText("threadbase act="); RTIO.PutAddr(me.handle); RTIO.PutText("\n"); RTIO.Flush();
    END;

    GetStackBounds(me.stackStart, me.stackEnd); (* enable GC scanning of this stack *)

    (* add to the list of active threads *)
    <*ASSERT allThreads # NIL*>
    LockRE(activeLock);
      me.next := allThreads;
      me.prev := allThreads.prev;
      allThreads.prev.next := me;
      allThreads.prev := me;
    UnlockRE(activeLock);

    (* begin "RunThread" *)

        IF DEBUG THEN ThreadDebug.RunThread(); END;
        IF perfOn THEN PerfChanged(State.alive) END;

        self := slots [me.slot]; (* produce traced references (slots briefly and self) *)

        IF perfOn THEN PerfRunning() END;

        (*** Run the user-level code. ***)
        self.result := self.closure.apply();

        IF perfOn THEN PerfChanged(State.dying) END;

        LOCK self DO
          Broadcast(self.join); (* let everybody know that "self" is done *)
          self.join := NIL;     (* mark me done *)
        END;

        IF perfOn THEN PerfChanged(State.dead) END;

        (* we're dying *)
        RTHeapRep.FlushThreadState(me.heapState);

        IF perfOn THEN PerfDeleted() END;
        FreeSlot(self, me);
        (* Since we're no longer slotted, we cannot touch traced refs. *)

        self := NIL; (* drop traced reference *)
        me.stackStart := NIL; (* disable GC scanning of my stack *)
        me.stackEnd := NIL;

    (* end "RunThread" *)

    (* remove from the list of active threads *)
    <*ASSERT allThreads # NIL*>
    <*ASSERT allThreads # me*>
    LockRE(activeLock);
      me.next.prev := me.prev;
      me.prev.next := me.next;
    UnlockRE(activeLock);
    me.next := NIL;
    me.prev := NIL;

    EVAL WinGDI.GdiFlush ();  (* help out Trestle *)

    DeleteActivation(me);
    RETURN 0;
  END ThreadBase;

PROCEDURE Fork(closure: Closure): T =
  VAR t: T;
      stack_size: DWORD;
      act: Activation := NIL;
      id: DWORD;
      handle: HANDLE := NIL;
  BEGIN
    TRY
      IF DEBUG THEN ThreadDebug.Fork(); END;

      <*ASSERT allThreads # NIL*>
      <*ASSERT allThreads.next # NIL*>
      <*ASSERT allThreads.prev # NIL*>

      (* determine the initial size of the stack for this thread *)
      stack_size := default_stack;
      TYPECASE closure OF
      | SizedClosure (scl) => IF scl.stackSize # 0 THEN 
                                stack_size := scl.stackSize * BYTESIZE(INTEGER);
                              END;
      ELSE (*skip*)
      END;

      act := NEW(Activation);
      t := CreateT(act);
      t.closure := closure;

      (* create suspended just so we can store act.handle before it runs;
      act.handle := CreateThread() is not good enough. *)

      handle := CreateThread(NIL, stack_size, ThreadBase, act, CREATE_SUSPENDED, ADR(id));
      IF handle = NIL THEN
        RuntimeError.Raise(RuntimeError.T.SystemError);
      END;
      act.handle := handle;
      act := NIL;
      EVAL ResumeThread(handle);
      handle := NIL;
      RETURN t;
    FINALLY
      DeleteActivation(act);
    END;
  END Fork;

PROCEDURE XJoin (t: T; alertable: BOOLEAN): REFANY RAISES {Alerted} =
(* This should be shared between Win32 and Pthread. *)
  VAR self := Self();
      act := self.act;
  BEGIN
    LOCK t DO
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice") END;
      TRY
        t.joined := TRUE;
        WHILE t.join # NIL DO InnerWait(t, t.join, act, self, alertable) END;
      FINALLY
        IF t.join # NIL THEN t.joined := FALSE END;
      END;
    END;
    RETURN t.result;
  END XJoin;

PROCEDURE Join(t: T): REFANY =
  <*FATAL Alerted*>
  BEGIN
    IF DEBUG THEN ThreadDebug.Join(t); END;
    RETURN XJoin(t, alertable := FALSE);
  END Join;

PROCEDURE AlertJoin(t: T): REFANY RAISES {Alerted} =
  BEGIN
    IF DEBUG THEN ThreadDebug.AlertJoin(t); END;
    RETURN XJoin(t, alertable := TRUE);
  END AlertJoin;

(*---------------------------------------------------- Scheduling support ---*)

PROCEDURE Pause(n: LONGREAL) =
  VAR amount, thisTime: LONGREAL;
  CONST Limit = FLOAT(LAST(CARDINAL), LONGREAL) / 1000.0D0 - 1.0D0;
  BEGIN
  (* The loop here is to enable waiting more than 4billion milliseconds;
     most waits will just wait once.
   *)
    amount := n;
    WHILE amount > 0.0D0 DO
      thisTime := MIN (Limit, amount);
      amount := amount - thisTime;
      Sleep(ROUND(thisTime*1000.0D0));
    END;
  END Pause;

PROCEDURE AlertPause(n: LONGREAL) RAISES {Alerted} =
  VAR amount, thisTime: LONGREAL;
      self := Self();
      act: Activation;
      alerted := FALSE;
  CONST Limit = FLOAT(LAST(CARDINAL), LONGREAL) / 1000.0D0 - 1.0D0;
  BEGIN
  (* The loop here is to enable waiting more than 4billion milliseconds;
   * most waits will just wait once.
   *)
    IF self = NIL THEN Die(ThisLine(), "Pause called from a non-Modula-3 thread") END;
    <* ASSERT self.waitingOn = NIL *>
    <* ASSERT self.nextWaiter = NIL *>
    IF n <= 0.0d0 THEN RETURN END;
    act := self.act;
    amount := n;
    IF perfOn THEN PerfChanged(State.pausing) END;
    WHILE (NOT alerted) AND (amount > 0.0D0) DO
      thisTime := MIN (Limit, amount);
      amount := amount - thisTime;
      <* ASSERT self.waitingOn = NIL *>
      <* ASSERT self.nextWaiter = NIL *>
      alerted := (WaitForSingleObject(act.alertEvent, ROUND(thisTime*1000.0D0)) = WAIT_OBJECT_0);
    END;
    <* ASSERT self.waitingOn = NIL *>
    <* ASSERT self.nextWaiter = NIL *>
    IF perfOn THEN PerfRunning() END;
    IF alerted THEN
      RAISE Alerted;
    END;
  END AlertPause;

PROCEDURE Yield() =
  BEGIN
    Sleep(0);
  END Yield;

(*--------------------------------------------------- Stack size controls ---*)

PROCEDURE GetDefaultStackSize(): CARDINAL=
  BEGIN
    RETURN default_stack DIV BYTESIZE (INTEGER);
  END GetDefaultStackSize;

PROCEDURE MinDefaultStackSize(new_min: CARDINAL)=
  BEGIN
    default_stack := MAX (default_stack, new_min * BYTESIZE (INTEGER));
  END MinDefaultStackSize;

PROCEDURE IncDefaultStackSize(inc: CARDINAL)=
  BEGIN
    INC (default_stack, inc * BYTESIZE (INTEGER));
  END IncDefaultStackSize;

(*--------------------------------------------- Garbage collector support ---*)
(* NOTE: These routines are called indirectly by the low-level page fault
   handler of the garbage collector.  So, if they touched traced references,
   they could trigger indefinite invocations of the fault handler. *)

(* In versions of SuspendOthers prior to the addition of the incremental
   collector, it acquired 'giant' to guarantee that no suspended thread held it.
   That way when the collector tried to acquire a mutex or signal a
   condition, it wouldn't deadlock with the suspended thread that held giant.
   
   With the VM-synchronized, incremental collector this design is inadequate.
   Here's a deadlock that occurred:
      Thread.Broadcast held giant,
      then it touched its condition argument,
      the page containing the condition was protected by the collector,
      another thread started running the page fault handler,
      the handler called SuspendOthers,
      SuspendOthers tried to acquire giant.

   So, SuspendOthers doesn't grab "giant" before shutting down the other
   threads.  If the collector tries to use any of the thread functions
   that acquire "giant", it'll be deadlocked.
*)

VAR
  suspend_cnt: CARDINAL := 0;  (* LL = giant *)

PROCEDURE GetContextAndCheckStack(act: Activation): BOOLEAN =
BEGIN
  (* helper function used by SuspendOthers

  If the stack pointer is not within bounds, then this might
  be a Windows 95 bug; let the thread run longer and try again.
  Our historical behavior here was wierd. If stackbase - stackpointer > 10000,
  do some VirtualQuery calls to confirm readability. As well, historically,
  we called GetThreadContext on the currently running thread, which
  is documented as not working. As well, historically, GetThreadContext
  was called later, in ProcessStacks. See versions prior to November 22 2009.
  I really don't know if the stack ever comes back invalid, and I didn't
  test on Windows 95, but this seems like a better cheaper way to attempt
  to honor the historical goals. Note also that GetStackBounds should be
  tested on Windows 95. *)

  IF GetThreadContext(act.handle, act.context) = 0 THEN Choke(ThisLine()) END;
  act.stackPointer := StackPointerFromContext(act.context);
  RETURN (act.stackPointer >= act.stackStart AND act.stackPointer < act.stackEnd);

END GetContextAndCheckStack;

PROCEDURE SuspendOthers () =
  (* LL=0. Always bracketed with ResumeOthers which releases "activeMu". *)
  VAR me: Activation;
      act: Activation;
      nLive := 0;
  BEGIN
    LockRE(activeLock);

    INC (suspend_cnt);
    IF suspend_cnt # 1 THEN
      RETURN
    END;
    me := GetActivation();
    LOOP
      act := me.next;
      nLive := 0;
      WHILE act # me DO
        IF act.suspendCount = 0 THEN
          IF DEBUG THEN
            RTIO.PutText("suspending act="); RTIO.PutAddr(act.handle); RTIO.PutText("\n"); RTIO.Flush();
          END;
          IF act.stackStart # NIL AND act.stackEnd # NIL THEN
            IF SuspendThread(act.handle) = -1 THEN Choke(ThisLine()) END;
            IF act.heapState.inCritical # 0 OR NOT GetContextAndCheckStack(act) THEN
              IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
              INC(nLive);
            ELSE
              INC(act.suspendCount);
            END;
          END;
        END;
        act := act.next;
      END;
      IF nLive = 0 THEN
        RETURN
      END;
      Sleep(1);
    END;
  END SuspendOthers;

PROCEDURE ResumeOthers () =
  (* LL=activeMu.  Always preceded by SuspendOthers. *)
  VAR act: Activation;
      me: Activation;
  BEGIN
    DEC (suspend_cnt);
    IF suspend_cnt = 0 THEN
      me := GetActivation();
      act := me.next;
      WHILE act # me DO
        IF DEBUG THEN
          RTIO.PutText("resuming act="); RTIO.PutAddr(act.handle); RTIO.PutText("\n"); RTIO.Flush();
        END;
        <*ASSERT (act.suspendCount > 0 AND act.stackPointer # NIL) OR (act.stackStart = NIL AND act.stackEnd = NIL)*>
        act.stackPointer := NIL;
        IF act.suspendCount > 0 THEN
          IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
          DEC(act.suspendCount);
        END;
        act := act.next;
      END;
    END;

    UnlockRE(activeLock);
  END ResumeOthers;

PROCEDURE ProcessStacks (p: PROCEDURE (start, limit: ADDRESS)) =
  (* LL=activeMu.  Only called within {SuspendOthers, ResumeOthers} *)
  VAR me := GetActivation();
      act: Activation;
  BEGIN
    ProcessMe(me, p);
    act := me.next;
    WHILE act # me DO
      ProcessOther(act, p);
      act := act.next;
    END;
  END ProcessStacks;

PROCEDURE ProcessMe (me: Activation;  p: PROCEDURE (start, limit: ADDRESS)) =
  (* LL=activeMu *)
  BEGIN
    IF DEBUG THEN
      RTIO.PutText("Processing me="); RTIO.PutAddr(me.handle); RTIO.PutText("\n"); RTIO.Flush();
    END;
    RTHeapRep.FlushThreadState(me.heapState);
    ProcessLive(me.stackEnd, p);
  END ProcessMe;

PROCEDURE ProcessOther (act: Activation;  p: PROCEDURE (start, stop: ADDRESS)) =
  BEGIN
    IF DEBUG THEN
      RTIO.PutText("Processing act="); RTIO.PutAddr(act.handle); RTIO.PutText("\n"); RTIO.Flush();
    END;
    RTHeapRep.FlushThreadState(act.heapState);
    ProcessStopped(act.stackStart, act.stackEnd, act.context, p);
  END ProcessOther;

PROCEDURE ProcessEachStack (<*UNUSED*>p: PROCEDURE (start, limit: ADDRESS)) =
  BEGIN
    (* experimental, unimplemented here *)
    <*ASSERT FALSE*>
  END ProcessEachStack;

(*------------------------------------------------------------ misc. stuff ---*)

PROCEDURE MyId(): Id RAISES {}=
  BEGIN
    RETURN GetCurrentThreadId();
  END MyId;

PROCEDURE MyHeapState(): UNTRACED REF RTHeapRep.ThreadState =
  BEGIN
    RETURN ADR(GetActivation().heapState);
  END MyHeapState;

PROCEDURE MyFPState(): UNTRACED REF FloatMode.ThreadState =
  BEGIN
    RETURN ADR(GetActivation().floatState);
  END MyFPState;

PROCEDURE DisableSwitching () =
  BEGIN
    (* no user-level thread switching *)
  END DisableSwitching;

PROCEDURE EnableSwitching () =
  BEGIN
    (* no user-level thread switching *)
  END EnableSwitching;

(*---------------------------------------------------------------- errors ---*)

PROCEDURE Die (lineno: INTEGER; msg: TEXT) =
  BEGIN
    RTError.Msg (ThisFile(), lineno, "Thread client error: ", msg);
  END Die;

PROCEDURE Choke (lineno: INTEGER) =
  BEGIN
    RTError.MsgI (ThisFile(), lineno, "Windows OS failure, GetLastError = ", GetLastError ());
  END Choke;

(*------------------------------------------------------ ShowThread hooks ---*)

VAR
  perfW : RTPerfTool.Handle;
  (* perfOn: BOOLEAN := FALSE;                          (* LL = perfMu *) *)

PROCEDURE PerfStart () =
  BEGIN
    IF RTPerfTool.Start ("showthread", perfW) THEN
      perfOn := TRUE;
      RTProcess.RegisterExitor (PerfStop);
    END;
  END PerfStart;

PROCEDURE PerfStop () =
  BEGIN
    (* UNSAFE, but needed to prevent deadlock if we're crashing! *)
    RTPerfTool.Close (perfW);
  END PerfStop;

CONST
  EventSize = (BITSIZE(ThreadEvent.T) + BITSIZE(CHAR) - 1) DIV BITSIZE(CHAR);

TYPE
  TE = ThreadEvent.Kind;

PROCEDURE PerfChanged (s: State) =
  VAR e := ThreadEvent.T {kind := TE.Changed, id := GetCurrentThreadId(), state := s};
  BEGIN
    LockRE(perfLock);
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    UnlockRE(perfLock);
  END PerfChanged;

PROCEDURE PerfDeleted () =
  VAR e := ThreadEvent.T {kind := TE.Deleted, id := GetCurrentThreadId()};
  BEGIN
    LockRE(perfLock);
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    UnlockRE(perfLock);
  END PerfDeleted;

PROCEDURE PerfRunning () =
  VAR e := ThreadEvent.T {kind := TE.Running, id := GetCurrentThreadId()};
  BEGIN
    LockRE(perfLock);
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    UnlockRE(perfLock);
  END PerfRunning;

(*-------------------------------------------------------- Initialization ---*)

PROCEDURE Init() =
  VAR
    self: T;
    me := NEW(Activation);
  BEGIN
    me.handle := InitC(ADR(self));
    me.next := me;
    me.prev := me;
    SetActivation(me);
    <* ASSERT allThreads = NIL *>
    allThreads := me;
    IF me.handle = NIL THEN
      Choke(ThisLine());
    END;

    self := CreateT(me);

    HeapWaitMutex := NEW(MUTEX);
    HeapWaitCondition := NEW(Condition);

    GetStackBounds(me.stackStart, me.stackEnd);
    IF me.stackStart = NIL OR me.stackEnd = NIL THEN Choke(ThisLine()); END;

    IF DEBUG THEN
      RTIO.PutText("created initial act="); RTIO.PutAddr(me.handle); RTIO.PutText("\n"); RTIO.Flush();
    END;

    <*ASSERT HeapInCritical = 1*>
    HeapInCritical := 0;

    PerfStart();
    IF perfOn THEN PerfChanged(State.alive) END;

    IF RTParams.IsPresent("backgroundgc") THEN
      RTCollectorSRC.StartBackgroundCollection();
    END;
    IF RTParams.IsPresent("foregroundgc") THEN
      RTCollectorSRC.StartForegroundCollection();
    END;
  END Init;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector. *)

VAR
  HeapInCritical := 1;  (* LL = heap *)
  HeapDoSignal: LONG;   (* LL = heap, but Interlocked just in case *)
  HeapWaitMutex: MUTEX;
  HeapWaitCondition: Condition;

PROCEDURE LockHeap () =
  BEGIN
    IF DEBUG THEN ThreadDebug.LockHeap(); END;
    LockRE(heapLock);
    INC(HeapInCritical);
  END LockHeap;

PROCEDURE UnlockHeap () =
  VAR sig: LONG := 0;
  BEGIN   
    IF DEBUG THEN ThreadDebug.UnlockHeap(); END;
    DEC(HeapInCritical);
    IF HeapInCritical = 0 THEN
      sig := InterlockedCompareExchange(ADR(HeapDoSignal), 0, 1);
    END;
    UnlockRE(heapLock);
    IF sig = 1 THEN Broadcast(HeapWaitCondition); END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  BEGIN
    IF DEBUG THEN ThreadDebug.WaitHeap(); END;
    LOCK HeapWaitMutex DO
      DEC(HeapInCritical);
      <*ASSERT HeapInCritical = 0*>
      UnlockRE(heapLock);
      Wait(HeapWaitMutex, HeapWaitCondition);
      LockRE(heapLock);
      <*ASSERT HeapInCritical = 0*>
      INC(HeapInCritical);
    END;
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  (* LL >= RTOS.LockHeap *)
  BEGIN
    IF DEBUG THEN ThreadDebug.BroadcastHeap(); END;
    EVAL InterlockedExchange(ADR(HeapDoSignal), 1);
  END BroadcastHeap;

(*--------------------------------------------- exception handling support --*)

PROCEDURE GetCurrentHandlers (): ADDRESS =
  BEGIN
    RETURN GetActivation().frame;
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers (h: ADDRESS) =
  BEGIN
    GetActivation().frame := h;
  END SetCurrentHandlers;

(*RTHooks.PushEFrame*)
PROCEDURE PushEFrame (frame: ADDRESS) =
  VAR me := GetActivation();
      f: UNTRACED REF RECORD next: ADDRESS END := frame;
  BEGIN
    f.next := me.frame;
    me.frame := f;
  END PushEFrame;

(*RTHooks.PopEFrame*)
PROCEDURE PopEFrame (frame: ADDRESS) =
  BEGIN
    GetActivation().frame := frame;
  END PopEFrame;

VAR DEBUG := RTParams.IsPresent("debugthreads");

BEGIN
END ThreadWin32.
