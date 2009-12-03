(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE MODULE ThreadWin32 EXPORTS
Thread, ThreadF, Scheduler, RTThread, RTOS, RTHooks, ThreadWin32;

IMPORT RTMisc, RTError, WinGDI, RTParams, FloatMode, RuntimeError;
IMPORT Word, MutexRep, RTHeapRep, RTCollectorSRC, RTIO;
IMPORT ThreadEvent, RTPerfTool, RTProcess, ThreadDebug;
FROM Compiler IMPORT ThisFile, ThisLine;
FROM WinNT IMPORT LONG, HANDLE, DWORD;
FROM WinBase IMPORT WaitForSingleObject, INFINITE, ReleaseSemaphore,
    CreateSemaphore, CloseHandle, CreateThread, ResumeThread, Sleep,
    SuspendThread, GetThreadContext, GetLastError, CREATE_SUSPENDED,
    GetCurrentThreadId;
FROM ThreadContext IMPORT CONTEXT, CONTEXT_CONTROL, CONTEXT_INTEGER;
FROM WinNT IMPORT MemoryBarrier;

(*----------------------------------------- Exceptions, types and globals ---*)

VAR
  default_stack: DWORD := 8192;

REVEAL
  Mutex = MutexRep.Public BRANDED "MUTEX Win32-1.0" OBJECT
      waiters: T := NIL;
        (* LL = giant; List of threads waiting on this mutex. *)
      holder: T := NIL;
        (* LL = giant; The thread currently holding this mutex. *)
    OVERRIDES
      acquire := LockMutex;
      release := UnlockMutex;
    END;

  Condition = BRANDED "Thread.Condition Win32-1.0" OBJECT
      waiters: T := NIL;
        (* LL = giant; List of threads waiting on this CV. *)
    END;

  T = MUTEX BRANDED "Thread.T Win32-1.0" OBJECT
      act: Activation := NIL;       (* LL = Self(); live (untraced) thread data *)
      closure: Closure := NIL;      (* our work and its result *)
      result: REFANY := NIL;        (* our work and its result *)
      join: Condition;              (* LL = Self(); wait here to join *)
      waitingOn: Condition := NIL;  (* LL = giant; CV that we're blocked on *)
      nextWaiter: T := NIL;         (* LL = giant; queue of threads waiting on the same CV *)
      joined: BOOLEAN := FALSE;     (* LL = Self(); "Join" or "AlertJoin" has already returned *)
    END;

TYPE
  REVEAL Activation = UNTRACED BRANDED REF RECORD
      frame: ADDRESS := NIL;            (* exception handling support; this field is accessed MANY times
                                        so perhaps therefore should be first *)
      next, prev: Activation := NIL;    (* LL = activeMu; global doubly-linked, circular list of all active threads *)
      handle: HANDLE := NIL;            (* thread handle in Windows *)
      stackStart: ADDRESS := NIL;       (* stack bounds for use by GC *)
      stackEnd: ADDRESS := NIL;         (* stack bounds for use by GC *)
      slot: INTEGER;                    (* LL = slotLock;  index into global array of active, slotted threads *)
      suspendCount := 1;                (* LL = activeLock *)
      context: CONTEXT;                 (* registers of suspended thread *)
      stackPointer: ADDRESS;            (* LOOPHOLE(context.Esp, ADDRESS); *)
      waitSema: HANDLE := NIL;          (* binary semaphore for blocking during "Wait" *)
      alertable: BOOLEAN := FALSE;      (* LL = giant; distinguishes between "Wait" and "AlertWait" *)
      alerted: BOOLEAN := FALSE;        (* LL = giant; the alert flag, of course *)
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

PROCEDURE LockMutex (m: Mutex) =
  VAR self := Self();  wait := FALSE;  next, prev: T;
      act: Activation;
  BEGIN
    IF DEBUG THEN ThreadDebug.LockMutex(m); END;
    IF self = NIL THEN Die(ThisLine(), "LockMutex called from non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(State.locking) END;
    act := self.act;

    Lock(giantLock);

      act.alertable := FALSE;
      IF m.holder = NIL THEN
        m.holder := self;  (* I get it! *)
      ELSIF m.holder = self THEN
        Die(ThisLine(), "Attempt to lock mutex already locked by self");
      ELSE
        (* somebody else already has the mutex locked.  We'll need to wait *)
        wait := TRUE;
        self.nextWaiter := NIL;
        next := m.waiters;
        IF next = NIL THEN
          m.waiters := self;
        ELSE
          (* put me at the end of the list of waiters.*)
          prev := NIL;
          WHILE (next # NIL) DO  prev := next;  next := next.nextWaiter; END;
          prev.nextWaiter := self;
        END;
      END;

    Unlock(giantLock);

    IF wait THEN
      (* I didn't get the mutex, I need to wait for my turn... *)
      IF WaitForSingleObject(act.waitSema, INFINITE) # 0 THEN
        Choke(ThisLine());
      END;
    END;

    IF perfOn THEN PerfRunning() END;

  END LockMutex;

PROCEDURE UnlockMutex(m: Mutex) =
  VAR self := Self(); next: T;
  BEGIN
    IF DEBUG THEN ThreadDebug.UnlockMutex(m); END;
    IF self = NIL THEN Die(ThisLine(), "UnlockMutex called from non-Modula-3 thread") END;
    Lock(giantLock);

      (* Make sure I'm allowed to release this mutex. *)
      IF m.holder = self THEN
        (* ok, we're releasing the mutex *)
        m.holder := NIL;
      ELSIF m.holder = NIL THEN
        Die(ThisLine(), "attempt to release an unlocked mutex");
      ELSE
        Die(ThisLine(), "attempt to release an mutex locked by another thread");
      END;

      (* Let the next guy go... *)
      next := m.waiters;
      IF next # NIL THEN
        (* let the next guy go... *)
        m.waiters := next.nextWaiter;
        next.nextWaiter := NIL;
        m.holder := next;
        IF ReleaseSemaphore(next.act.waitSema, 1, NIL) = 0 THEN
          Choke(ThisLine());
        END;
      END;

    Unlock(giantLock);
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

PROCEDURE InnerWait(m: Mutex; c: Condition; self: T) =
    (* LL = giant+m on entry; LL = m on exit *)
  VAR act := self.act;
  BEGIN
    IF DEBUG THEN ThreadDebug.InnerWait(m, c, self); END;
    <* ASSERT( (self.waitingOn=NIL) AND (self.nextWaiter=NIL) ) *>

    self.waitingOn := c;
    self.nextWaiter := c.waiters;
    c.waiters := self;

    Unlock(giantLock);
    m.release();
    IF perfOn THEN PerfChanged(State.waiting) END;
    IF WaitForSingleObject(act.waitSema, INFINITE) # 0 THEN
      Choke(ThisLine());
    END;
    m.acquire();
  END InnerWait;

PROCEDURE InnerTestAlert(self: Activation) RAISES {Alerted} =
  (* LL = giant on entry; LL = giant on normal exit, 0 on exception exit *)
  (* If act.alerted, clear "alerted", leave giant and raise
     "Alerted". *)
  BEGIN
    (*IF DEBUG THEN ThreadDebug.InnerTestAlert(self); END;*)
    IF self.alerted THEN
      self.alerted := FALSE;
      Unlock(giantLock);
      RAISE Alerted
    END;
  END InnerTestAlert;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := Self();
      act := self.act;
  BEGIN
    IF DEBUG THEN ThreadDebug.AlertWait(m, c); END;
    IF self = NIL THEN Die(ThisLine(), "AlertWait called from non-Modula-3 thread") END;
    Lock(giantLock);
    InnerTestAlert(act);
    act.alertable := TRUE;
    InnerWait(m, c, self);
    Lock(giantLock);
    InnerTestAlert(act);
    Unlock(giantLock);
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF DEBUG THEN ThreadDebug.Wait(m, c); END;
    IF self = NIL THEN Die(ThisLine(), "Wait called from non-Modula-3 thread") END;
    Lock(giantLock);
    InnerWait(m, c, self);
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
    act.alertable := FALSE;
    IF ReleaseSemaphore(act.waitSema, 1, NIL) = 0 THEN
      Choke(ThisLine());
    END;
  END DequeueHead;

PROCEDURE Signal (c: Condition) =
  BEGIN
    IF DEBUG THEN ThreadDebug.Signal(c); END;
    Lock(giantLock);
    IF c.waiters # NIL THEN DequeueHead(c) END;
    Unlock(giantLock);
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  BEGIN
    IF DEBUG THEN ThreadDebug.Broadcast(c); END;
    Lock(giantLock);
    WHILE c.waiters # NIL DO DequeueHead(c) END;
    Unlock(giantLock);
  END Broadcast;

PROCEDURE Alert(t: T) =
  VAR prev, next: T;
      act: Activation;
  BEGIN
    IF DEBUG THEN ThreadDebug.Alert(t); END;
    IF t = NIL THEN Die(ThisLine(), "Alert called from non-Modula-3 thread") END;
    act := t.act;
    Lock(giantLock);
    act.alerted := TRUE;
    IF act.alertable THEN
      (* Dequeue from any CV and unblock from the semaphore *)
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
      act.alertable := FALSE;
      IF ReleaseSemaphore(act.waitSema, 1, NIL) = 0 THEN
        Choke(ThisLine());
      END;
    END;
    Unlock(giantLock);
  END Alert;

PROCEDURE XTestAlert (self: Activation): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    (*IF DEBUG THEN ThreadDebug.XTestAlert(self); END;*)
    Lock(giantLock);
    result := self.alerted; IF result THEN self.alerted := FALSE END;
    Unlock(giantLock);
    RETURN result;
  END XTestAlert;

PROCEDURE TestAlert(): BOOLEAN =
  VAR self := GetActivation();
  BEGIN
    IF DEBUG THEN ThreadDebug.TestAlert(); END;
    IF self = NIL
      (* Not created by Fork; not alertable *)
      THEN RETURN FALSE
      ELSE RETURN XTestAlert(self);
    END;
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
    Lock(slotLock);
      WHILE retry DO
        retry := FALSE;

        (* make sure we have room to register this guy *)
        IF slots = NIL THEN
          Unlock(slotLock);
            slots := NEW (REF ARRAY OF T, 20);
          Lock(slotLock);
        END;
        IF InterlockedRead(n_slotted) >= LAST (slots^) THEN
          old_slots := slots;
          n := NUMBER (old_slots^);
          Unlock(slotLock);
            new_slots := NEW (REF ARRAY OF T, n+n);
          Lock(slotLock);
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

      InterlockedIncrement(n_slotted);
      t.act.slot := next_slot;
      slots [next_slot] := t;

    Unlock(slotLock);
  END AssignSlot;

PROCEDURE FreeSlot (t: T; act: Activation) =
  (* LL = 0 *)
  BEGIN
    InterlockedDecrement(n_slotted);
    WITH z = slots [act.slot] DO
      IF z # t THEN Die (ThisLine(), "unslotted thread!"); END;
      z := NIL; (* need write this carefully? I don't think so. *)
    END;
    act.slot := 0;
  END FreeSlot;

(*------------------------------------------------------------ Fork, Join ---*)

VAR (* LL=activeMu *)
  allThreads  : Activation := NIL;  (* global list of active threads *)

PROCEDURE CreateT (act: Activation): T =
  (* LL = 0, because allocating a traced reference may cause
     the allocator to start a collection which will call "SuspendOthers"
     which will try to acquire "activeMu". *)
  CONST UserRegs = Word.Or(CONTEXT_CONTROL,
                           CONTEXT_INTEGER);
  VAR t := NEW(T, act := act);
  BEGIN
    RTMisc.Zero(ADR(act.context), BYTESIZE(act.context));
    act.context.ContextFlags := UserRegs;
    act.waitSema := CreateSemaphore(NIL, 0, 1, NIL);
    t.join     := NEW(Condition);
    AssignSlot (t);
    RETURN t;
  END CreateT;

<*WINAPI*>
PROCEDURE ThreadBase (param: ADDRESS): DWORD =
  VAR
    me: Activation   := param;
    self: T;
  BEGIN
    SetActivation (me);
    (* We need to establish this binding before this thread touches any
       traced references.  Otherwise, it may trigger a heap page fault,
       which would call SuspendOthers, which requires an Activation. *)

    GetStackBounds(me.stackStart, me.stackEnd);

    IF DEBUG THEN
      RTIO.PutText("threadbase act="); RTIO.PutAddr(me.handle); RTIO.PutText("\n"); RTIO.Flush();
    END;

    (* RunThread *)

        IF DEBUG THEN ThreadDebug.RunThread(); END;
        IF perfOn THEN PerfChanged(State.alive) END;
        self := slots [me.slot];

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

        IF CloseHandle(me.waitSema) = 0 THEN Choke(ThisLine()) END;
        me.waitSema := NIL;

        IF perfOn THEN PerfDeleted() END;
        FreeSlot(self, me);
        (* Since we're no longer slotted, we cannot touch traced refs. *)

        (* remove ourself from the list of active threads *)
        Lock(activeLock);
          <*ASSERT allThreads # me*>
          me.next.prev := me.prev;
          me.prev.next := me.next;
        Unlock(activeLock);

        me.next := NIL;
        me.prev := NIL;
        IF CloseHandle(me.handle) = 0 THEN Choke(ThisLine()) END;
        me.handle := NIL;

    (* RunThread *)

    me.stackStart := NIL; (* disable GC scanning of my stack *)
    me.stackEnd := NIL;
    EVAL WinGDI.GdiFlush ();  (* help out Trestle *)

    <*ASSERT me # allThreads*>
    DISPOSE (me);
    RETURN 0;
  END ThreadBase;

PROCEDURE Fork(closure: Closure): T =
  VAR t: T;
      stack_size: DWORD;
      act: Activation;
      id: DWORD;
      handle: HANDLE;
  BEGIN
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
    handle := CreateThread(NIL, stack_size, ThreadBase, act, CREATE_SUSPENDED, ADR(id));
    IF DEBUG THEN
      RTIO.PutText("creating act="); RTIO.PutAddr(handle); RTIO.PutText("\n"); RTIO.Flush();
    END;
    IF handle = NIL THEN
      RuntimeError.Raise(RuntimeError.T.SystemError);
    END;
    act.handle := handle;
    t := CreateT(act);
    t.closure := closure;
    Lock(activeLock);
      act.next := allThreads;
      act.prev := allThreads.prev;
      allThreads.prev.next := act;
      allThreads.prev := act;
      IF DEBUG THEN
        RTIO.PutText("initial resume act="); RTIO.PutAddr(handle); RTIO.PutText("\n"); RTIO.Flush();
      END;
      IF ResumeThread(handle) = -1 THEN Choke(ThisLine()) END;
      DEC(act.suspendCount);
    Unlock(activeLock);
    RETURN t;
  END Fork;

PROCEDURE XWait (m: Mutex; c: Condition; alertable: BOOLEAN) RAISES {Alerted} =
(* In future maybe Wait and AlertWait merged into here, esp. if/when
   we use Win32 alert mechanism and just pass alertable onto WaitForSingleObjectEx(). *)
  BEGIN
    IF alertable THEN
      AlertWait(m, c);
    ELSE
      Wait(m, c);
    END;
  END XWait;

PROCEDURE XJoin (t: T; alertable: BOOLEAN): REFANY RAISES {Alerted} =
(* This should be shared between Win32 and Pthread. *)
  BEGIN
    LOCK t DO
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice") END;
      TRY
        t.joined := TRUE;
        WHILE t.join # NIL DO XWait(t, t.join, alertable) END;
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
    act := self.act;
  CONST Limit = FLOAT(LAST(CARDINAL), LONGREAL) / 1000.0D0 - 1.0D0;
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "Pause called from a non-Modula-3 thread") END;
    IF n <= 0.0d0 THEN RETURN END;
    IF perfOn THEN PerfChanged(State.pausing) END;
    amount := n;
    WHILE amount > 0.0D0 DO
      thisTime := MIN (Limit, amount);
      amount := amount - thisTime;
      Lock(giantLock);
      InnerTestAlert(act);
      act.alertable := TRUE;
      <* ASSERT(self.waitingOn = NIL) *>
      Unlock(giantLock);
      EVAL WaitForSingleObject(act.waitSema, ROUND(thisTime*1000.0D0));
      Lock(giantLock);
      act.alertable := FALSE;
      IF act.alerted THEN
        (* Sadly, the alert might have happened after we timed out on the
           semaphore and before we entered "giant". In that case, we need to
           decrement the semaphore's count *)
        EVAL WaitForSingleObject(act.waitSema, 0);
        InnerTestAlert(act);
      END;
      Unlock(giantLock);
    END;
    IF perfOn THEN PerfRunning() END;
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

  IF GetThreadContext(act.handle, ADR(act.context)) = 0 THEN Choke(ThisLine()) END;
  act.stackPointer := StackPointerFromContext(ADR(act.context));
  RETURN (act.stackPointer >= act.stackStart AND act.stackPointer < act.stackEnd);

END GetContextAndCheckStack;

PROCEDURE SuspendOthers () =
  (* LL=0. Always bracketed with ResumeOthers which releases "activeMu". *)
  VAR me: Activation;
      act: Activation;
      nLive := 0;
  BEGIN
    Lock(activeLock);

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
          IF SuspendThread(act.handle) = -1 THEN Choke(ThisLine()) END;
          IF act.heapState.inCritical # 0 OR NOT GetContextAndCheckStack(act) THEN
            IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
            INC(nLive);
          ELSE
            INC(act.suspendCount);
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
        <*ASSERT act.suspendCount > 0*>
        <*ASSERT act.stackPointer # NIL*>
        act.stackPointer := NIL;
        IF DEBUG THEN
          RTIO.PutText("resuming act="); RTIO.PutAddr(act.handle); RTIO.PutText("\n"); RTIO.Flush();
        END;
        IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
        DEC(act.suspendCount);
        act := act.next;
      END;
    END;

    Unlock(activeLock);
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
    IF act.stackStart = NIL OR act.stackEnd = NIL THEN
      RETURN
    END;
    RTHeapRep.FlushThreadState(act.heapState);
    ProcessStopped(act.stackEnd, ADR(act.context), p);
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
  (* LL = Self() *)
  VAR e := ThreadEvent.T {kind := TE.Changed, id := GetCurrentThreadId(), state := s};
  BEGIN
    Lock(perfLock);
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    Unlock(perfLock);
  END PerfChanged;

PROCEDURE PerfDeleted () =
  (* LL = Self() *)
  VAR e := ThreadEvent.T {kind := TE.Deleted, id := GetCurrentThreadId()};
  BEGIN
    Lock(perfLock);
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    Unlock(perfLock);
  END PerfDeleted;

PROCEDURE PerfRunning () =
  (* LL = Self() *)
  VAR e := ThreadEvent.T {kind := TE.Running, id := GetCurrentThreadId()};
  BEGIN
    Lock(perfLock);
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    Unlock(perfLock);
  END PerfRunning;

(*-------------------------------------------------------- Initialization ---*)

PROCEDURE Init() =
  VAR
    self: T;
    me := NEW(Activation);
  BEGIN
    me.suspendCount := 0;
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

    mutex := NEW(MUTEX);
    condition := NEW(Condition);

    GetStackBounds(me.stackStart, me.stackEnd);
    IF me.stackStart = NIL OR me.stackEnd = NIL THEN Choke(ThisLine()); END;

    IF DEBUG THEN
      RTIO.PutText("created initial act="); RTIO.PutAddr(me.handle); RTIO.PutText("\n"); RTIO.Flush();
    END;

    <*ASSERT inCritical = 1*>
    inCritical := 0;

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
  inCritical := 1;     (* LL = heap *)
  do_signal := FALSE;  (* LL = heap *)
  mutex: MUTEX;
  condition: Condition;

PROCEDURE LockHeap () =
  BEGIN
    IF DEBUG THEN ThreadDebug.LockHeap(); END;
    Lock(heapLock);
    INC(inCritical);
  END LockHeap;

PROCEDURE UnlockHeap () =
  VAR sig := FALSE;
  BEGIN   
    IF DEBUG THEN ThreadDebug.UnlockHeap(); END;
    DEC(inCritical);
    IF (inCritical = 0) AND do_signal THEN sig := TRUE; do_signal := FALSE; END;
    Unlock(heapLock);
    IF sig THEN Broadcast(condition); END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  BEGIN
    IF DEBUG THEN ThreadDebug.WaitHeap(); END;
    LOCK mutex DO
      DEC(inCritical);
      <*ASSERT inCritical = 0*>
      Unlock(heapLock);
      Wait(mutex, condition);
      Lock(heapLock);
      <*ASSERT inCritical = 0*>
      INC(inCritical);
    END;
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  BEGIN
    IF DEBUG THEN ThreadDebug.BroadcastHeap(); END;
    Lock(heapLock);
      do_signal := TRUE;
    Unlock(heapLock);
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
