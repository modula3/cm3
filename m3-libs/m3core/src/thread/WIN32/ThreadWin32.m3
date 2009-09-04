(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE MODULE ThreadWin32
EXPORTS ThreadWin32, ThreadInternal, Scheduler, Thread, ThreadF, RTOS, RTHooks;

IMPORT RTError, WinGDI, RTParams;
IMPORT ThreadContext, Word, MutexRep, RTHeapRep, RTCollectorSRC;
IMPORT ThreadEvent, RTPerfTool, RTProcess;
FROM Compiler IMPORT ThisFile, ThisLine;
FROM WinNT IMPORT HANDLE, LONG, DWORD, SIZE_T, DUPLICATE_SAME_ACCESS,
    MEMORY_BASIC_INFORMATION, PAGE_READWRITE, PAGE_READONLY;
FROM WinBase IMPORT WaitForSingleObject, INFINITE, ReleaseSemaphore,
    GetCurrentProcess, DuplicateHandle, GetCurrentThread, CreateSemaphore,
    CloseHandle, CreateThread, ResumeThread, Sleep, SuspendThread,
    GetThreadContext, VirtualQuery, GetLastError, CREATE_SUSPENDED;

(*----------------------------------------- Exceptions, types and globals ---*)

VAR
  default_stack: DWORD := 8192;

  nextId: Id := 1;

  threadMu: Mutex;
    (* Global lock for internal fields of Thread.T *)

REVEAL
  Mutex = MutexRep.Public BRANDED "MUTEX Win32-1.0" OBJECT
      waiters: T := NIL;
        (* LL = cm; List of threads waiting on this mutex. *)
      holder: T := NIL;
        (* LL = cm; The thread currently holding this mutex. *)
    OVERRIDES
      acquire := LockMutex;
      release := UnlockMutex;
    END;

  Condition = BRANDED "Thread.Condition Win32-1.0" OBJECT
      waiters: T := NIL;
        (* LL = cm; List of threads waiting on this CV. *)
    END;

  T = BRANDED "Thread.T Win32-1.0" OBJECT
      act: Activation := NIL;
        (* LL = threadMu;  live thread data *)
      nextIdle: T := NIL;
        (* LL = idleMu; global list of idle threads *)
      closure: Closure := NIL;
        (* LL = threadMu *)
      result: REFANY := NIL;
        (* LL = threadMu;  if not self.completed, used only by self;
           if self.completed, read-only. *)
      cond: Condition;
        (* LL = threadMu; wait here to join, or for rebirth *)
      waitingOn: Condition := NIL;
        (* LL = cm; CV that we're blocked on *)
      nextWaiter: T := NIL;
        (* LL = cm; queue of threads waiting on the same CV *)
      waitSema: HANDLE := NIL;
        (* binary semaphore for blocking during "Wait" *)
      alertable: BOOLEAN := FALSE;
        (* LL = cm; distinguishes between "Wait" and "AlertWait" *)
      alerted: BOOLEAN := FALSE;
        (* LL = cm; the alert flag, of course *)
      completed: BOOLEAN := FALSE;
        (* LL = threadMu; indicates that "result" is set *)
      joined: BOOLEAN := FALSE;
        (* LL = threadMu; "Join" or "AlertJoin" has already returned *)
      id: Id;
        (* LL = threadMu; unique ID of this thread *)
    END;

TYPE
  Activation = UNTRACED REF RECORD
      (* exception handling support *)
      frame: ADDRESS := NIL;
      next, prev: Activation := NIL;
        (* LL = activeMu; global doubly-linked, circular list of all active threads *)
      handle: HANDLE := NIL;
        (* LL = activeMu; thread handle in Windows *)
      stackbase: ADDRESS := NIL;
        (* LL = activeMu; base of thread stack for use by GC *)
      slot: INTEGER;
        (* LL = slotMu;  index into global array of active, slotted threads *)

      (* thread state *)
      heapState: RTHeapRep.ThreadState;
    END;

(*----------------------------------------------------------------- Mutex ---*)
         
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
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "LockMutex called from non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(self.id, State.locking) END;

    EnterCriticalSection_cm();

      self.alertable := FALSE;
      IF (m.holder = NIL) THEN
        m.holder := self;  (* I get it! *)
      ELSIF (m.holder = self) THEN
        Die(ThisLine(), "Attempt to lock mutex already locked by self");
      ELSE
        (* somebody else already has the mutex locked.  We'll need to wait *)
        wait := TRUE;
        self.nextWaiter := NIL;
        next := m.waiters;
        IF (next = NIL) THEN
          m.waiters := self;
        ELSE
          (* put me at the end of the list of waiters.*)
          prev := NIL;
          WHILE (next # NIL) DO  prev := next;  next := next.nextWaiter; END;
          prev.nextWaiter := self;
        END;
      END;

    LeaveCriticalSection_cm();

    IF wait THEN
      (* I didn't get the mutex, I need to wait for my turn... *)
      IF WaitForSingleObject(self.waitSema, INFINITE) # 0 THEN
        Choke(ThisLine());
      END;
    END;

    IF perfOn THEN PerfChanged(self.id, State.alive) END;

  END LockMutex;

PROCEDURE UnlockMutex(m: Mutex) =
  VAR self := Self();  prevCount: LONG;  next: T;
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "UnlockMutex called from non-Modula-3 thread") END;
    EnterCriticalSection_cm();

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
        IF ReleaseSemaphore(next.waitSema, 1, ADR(prevCount)) = 0 THEN
          Choke(ThisLine());
        END;
      END;

    LeaveCriticalSection_cm();
  END UnlockMutex;

(**********
PROCEDURE DumpSlots () =
  VAR
    me := LOOPHOLE (TlsGetValue_threadIndex(), Activation);
  BEGIN
    RTIO.PutText ("me = ");
    RTIO.PutAddr (me);
    RTIO.PutText ("  slot = ");
    RTIO.PutInt  (me.slot);
    RTIO.PutText ("  self = ");
    RTIO.PutAddr (LOOPHOLE (slots[me.slot], ADDRESS));
    RTIO.PutText ("\r\n");
    FOR i := 1 TO n_slotted DO
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
    (* LL = cm+m on entry; LL = m on exit *)
  BEGIN
    <* ASSERT( (self.waitingOn=NIL) AND (self.nextWaiter=NIL) ) *>
    self.waitingOn := c;
    self.nextWaiter := c.waiters;
    c.waiters := self;
    LeaveCriticalSection_cm();
    UnlockMutex(m);
    IF WaitForSingleObject(self.waitSema, INFINITE) # 0 THEN
      Choke(ThisLine());
    END;
    LockMutex(m);
  END InnerWait;

PROCEDURE InnerTestAlert(self: T) RAISES {Alerted} =
  (* LL = cm on entry; LL = cm on normal exit, 0 on exception exit *)
  (* If self.alerted, clear "alerted", leave cm and raise
     "Alerted". *)
  BEGIN
    IF self.alerted THEN
      self.alerted := FALSE;
      LeaveCriticalSection_cm();
      RAISE Alerted
    END;
  END InnerTestAlert;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "AlertWait called from non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(self.id, State.waiting) END;
    EnterCriticalSection_cm();
    InnerTestAlert(self);
    self.alertable := TRUE;
    InnerWait(m, c, self);
    EnterCriticalSection_cm();
    InnerTestAlert(self);
    LeaveCriticalSection_cm();
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "Wait called from non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(self.id, State.waiting) END;
    EnterCriticalSection_cm();
    InnerWait(m, c, self);
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
  END Wait;

PROCEDURE DequeueHead(c: Condition) =
  (* LL = cm *)
  VAR t: T; prevCount: LONG;
  BEGIN
    t := c.waiters; c.waiters := t.nextWaiter;
    t.nextWaiter := NIL;
    t.waitingOn := NIL;
    t.alertable := FALSE;
    IF ReleaseSemaphore(t.waitSema, 1, ADR(prevCount)) = 0 THEN
      Choke(ThisLine());
    END;
  END DequeueHead;

PROCEDURE Signal (c: Condition) =
  BEGIN
    EnterCriticalSection_cm();
    IF c.waiters # NIL THEN DequeueHead(c) END;
    LeaveCriticalSection_cm();
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  BEGIN
    EnterCriticalSection_cm();
    WHILE c.waiters # NIL DO DequeueHead(c) END;
    LeaveCriticalSection_cm();
  END Broadcast;

PROCEDURE Alert(t: T) =
    VAR prevCount: LONG; prev, next: T;
  BEGIN
    IF t = NIL THEN Die(ThisLine(), "Alert called from non-Modula-3 thread") END;
    EnterCriticalSection_cm();
    t.alerted := TRUE;
    IF t.alertable THEN
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
      t.alertable := FALSE;
      IF ReleaseSemaphore(t.waitSema, 1, ADR(prevCount)) = 0 THEN
        Choke(ThisLine());
      END;
    END;
    LeaveCriticalSection_cm();
  END Alert;

PROCEDURE XTestAlert (self: T): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    EnterCriticalSection_cm();
    result := self.alerted; IF result THEN self.alerted := FALSE END;
    LeaveCriticalSection_cm();
    RETURN result;
  END XTestAlert;

PROCEDURE TestAlert(): BOOLEAN =
  VAR self := Self();
  BEGIN
    IF self = NIL
      (* Not created by Fork; not alertable *)
      THEN RETURN FALSE
      ELSE RETURN XTestAlert(self);
    END;
  END TestAlert;

(*------------------------------------------------------------------ Self ---*)

VAR (* LL = slotMu *)
  n_slotted := 0;
  next_slot := 1;
  slots     : REF ARRAY OF T;  (* NOTE: we don't use slots[0]. *)

PROCEDURE InitActivations (): Activation =
  VAR me := NEW(Activation);
  BEGIN
    InitC();
    IF TlsSetValue_threadIndex(LOOPHOLE (me, SIZE_T)) = 0 THEN
      Choke(ThisLine());
    END;
    IF DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(),
            ADR(me.handle), 0, 0, DUPLICATE_SAME_ACCESS) = 0 THEN
      Choke(ThisLine());
    END;
    me.next := me;
    me.prev := me;
    <* ASSERT allThreads = NIL *>
    allThreads := me;
    RETURN me;
  END InitActivations;

PROCEDURE SetActivation (act: Activation) =
  (* LL = 0 *)
  BEGIN
    IF TlsSetValue_threadIndex(LOOPHOLE (act, SIZE_T)) = 0 THEN
      Choke(ThisLine());
    END;
  END SetActivation;

PROCEDURE GetActivation (): Activation =
  (* If not the initial thread and not created by Fork, returns NIL *)
  (* LL = 0 *)
  BEGIN
    RETURN LOOPHOLE (TlsGetValue_threadIndex(), Activation);
  END GetActivation;

PROCEDURE Self (): T =
  (* If not the initial thread and not created by Fork, returns NIL *)
  (* LL = 0 *)
  VAR
    me := GetActivation();
    t: T;
  BEGIN
    IF me = NIL THEN RETURN NIL; END;
    EnterCriticalSection_slotMu();
      t := slots[me.slot];
    LeaveCriticalSection_slotMu();
    IF (t.act # me) THEN Die (ThisLine(), "thread with bad slot!"); END;
    RETURN t;
  END Self;

PROCEDURE AssignSlot (t: T) =
  (* LL = 0, cause we allocate stuff with NEW! *)
  VAR n: CARDINAL;  new_slots: REF ARRAY OF T;
  BEGIN
    EnterCriticalSection_slotMu();

      (* make sure we have room to register this guy *)
      IF (slots = NIL) THEN
        LeaveCriticalSection_slotMu();
          slots := NEW (REF ARRAY OF T, 20);
        EnterCriticalSection_slotMu();
      END;
      IF (n_slotted >= LAST (slots^)) THEN
        n := NUMBER (slots^);
        LeaveCriticalSection_slotMu();
          new_slots := NEW (REF ARRAY OF T, n+n);
        EnterCriticalSection_slotMu();
        IF (n = NUMBER (slots^)) THEN
          (* we won any races that may have occurred. *)
          SUBARRAY (new_slots^, 0, n) := slots^;
          slots := new_slots;
        ELSIF (n_slotted < LAST (slots^)) THEN
          (* we lost a race while allocating a new slot table,
             and the new table has room for us. *)
        ELSE
          (* ouch, the new table is full too!   Bail out and retry *)
          LeaveCriticalSection_slotMu();
          AssignSlot (t);
        END;
      END;
     
      (* look for an empty slot *)
      WHILE (slots [next_slot] # NIL) DO
        INC (next_slot);
        IF (next_slot >= NUMBER (slots^)) THEN next_slot := 1; END;
      END;

      INC (n_slotted);
      t.act.slot := next_slot;
      slots [next_slot] := t;

    LeaveCriticalSection_slotMu();
  END AssignSlot;

PROCEDURE FreeSlot (t: T) =
  (* LL = 0 *)
  BEGIN
    EnterCriticalSection_slotMu();
    
      DEC (n_slotted);
      WITH z = slots [t.act.slot] DO
        IF (z # t) THEN Die (ThisLine(), "unslotted thread!"); END;
        z := NIL;
      END;
      t.act.slot := 0;

    LeaveCriticalSection_slotMu();
  END FreeSlot;

PROCEDURE CheckSlot (t: T) =
  (* LL = 0 *)
  VAR me := t.act;
  BEGIN
    <*ASSERT me # NIL *>
    <*ASSERT me.slot > 0 *>
    EnterCriticalSection_slotMu();
       <*ASSERT slots[me.slot] = t *>
    LeaveCriticalSection_slotMu();
  END CheckSlot;

(*------------------------------------------------------------ Fork, Join ---*)

CONST
  MaxIdle = 10;

VAR (* LL=activeMu *)
  allThreads  : Activation := NIL;  (* global list of active threads *)

VAR (* LL=idleMu *)
  idleThreads : T          := NIL;  (* global list of idle threads *)
  nIdle       : INTEGER    := 0;

PROCEDURE CreateT (act: Activation): T =
  (* LL = 0, because allocating a traced reference may cause
     the allocator to start a collection which will call "SuspendOthers"
     which will try to acquire "activeMu". *)
  VAR t := NEW(T, act := act);
  BEGIN
    t.waitSema := CreateSemaphore(NIL, 0, 1, NIL);
    t.cond     := NEW(Condition);
    AssignSlot (t);
    RETURN t;
  END CreateT;

(* ThreadBase calls RunThread after finding (approximately) where
   its stack begins.  This dance ensures that all of ThreadMain's
   traced references are within the stack scanned by the collector.

   If RunThread decides to put itself on the idle list, it returns
   a Win32 semaphore that ThreadBase waits on.  It's important that
   ThreadBase's stack frame doesn't contain traced references.
   Otherwise, while it waited for its rebirth signal each reference
   would pin a heap page.
*)

<*WINAPI*>
PROCEDURE ThreadBase (param: ADDRESS): DWORD =
  VAR
    me       : Activation   := param;
    waitSema : HANDLE       := NIL;
  BEGIN
    SetActivation (me);
    (* We need to establish this binding before this thread touches any
       traced references.  Otherwise, it may trigger a heap page fault,
       which would call SuspendOthers, which requires an Activation. *)

    LOOP
      me.stackbase := ADR (me); (* enable GC scanning of this stack *)
      waitSema := RunThread (me);
      me.stackbase := NIL; (* disable GC scanning of my stack *)
      EVAL WinGDI.GdiFlush ();  (* help out Trestle *)
      IF (waitSema = NIL) THEN EXIT; END;
      IF WaitForSingleObject(waitSema, INFINITE) # 0 THEN
        Choke(ThisLine());
      END;
    END;

    <*ASSERT me # allThreads*>
    DISPOSE (me);
    RETURN 0;
  END ThreadBase;

PROCEDURE RunThread (me: Activation): HANDLE =
  VAR self, next_self: T;  cl: Closure; res: REFANY;
  BEGIN
    EnterCriticalSection_slotMu();
      self := slots [me.slot];
    LeaveCriticalSection_slotMu();

    LOCK threadMu DO
      cl := self.closure;
      self.id := nextId;  INC (nextId);
    END;

    IF (cl = NIL) THEN
      Die (ThisLine(), "NIL closure passed to Thread.Fork!");
    END;

    (* Run the user-level code. *)
    IF perfOn THEN PerfRunning(self.id) END;
    res := cl.apply();

    next_self := NIL;
    IF nIdle < MaxIdle THEN
      (* apparently the cache isn't full, although we don't hold idleMu
         so we can't be certain, we're committed now.  Hopefully we'll
         be reborn soon... *)

      (* transplant the active guts of "self" into "next_self" *)
      next_self          := NEW(T);
      next_self.act      := me;
      next_self.waitSema := self.waitSema;
      next_self.cond     := self.cond;

      (* hijack "self"s entry in the slot table *)
      EnterCriticalSection_slotMu();
        slots[me.slot] := next_self;
      LeaveCriticalSection_slotMu();
    END;

    LOCK threadMu DO
      (* mark "self" done and clean it up a bit *)
      self.result := res;
      self.completed := TRUE;
      Broadcast(self.cond); (* let everybody know that "self" is done *)
      IF perfOn THEN PerfChanged(self.id, State.dying) END;
    END;

    IF perfOn THEN PerfDeleted(self.id) END;

    IF next_self # NIL THEN
      (* we're going to be reborn! *)
      (* put "next_self" on the list of idle threads *)
      EnterCriticalSection_idleMu();
        next_self.nextIdle := idleThreads;
        idleThreads := next_self;
        INC(nIdle);
      LeaveCriticalSection_idleMu();
      (* let the rebirth loop in ThreadBase know where to wait... *)
      RETURN next_self.waitSema;
    ELSE
      (* we're dying *)
      RTHeapRep.FlushThreadState(me.heapState);

      IF CloseHandle(self.waitSema) = 0 THEN Choke(ThisLine()) END;
      self.waitSema := NIL;

      FreeSlot(self);  (* note: needs self.act ! *)
      (* Since we're no longer slotted, we cannot touch traced refs. *)

      (* remove ourself from the list of active threads *)
      LeaveCriticalSection_activeMu();
        IF allThreads = me THEN allThreads := me.next; END;
        me.next.prev := me.prev;
        me.prev.next := me.next;
        me.next := NIL;
        me.prev := NIL;
        IF CloseHandle(me.handle) = 0 THEN Choke(ThisLine()) END;
        me.handle := NIL;
      LeaveCriticalSection_activeMu();

      RETURN NIL; (* let the rebirth loop know we're dying. *)
    END;
  END RunThread;

PROCEDURE Fork(closure: Closure): T =
  VAR
    t: T := NIL;
    id, stack_size: DWORD;
    prevCount: LONG;
    new_born: BOOLEAN;
    act: Activation := NIL;
  BEGIN
    (* determine the initial size of the stack for this thread *)
    stack_size := default_stack;
    TYPECASE closure OF
    | SizedClosure (scl) => IF scl.stackSize # 0 THEN 
                              stack_size := scl.stackSize * BYTESIZE(INTEGER);
                            END;
    ELSE (*skip*)
    END;

    (* try the cache for a thread *)
    EnterCriticalSection_idleMu();
      IF nIdle > 0 THEN
        new_born := FALSE;
        <* ASSERT(idleThreads # NIL) *>
        DEC(nIdle);
        t := idleThreads;
        idleThreads := t.nextIdle;
        t.nextIdle := NIL;
      ELSE (* empty cache => we need a fresh thread *)
        new_born := TRUE;
        LeaveCriticalSection_idleMu();
          t := CreateT(NEW(Activation));
        EnterCriticalSection_idleMu();
        act := t.act;
        act.handle := CreateThread(NIL, stack_size, ThreadBase,
                         act, CREATE_SUSPENDED, ADR(id));
        LeaveCriticalSection_activeMu();
          act.next := allThreads;
          act.prev := allThreads.prev;
          allThreads.prev.next := act;
          allThreads.prev := act;
        LeaveCriticalSection_activeMu();
      END;
    LeaveCriticalSection_idleMu();

    t.closure := closure;

    (* last minute sanity checking *)
    CheckSlot (t);
    act := t.act;
    IF (act.handle = NIL) OR (act.next = NIL) OR (act.prev = NIL) THEN Choke(ThisLine()) END;

    IF new_born THEN
      IF ResumeThread(t.act.handle) = -1 THEN Choke(ThisLine()) END;
    ELSE
      IF ReleaseSemaphore(t.waitSema, 1, ADR(prevCount)) = 0 THEN
        Choke(ThisLine());
      END;
    END;

    IF perfOn THEN PerfChanged(t.id, State.alive) END;

    RETURN t
  END Fork;

PROCEDURE Join(t: T): REFANY =
  VAR res: REFANY;
  BEGIN
    LOCK threadMu DO
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice"); END;
      WHILE NOT t.completed DO Wait(threadMu, t.cond) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
      t.cond := NIL;
      IF perfOn THEN PerfChanged(t.id, State.dead) END;
    END;
    RETURN res;
  END Join;

PROCEDURE AlertJoin(t: T): REFANY RAISES {Alerted} =
  VAR res: REFANY;
  BEGIN
    LOCK threadMu DO
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice"); END;
      WHILE NOT t.completed DO AlertWait(threadMu, t.cond) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
      t.cond := NIL;
      IF perfOn THEN PerfChanged(t.id, State.dead) END;
    END;
    RETURN res;
  END AlertJoin;

(*------------------------------------------------ timer-based preemption ---*)

PROCEDURE SetSwitchingInterval (<*UNUSED*> usec: CARDINAL) =
  BEGIN
  END SetSwitchingInterval;

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
  CONST Limit = FLOAT(LAST(CARDINAL), LONGREAL) / 1000.0D0 - 1.0D0;
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "Pause called from a non-Modula-3 thread") END;
    IF n <= 0.0d0 THEN RETURN END;
    IF perfOn THEN PerfChanged(self.id, State.pausing) END;
    amount := n;
    WHILE amount > 0.0D0 DO
      thisTime := MIN (Limit, amount);
      amount := amount - thisTime;
      EnterCriticalSection_cm();
      InnerTestAlert(self);
      self.alertable := TRUE;
      <* ASSERT(self.waitingOn = NIL) *>
      LeaveCriticalSection_cm();
      EVAL WaitForSingleObject(self.waitSema, ROUND(thisTime*1000.0D0));
      EnterCriticalSection_cm();
      self.alertable := FALSE;
      IF self.alerted THEN
        (* Sadly, the alert might have happened after we timed out on the
           semaphore and before we entered "cm". In that case, we need to
           decrement the semaphore's count *)
        EVAL WaitForSingleObject(self.waitSema, 0);
        InnerTestAlert(self);
      END;
      LeaveCriticalSection_cm();
    END;
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
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
   collector, it acquired 'cm' to guarantee that no suspended thread held it.
   That way when the collector tried to acquire a mutex or signal a
   condition, it wouldn't deadlock with the suspended thread that held cm.
   
   With the VM-synchronized, incremental collector this design is inadequate.
   Here's a deadlock that occurred:
      Thread.Broadcast held cm,
      then it touched its condition argument,
      the page containing the condition was protected by the collector,
      another thread started running the page fault handler,
      the handler called SuspendOthers,
      SuspendOthers tried to acquire cm.

   So, SuspendOthers doesn't grab "cm" before shutting down the other
   threads.  If the collector tries to use any of the thread functions
   that acquire "cm", it'll be deadlocked.
*)

VAR
  suspend_cnt: CARDINAL := 0;  (* LL = cm *)

PROCEDURE SuspendOthers () =
  (* LL=0. Always bracketed with ResumeOthers which releases "activeMu". *)
  VAR me := GetActivation();
  BEGIN
    <*ASSERT me # NIL*>
    LeaveCriticalSection_activeMu();

    INC (suspend_cnt);
    IF (suspend_cnt = 1) THEN StopWorld(me) END;
  END SuspendOthers;

PROCEDURE StopWorld (me: Activation) =
  (* LL=activeMu *)
  VAR
    nLive := 0;
    act := me.next;
  BEGIN
    LOOP
      WHILE act # me DO
        IF SuspendThread(act.handle) = -1 THEN Choke(ThisLine()) END;
        IF act.heapState.inCritical # 0 THEN
          IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
          INC(nLive);
        END;
        act := act.next;
      END;
      IF nLive = 0 THEN EXIT END;
      Sleep(1);
      act := me.next;
      nLive := 0;
    END;
  END StopWorld;

PROCEDURE ResumeOthers () =
  (* LL=activeMu.  Always preceded by SuspendOthers. *)
  VAR act: Activation;  me := GetActivation();
  BEGIN
    DEC (suspend_cnt);
    IF (suspend_cnt = 0) THEN
      act := me.next;
      WHILE (act # me) DO
        IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
        act := act.next;
      END;
    END;

    LeaveCriticalSection_activeMu();
  END ResumeOthers;

PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS)) =
  (* LL=activeMu.  Only called within {SuspendOthers, ResumeOthers} *)
  CONST UserRegs = Word.Or(ThreadContext.CONTEXT_CONTROL,
                           ThreadContext.CONTEXT_INTEGER);
  VAR act := allThreads;  context: ThreadContext.CONTEXT;  fixed_SP: ADDRESS;
  BEGIN
    REPEAT
      IF (act.stackbase # NIL) THEN
        context.ContextFlags := UserRegs;
        IF GetThreadContext(act.handle, ADR(context))=0 THEN Choke(ThisLine()) END;
        fixed_SP := LOOPHOLE (context.Esp, ADDRESS);
        IF (act.stackbase - fixed_SP) > 10000 THEN
          fixed_SP := VerifySP (fixed_SP, act.stackbase);
        END;
        RTHeapRep.FlushThreadState(act.heapState);
        p(fixed_SP, act.stackbase); (* Process the stack *)
        p(ADR(context.Edi), ADR(context.Eip));  (* Process the registers *)
      END;
      act := act.next;
    UNTIL (act = allThreads);
  END ProcessStacks;

PROCEDURE VerifySP (start, stop: ADDRESS): ADDRESS =
  (* Apparently, Win95 will lie about a thread's stack pointer! *)
  (* Verify that the claimed stack pages are really readable... *)
  CONST PageSize = 4096;
  CONST N = BYTESIZE (info);
  VAR info: MEMORY_BASIC_INFORMATION;
  BEGIN
    info.BaseAddress := LOOPHOLE (stop-1, ADDRESS);
    LOOP
      IF (info.BaseAddress <= start) THEN
        info.BaseAddress := start;
        EXIT;
      END;

      IF VirtualQuery (info.BaseAddress, ADR (info), N) # N THEN
        Choke(ThisLine());
      END;
 
      (* is this chunk readable? *)
      IF (info.Protect # PAGE_READWRITE)
        AND (info.Protect # PAGE_READONLY) THEN
        (* nope, return the base of the last good chunk *)
        INC (info.BaseAddress, info.RegionSize);
        EXIT;
      END;

      (* yep, try the next chunk *)
      DEC (info.BaseAddress, PageSize);
    END;

    RETURN info.BaseAddress;
  END VerifySP;

(*------------------------------------------------------------ misc. stuff ---*)

PROCEDURE MyId(): Id RAISES {}=
  VAR self := Self ();
  BEGIN
    RETURN self.id;
  END MyId;

PROCEDURE MyHeapState(): UNTRACED REF RTHeapRep.ThreadState =
  VAR me := GetActivation();
  BEGIN
    RETURN ADR(me.heapState);
  END MyHeapState;

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

PROCEDURE PerfChanged (id: Id; s: State) =
  (* LL = threadMu *)
  VAR e := ThreadEvent.T {kind := TE.Changed, id := id, state := s};
  BEGIN
    EnterCriticalSection_perfMu();
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    LeaveCriticalSection_perfMu();
  END PerfChanged;

PROCEDURE PerfDeleted (id: Id) =
  (* LL = threadMu *)
  VAR e := ThreadEvent.T {kind := TE.Deleted, id := id};
  BEGIN
    EnterCriticalSection_perfMu();
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    LeaveCriticalSection_perfMu();
  END PerfDeleted;

PROCEDURE PerfRunning (id: Id) =
  (* LL = threadMu *)
  VAR e := ThreadEvent.T {kind := TE.Running, id := id};
  BEGIN
    EnterCriticalSection_perfMu();
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    LeaveCriticalSection_perfMu();
  END PerfRunning;

(*-------------------------------------------------------- Initialization ---*)

PROCEDURE Init() =
  VAR
    self: T;
    me := InitActivations();
  BEGIN
    threadMu := NEW(Mutex);
    self := CreateT(me);
    self.id := nextId;  INC (nextId);

    heapCond := NEW(Condition);

    me.stackbase := InitialStackBase (ADR (self));
    IF me.stackbase = NIL THEN Choke(ThisLine()); END;

    PerfStart();
    IF perfOn THEN PerfChanged(self.id, State.alive) END;

    IF RTParams.IsPresent("backgroundgc") THEN
      RTCollectorSRC.StartBackgroundCollection();
    END;
    IF RTParams.IsPresent("foregroundgc") THEN
      RTCollectorSRC.StartForegroundCollection();
    END;
  END Init;

PROCEDURE InitialStackBase (start: ADDRESS): ADDRESS =
  (* Find the bottom of the stack containing "start". *)
  CONST N = BYTESIZE (info);
  VAR info: MEMORY_BASIC_INFORMATION;  last_good: ADDRESS;
  BEGIN
    last_good := start;
    info.BaseAddress := start;
    LOOP
      IF VirtualQuery (info.BaseAddress, ADR (info), N) # N THEN
        Choke(ThisLine());
      END;
 
      (* is this chunk readable? *)
      IF (info.Protect # PAGE_READWRITE)
        AND (info.Protect # PAGE_READONLY) THEN
        (* nope, return the base of the last good chunk *)
        RETURN last_good;
      END;

      (* yep, try the previous chunk *)
      last_good := info.BaseAddress + info.RegionSize;
      info.BaseAddress := last_good;
    END;
  END InitialStackBase;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector. *)

VAR
  inCritical := 0;      (* LL = cs *)
  heapCond: Condition;

PROCEDURE LockHeap (<*UNUSED*> VAR me: RTHeapRep.ThreadState) =
  BEGIN
    EnterCriticalSection_cs();
    INC(inCritical);
  END LockHeap;

PROCEDURE UnlockHeap (<*UNUSED*> VAR me: RTHeapRep.ThreadState) =
  BEGIN
    DEC(inCritical);
    LeaveCriticalSection_cs();
  END UnlockHeap;

PROCEDURE WaitHeap (<*UNUSED*> VAR me: RTHeapRep.ThreadState) =
  VAR self := Self();
  BEGIN
    EnterCriticalSection_cm();

    <* ASSERT( (self.waitingOn=NIL) AND (self.nextWaiter=NIL) ) *>
    self.waitingOn := heapCond;
    self.nextWaiter := heapCond.waiters;
    heapCond.waiters := self;
    LeaveCriticalSection_cm();

    DEC(inCritical);
    <*ASSERT inCritical = 0*>
    LeaveCriticalSection_cs();

    IF WaitForSingleObject(self.waitSema, INFINITE) # 0 THEN
      Choke(ThisLine());
    END;

    EnterCriticalSection_cs();
    <*ASSERT inCritical = 0*>
    INC(inCritical);
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  BEGIN
    Broadcast(heapCond);
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

BEGIN
END ThreadWin32.
