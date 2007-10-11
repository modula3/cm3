(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

UNSAFE MODULE ThreadPThread
EXPORTS Thread, ThreadF, Scheduler, SchedulerPosix, RTOS, RTHooks;

IMPORT Cerrno, FloatMode, MutexRep,
       RTCollectorSRC, RTError,  RTHeapRep, RTIO, RTMachine, RTParams,
       RTPerfTool, RTProcess, ThreadEvent, Time,
       Unix, Utime, Word, Upthread, Usched, Usem, Usignal,
       Uucontext, Uerror;
FROM Upthread
IMPORT pthread_t, pthread_cond_t, pthread_key_t, pthread_attr_t,
       PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER;
FROM Compiler IMPORT ThisFile, ThisLine;
IMPORT Ctypes, Utypes;

(*----------------------------------------------------- types and globals ---*)

VAR
  cm := PTHREAD_MUTEX_INITIALIZER; (* global lock for fields of Mutex/Condition *)

  stack_grows_down: BOOLEAN;

  nextId: CARDINAL := 1;

  threadMu: Mutex;			 (* global lock for fields of T *)

  activeMu := PTHREAD_MUTEX_INITIALIZER; (* global lock for list of active threads *)
  slotMu   := PTHREAD_MUTEX_INITIALIZER; (* global lock for thread slot table *)

REVEAL
  Mutex = MutexRep.Public BRANDED "Mutex Pthread-1.0" OBJECT
    waiters: T := NIL;			 (* LL = cm *)
    holder:  T := NIL;			 (* LL = cm *)
  OVERRIDES
    acquire := Acquire;
    release := Release;
  END;

  Condition = BRANDED "Thread.Condition Pthread-1.0" OBJECT
    waiters: T := NIL;			 (* LL = cm *)
    mutex: Mutex := NIL;		 (* LL = cm *)
  END;

  T = BRANDED "Thread.T Pthread-1.6" OBJECT
    (* live thread data *)
    act: Activation := NIL;		 (* LL = threadMu *)

    (* our work and its result *)
    closure: Closure := NIL;		 (* LL = threadMu *)
    result: REFANY := NIL;		 (* LL = threadMu *)

    (* wait here to join *)
    cond: Condition := NIL;		 (* LL = threadMu *)

    (* CV that we're blocked on *)
    waitingOn: Condition := NIL;	 (* LL = cm *)
    (* queue of threads waiting on the same CV *)
    nextWaiter: T := NIL;		 (* LL = cm *)

    (* condition for blocking during "Wait" *)
    waitCond: UNTRACED REF pthread_cond_t;

    (* the alert flag *)
    alerted : BOOLEAN := FALSE;		 (* LL = cm *)

    (* indicates that "result" is set *)
    completed: BOOLEAN := FALSE;	 (* LL = threadMu *)

    (* "Join" or "AlertJoin" has already returned *)
    joined: BOOLEAN := FALSE;            (* LL = threadMu *)

    (* unique Id of this thread *)
    id: Id := 0;			 (* LL = threadMu *)
  END;

TYPE
  ActState = { Starting, Started, Stopping, Stopped };
  Activation = UNTRACED REF RECORD
    (* global doubly-linked, circular list of all active threads *)
    next, prev: Activation := NIL;	 (* LL = activeMu *)
    (* thread handle *)
    handle: pthread_t;			 (* LL = activeMu *)
    (* base of thread stack for use by GC *)
    stackbase: ADDRESS := NIL;		 (* LL = activeMu *)
    sp: ADDRESS := NIL;			 (* LL = activeMu *)
    size: INTEGER;			 (* LL = activeMu *)

    state := ActState.Started;		 (* LL = activeMu *)

    (* index into global array of active, slotted threads *)
    slot: INTEGER;			 (* LL = slotMu *)

    (* state that is available to the floating point routines *)
    floatState : FloatMode.ThreadState;

    (* state that is available to the heap routines *)
    heapState : RTHeapRep.ThreadState;
  END;

(*----------------------------------------------------------------- Mutex ---*)

PROCEDURE InnerLockMutex (self: T; m: Mutex) =
  (* LL = cm *)
  VAR next, prev: T;
  BEGIN
    IF (m.holder = NIL) THEN
      m.holder := self;
    ELSIF (m.holder = self) THEN
      Die(ThisLine(), "Acquire of mutex already locked by self");
    ELSE
      (* somebody already has the mutex locked.  We'll need to wait *)
      <*ASSERT self.waitingOn = NIL*>
      <*ASSERT self.nextWaiter = NIL*>
      next := m.waiters;
      IF (next = NIL) THEN
        m.waiters := self;
      ELSE
        (* put me on the list of waiters *)
        prev := NIL;
        WHILE (next # NIL) DO prev := next; next := next.nextWaiter; END;
        prev.nextWaiter := self;
      END;
      LOOP
        WITH r = Upthread.cond_wait(self.waitCond^, cm) DO <*ASSERT r=0*> END;
        IF m.holder = self THEN EXIT END;
      END;
      <*ASSERT self.waitingOn = NIL*>
      <*ASSERT self.nextWaiter = NIL*>
    END;
  END InnerLockMutex;

PROCEDURE Acquire (m: Mutex) =
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "Acquire called from a non-Modula-3 thread");
    END;
    IF perfOn THEN PerfChanged(self.id, State.locking) END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    InnerLockMutex(self, m);
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
    IF perfOn THEN PerfRunning(self.id) END;
  END Acquire;

PROCEDURE InnerUnlockMutex (self: T; m: Mutex) =
  (* LL = cm+m *)
  VAR next: T;
  BEGIN
    IF (m.holder = NIL) THEN
      Die(ThisLine(), "Release of unlocked mutex");
    ELSIF (m.holder = self) THEN
      m.holder := NIL;
    ELSE
      Die(ThisLine(), "Release of mutex locked by another thread");
    END;

    (* Let the next guy go... *)
    next := m.waiters;
    IF next # NIL THEN
      (* let the next guy go... *)
      m.waiters := next.nextWaiter;
      next.nextWaiter := NIL;
      <*ASSERT next.waitingOn = NIL*>
      m.holder := next;
      WITH r = Upthread.cond_signal(next.waitCond^) DO <*ASSERT r=0*> END;
    END;
  END InnerUnlockMutex;

PROCEDURE Release (m: Mutex) =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "Release called from a non-Modula-3 thread");
    END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    InnerUnlockMutex(self, m);
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
  END Release;

(*---------------------------------------- Condition variables and Alerts ---*)

PROCEDURE XWait (self: T; m: Mutex; c: Condition; alertable: BOOLEAN)
  RAISES {Alerted} =
  (* LL = m *)
  VAR next, prev: T;
  BEGIN
    TRY
      <*ASSERT self.waitingOn = NIL*>
      <*ASSERT self.nextWaiter = NIL*>
      IF perfOn THEN PerfChanged(self.id, State.waiting) END;
      WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
      IF c.mutex = NIL THEN
        <*ASSERT c.waiters = NIL*>
      ELSIF c.mutex = m THEN
        <*ASSERT c.waiters # NIL*>
      ELSE
        Die(ThisLine(), "Wait using mutex not associated with condition");
      END;
      InnerUnlockMutex(self, m);
      IF alertable AND self.alerted THEN self.alerted := FALSE; RAISE Alerted; END;
      self.waitingOn := c;
      next := c.waiters;
      IF next = NIL THEN
        c.mutex := m;
        c.waiters := self;
      ELSE
        (* put me on the list of waiters *)
        prev := NIL;
        WHILE next # NIL DO prev := next; next := next.nextWaiter; END;
        prev.nextWaiter := self;
      END;
      LOOP
        WITH r = Upthread.cond_wait(self.waitCond^, cm) DO <*ASSERT r=0*> END;
        IF alertable AND self.alerted THEN self.alerted := FALSE; RAISE Alerted; END;
        IF self.waitingOn = NIL THEN RETURN END;
      END;
    FINALLY
      IF self.waitingOn # NIL THEN
        (* alerted: dequeue from condition *)
        next := self.waitingOn.waiters; prev := NIL;
        WHILE next # self DO
          <*ASSERT next # NIL*>
          prev := next; next := next.nextWaiter;
        END;
        IF prev = NIL
          THEN self.waitingOn.waiters := self.nextWaiter;
          ELSE prev.nextWaiter := self.nextWaiter;
        END;
        self.nextWaiter := NIL;
        self.waitingOn := NIL;
      END;
      InnerLockMutex(self, m);
      WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
      IF perfOn THEN PerfRunning(self.id) END;
      <*ASSERT self.waitingOn = NIL*>
      <*ASSERT self.nextWaiter = NIL*>
    END;
  END XWait;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "AlertWait called from non-Modula-3 thread");
    END;
    XWait(self, m, c, alertable := TRUE);
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  <*FATAL Alerted*>
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "Wait called from non-Modula-3 thread");
    END;
    XWait(self, m, c, alertable := FALSE);
  END Wait;

PROCEDURE DequeueHead(c: Condition) =
  (* LL = cm *)
  VAR t: T;
  BEGIN
    t := c.waiters; c.waiters := t.nextWaiter;
    t.nextWaiter := NIL;
    t.waitingOn := NIL;
    IF c.waiters = NIL THEN c.mutex := NIL END;
    WITH r = Upthread.cond_signal(t.waitCond^) DO <*ASSERT r=0*> END;
  END DequeueHead;

PROCEDURE Signal (c: Condition) =
  BEGIN
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    IF c.waiters # NIL THEN DequeueHead(c) END;
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  BEGIN
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    WHILE c.waiters # NIL DO DequeueHead(c) END;
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
  END Broadcast;

PROCEDURE Alert (t: T) =
  BEGIN
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    t.alerted := TRUE;
    IF t.waitCond # NIL THEN
      WITH r = Upthread.cond_signal(t.waitCond^) DO <*ASSERT r=0*> END;
    END;
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
  END Alert;

PROCEDURE XTestAlert (self: T): BOOLEAN =
  BEGIN
    TRY
      WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
      RETURN self.alerted;
    FINALLY
      self.alerted := FALSE;
      WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
    END;
  END XTestAlert;

PROCEDURE TestAlert (): BOOLEAN =
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "TestAlert called from non-Modula-3 thread");
    END;
    RETURN XTestAlert(self);
  END TestAlert;

(*------------------------------------------------------------------ Self ---*)

VAR
  initActivations := TRUE;
  activations: pthread_key_t;		 (* TLS index *)

VAR (* LL = slotMu *)
  n_slotted := 0;
  next_slot := 1;
  slots: REF ARRAY OF T;		 (* NOTE: we don't use slots[0] *)

PROCEDURE InitActivations () =
  VAR me := NEW(Activation);
  BEGIN
    WITH r = Upthread.key_create(activations, NIL) DO <*ASSERT r=0*> END;
    WITH r = Upthread.setspecific(activations, me) DO <*ASSERT r=0*> END;
    WITH r = Upthread.mutex_lock(activeMu) DO <*ASSERT r=0*> END;
      <* ASSERT allThreads = NIL *>
      me.handle := Upthread.self();
      me.next := me;
      me.prev := me;
      allThreads := me;
      initActivations := FALSE;
    WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
  END InitActivations;

PROCEDURE SetActivation (act: Activation) =
  (* LL = 0 *)
  VAR v := LOOPHOLE(act, ADDRESS);
  BEGIN
    IF initActivations THEN InitActivations() END;
    WITH r = Upthread.setspecific(activations, v) DO <*ASSERT r=0*> END;
  END SetActivation;

PROCEDURE GetActivation (): Activation =
  (* If not the initial thread and not created by Fork, returns NIL *)
  (* LL = 0 *)
  BEGIN
    IF initActivations THEN InitActivations() END;
    RETURN LOOPHOLE(Upthread.getspecific(activations), Activation);
  END GetActivation;

PROCEDURE Self (): T =
  (* If not the initial thread and not created by Fork, returns NIL *)
  (* LL = 0 *)
  VAR
    me := GetActivation();
    t: T;
  BEGIN
    IF me = NIL THEN RETURN NIL END;
    WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
      t := slots[me.slot];
    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
    IF (t.act # me) THEN Die(ThisLine(), "thread with bad slot!") END;
    RETURN t;
  END Self;

PROCEDURE AssignSlot (t: T) =
  (* LL = 0, cause we allocate stuff with NEW! *)
  VAR n: CARDINAL;  new_slots: REF ARRAY OF T;
  BEGIN
    WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;

      (* make sure we have room to register this guy *)
      IF (slots = NIL) THEN
        WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
          slots := NEW (REF ARRAY OF T, 20);
        WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
      END;
      IF (n_slotted >= LAST (slots^)) THEN
        n := NUMBER (slots^);
        WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
          new_slots := NEW (REF ARRAY OF T, n+n);
        WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
        IF (n = NUMBER (slots^)) THEN
          (* we won any races that may have occurred. *)
          SUBARRAY (new_slots^, 0, n) := slots^;
          slots := new_slots;
        ELSIF (n_slotted < LAST (slots^)) THEN
          (* we lost a race while allocating a new slot table,
             and the new table has room for us. *)
        ELSE
          (* ouch, the new table is full too!   Bail out and retry *)
          WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
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

    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
  END AssignSlot;

PROCEDURE FreeSlot (t: T) =
  (* LL = 0 *)
  BEGIN
    WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;

      DEC (n_slotted);
      WITH z = slots [t.act.slot] DO
        IF (z # t) THEN Die (ThisLine(), "unslotted thread!"); END;
        z := NIL;
      END;
      t.act.slot := 0;

    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
  END FreeSlot;

PROCEDURE CheckSlot (t: T): BOOLEAN =
  (* LL = 0 *)
  VAR
    me := t.act;
    result := me # NIL AND me.slot > 0;
  BEGIN
    WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
      result := result AND slots[me.slot] = t;
    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
    RETURN result;
  END CheckSlot;

PROCEDURE DumpThread (t: T) =
  BEGIN
    RTIO.PutText("Thread: "); RTIO.PutAddr(LOOPHOLE(t, ADDRESS)); RTIO.PutChar('\n');
    RTIO.PutText("  act:        "); RTIO.PutAddr(LOOPHOLE(t.act, ADDRESS));           RTIO.PutChar('\n');
    RTIO.PutText("  closure:    "); RTIO.PutAddr(LOOPHOLE(t.closure, ADDRESS));       RTIO.PutChar('\n');
    RTIO.PutText("  result:     "); RTIO.PutAddr(LOOPHOLE(t.result, ADDRESS));        RTIO.PutChar('\n');
    RTIO.PutText("  cond:       "); RTIO.PutAddr(LOOPHOLE(t.cond, ADDRESS));          RTIO.PutChar('\n');
    RTIO.PutText("  waitingOn:  "); RTIO.PutAddr(LOOPHOLE(t.waitingOn, ADDRESS));     RTIO.PutChar('\n');
    RTIO.PutText("  nextWaiter: "); RTIO.PutAddr(LOOPHOLE(t.nextWaiter, ADDRESS));    RTIO.PutChar('\n');
    RTIO.PutText("  waitCond:   "); RTIO.PutAddr(t.waitCond);      RTIO.PutChar('\n');
    RTIO.PutText("  alerted:    "); RTIO.PutInt(ORD(t.alerted));   RTIO.PutChar('\n');
    RTIO.PutText("  completed:  "); RTIO.PutInt(ORD(t.completed)); RTIO.PutChar('\n');
    RTIO.PutText("  joined:     "); RTIO.PutInt(ORD(t.joined));    RTIO.PutChar('\n');
    RTIO.PutText("  id:         "); RTIO.PutInt(t.id);             RTIO.PutChar('\n');
    RTIO.Flush();
  END DumpThread;

<*UNUSED*>
PROCEDURE DumpThreads () =
  VAR t: T;
  BEGIN
    FOR i := 1 TO LAST(slots^) DO
      t := slots[i];
      IF t # NIL THEN
        DumpThread(t);
      END;
    END;
  END DumpThreads;

(*------------------------------------------------------------ Fork, Join ---*)

VAR (* LL=activeMu *)
  allThreads: Activation := NIL;	 (* global list of active threads *)

PROCEDURE CreateT (act: Activation): T =
  (* LL = 0, because allocating a traced reference may cause
     the allocator to start a collection which will call "SuspendOthers"
     which will try to acquire "activeMu". *)
  VAR t := NEW(T, act := act);
  BEGIN
    t.waitCond := NEW(UNTRACED REF pthread_cond_t);
    WITH r = Upthread.cond_init (t.waitCond^, NIL) DO <*ASSERT r=0*> END;
    t.cond     := NEW(Condition);
    FloatMode.InitThread (act.floatState);
    AssignSlot (t);
    RETURN t;
  END CreateT;

(* ThreadBase calls RunThread after finding (approximately) where
   its stack begins.  This dance ensures that all of ThreadMain's
   traced references are within the stack scanned by the collector. *)

PROCEDURE ThreadBase (param: ADDRESS): ADDRESS =
  VAR
    xx: INTEGER;
    me: Activation := LOOPHOLE (param, Activation);
  BEGIN
    SetActivation (me);
    (* We need to establish this binding before this thread touches any
       traced references.  Otherwise, it may trigger a heap page fault,
       which would call SuspendOthers, which requires an Activation. *)

    me.stackbase := ADR(xx);          (* enable GC scanning of this stack *)
    RunThread(me);
    me.stackbase := NIL;              (* disable GC scanning of my stack *)

    DISPOSE (me);
    RETURN NIL;
  END ThreadBase;

PROCEDURE RunThread (me: Activation) =
  VAR self: T;  res: REFANY;
  BEGIN
    WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
      self := slots [me.slot];
    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;

    (* Let parent know we are running *)
    LOCK threadMu DO Signal(self.cond) END;

    (* Run the user-level code. *)
    IF perfOn THEN PerfRunning(self.id) END;
    res := self.closure.apply();
    IF perfOn THEN PerfChanged(self.id, State.dying) END;

    LOCK threadMu DO
      (* mark "self" done and clean it up a bit *)
      self.result := res;
      self.closure := NIL;
      self.completed := TRUE;
      Broadcast(self.cond); (* let everybody know that "self" is done *)
    END;

    IF perfOn THEN PerfDeleted(self.id) END;

    (* we're dying *)
    RTHeapRep.ClosePool(me.heapState.newPool);

    WITH r = Upthread.cond_destroy(self.waitCond^) DO <*ASSERT r=0*> END;
    DISPOSE(self.waitCond);

    FreeSlot(self);  (* note: needs self.act ! *)
    (* Since we're no longer slotted, we cannot touch traced refs. *)

    (* remove ourself from the list of active threads *)
    WITH r = Upthread.mutex_lock(activeMu) DO <*ASSERT r=0*> END;
      IF allThreads = me THEN allThreads := me.next; END;
      me.next.prev := me.prev;
      me.prev.next := me.next;
      me.next := NIL;
      me.prev := NIL;
      WITH r = Upthread.detach(me.handle) DO <*ASSERT r=0*> END;
    WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
  END RunThread;

PROCEDURE Fork (closure: Closure): T =
  VAR
    act := NEW(Activation);
    t := CreateT(act);
    attr: pthread_attr_t;
    size := defaultStackSize;
    bytes: Utypes.size_t;
  BEGIN
    (* determine the initial size of the stack for this thread *)
    TYPECASE closure OF
    | SizedClosure (scl) => size := scl.stackSize;
    ELSE (*skip*)
    END;

    WITH r = Upthread.mutex_lock(activeMu) DO <*ASSERT r=0*> END;
      WITH r = Upthread.attr_init(attr) DO <*ASSERT r=0*> END;
      WITH r = Upthread.attr_getstacksize(attr, bytes)  DO <*ASSERT r=0*> END;
      bytes := MAX(bytes, size * ADRSIZE(Word.T));
      EVAL Upthread.attr_setstacksize(attr, bytes);
      act.next := allThreads;
      act.prev := allThreads.prev;
      act.size := size;
      allThreads.prev.next := act;
      allThreads.prev := act;
      LOCK threadMu DO
        WITH r = Upthread.create(act.handle, attr, ThreadBase, act) DO
          IF r # 0 THEN
            RTError.MsgI(ThisFile(), ThisLine(),
                         "Thread client error: Fork failed with error: ", r);
          END;
        END;
        (* last minute sanity checking *)
        <* ASSERT CheckSlot (t) *>
        <* ASSERT t.act.next # NIL *>
        <* ASSERT t.act.prev # NIL *>

        t.closure := closure;
        t.id := nextId;  INC(nextId);
        IF perfOn THEN PerfChanged(t.id, State.alive) END;
        Wait(threadMu, t.cond);
      END;
    WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;

    RETURN t;
  END Fork;

PROCEDURE Join (t: T): REFANY =
  VAR res: REFANY;
  BEGIN
    LOCK threadMu DO
      IF t.joined THEN
        Die(ThisLine(), "attempt to join with thread twice");
      END;
      WHILE NOT t.completed DO Wait(threadMu, t.cond) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
      t.cond := NIL;
      IF perfOn THEN PerfChanged(t.id, State.dead) END;
    END;
    RETURN res;
  END Join;

PROCEDURE AlertJoin (t: T): REFANY RAISES {Alerted} =
  VAR res: REFANY;
  BEGIN
    LOCK threadMu DO
      IF t.joined THEN
        Die(ThisLine(), "attempt to join with thread twice");
      END;
      WHILE NOT t.completed DO AlertWait(threadMu, t.cond) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
      t.cond := NIL;
      IF perfOn THEN PerfChanged(t.id, State.dead) END;
    END;
    RETURN res;
  END AlertJoin;

(*---------------------------------------------------- Scheduling support ---*)

PROCEDURE ToNTime (n: LONGREAL; VAR ts: Utime.struct_timespec) =
  BEGIN
    ts.tv_sec := TRUNC(n);
    ts.tv_nsec := ROUND((n - FLOAT(ts.tv_sec, LONGREAL)) * 1.0D9);
  END ToNTime;

PROCEDURE XPause (self: T; n: LONGREAL; alertable: BOOLEAN) RAISES {Alerted} =
  VAR amount, remaining: Utime.struct_timespec;
  BEGIN
    IF alertable AND XTestAlert(self) THEN RAISE Alerted END;
    IF n <= 0.0d0 THEN RETURN END;
    ToNTime(n, amount);
    LOOP
      WITH r = Utime.nanosleep(amount, remaining) DO
        IF alertable AND XTestAlert(self) THEN RAISE Alerted END;
        IF r = 0 THEN EXIT END;
        amount := remaining;
      END;
    END;
  END XPause;

PROCEDURE Pause (n: LONGREAL) =
  <*FATAL Alerted*>
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "Pause called from a non-Modula-3 thread");
    END;
    TRY
      IF perfOn THEN PerfChanged(self.id, State.pausing) END;
      XPause(self, n, alertable := FALSE);
    FINALLY
      IF perfOn THEN PerfRunning(self.id) END;
    END;
  END Pause;

PROCEDURE AlertPause (n: LONGREAL) RAISES {Alerted} =
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "AlertPause called from a non-Modula-3 thread");
    END;
    TRY
      IF perfOn THEN PerfChanged(self.id, State.pausing) END;
      XPause(self, n, alertable := TRUE);
    FINALLY
      IF perfOn THEN PerfRunning(self.id) END;
    END;
  END AlertPause;

PROCEDURE Yield () =
  BEGIN
    WITH r = Usched.yield() DO
      IF r # 0 THEN
        RTError.MsgI(ThisFile(), ThisLine(),
                     "Thread client error: Yield failed with error: ",
                     Cerrno.GetErrno());
      END;
    END;
  END Yield;

CONST FDSetSize = BITSIZE(INTEGER);

TYPE
  FDSet = SET OF [0 .. FDSetSize-1];
  FDS = REF ARRAY OF FDSet;

PROCEDURE IOWait (fd: INTEGER; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult =
  <*FATAL Alerted*>
  VAR self := Self();
  BEGIN
    TRY
      IF perfOn THEN PerfChanged(self.id, State.blocking) END;
      RETURN XIOWait(self, fd, read, timeoutInterval, alertable := FALSE);
    FINALLY
      IF perfOn THEN PerfRunning(self.id) END;
    END;
  END IOWait;

PROCEDURE IOAlertWait (fd: INTEGER; read: BOOLEAN;
                       timeoutInterval: LONGREAL := -1.0D0): WaitResult
  RAISES {Alerted} =
  VAR self := Self();
  BEGIN
    TRY
      IF perfOn THEN PerfChanged(self.id, State.blocking) END;
      RETURN XIOWait(self, fd, read, timeoutInterval, alertable := TRUE);
    FINALLY
      IF perfOn THEN PerfRunning(self.id) END;
    END;
  END IOAlertWait;

PROCEDURE XIOWait (self: T; fd: CARDINAL; read: BOOLEAN; interval: LONGREAL;
                   alertable: BOOLEAN): WaitResult
  RAISES {Alerted} =
  VAR
    res: INTEGER;
    fdindex := fd DIV FDSetSize;
    fdset := FDSet{fd MOD FDSetSize};
    gReadFDS, gWriteFDS, gExceptFDS: FDS := NEW(FDS, fdindex+1);
    subInterval: LONGREAL := 1.0d0;

  PROCEDURE TestFDS (index: CARDINAL; set: FDSet; read: BOOLEAN): WaitResult =
    BEGIN
      IF (set * gExceptFDS[index]) # FDSet{} THEN
        IF read THEN
          IF (set * gReadFDS[index]) # FDSet{} THEN
            RETURN WaitResult.Ready;
          END;
          IF (set * gWriteFDS[index]) = FDSet{} THEN
            RETURN WaitResult.FDError;
          END;
        ELSE
          IF (set * gWriteFDS[index]) # FDSet{} THEN
            RETURN WaitResult.Ready;
          END;
          IF (set * gReadFDS[index]) = FDSet{} THEN
            RETURN WaitResult.FDError;
          END;
        END;
      END;
      RETURN WaitResult.Timeout;
    END TestFDS;

  PROCEDURE CallSelect (nfd: CARDINAL; timeout: UNTRACED REF UTime): INTEGER =
    TYPE FDSPtr = UNTRACED REF Unix.FDSet;
    VAR res: INTEGER;
    BEGIN
      FOR i := 0 TO fdindex DO
        gExceptFDS[i] := gReadFDS[i] + gWriteFDS[i];
      END;
      res := Unix.select(nfd,
                         LOOPHOLE (ADR(gReadFDS[0]), FDSPtr),
                         LOOPHOLE (ADR(gWriteFDS[0]), FDSPtr),
                         LOOPHOLE (ADR(gExceptFDS[0]), FDSPtr),
                         timeout);
      IF res > 0 THEN
        FOR i := 0 TO fdindex DO
          gExceptFDS[i] := gExceptFDS[i] + gReadFDS[i] + gWriteFDS[i];
        END;
      END;
      RETURN res;
    END CallSelect;

  BEGIN
    IF NOT alertable THEN
      subInterval := interval;
    ELSIF interval < 0.0d0 THEN
      interval := LAST(LONGREAL);
    ELSIF interval < subInterval THEN
      subInterval := interval;
    END;

    IF alertable AND XTestAlert(self) THEN RAISE Alerted END;
    LOOP
      FOR i := 0 TO fdindex-1 DO
        gReadFDS[i] := FDSet{};
        gWriteFDS[i] := FDSet{};
      END;
      IF read
        THEN gReadFDS[fdindex] := fdset;
        ELSE gWriteFDS[fdindex] := fdset;
      END;

      IF subInterval >= 0.0D0 THEN
        VAR utimeout := UTimeFromTime(subInterval);
        BEGIN
          res := CallSelect(fd+1, ADR(utimeout));
        END;
      ELSE
        res := CallSelect(fd+1, NIL);
      END;

      IF alertable AND XTestAlert(self) THEN RAISE Alerted END;

      IF    res > 0 THEN RETURN TestFDS(fdindex, fdset, read);
      ELSIF res = 0 THEN
        interval := interval - subInterval;
        IF interval <= 0.0d0 THEN RETURN WaitResult.Timeout END;
        IF interval < subInterval THEN
          subInterval := interval;
        END;
      ELSE
        IF Cerrno.GetErrno() = Uerror.EINTR THEN
          (* spurious wakeups are OK *)
        ELSE
          RETURN WaitResult.Error;
        END;
      END;
    END;
  END XIOWait;

TYPE UTime = Utime.struct_timeval;
PROCEDURE UTimeFromTime (time: Time.T): UTime =
  VAR floor := FLOOR(time);
  BEGIN
    RETURN UTime{floor, FLOOR(1.0D6 * (time - FLOAT(floor, LONGREAL)))};
  END UTimeFromTime;

(*--------------------------------------------------- Stack size controls ---*)

VAR defaultStackSize := 4096;

PROCEDURE GetDefaultStackSize (): CARDINAL =
  BEGIN
    RETURN defaultStackSize;
  END GetDefaultStackSize;

PROCEDURE MinDefaultStackSize (size: CARDINAL) =
  BEGIN
    defaultStackSize := MAX(defaultStackSize, size);
  END MinDefaultStackSize;

PROCEDURE IncDefaultStackSize (inc: CARDINAL) =
  BEGIN
    INC(defaultStackSize, inc);
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

VAR suspend_cnt: CARDINAL := 0;		 (* LL=activeMu *)

PROCEDURE SuspendOthers () =
  (* LL=0. Always bracketed with ResumeOthers which releases "activeMu" *)
  VAR me := GetActivation();
  BEGIN
    WITH r = Upthread.mutex_lock(activeMu) DO <*ASSERT r=0*> END;
    IF suspend_cnt = 0 THEN StopWorld(me) END;
    INC(suspend_cnt);
  END SuspendOthers;

PROCEDURE ResumeOthers () =
  (* LL=activeMu.  Always preceded by SuspendOthers. *)
  VAR me := GetActivation();
  BEGIN
    DEC(suspend_cnt);
    IF (suspend_cnt = 0) THEN StartWorld(me) END;
    WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
  END ResumeOthers;

PROCEDURE ProcessPools (p: PROCEDURE (VAR pool: RTHeapRep.AllocPool)) =
  (* LL=activeMu.  Only called within {SuspendOthers, ResumeOthers} *)
  VAR act := allThreads;
  BEGIN
    REPEAT
      p(act.heapState.newPool);
      act := act.next;
    UNTIL act = allThreads;
  END ProcessPools;

PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS)) =
  (* LL=activeMu.  Only called within {SuspendOthers, ResumeOthers} *)
  VAR
    me := GetActivation();
    sp: ADDRESS;
    myState: RTMachine.State;
    state: RTMachine.ThreadState;
    act := me;
    xx: INTEGER;
  BEGIN
    REPEAT
      IF DEBUG THEN
        RTIO.PutText("Processing act="); RTIO.PutAddr(act); RTIO.PutText("\n"); RTIO.Flush();
      END;
      IF (act.stackbase # NIL) THEN
        (* Process the registers *)
        IF act = me THEN
          IF RTMachine.SaveRegsInStack # NIL
            THEN sp := RTMachine.SaveRegsInStack();
            ELSE sp := ADR(xx);
          END;
          EVAL RTMachine.SaveState(myState);
          WITH z = myState DO
            p(ADR(z), ADR(z) + ADRSIZE(z));
          END;
        ELSIF RTMachine.GetState # NIL THEN
          (* Process explicit state *)
          <*ASSERT act.state = ActState.Stopped*>
          sp := RTMachine.GetState(act.handle, state);
          WITH z = state DO
            p(ADR(z), ADR(z) + ADRSIZE(z));
          END;
        ELSE
          sp := act.sp;
        END;

        (* Process the stack *)
        IF stack_grows_down
          THEN p(sp, act.stackbase);
          ELSE p(act.stackbase, sp);
        END;
      END;
      act := act.next;
    UNTIL act = me;
  END ProcessStacks;

(* Signal based suspend/resume *)
VAR ackSem: Usem.sem_t;

CONST SIG = RTMachine.SIG_SUSPEND;

PROCEDURE SuspendAll (me: Activation): INTEGER =
  (* LL=activeMu *)
  VAR
    nLive := 0;
    act := me.next;
    wait, remaining: Utime.struct_timespec;
  CONST
    WAIT_UNIT = 3000000;
  BEGIN
    IF RTMachine.SuspendThread # NIL THEN
      (* Use the native suspend routine *)
      LOOP
        WHILE act # me DO
          IF act.state # ActState.Stopped THEN
            act.state := ActState.Stopping;
            IF DEBUG THEN
              RTIO.PutText("Stopping act="); RTIO.PutAddr(act); RTIO.PutText("\n"); RTIO.Flush();
            END;
            IF RTMachine.SuspendThread(act.handle) THEN
              IF act.heapState.busy THEN
                RTMachine.RestartThread(act.handle);
                INC(nLive);
              ELSE
                act.state := ActState.Stopped;
                IF DEBUG THEN
                  RTIO.PutText("Stopped act="); RTIO.PutAddr(act); RTIO.PutText("\n"); RTIO.Flush();
                END;
              END;
            ELSE
              INC(nLive);
            END;
          END;
          act := act.next;
        END;
        IF nLive = 0 THEN EXIT END;
        wait.tv_sec := 0;
        wait.tv_nsec := WAIT_UNIT;
        WHILE Utime.nanosleep(wait, remaining) # 0 DO
          wait := remaining;
        END;
        act := me.next;
        nLive := 0;
      END;
    ELSE
      (* No native suspend routine so signal thread to suspend *)
      WHILE act # me DO
        IF act.state # ActState.Stopped THEN
          LOOP
            act.state := ActState.Stopping;
            IF DEBUG THEN
              RTIO.PutText("Stopping act="); RTIO.PutAddr(act); RTIO.PutText("\n"); RTIO.Flush();
            END;
            WITH r = Upthread.kill(act.handle, SIG) DO
              IF r = 0 THEN EXIT END;
              <*ASSERT r = Uerror.EAGAIN*>
              (* try it again... *)
            END;
          END;
          INC(nLive);
        END;
        act := act.next;
      END;
    END;
    RETURN nLive;	      (* return number still live (i.e., signalled) *)
  END SuspendAll;

PROCEDURE RestartAll (me: Activation) =
  (* LL=activeMu *)
  VAR act := me.next;
  BEGIN
    IF RTMachine.RestartThread # NIL THEN
      (* Use the native restart routine *)
      WHILE act # me DO
        <*ASSERT act.state = ActState.Stopped*>
        <*ASSERT NOT act.heapState.busy*>
        act.state := ActState.Starting;
        IF DEBUG THEN
          RTIO.PutText("Starting act="); RTIO.PutAddr(act); RTIO.PutText("\n"); RTIO.Flush();
        END;
        RTMachine.RestartThread(act.handle);
        act.state := ActState.Started;
        act := act.next;
      END;
    ELSE
      (* No native restart routine so signal thread to resume *)
      WHILE act # me DO
        <*ASSERT act.state = ActState.Stopped*>
        <*ASSERT NOT act.heapState.busy*>
        act.state := ActState.Starting;
        LOOP
          IF DEBUG THEN
            RTIO.PutText("Starting act="); RTIO.PutAddr(act); RTIO.PutText("\n"); RTIO.Flush();
          END;
          WITH r = Upthread.kill(act.handle, SIG) DO
            IF r = 0 THEN EXIT END;
            <*ASSERT r = Uerror.EAGAIN*>
            (* try it again... *)
          END;
        END;
        act := act.next;
      END;
    END;
  END RestartAll;

PROCEDURE StopWorld (me: Activation) =
  (* LL=activeMu *)
  VAR
    nLive, newlySent: INTEGER;
    wait_nsecs := 0;
    wait, remaining: Utime.struct_timespec;
    acks: Ctypes.int;
  CONST
    WAIT_UNIT = 1000000;
    RETRY_INTERVAL = 10000000;
  BEGIN
    IF DEBUG THEN
      RTIO.PutText("Stopping from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;
    nLive := SuspendAll (me);
    IF nLive > 0 THEN
      LOOP
        WITH r = Usem.getvalue(ackSem, acks) DO <*ASSERT r=0*> END;
        IF acks = nLive THEN EXIT END;
        <*ASSERT acks < nLive*>
        IF wait_nsecs > RETRY_INTERVAL THEN
          newlySent := SuspendAll(me);
          WITH r = Usem.getvalue(ackSem, acks) DO <*ASSERT r=0*> END;
          IF newlySent < nLive - acks THEN
            (* how did we manage to lose some? *)
            nLive := acks + newlySent;
          END;
          wait_nsecs := 0;
        END;
        wait.tv_sec := 0;
        wait.tv_nsec := WAIT_UNIT;
        WHILE Utime.nanosleep(wait, remaining) # 0 DO
          wait := remaining;
        END;
        INC(wait_nsecs, WAIT_UNIT);
      END;
      FOR i := 0 TO nLive-1 DO
        LOOP
          WITH r = Usem.wait(ackSem) DO
            IF r = 0 THEN EXIT END;
            IF Cerrno.GetErrno() = Uerror.EINTR THEN
              (*retry*)
            ELSE
              <*ASSERT FALSE*>
            END;
          END;
        END;
      END;
    END;

    IF DEBUG THEN
      RTIO.PutText("Stopped from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;
  END StopWorld;

PROCEDURE StartWorld (me: Activation) =
  BEGIN
    IF DEBUG THEN
      RTIO.PutText("Starting from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;
    RestartAll (me);
    IF DEBUG THEN
      RTIO.PutText("Started from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;
  END StartWorld;

PROCEDURE SignalHandler (sig: Ctypes.int;
                         <*UNUSED*> sip: Usignal.siginfo_t_star;
                         <*UNUSED*> uap: Uucontext.ucontext_t_star) =
  VAR
    errno := Cerrno.GetErrno();
    xx: INTEGER;
    me := GetActivation();
    m: Usignal.sigset_t;
  BEGIN
    <*ASSERT sig = SIG*>
    IF me = NIL THEN RETURN END;
    IF me.state # ActState.Stopping THEN RETURN END;
    IF me.heapState.busy THEN RETURN END;
    IF RTMachine.SaveRegsInStack # NIL
      THEN me.sp := RTMachine.SaveRegsInStack();
      ELSE me.sp := ADR(xx);
    END;
    me.state := ActState.Stopped;
    IF DEBUG THEN
      RTIO.PutText("Stopped act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;
    WITH r = Usem.post(ackSem) DO <*ASSERT r=0*> END;

    WITH r = Usignal.sigfillset(m) DO <*ASSERT r=0*> END;
    WITH r = Usignal.sigdelset(m, SIG) DO <*ASSERT r=0*> END;
    REPEAT EVAL Usignal.sigsuspend(m) UNTIL me.state = ActState.Starting;
    me.sp := NIL;
    me.state := ActState.Started;
    IF DEBUG THEN
      RTIO.PutText("Started act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;

    Cerrno.SetErrno(errno);
  END SignalHandler;

PROCEDURE SetupHandlers () =
  VAR act, oact: Usignal.struct_sigaction;
  BEGIN
    IF RTMachine.SuspendThread # NIL THEN <*ASSERT SIG = 0*> END;
    IF SIG = 0 THEN RETURN END;

    WITH r = Usem.init(ackSem, 0, 0) DO <*ASSERT r=0*> END;

    act.sa_flags := Word.Or(Usignal.SA_RESTART, Usignal.SA_SIGINFO);
    WITH r = Usignal.sigfillset(act.sa_mask) DO <*ASSERT r=0*> END;
    WITH r = Usignal.sigdelset(act.sa_mask, Usignal.SIGINT) DO <*ASSERT r=0*> END;
    WITH r = Usignal.sigdelset(act.sa_mask, Usignal.SIGQUIT) DO <*ASSERT r=0*> END;
    WITH r = Usignal.sigdelset(act.sa_mask, Usignal.SIGABRT) DO <*ASSERT r=0*> END;
    WITH r = Usignal.sigdelset(act.sa_mask, Usignal.SIGTERM) DO <*ASSERT r=0*> END;
    act.sa_sigaction := SignalHandler;
    WITH r = Usignal.sigaction(SIG, act, oact) DO <*ASSERT r=0*> END;
  END SetupHandlers;

(*------------------------------------------------------------ misc. junk ---*)

PROCEDURE MyId (): Id RAISES {} =
  VAR self := Self();
  BEGIN
    RETURN self.id;
  END MyId;

PROCEDURE GetMyFPState (reader: PROCEDURE(READONLY s: FloatMode.ThreadState)) =
  VAR me := GetActivation();
  BEGIN
    reader(me.floatState);
  END GetMyFPState;

PROCEDURE SetMyFPState (writer: PROCEDURE(VAR s: FloatMode.ThreadState)) =
  VAR me := GetActivation();
  BEGIN
    writer(me.floatState);
  END SetMyFPState;

PROCEDURE MyHeapState (): UNTRACED REF RTHeapRep.ThreadState =
  VAR me := GetActivation();
  BEGIN
    RETURN ADR(me.heapState);
  END MyHeapState;

PROCEDURE DisableSwitching () =
  BEGIN
  END DisableSwitching;

PROCEDURE EnableSwitching () =
  BEGIN
  END EnableSwitching;

(*---------------------------------------------------------------- errors ---*)

PROCEDURE Die (lineno: INTEGER; msg: TEXT) =
  BEGIN
    RTError.Msg (ThisFile(), lineno, "Thread client error: ", msg);
  END Die;

(*------------------------------------------------------ ShowThread hooks ---*)

VAR
  perfW : RTPerfTool.Handle;
  perfOn: BOOLEAN := FALSE;		 (* LL = perfMu *)
  perfMu := PTHREAD_MUTEX_INITIALIZER;

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
  VAR e := ThreadEvent.T {kind := TE.Changed, id := id, state := s};
  BEGIN
    WITH r = Upthread.mutex_lock(perfMu) DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = Upthread.mutex_unlock(perfMu) DO <*ASSERT r=0*> END;
  END PerfChanged;

PROCEDURE PerfDeleted (id: Id) =
  VAR e := ThreadEvent.T {kind := TE.Deleted, id := id};
  BEGIN
    WITH r = Upthread.mutex_lock(perfMu) DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = Upthread.mutex_unlock(perfMu) DO <*ASSERT r=0*> END;
  END PerfDeleted;

PROCEDURE PerfRunning (id: Id) =
  VAR e := ThreadEvent.T {kind := TE.Running, id := id};
  BEGIN
    WITH r = Upthread.mutex_lock(perfMu) DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = Upthread.mutex_unlock(perfMu) DO <*ASSERT r=0*> END;
  END PerfRunning;

(*-------------------------------------------------------- Initialization ---*)

PROCEDURE Init ()=
  VAR
    xx: INTEGER;
    self: T;
    me := GetActivation();
  BEGIN
    SetupHandlers ();

    (* cm, activeMu, slotMu: initialized statically *)
    threadMu := NEW(Mutex);
    self := CreateT(me);
    self.id := nextId;  INC(nextId);

    heapMu := NEW(MUTEX);
    heapCond := NEW(Condition);

    stack_grows_down := ADR(xx) > XX();

    WITH r = Upthread.mutex_lock(activeMu) DO <*ASSERT r=0*> END;
      me.stackbase := ADR(xx);
    WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
    PerfStart();
    IF perfOn THEN PerfRunning(self.id) END;
    IF RTParams.IsPresent("backgroundgc") THEN
      RTCollectorSRC.StartBackgroundCollection();
    END;
    IF RTParams.IsPresent("foregroundgc") THEN
      RTCollectorSRC.StartForegroundCollection();
    END;
  END Init;

PROCEDURE XX (): ADDRESS =
  VAR xx: INTEGER;
  BEGIN
    RETURN ADR(xx);
  END XX;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector. *)

VAR
  lockMu := PTHREAD_MUTEX_INITIALIZER;
  lockCond := PTHREAD_COND_INITIALIZER;
  holder: pthread_t;
  lock_cnt := 0;
  do_signal := FALSE;
  heapMu: MUTEX;
  heapCond: Condition;

PROCEDURE LockHeap () =
  VAR self := Upthread.self();
  BEGIN
    (* suspend_cnt # 0 => other threads are stopped and we hold the lock *)
    IF suspend_cnt # 0 THEN
      <*ASSERT lock_cnt # 0*>
      <*ASSERT Upthread.equal(holder, self) # 0*>
      RETURN;
    END;
    WITH r = Upthread.mutex_lock(lockMu) DO <*ASSERT r=0*> END;
    LOOP
      IF lock_cnt = 0 THEN holder := self; EXIT END;
      IF Upthread.equal(holder, self) # 0 THEN EXIT END;
      WITH r = Upthread.cond_wait(lockCond, lockMu) DO <*ASSERT r=0*> END;
    END;
    INC(lock_cnt);
    WITH r = Upthread.mutex_unlock(lockMu) DO <*ASSERT r=0*> END;
  END LockHeap;

PROCEDURE UnlockHeap () =
  VAR
    sig := FALSE;
    self := Upthread.self();
  BEGIN
    (* suspend_cnt # 0 => other threads are stopped and we hold the lock *)
    IF suspend_cnt # 0 THEN
      <*ASSERT lock_cnt # 0*>
      <*ASSERT Upthread.equal(holder, self) # 0*>
      RETURN;
    END;
    WITH r = Upthread.mutex_lock(lockMu) DO <*ASSERT r=0*> END;
      <*ASSERT Upthread.equal(holder, self) # 0*>
      DEC(lock_cnt);
      IF lock_cnt = 0 THEN
        WITH r = Upthread.cond_signal(lockCond) DO <*ASSERT r=0*> END;
        IF do_signal THEN sig := TRUE; do_signal := FALSE; END;
      END;
    WITH r = Upthread.mutex_unlock(lockMu) DO <*ASSERT r=0*> END;
    IF sig THEN Broadcast(heapCond) END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  (* LL = 0 *)
  BEGIN
    LOCK heapMu DO Wait(heapMu, heapCond); END;
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  (* LL = LockHeap *)
  BEGIN
    do_signal := TRUE;
  END BroadcastHeap;

(*--------------------------------------------- exception handling support --*)

VAR
  initHandlers := TRUE;
  handlers: pthread_key_t;

PROCEDURE GetCurrentHandlers (): ADDRESS =
  BEGIN
    IF initHandlers THEN InitHandlers() END;
    RETURN Upthread.getspecific(handlers);
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers (h: ADDRESS) =
  BEGIN
    IF initHandlers THEN InitHandlers() END;
    WITH r = Upthread.setspecific(handlers, h) DO <*ASSERT r=0*> END;
  END SetCurrentHandlers;

(*RTHooks.PushEFrame*)
PROCEDURE PushEFrame (frame: ADDRESS) =
  TYPE Frame = UNTRACED REF RECORD next: ADDRESS END;
  VAR f := LOOPHOLE (frame, Frame);
  BEGIN
    IF initHandlers THEN InitHandlers() END;
    f.next := Upthread.getspecific(handlers);
    WITH r = Upthread.setspecific(handlers, f) DO <*ASSERT r=0*> END;
  END PushEFrame;

(*RTHooks.PopEFrame*)
PROCEDURE PopEFrame (frame: ADDRESS) =
  BEGIN
    IF initHandlers THEN InitHandlers() END;
    WITH r = Upthread.setspecific(handlers, frame) DO <*ASSERT r=0*> END;
  END PopEFrame;

PROCEDURE InitHandlers () =
  BEGIN
    WITH r = Upthread.key_create(handlers, NIL) DO <*ASSERT r=0*> END;
    WITH r = Upthread.setspecific(handlers, NIL) DO <*ASSERT r=0*> END;
    initHandlers := FALSE;
  END InitHandlers;

VAR DEBUG := RTParams.IsPresent("debugthreads");

BEGIN
END ThreadPThread.
