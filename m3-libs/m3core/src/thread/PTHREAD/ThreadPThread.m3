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
  idleMu   := PTHREAD_MUTEX_INITIALIZER; (* global lock for list of idle threads *)
  slotMu   := PTHREAD_MUTEX_INITIALIZER; (* global lock for thread slot table *)

REVEAL
  Mutex = MutexRep.Public BRANDED "Mutex Pthread-1.0" OBJECT
    waiters: T := NIL;			 (* LL = cm *)
    holder:  T := NIL;			 (* LL = cm *)
  OVERRIDES
    acquire := LockMutex;
    release := UnlockMutex;
  END;

  Condition = BRANDED "Thread.Condition Pthread-1.0" OBJECT
    waiters: T := NIL;			 (* LL = cm *)
  END;

  T = BRANDED "Thread.T Pthread-1.6" OBJECT
    (* live thread data *)
    act: Activation := NIL;		 (* LL = threadMu *)

    (* our work and its result *)
    closure: Closure := NIL;		 (* LL = threadMu *)
    result: REFANY := NIL;		 (* LL = threadMu *)

    (* wait here to join, or for rebirth *)
    cond: Condition := NIL;		 (* LL = threadMu *)

    (* CV that we're blocked on *)
    waitingOn: Condition := NIL;	 (* LL = cm *)
    (* queue of threads waiting on the same CV *)
    nextWaiter: T := NIL;		 (* LL = cm *)

    (* condition for blocking during "Wait" *)
    waitCond: UNTRACED REF pthread_cond_t;

    (* distinguishes between "Wait" and "AlertWait" *)
    alertable: BOOLEAN := FALSE;	 (* LL = cm *)
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
  Activation = UNTRACED REF RECORD
    (* global doubly-linked, circular list of all active threads *)
    next, prev: Activation := NIL;	 (* LL = activeMu *)
    (* thread handle *)
    handle: pthread_t;			 (* LL = activeMu *)
    (* base of thread stack for use by GC *)
    stackbase: ADDRESS := NIL;
    sp: ADDRESS := NIL;
    size: INTEGER;
    lastStopCount: CARDINAL := 0;
    signal := 0;
    (* index into global array of active, slotted threads *)
    slot: INTEGER;			 (* LL = slotMu *)
    idle: BOOLEAN := FALSE;		 (* LL = idleMu *)

    (* state that is available to the floating point routines *)
    floatState : FloatMode.ThreadState;

    (* allocation pool *)
    newPool := RTHeapRep.NewPool;

    heapLockedByMe := FALSE;
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

PROCEDURE InnerLockMutex (m: Mutex; self: T) =
  (* LL = cm *)
  VAR next, prev: T;
  BEGIN
    self.alertable := FALSE;
    IF (m.holder = NIL) THEN
      m.holder := self;			 (* I get it! *)
    ELSIF (m.holder = self) THEN
      Die(ThisLine(), "Attempt to lock mutex already locked by self");
    ELSE
      (* somebody already has the mutex locked.  We'll need to wait *)
      self.nextWaiter := NIL;
      next := m.waiters;
      IF (next = NIL) THEN
        m.waiters := self;
      ELSE
        (* put me on the list of waiters *)
        prev := NIL;
        WHILE (next # NIL) DO prev := next; next := next.nextWaiter; END;
        prev.nextWaiter := self;
      END;
      WHILE m.holder # self DO
        WITH r = Upthread.cond_wait(self.waitCond^, cm) DO <*ASSERT r=0*> END;
      END;
    END;
  END InnerLockMutex;

PROCEDURE LockMutex (m: Mutex) =
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "Acquire called from a non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(self.id, State.locking) END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    InnerLockMutex(m, self);
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
  END LockMutex;

PROCEDURE InnerUnlockMutex (m: Mutex; self: T) =
  (* LL = cm *)
  VAR next: T;
  BEGIN
    (* Make sure I'm allowed to release this mutex *)
    IF m.holder = self THEN
      (* ok we're releasing the mutex *)
      m.holder := NIL;
    ELSIF m.holder = NIL THEN
      Die(ThisLine(), "attempt to release an unlocked mutex");
    ELSE
      Die(ThisLine(), "attempt to release a mutex locked by another thread");
    END;

    (* Let the next guy go... *)
    next := m.waiters;
    IF next # NIL THEN
      (* let the next guy go... *)
      m.waiters := next.nextWaiter;
      next.nextWaiter := NIL;
      m.holder := next;
      WITH r = Upthread.cond_signal(next.waitCond^) DO <*ASSERT r=0*> END;
    END;
  END InnerUnlockMutex;

PROCEDURE UnlockMutex (m: Mutex) =
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "Acquire called from a non-Modula-3 thread") END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    InnerUnlockMutex(m, self);
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
  END UnlockMutex;

(*---------------------------------------- Condition variables and Alerts ---*)

PROCEDURE InnerWait(m: Mutex; c: Condition; self: T) =
  (* LL = cm+m *)
  VAR next, prev: T;
  BEGIN
    <* ASSERT( (self.waitingOn=NIL) AND (self.nextWaiter=NIL) ) *>
    self.waitingOn := c;
    self.nextWaiter := NIL;
    next := c.waiters;
    IF next = NIL THEN
      c.waiters := self;
    ELSE
      (* put me on the list of waiters *)
      prev := NIL;
      WHILE next # NIL DO prev := next; next := next.nextWaiter; END;
      prev.nextWaiter := self;
    END;
    InnerUnlockMutex(m, self);
    WHILE self.waitingOn # NIL DO
      WITH r = Upthread.cond_wait(self.waitCond^, cm) DO <*ASSERT r=0*> END;
    END;
    InnerLockMutex(m, self);
  END InnerWait;

PROCEDURE InnerTestAlert(self: T) RAISES {Alerted} =
  (* LL = cm on entry; LL = cm on normal exit, 0 on exception exit *)
  (* If self.alerted, clear "alerted", leave cm and raise "Alerted". *)
  BEGIN
    IF self.alerted THEN
      self.alerted := FALSE;
      WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
      IF perfOn THEN PerfChanged(self.id, State.alive) END;
      RAISE Alerted;
    END;
  END InnerTestAlert;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "AlertWait called from non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(self.id, State.waiting) END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    InnerTestAlert(self);
    self.alertable := TRUE;
    InnerWait(m, c, self);
    InnerTestAlert(self);
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "Wait called from non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(self.id, State.waiting) END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    InnerWait(m, c, self);
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
  END Wait;

PROCEDURE DequeueHead(c: Condition) =
  (* LL = cm *)
  VAR t: T;
  BEGIN
    t := c.waiters; c.waiters := t.nextWaiter;
    t.nextWaiter := NIL;
    t.waitingOn := NIL;
    t.alertable := FALSE;
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
  VAR prev, next: T;
  BEGIN
    IF t = NIL THEN Die(ThisLine(), "Alert called from non-Modula-3 thread") END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    t.alerted := TRUE;
    IF t.alertable THEN
      (* Dequeue from any CV and unblock from the semaphore *)
      IF t.waitingOn # NIL THEN
        next := t.waitingOn.waiters; prev := NIL;
        WHILE next # t DO
          <* ASSERT next # NIL *>
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
      WITH r = Upthread.cond_signal(t.waitCond^) DO <*ASSERT r=0*> END;
    END;
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
  END Alert;

PROCEDURE TestAlert (): BOOLEAN =
  VAR self := Self(); result: BOOLEAN;
  BEGIN
    IF self = NIL THEN
      (* Not created by Fork; not alertable *)
      RETURN FALSE;
    ELSE
      WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
      result := self.alerted; IF result THEN self.alerted := FALSE END;
      WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
      RETURN result;
    END;
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

<*UNUSED*> PROCEDURE DumpThread (t: T) =
  BEGIN
    RTIO.PutText("Thread: "); RTIO.PutAddr(LOOPHOLE(t, ADDRESS)); RTIO.PutChar('\n');
    RTIO.PutText("  act:        "); RTIO.PutAddr(LOOPHOLE(t.act, ADDRESS));           RTIO.PutChar('\n');
    RTIO.PutText("  closure:    "); RTIO.PutAddr(LOOPHOLE(t.closure, ADDRESS));       RTIO.PutChar('\n');
    RTIO.PutText("  result:     "); RTIO.PutAddr(LOOPHOLE(t.result, ADDRESS));        RTIO.PutChar('\n');
    RTIO.PutText("  cond:       "); RTIO.PutAddr(LOOPHOLE(t.cond, ADDRESS));          RTIO.PutChar('\n');
    RTIO.PutText("  waitingOn:  "); RTIO.PutAddr(LOOPHOLE(t.waitingOn, ADDRESS));     RTIO.PutChar('\n');
    RTIO.PutText("  nextWaiter: "); RTIO.PutAddr(LOOPHOLE(t.nextWaiter, ADDRESS));    RTIO.PutChar('\n');
    RTIO.PutText("  waitCond:   "); RTIO.PutAddr(t.waitCond);      RTIO.PutChar('\n');
    RTIO.PutText("  alertable:  "); RTIO.PutInt(ORD(t.alertable)); RTIO.PutChar('\n');
    RTIO.PutText("  alerted:    "); RTIO.PutInt(ORD(t.alerted));   RTIO.PutChar('\n');
    RTIO.PutText("  completed:  "); RTIO.PutInt(ORD(t.completed)); RTIO.PutChar('\n');
    RTIO.PutText("  joined:     "); RTIO.PutInt(ORD(t.joined));    RTIO.PutChar('\n');
    RTIO.PutText("  id:         "); RTIO.PutInt(t.id);             RTIO.PutChar('\n');
    RTIO.Flush();
  END DumpThread;

(*------------------------------------------------------------ Fork, Join ---*)

CONST
  MaxIdle = 10;

VAR (* LL=activeMu *)
  allThreads: Activation := NIL;	 (* global list of active threads *)

VAR (* LL=idleMu *)
  idleThreads: ARRAY [0..MaxIdle-1] OF T; (* global pool of idle threads *)
  nIdle:       CARDINAL := 0;
  idleClock:   CARDINAL := 0;

PROCEDURE CreateT (act: Activation): T =
  (* LL = 0, because allocating a traced reference may cause
     the allocator to start a collection which will call "SuspendOthers"
     which will try to acquire "activeMu". *)
  VAR t := NEW(T, act := act);
  BEGIN
    t.waitCond := NEW(UNTRACED REF pthread_cond_t);
    t.cond     := NEW(Condition);
    FloatMode.InitThread (act.floatState);
    AssignSlot (t);
    RETURN t;
  END CreateT;

(* ThreadBase calls RunThread after finding (approximately) where
   its stack begins.  This dance ensures that all of ThreadMain's
   traced references are within the stack scanned by the collector.

   If RunThread decides to put itself on the idle list, it returns TRUE to
   indicate that ThreadBase should wait.  It's important that ThreadBase's
   stack frame doesn't contain traced references.  Otherwise, while it waited
   for its rebirth signal each reference would pin a heap page.  *)

PROCEDURE ThreadBase (param: ADDRESS): ADDRESS =
  VAR
    xx: INTEGER;
    me: Activation := LOOPHOLE (param, Activation);
    waitCond: UNTRACED REF pthread_cond_t;
  BEGIN
    SetActivation (me);
    (* We need to establish this binding before this thread touches any
       traced references.  Otherwise, it may trigger a heap page fault,
       which would call SuspendOthers, which requires an Activation. *)

    LOOP
      me.stackbase := ADR(xx);		(* enable GC scanning of this stack *)
      waitCond := RunThread(me);
      me.stackbase := NIL;		(* disable GC scanning of my stack *)
      IF (waitCond = NIL) THEN EXIT; END;
      WITH r = Upthread.mutex_lock(idleMu) DO <*ASSERT r=0*> END;
      WHILE me.idle DO
        WITH r = Upthread.cond_wait(waitCond^, idleMu) DO <*ASSERT r=0*> END;
      END;
      WITH r = Upthread.mutex_unlock(idleMu) DO <*ASSERT r=0*> END;
    END;

    DISPOSE (me);
    RETURN NIL;
  END ThreadBase;

PROCEDURE RunThread (me: Activation): UNTRACED REF pthread_cond_t =
  VAR self, next_self, victim: T;  cl: Closure; res: REFANY;
  BEGIN
    WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
      self := slots [me.slot];
    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;

    LockMutex(threadMu);
      WHILE self.id = 0 DO Wait(threadMu, self.cond) END;
      cl := self.closure;
    UnlockMutex(threadMu);

    (* Run the user-level code. *)
    next_self := NIL;
    IF cl # NIL THEN
      IF perfOn THEN PerfRunning(self.id) END;
      res := cl.apply();

      (* transplant the active guts of "self" into "next_self" *)
      next_self          := NEW(T);
      next_self.act      := me;
      next_self.waitCond := self.waitCond;
      next_self.cond     := self.cond;
      FloatMode.InitThread (me.floatState);

      (* hijack "self"s entry in the slot table *)
      WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
        slots[me.slot] := next_self;
      WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;

      LockMutex(threadMu);
        (* mark "self" done and clean it up a bit *)
        self.result := res;
        self.closure := NIL;
        self.completed := TRUE;
        Broadcast(self.cond); (* let everybody know that "self" is done *)
        IF perfOn THEN PerfChanged(self.id, State.dying) END;
      UnlockMutex(threadMu);

      IF perfOn THEN PerfDeleted(self.id) END;

      (* put "next_self" on the list of idle threads *)
      victim := NIL;
      WITH r = Upthread.mutex_lock(idleMu) DO <*ASSERT r=0*> END;
        IF nIdle < NUMBER(idleThreads) THEN
          (* the pool isn't full *)
          idleThreads[nIdle] := next_self;
          INC(nIdle);
        ELSE
          (* no room in the pool => free an old thread from the pool *)
          IF idleClock >= nIdle THEN idleClock := 0 END;
          victim := idleThreads[idleClock];
          idleThreads[idleClock] := next_self;
          INC(idleClock);
          victim.act.idle := FALSE;
          WITH r = Upthread.cond_signal(victim.waitCond^) DO <*ASSERT r=0*> END;
        END;
        me.idle := TRUE;
      WITH r = Upthread.mutex_unlock(idleMu) DO <*ASSERT r=0*> END;

      IF victim # NIL THEN
        LockMutex(threadMu);
          victim.closure := NIL;
          victim.id := -1;
          Signal(victim.cond);
        UnlockMutex(threadMu);
      END;

      (* let the rebirth loop in ThreadBase know where to wait... *)
      RETURN next_self.waitCond;
    END;

    (* we're dying *)
    RTHeapRep.ClosePool(me.newPool);

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

    RETURN NIL; (* let the rebirth loop know we're dying. *)
  END RunThread;

PROCEDURE Fork(closure: Closure): T =
  VAR
    t: T;
    act: Activation := NIL;
    attr: pthread_attr_t;
    size := defaultStackSize;
    best: INTEGER := -1;
    sz, best_sz: INTEGER;
    bytes: Utypes.size_t;
  BEGIN
    (* determine the initial size of the stack for this thread *)
    TYPECASE closure OF
    | SizedClosure (scl) => size := scl.stackSize;
    ELSE (*skip*)
    END;

    (* try the cache for a thread *)
    WITH r = Upthread.mutex_lock(idleMu) DO <*ASSERT r=0*> END;
      FOR p := nIdle-1 TO FIRST(idleThreads) BY -1 DO
        sz := idleThreads[p].act.size;
        IF sz = size THEN
          (* exact match *)
          best := p;
          best_sz := sz;
          EXIT;
        ELSIF sz > size AND (best < 0 OR sz < best_sz) THEN
          (* a new best match *)
          best := p;
          best_sz := sz;
        END;
      END;
      IF best >= 0 THEN
        DEC(nIdle);
        WITH pp = idleThreads[best] DO
          t := pp;
          pp := idleThreads[nIdle];
        END;
        t.act.idle := FALSE;
        WITH r = Upthread.cond_signal(t.waitCond^) DO <*ASSERT r=0*> END;
      ELSE (* no match in cache => we need a fresh thread *)
        WITH r = Upthread.mutex_unlock(idleMu) DO <*ASSERT r=0*> END;
          t := CreateT(NEW(Activation));
        WITH r = Upthread.mutex_lock(idleMu) DO <*ASSERT r=0*> END;
        act := t.act;
        WITH r = Upthread.mutex_lock(activeMu) DO <*ASSERT r=0*> END;
          WITH r = Upthread.attr_init(attr) DO <*ASSERT r=0*> END;
          WITH r = Upthread.attr_getstacksize(attr, bytes)  DO <*ASSERT r=0*> END;
          bytes := MAX(bytes, size * ADRSIZE(Word.T));
          WITH r = Upthread.attr_setstacksize(attr, bytes) DO <*ASSERT r=0*> END;
          WITH r = Upthread.create(act.handle, attr, ThreadBase, act) DO
            <*ASSERT r=0*>
          END;
          act.next := allThreads;
          act.prev := allThreads.prev;
          act.size := size;
          allThreads.prev.next := act;
          allThreads.prev := act;
        WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
      END;
    WITH r = Upthread.mutex_unlock(idleMu) DO <*ASSERT r=0*> END;

    (* last minute sanity checking *)
    <* ASSERT CheckSlot (t) *>
    <* ASSERT t.act.next # NIL *>
    <* ASSERT t.act.prev # NIL *>

    LockMutex(threadMu);
      t.closure := closure;
      t.id := nextId;  INC(nextId);
      Signal(t.cond);
      IF perfOn THEN PerfChanged(t.id, State.alive) END;
    UnlockMutex(threadMu);

    RETURN t;
  END Fork;

PROCEDURE Join(t: T): REFANY =
  VAR res: REFANY;
  BEGIN
    LockMutex(threadMu);
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice"); END;
      WHILE NOT t.completed DO Wait(threadMu, t.cond) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
      t.cond := NIL;
      IF perfOn THEN PerfChanged(t.id, State.dead) END;
    UnlockMutex(threadMu);
    RETURN res;
  END Join;

PROCEDURE AlertJoin(t: T): REFANY RAISES {Alerted} =
  VAR res: REFANY;
  BEGIN
    LockMutex(threadMu);
    TRY
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice"); END;
      WHILE NOT t.completed DO AlertWait(threadMu, t.cond) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
      t.cond := NIL;
      IF perfOn THEN PerfChanged(t.id, State.dead) END;
    FINALLY
      UnlockMutex(threadMu);
    END;
    RETURN res;
  END AlertJoin;

(*---------------------------------------------------- Scheduling support ---*)

PROCEDURE ToNTime (n: LONGREAL; VAR ts: Utime.struct_timespec) =
  BEGIN
    ts.tv_sec := TRUNC(n);
    ts.tv_nsec := ROUND((n - FLOAT(ts.tv_sec, LONGREAL)) * 1.0D9);
  END ToNTime;

PROCEDURE Pause(n: LONGREAL) =
  VAR
    amount, remaining: Utime.struct_timespec;
    self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "Pause called from a non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(self.id, State.pausing) END;
    IF n <= 0.0d0 THEN RETURN END;
    ToNTime(n, amount);
    WHILE Utime.nanosleep(amount, remaining) # 0 DO
      amount := remaining;
    END;
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
  END Pause;

PROCEDURE AlertPause(n: LONGREAL) RAISES {Alerted} =
  VAR
    until: Utime.struct_timespec;
    self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "Pause called from a non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(self.id, State.pausing) END;
    IF n <= 0.0d0 THEN RETURN END;
    ToNTime(Time.Now() + n, until);
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    InnerTestAlert(self);
    self.alertable := TRUE;
    WHILE Upthread.cond_timedwait(self.waitCond^, cm, until) = 0 DO
      self.alertable := FALSE;
      InnerTestAlert(self);
      self.alertable := TRUE;
    END;
    self.alertable := FALSE;
    InnerTestAlert(self);
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
  END AlertPause;

PROCEDURE Yield() =
  BEGIN
    WITH r = Usched.yield() DO <*ASSERT r=0*> END;
  END Yield;

CONST FDSetSize = BITSIZE(INTEGER);

TYPE
  FDSet = SET OF [0 .. FDSetSize-1];
  FDS = REF ARRAY OF FDSet;

PROCEDURE IOWait(fd: INTEGER; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult =
  VAR self := Self();  result: WaitResult;
  BEGIN
    IF perfOn THEN PerfChanged(self.id, State.blocking) END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    self.alertable := FALSE;
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
    result := XIOWait(fd, read, timeoutInterval);
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
    RETURN result;
  END IOWait;

PROCEDURE IOAlertWait(fd: INTEGER; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult
                  RAISES {Alerted} =
  VAR self := Self();  result: WaitResult;
  BEGIN
    IF perfOn THEN PerfChanged(self.id, State.blocking) END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
      InnerTestAlert(self);
      self.alertable := TRUE;
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
    result := XIOWait(fd, read, timeoutInterval);
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
      InnerTestAlert(self);
      self.alertable := FALSE;
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
    RETURN result;
  END IOAlertWait;

PROCEDURE XIOWait (fd: CARDINAL; read: BOOLEAN; interval: LONGREAL): WaitResult =
  VAR
    res: INTEGER;
    fdindex := fd DIV FDSetSize;
    fdset := FDSet{fd MOD FDSetSize};
    gReadFDS, gWriteFDS, gExceptFDS: FDS := NEW(FDS, fdindex+1);

  PROCEDURE TestFDS(index: CARDINAL; set: FDSet; read: BOOLEAN): WaitResult =
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

  PROCEDURE CallSelect(nfd: CARDINAL; timeout: UNTRACED REF UTime): INTEGER =
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
    FOR i := 0 TO fdindex-1 DO
      gReadFDS[i] := FDSet{};
      gWriteFDS[i] := FDSet{};
    END;
    IF read
      THEN gReadFDS[fdindex] := fdset;
      ELSE gWriteFDS[fdindex] := fdset;
    END;
    IF interval >= 0.0D0 THEN
      VAR utimeout := UTimeFromTime(interval);
      BEGIN
        res := CallSelect(fd+1, ADR(utimeout));
      END;
    ELSE
      res := CallSelect(fd+1, NIL);
    END;
    IF    res > 0 THEN RETURN TestFDS(fdindex, fdset, read);
    ELSIF res = 0 THEN RETURN WaitResult.Timeout;
    ELSE
      IF Cerrno.GetErrno() = Uerror.EINTR THEN
        RETURN WaitResult.Ready;       (* spurious wakeups are OK *)
      END;
      RETURN WaitResult.Error;
    END;
  END XIOWait;

TYPE UTime = Utime.struct_timeval;
PROCEDURE UTimeFromTime(time: Time.T): UTime =
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
    <*ASSERT me # NIL*>
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

PROCEDURE LookupActivation (pthread: pthread_t): Activation =
  (* LL=activeMu *)
  VAR act := allThreads;
  BEGIN
    REPEAT
      IF Upthread.equal(pthread, act.handle) # 0 THEN RETURN act END;
      act := act.next;
    UNTIL act = allThreads;
    RETURN NIL;
  END LookupActivation;

PROCEDURE ProcessPools (p: PROCEDURE (VAR pool: RTHeapRep.AllocPool)) =
  (* LL=activeMu.  Only called within {SuspendOthers, ResumeOthers} *)
  VAR act := allThreads;
  BEGIN
    REPEAT
      p(act.newPool);
      act := act.next;
    UNTIL act = allThreads;
  END ProcessPools;

PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS)) =
  (* LL=activeMu.  Only called within {SuspendOthers, ResumeOthers} *)
  VAR
    me := GetActivation();
    myState: RTMachine.State;
    state: RTMachine.ThreadState;
    act := me;
    xx: INTEGER;
  BEGIN
    REPEAT
      IF (act.stackbase # NIL) THEN
        (* Process the registers *)
        IF act = me THEN
          IF RTMachine.SaveRegsInStack # NIL THEN
            me.sp := RTMachine.SaveRegsInStack();
          ELSE
            me.sp := ADR(xx);
          END;
          EVAL RTMachine.SaveState(myState);
          WITH z = myState DO
            p(ADR(z), ADR(z) + ADRSIZE(z));
          END;
        ELSIF RTMachine.GetState # NIL THEN
          (* Process explicit state *)
          RTMachine.GetState(act.handle, act.sp, state);
          WITH z = state DO
            p(ADR(z), ADR(z) + ADRSIZE(z));
          END;
        ELSE
          (* assume registers are saved in suspended thread's stack *)
        END;

        (* Process the stack *)
        IF stack_grows_down THEN
          p(act.sp, act.stackbase);
        ELSE
          p(act.stackbase, act.sp);
        END;
      END;
      act := act.next;
    UNTIL act = me;
  END ProcessStacks;

(* Signal based suspend/resume *)
VAR
  stopCount: CARDINAL := 0;
  suspendAckSem: Usem.sem_t;
  suspendMask: Usignal.sigset_t;

CONST
  SIG_SUSPEND = RTMachine.SIG_SUSPEND;
  SIG_RESTART = RTMachine.SIG_RESTART;

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
          IF act.lastStopCount # stopCount THEN
            RTMachine.SuspendThread(act.handle);
            IF act.newPool.busy THEN
              RTMachine.RestartThread(act.handle);
              INC(nLive);
            ELSE
              act.lastStopCount := stopCount;
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
        IF act.lastStopCount # stopCount THEN
          WITH r = Upthread.kill(act.handle, SIG_SUSPEND) DO
            <*ASSERT r=0*>
          END;
          INC(nLive);
        END;
        act := act.next;
      END;
    END;
    RETURN nLive;	      (* return number still live (i.e., signalled) *)
  END SuspendAll;

PROCEDURE StopWorld (me: Activation) =
  (* LL=activeMu *)
  VAR
    nLive, newlySent: INTEGER;
    wait_nsecs := 0;
    wait, remaining: Utime.struct_timespec;
    acks: Ctypes.int;
  CONST
    WAIT_UNIT = 3000000;
    RETRY_INTERVAL = 10000000;
  BEGIN
    INC(stopCount);
    nLive := SuspendAll (me);
    IF nLive = 0 THEN RETURN END;
    LOOP
      WITH r = Usem.getvalue(suspendAckSem, acks) DO <*ASSERT r=0*> END;
      IF acks = nLive THEN EXIT END;
      IF wait_nsecs > RETRY_INTERVAL THEN
        newlySent := SuspendAll(me);
        WITH r = Usem.getvalue(suspendAckSem, acks) DO <*ASSERT r=0*> END;
        IF newlySent < nLive - acks THEN
          nLive := acks + newlySent;
        END;
        wait_nsecs  := 0;
      END;
      wait.tv_sec := 0;
      wait.tv_nsec := WAIT_UNIT;
      WHILE Utime.nanosleep(wait, remaining) # 0 DO
        wait := remaining;
      END;
      INC(wait_nsecs, WAIT_UNIT);
    END;

    FOR i := 0 TO nLive-1 DO
      WHILE Usem.wait(suspendAckSem) # 0 DO
        <* ASSERT Cerrno.GetErrno() = Uerror.EINTR *>
      END;
    END;
  END StopWorld;

PROCEDURE StartWorld (me: Activation) =
  VAR act := me.next;
  BEGIN
    IF RTMachine.RestartThread # NIL THEN
      (* Use the native restart routine *)
      WHILE act # me DO
        RTMachine.RestartThread(act.handle);
        act := act.next;
      END;
    ELSE
      (* No native restart routine so signal thread to restart *)
      WHILE act # me DO
        WITH r = Upthread.kill(act.handle, SIG_RESTART) DO <*ASSERT r=0*> END;
        act := act.next;
      END;
    END;
  END StartWorld;

PROCEDURE SuspendHandler (sig: Ctypes.int;
                          <*UNUSED*> sip: Usignal.siginfo_t_star;
                          <*UNUSED*> uap: Uucontext.ucontext_t_star) =
  VAR
    errno := Cerrno.GetErrno();
    xx: INTEGER;
    self := Upthread.self();
    me := LookupActivation(self);
    myStopCount := stopCount;
  BEGIN
    <*ASSERT sig = SIG_SUSPEND*>
    IF me = NIL THEN RETURN END;
    IF me.newPool.busy THEN RETURN END;
    IF me.lastStopCount = myStopCount THEN RETURN END;
    IF RTMachine.SaveRegsInStack # NIL THEN
      me.sp := RTMachine.SaveRegsInStack();
    ELSE
      me.sp := ADR(xx);
    END;
    WITH r = Usem.post(suspendAckSem) DO <*ASSERT r=0*> END;
    me.lastStopCount := myStopCount;
    REPEAT
      me.signal := 0;
      EVAL Usignal.sigsuspend(suspendMask); (* wait for signal *)
    UNTIL me.signal = SIG_RESTART;
    Cerrno.SetErrno(errno);
  END SuspendHandler;

PROCEDURE RestartHandler (<*UNUSED*> sig: Ctypes.int;
                          <*UNUSED*> sip: Usignal.siginfo_t_star;
                          <*UNUSED*> uap: Uucontext.ucontext_t_star) =
  VAR
    self := Upthread.self();
    me := LookupActivation(self);
  BEGIN
    me.signal := SIG_RESTART;
  END RestartHandler;

PROCEDURE SetupHandlers () =
  VAR act, oact: Usignal.struct_sigaction;
  BEGIN
    IF RTMachine.SuspendThread # NIL THEN <*ASSERT SIG_SUSPEND = 0*> END;
    IF RTMachine.RestartThread # NIL THEN <*ASSERT SIG_RESTART = 0*> END;
    IF SIG_SUSPEND = 0 AND SIG_SUSPEND = 0 THEN RETURN END;
      
    WITH r = Usem.init(suspendAckSem, 0, 0) DO <*ASSERT r=0*> END;

    act.sa_flags := Word.Or(Usignal.SA_RESTART, Usignal.SA_SIGINFO);
    WITH r = Usignal.sigfillset(act.sa_mask) DO <*ASSERT r=0*> END;
    (* SIG_RESTART is set in the resulting mask.      *)
    (* It is unmasked by the handler when necessary. *)
    act.sa_sigaction := SuspendHandler;
    WITH r = Usignal.sigaction(SIG_SUSPEND, act, oact) DO <*ASSERT r=0*> END;

    act.sa_sigaction := RestartHandler;
    WITH r = Usignal.sigaction(SIG_RESTART, act, oact) DO <*ASSERT r=0*> END;

    (* Initialize suspendMask.  It excludes SIG_RESTART. *)
    WITH r = Usignal.sigfillset(suspendMask) DO <*ASSERT r=0*> END;
    WITH r = Usignal.sigdelset(suspendMask, SIG_RESTART) DO <*ASSERT r=0*> END;
  END SetupHandlers;

(*------------------------------------------------------------ misc. junk ---*)

PROCEDURE MyId(): Id RAISES {} =
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

PROCEDURE MyAllocPool (): UNTRACED REF RTHeapRep.AllocPool =
  VAR me := GetActivation();
  BEGIN
    RETURN ADR(me.newPool);
  END MyAllocPool;

PROCEDURE DisableSwitching () =
  BEGIN
  END DisableSwitching;

PROCEDURE EnableSwitching () =
  BEGIN
  END EnableSwitching;

(*---------------------------------------------------------------- errors ---*)

PROCEDURE Die(lineno: INTEGER; msg: TEXT) =
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
  (* LL = threadMu *)
  VAR e := ThreadEvent.T {kind := TE.Changed, id := id, state := s};
  BEGIN
    WITH r = Upthread.mutex_lock(perfMu) DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = Upthread.mutex_unlock(perfMu) DO <*ASSERT r=0*> END;
  END PerfChanged;

PROCEDURE PerfDeleted (id: Id) =
  (* LL = threadMu *)
  VAR e := ThreadEvent.T {kind := TE.Deleted, id := id};
  BEGIN
    WITH r = Upthread.mutex_lock(perfMu) DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = Upthread.mutex_unlock(perfMu) DO <*ASSERT r=0*> END;
  END PerfDeleted;

PROCEDURE PerfRunning (id: Id) =
  (* LL = threadMu *)
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

    (* cm, activeMu, idleMu, slotMu: initialized statically *)
    threadMu := NEW(Mutex);
    self := CreateT(me);
    self.id := nextId;  INC(nextId);

    mutex := NEW(MUTEX);
    condition := NEW(Condition);

    stack_grows_down := ADR(xx) > XX();

    WITH r = Upthread.mutex_lock(activeMu) DO <*ASSERT r=0*> END;
      me.stackbase := ADR(xx);
    WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
    PerfStart();
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
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
  heapMutex := PTHREAD_MUTEX_INITIALIZER;
  heapCond := PTHREAD_COND_INITIALIZER;
  holder: pthread_t;
  lockers: CARDINAL := 0;
  lock_cnt := 0;
  do_signal := FALSE;
  mutex: MUTEX;
  condition: Condition;

PROCEDURE LockHeap () =
  VAR self := Upthread.self();
  BEGIN
    (* suspend_cnt # 0 => other threads are stopped and we hold the lock *)
    IF suspend_cnt # 0 THEN
      <*ASSERT lock_cnt # 0*>
      <*ASSERT holder = self*>
      RETURN;
    END;
    WITH r = Upthread.mutex_lock(heapMutex) DO <*ASSERT r=0*> END;
    IF lock_cnt = 0 THEN
      holder := self;
    ELSIF Upthread.equal(holder, self) = 0 THEN
      REPEAT
        INC(lockers);
        WITH r = Upthread.cond_wait(heapCond, heapMutex) DO <*ASSERT r=0*> END;
        DEC(lockers);
      UNTIL lock_cnt = 0;
      holder := self;
    END;
    INC(lock_cnt);
    WITH r = Upthread.mutex_unlock(heapMutex) DO <*ASSERT r=0*> END;
  END LockHeap;

PROCEDURE UnlockHeap () =
  VAR sig := FALSE;
  BEGIN
    (* suspend_cnt # 0 => other threads are stopped and we hold the lock *)
    IF suspend_cnt # 0 THEN
      <*ASSERT lock_cnt # 0*>
      <*ASSERT holder = Upthread.self()*>
      RETURN;
    END;
    WITH r = Upthread.mutex_lock(heapMutex) DO <*ASSERT r=0*> END;
      DEC(lock_cnt);
      IF lock_cnt = 0 THEN
        IF lockers # 0 THEN
          WITH r = Upthread.cond_signal(heapCond) DO <*ASSERT r=0*> END;
        END;
        IF do_signal THEN sig := TRUE; do_signal := FALSE; END;
      END;
    WITH r = Upthread.mutex_unlock(heapMutex) DO <*ASSERT r=0*> END;
    IF sig THEN Broadcast(condition) END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  (* LL = 0 *)
  BEGIN
    LOCK mutex DO Wait(mutex, condition); END;
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

BEGIN
END ThreadPThread.
