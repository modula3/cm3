(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

UNSAFE MODULE ThreadPThread EXPORTS
Thread, ThreadF, ThreadPThread, Scheduler, SchedulerPosix,
RTThreadInit, RTOS, RTHooks;

IMPORT Cerrno, FloatMode, MutexRep,
       RTError, RTMachine, RTPerfTool,
       RTProcess, ThreadEvent, Time,
       Unix, Utime, Word, Upthread, Usched, Usem, Usignal,
       Uucontext, Uerror;
FROM Upthread
IMPORT pthread_t, pthread_cond_t, pthread_key_t, pthread_attr_t,
       PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER,
       PTHREAD_ONCE_INITIALIZER, PTHREAD_STACK_MIN;
IMPORT Ctypes;

(*----------------------------------------------------- types and globals ---*)

VAR
  cm := PTHREAD_MUTEX_INITIALIZER; (* global lock for fields of Mutex/Condition *)

  defaultStackSize := PTHREAD_STACK_MIN;
  stack_grows_down: BOOLEAN;

  nextId: Id := 1;

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

    (* global list of idle threads *)
    nextIdle: T := NIL;			 (* LL = idleMu *)

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
    id: Id;				 (* LL = threadMu *)

    (* state that is available to the floating point routines *)
    floatState : FloatMode.ThreadState;	 (* LL = threadMu *)
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
      Die("Attempt to lock mutex already locked by self");
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
    IF self = NIL THEN Die("Acquire called from a non-Modula-3 thread") END;
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
      Die("attempt to release an unlocked mutex");
    ELSE
      Die("attempt to release a mutex locked by another thread");
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
    IF self = NIL THEN Die("Acquire called from a non-Modula-3 thread") END;
    WITH r = Upthread.mutex_lock(cm) DO <*ASSERT r=0*> END;
    InnerUnlockMutex(m, self);
    WITH r = Upthread.mutex_unlock(cm) DO <*ASSERT r=0*> END;
  END UnlockMutex;

(*---------------------------------------- Condition variables and Alerts ---*)

PROCEDURE InnerWait(m: Mutex; c: Condition; self: T) =
    (* LL = cm+m *)
  BEGIN
    <* ASSERT( (self.waitingOn=NIL) AND (self.nextWaiter=NIL) ) *>
    self.waitingOn := c;
    self.nextWaiter := c.waiters;
    c.waiters := self;
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
    IF self = NIL THEN Die("AlertWait called from non-Modula-3 thread") END;
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
    IF self = NIL THEN Die("Wait called from non-Modula-3 thread") END;
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
    IF t = NIL THEN Die("Alert called from non-Modula-3 thread") END;
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
  threadIndex: pthread_key_t;		 (* TLS index *)

VAR (* LL = slotMu *)
  n_slotted := 0;
  next_slot := 1;
  slots: REF ARRAY OF T;		 (* NOTE: we don't use slots[0] *)

PROCEDURE SetActivation (act: Activation) =
  (* LL = 0 *)
  VAR v := LOOPHOLE(act, ADDRESS);
  BEGIN
    WITH r = Upthread.setspecific(threadIndex, v) DO <*ASSERT r=0*> END;
  END SetActivation;

PROCEDURE GetActivation (): Activation =
  (* If not the initial thread and not created by Fork, returns NIL *)
  (* LL = 0 *)
  BEGIN
    RETURN LOOPHOLE(Upthread.getspecific(threadIndex), Activation);
  END GetActivation;

PROCEDURE Self (): T =
  (* If not the initial thread and not created by Fork, returns NIL *)
  (* LL = 0 *)
  VAR
    me := LOOPHOLE(Upthread.getspecific(threadIndex), Activation);
    t: T;
  BEGIN
    IF me = NIL THEN RETURN NIL END;
    WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
      t := slots[me.slot];
    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
    IF (t.act # me) THEN Die("thread with bad slot!") END;
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
        IF (z # t) THEN Die ("unslotted thread!"); END;
        z := NIL;
      END;
      t.act.slot := 0;

    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
  END FreeSlot;

PROCEDURE CheckSlot (t: T) =
  (* LL = 0 *)
  VAR me := t.act;
  BEGIN
    <*ASSERT me # NIL *>
    <*ASSERT me.slot > 0 *>
    WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
       <*ASSERT slots[me.slot] = t *>
    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
  END CheckSlot;

(*------------------------------------------------------------ Fork, Join ---*)

CONST
  MaxIdle = 10;

VAR (* LL=activeMu *)
  allThreads: Activation := NIL;	 (* global list of active threads *)

VAR (* LL=idleMu *)
  idleThreads: T          := NIL;	 (* global list of idle threads *)
  nIdle:       INTEGER    := 0;

PROCEDURE CreateT (): T =
  (* LL = 0, because allocating a traced reference may cause
     the allocator to start a collection which will call "SuspendOthers"
     which will try to acquire "activeMu". *)
  VAR t := NEW(T);
  BEGIN
    t.act      := NEW(Activation);
    t.waitCond := NEW(UNTRACED REF pthread_cond_t);
    t.cond     := NEW(Condition);
    FloatMode.InitThread (t.floatState);
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
  TYPE ObjRef = UNTRACED REF MethodList;
       MethodList = UNTRACED REF RECORD typecode: INTEGER;  method0: ADDRESS; END;
  VAR self, next_self: T;  cl: Closure; res: REFANY;
  BEGIN
    WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
      self := slots [me.slot];
    WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;

    LockMutex(threadMu);
      WHILE self.closure = NIL DO Wait(threadMu, self.cond) END;
      cl := self.closure;
      self.id := nextId;  INC (nextId);
      IF perfOn THEN PerfRunning(self.id) END;
    UnlockMutex(threadMu);

    IF (cl = NIL) THEN
      Die ("NIL closure passed to Thread.Fork!");
    ELSIF (LOOPHOLE (cl, ObjRef)^^.method0 = NIL) THEN
      Die ("NIL apply method passed to Thread.Fork!");
    END;

    (* Run the user-level code. *)
    res := cl.apply();

    next_self := NIL;
    IF nIdle < MaxIdle THEN
      (* apparently the cache isn't full, although we don't hold idleMu
         so we can't be certain, we're committed now.  Hopefully we'll
         be reborn soon... *)

      (* transplant the active guts of "self" into "next_self" *)
      next_self          := NEW(T);
      next_self.act      := me;
      next_self.waitCond := self.waitCond;
      next_self.cond     := self.cond;

      (* hijack "self"s entry in the slot table *)
      WITH r = Upthread.mutex_lock(slotMu) DO <*ASSERT r=0*> END;
        slots[me.slot] := next_self;
      WITH r = Upthread.mutex_unlock(slotMu) DO <*ASSERT r=0*> END;
    END;

    LockMutex(threadMu);
      (* mark "self" done and clean it up a bit *)
      self.result := res;
      self.closure := NIL;
      self.completed := TRUE;
      Broadcast(self.cond); (* let everybody know that "self" is done *)
      IF perfOn THEN PerfChanged(self.id, State.dying) END;
    UnlockMutex(threadMu);

    IF perfOn THEN PerfDeleted(self.id) END;
    IF next_self # NIL THEN
      (* we're going to be reborn! *)
      (* put "next_self" on the list of idle threads *)
      WITH r = Upthread.mutex_lock(idleMu) DO <*ASSERT r=0*> END;
        next_self.nextIdle := idleThreads;
        idleThreads := next_self;
        INC(nIdle);
        me.idle := TRUE;
        WITH r = Upthread.mutex_unlock(idleMu) DO <*ASSERT r=0*> END;
      (* let the rebirth loop in ThreadBase know where to wait... *)
      RETURN next_self.waitCond;
    ELSE
      (* we're dying *)
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
        me.handle := NIL;
      WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;

      RETURN NIL; (* let the rebirth loop know we're dying. *)
    END;
  END RunThread;

PROCEDURE Fork(closure: Closure): T =
  VAR
    t: T := NIL;
    stack_size := defaultStackSize;
    act: Activation := NIL;
    attr: pthread_attr_t;
  BEGIN
    (* determine the initial size of the stack for this thread *)
    TYPECASE closure OF
    | SizedClosure (scl) =>
      stack_size := (scl.stackSize * BYTESIZE(Word.T) DIV PTHREAD_STACK_MIN);
      stack_size := MAX(PTHREAD_STACK_MIN, stack_size * PTHREAD_STACK_MIN);
    ELSE (*skip*)
    END;

    WITH r = Upthread.attr_init(attr) DO <*ASSERT r=0*> END;
    WITH r = Upthread.attr_setstacksize(attr, stack_size) DO <*ASSERT r=0*> END;

    (* try the cache for a thread *)
    WITH r = Upthread.mutex_lock(idleMu) DO <*ASSERT r=0*> END;
      IF nIdle > 0 THEN
        <* ASSERT(idleThreads # NIL) *>
        DEC(nIdle);
        t := idleThreads;
        idleThreads := t.nextIdle;
        t.nextIdle := NIL;
        t.act.idle := FALSE;
        WITH r = Upthread.cond_signal(t.waitCond^) DO <*ASSERT r=0*> END;
      ELSE (* empty cache => we need a fresh thread *)
        WITH r = Upthread.mutex_unlock(idleMu) DO <*ASSERT r=0*> END;
          t := CreateT();
        WITH r = Upthread.mutex_lock(idleMu) DO <*ASSERT r=0*> END;
        act := t.act;
        WITH r = Upthread.mutex_lock(activeMu) DO <*ASSERT r=0*> END;
          WITH r = Upthread.create(act.handle, attr, ThreadBase, act) DO
            <*ASSERT r=0*>
          END;
          act.next := allThreads;
          act.prev := allThreads.prev;
          allThreads.prev.next := act;
          allThreads.prev := act;
        WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
      END;
    WITH r = Upthread.mutex_unlock(idleMu) DO <*ASSERT r=0*> END;

    (* last minute sanity checking *)
    CheckSlot (t);
    act := t.act;
    <* ASSERT act.handle # NIL *>
    <* ASSERT act.next # NIL *>
    <* ASSERT act.prev # NIL *>

    LockMutex(threadMu);
      t.closure := closure;
      Signal(t.cond);
      IF perfOn THEN PerfChanged(t.id, State.alive) END;
    UnlockMutex(threadMu);

    RETURN t;
  END Fork;

PROCEDURE Join(t: T): REFANY =
  VAR res: REFANY;
  BEGIN
    LockMutex(threadMu);
      IF t.joined THEN Die("attempt to join with thread twice"); END;
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
      IF t.joined THEN Die("attempt to join with thread twice"); END;
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
    IF self = NIL THEN Die("Pause called from a non-Modula-3 thread") END;
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
    IF self = NIL THEN Die("Pause called from a non-Modula-3 thread") END;
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
    gReadFDS, gWriteFDS, gExceptFDS: FDS := NEW(FDS, 1);

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

PROCEDURE GetDefaultStackSize (): CARDINAL =
  BEGIN
    RETURN defaultStackSize DIV BYTESIZE(Word.T);
  END GetDefaultStackSize;

PROCEDURE MinDefaultStackSize (new_min: CARDINAL) =
  BEGIN
    defaultStackSize := MAX (defaultStackSize, new_min * BYTESIZE(Word.T));
  END MinDefaultStackSize;

PROCEDURE IncDefaultStackSize (inc: CARDINAL) =
  BEGIN
    INC (defaultStackSize, inc * BYTESIZE(Word.T));
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
    INC (suspend_cnt);
    IF (suspend_cnt = 1) THEN StopWorld(me) END;
  END SuspendOthers;

PROCEDURE ResumeOthers () =
  (* LL=activeMu.  Always preceded by SuspendOthers. *)
  VAR me := GetActivation();
  BEGIN
    DEC (suspend_cnt);
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
  BEGIN
    IF RTMachine.SuspendThread # NIL THEN
      (* Use the native suspend routine *)
      WHILE act # me DO
        <*ASSERT act.lastStopCount # stopCount*>
        RTMachine.SuspendThread(act.handle);
        act.lastStopCount := stopCount;
        act := act.next;
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
    IF RTMachine.SuspendThread # NIL THEN
      <*ASSERT RTMachine.RestartThread # NIL*>
      <*ASSERT SIG_SUSPEND = 0*>
      <*ASSERT SIG_RESTART = 0*>
      RETURN;
    END;
      
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

PROCEDURE MyId(): Id RAISES {}=
  VAR self := Self();
  BEGIN
    RETURN self.id;
  END MyId;

PROCEDURE MyFPState (): UNTRACED REF FloatMode.ThreadState =
  VAR self := Self();
  BEGIN
    RETURN ADR (self.floatState);
  END MyFPState;

PROCEDURE DisableSwitching () =
  BEGIN
  END DisableSwitching;

PROCEDURE EnableSwitching () =
  BEGIN
  END EnableSwitching;

(*---------------------------------------------------------------- errors ---*)

PROCEDURE Die(msg: TEXT) =
  BEGIN
    RTError.Msg ("ThreadPThread.m3", 799, "Thread client error: ", msg);
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
    act: Activation;
  BEGIN
    WITH r = Upthread.key_create(threadIndex, NIL) DO <*ASSERT r=0*> END;
    SetupHandlers ();

    (* cm, activeMu, idleMu, slotMu: initialized statically *)

    threadMu := NEW(Mutex);
    self := CreateT();
    self.id := nextId;  INC(nextId);

    stack_grows_down := ADR(xx) > XX();

    act := self.act;

    WITH r = Upthread.mutex_lock(activeMu) DO <*ASSERT r=0*> END;
      act.handle := Upthread.self();
      act.next   := act;
      act.prev   := act;
      allThreads := act;
      act.stackbase := ADR(xx);
    WITH r = Upthread.mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
    SetActivation(act);
    PerfStart();
    IF perfOn THEN PerfChanged(self.id, State.alive) END;
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
  mutex := PTHREAD_MUTEX_INITIALIZER;
  condition := PTHREAD_COND_INITIALIZER;
  thread: pthread_t := NIL;
  count: CARDINAL := 0;

PROCEDURE LockHeap () =
  BEGIN
    WITH r = Upthread.mutex_lock(mutex) DO <*ASSERT r=0*> END;
      IF count = 0 THEN
        thread := Upthread.self();
        INC(count);
      ELSIF thread = Upthread.self() THEN
        INC(count);
      ELSE
        WITH r = Upthread.cond_wait(condition, mutex) DO <*ASSERT r=0*> END;
      END;
    WITH r = Upthread.mutex_unlock(mutex) DO <*ASSERT r=0*> END;
  END LockHeap;

PROCEDURE UnlockHeap () =
  BEGIN
    WITH r = Upthread.mutex_lock(mutex) DO <*ASSERT r=0*> END;
      DEC(count);
      IF count = 0 THEN
        WITH r = Upthread.cond_signal(condition) DO <*ASSERT r=0*> END;
      END;
    WITH r = Upthread.mutex_unlock(mutex) DO <*ASSERT r=0*> END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  (* LL = 0 *)
  BEGIN
    WITH r = Upthread.mutex_lock(mutex) DO <*ASSERT r=0*> END;
      WITH r = Upthread.cond_wait(condition, mutex) DO <*ASSERT r=0*> END;
    WITH r = Upthread.mutex_unlock(mutex) DO <*ASSERT r=0*> END;
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  (* LL = inCritical *)
  BEGIN
    WITH r = Upthread.cond_broadcast(condition) DO <*ASSERT r=0*> END;
  END BroadcastHeap;

(*--------------------------------------------- exception handling support --*)

VAR
  once := PTHREAD_ONCE_INITIALIZER;
  handlersIndex: pthread_key_t;

PROCEDURE GetCurrentHandlers (): ADDRESS=
  BEGIN
    WITH r = Upthread.once(once, InitHandlers) DO <*ASSERT r=0*> END;
    RETURN Upthread.getspecific(handlersIndex);
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers (h: ADDRESS)=
  BEGIN
    WITH r = Upthread.once(once, InitHandlers) DO <*ASSERT r=0*> END;
    WITH r = Upthread.setspecific(handlersIndex, h) DO <*ASSERT r=0*> END;
  END SetCurrentHandlers;

(*RTHooks.PushEFrame*)
PROCEDURE PushEFrame (frame: ADDRESS) =
  TYPE Frame = UNTRACED REF RECORD next: ADDRESS END;
  VAR f := LOOPHOLE (frame, Frame);
  BEGIN
    WITH r = Upthread.once(once, InitHandlers) DO <*ASSERT r=0*> END;
    f.next := Upthread.getspecific(handlersIndex);
    WITH r = Upthread.setspecific(handlersIndex, f) DO <*ASSERT r=0*> END;
  END PushEFrame;

(*RTHooks.PopEFrame*)
PROCEDURE PopEFrame (frame: ADDRESS) =
  BEGIN
    WITH r = Upthread.once(once, InitHandlers) DO <*ASSERT r=0*> END;
    WITH r = Upthread.setspecific(handlersIndex, frame) DO <*ASSERT r=0*> END;
  END PopEFrame;

PROCEDURE InitHandlers () =
  BEGIN
    WITH r = Upthread.key_create(handlersIndex, NIL) DO <*ASSERT r=0*> END;
    WITH r = Upthread.setspecific(handlersIndex, NIL) DO <*ASSERT r=0*> END;
  END InitHandlers;

BEGIN
END ThreadPThread.
