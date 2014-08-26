(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

UNSAFE MODULE ThreadPThread EXPORTS Thread, ThreadF, RTThread, Scheduler,
SchedulerPosix, RTOS, RTHooks, ThreadPThread;

IMPORT Cerrno, FloatMode, MutexRep, RTCollectorSRC, RTError, RTHeapRep, RTIO,
       RTParams, RTPerfTool, RTProcess, ThreadEvent, Time,
       Word, Usched, Uerror, Uexec;
FROM Compiler IMPORT ThisFile, ThisLine;
FROM Ctypes IMPORT int;
IMPORT RuntimeError AS RTE;
FROM ThreadInternal IMPORT Poll;

(*----------------------------------------------------- types and globals ---*)

CONST
  MILLION = 1000 * 1000;
  WAIT_UNIT = MILLION; (* one million nanoseconds, one thousandth of a second *)
  RETRY_INTERVAL = 10 * MILLION; (* 10 million nanoseconds, one hundredth of a second *)

REVEAL
  Mutex = MutexRep.Public BRANDED "Mutex Pthread-1.0" OBJECT
    mutex: pthread_mutex_t := NIL;
    holder: Activation := NIL;
    waiters: Activation := NIL;
  OVERRIDES
    acquire := LockMutex;
    release := UnlockMutex;
  END;

  Condition = BRANDED "Thread.Condition Pthread-1.0" OBJECT
    mutex: pthread_mutex_t := NIL;
    waiters: Activation := NIL;     (* LL = mutex *)
  END;

  T = BRANDED "Thread.T Pthread-1.6" OBJECT
    act: Activation := NIL;         (* live untraced thread data *)
    closure: Closure := NIL;        (* our work and its result *)
    result: REFANY := NIL;          (* our work and its result *)
    join: Condition;                (* wait here to join; NIL when done *)
    joined: BOOLEAN := FALSE;       (* Is anyone waiting yet? *)
  END;

TYPE
  ActState = { Starting, Started, Stopping, Stopped };
  REVEAL Activation = UNTRACED BRANDED REF RECORD
    frame: ADDRESS := NIL;              (* exception handling support *)
    mutex: pthread_mutex_t := NIL;      (* write-once in CreateT *)
    cond: pthread_cond_t := NIL;        (* write-once in CreateT; a place to park while waiting *)
    alerted : BOOLEAN := FALSE;         (* LL = mutex; the alert flag *)
    waitingOn: pthread_mutex_t := NIL;  (* LL = mutex; The CV's mutex *)
    nextWaiter: Activation := NIL;      (* LL = mutex; waiting thread queue *)
    next, prev: Activation := NIL;      (* LL = activeMu; global doubly-linked, circular list of all active threads *)
    handle: pthread_t := NIL;           (* LL = activeMu; thread handle *)
    stackbase: ADDRESS := NIL;          (* LL = activeMu; stack base for GC *)
    context: ADDRESS := NIL;            (* LL = activeMu *)
    state := ActState.Started;          (* LL = activeMu *)
    slot: CARDINAL := 0;                (* LL = slotMu; index in slots *)
    floatState : FloatMode.ThreadState; (* per-thread floating point state *)
    heapState : RTHeapRep.ThreadState;  (* per-thread heap state *)
  END;

PROCEDURE SetState (act: Activation;  state: ActState) =
  CONST text = ARRAY ActState OF TEXT
    { "Starting", "Started", "Stopping", "Stopped" };
  BEGIN
    act.state := state;
    IF DEBUG THEN
      RTIO.PutText(text[state]);
      RTIO.PutText(" act=");
      RTIO.PutAddr(act);
      RTIO.PutText("\n");
      RTIO.Flush();
    END;
  END SetState;

(*----------------------------------------------------------------- Mutex ---*)

PROCEDURE Acquire (m: Mutex) =
  BEGIN
    m.acquire ();
  END Acquire;

PROCEDURE Release (m: Mutex) =
  BEGIN
    m.release ();
  END Release;

PROCEDURE CleanMutex (r: REFANY) =
  VAR m := NARROW(r, Mutex);
  BEGIN
    pthread_mutex_delete(m.mutex);
    m.mutex := NIL;
  END CleanMutex;

PROCEDURE InitMutex (VAR m: pthread_mutex_t; root: REFANY;
                     Clean: PROCEDURE(root: REFANY)) =
  VAR mutex := pthread_mutex_new();
  BEGIN
    TRY
      WITH r = pthread_mutex_lock(initMu) DO <*ASSERT r=0*> END;
      (* Did someone else win the race? *)
      IF m # NIL THEN RETURN END;
      (* We won the race, but we might have failed to allocate. *)
      IF mutex = NIL THEN RTE.Raise (RTE.T.OutOfMemory) END;
      RTHeapRep.RegisterFinalCleanup (root, Clean);
      m := mutex;
      mutex := NIL;
    FINALLY
      WITH r = pthread_mutex_unlock(initMu) DO <*ASSERT r=0*> END;
      pthread_mutex_delete(mutex);
    END;
  END InitMutex;

PROCEDURE LockMutex (m: Mutex) =
  VAR self := GetActivation();
  BEGIN
    IF perfOn THEN PerfChanged(State.locking) END;
    IF m.mutex = NIL THEN InitMutex(m.mutex, m, CleanMutex) END;
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_lock(m.mutex) DO <*ASSERT r=0*> END;
    IF m.holder = NIL THEN
      m.holder := self;
      WITH r = pthread_mutex_unlock(m.mutex) DO <*ASSERT r=0*> END;
      WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
      IF perfOn THEN PerfRunning() END;
      RETURN;
    END;
    <*ASSERT self.waitingOn = NIL*>
    <*ASSERT self.nextWaiter = NIL*>
    self.waitingOn := m.mutex;
    self.nextWaiter := m.waiters;
    m.waiters := self;
    IF m.holder = self THEN Die(ThisLine(), "impossible acquire") END;
    WITH r = pthread_mutex_unlock(m.mutex) DO <*ASSERT r=0*> END;
    REPEAT
      WITH r = pthread_cond_wait(self.cond, self.mutex) DO <*ASSERT r=0*> END;
    UNTIL self.waitingOn = NIL; (* m.holder = self *)
    <*ASSERT m.holder = self*>
    <*ASSERT self.nextWaiter = NIL*>
    WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
    IF perfOn THEN PerfRunning() END;
  END LockMutex;

PROCEDURE UnlockMutex (m: Mutex) =
  VAR
    self := GetActivation();
    t, prev: Activation;
  BEGIN
    IF m.mutex = NIL THEN InitMutex(m.mutex, m, CleanMutex) END;
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_lock(m.mutex) DO <*ASSERT r=0*> END;
    IF m.holder # self THEN Die(ThisLine(), "illegal release") END;
    t := m.waiters;
    IF t = NIL THEN
      m.holder := NIL;
      WITH r = pthread_mutex_unlock(m.mutex) DO <*ASSERT r=0*> END;
      WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
      RETURN;
    END;
    prev := NIL;
    WHILE t.nextWaiter # NIL DO
      prev := t;
      t := t.nextWaiter;
    END;
    IF prev # NIL
      THEN prev.nextWaiter := NIL;
      ELSE m.waiters := NIL;
    END;
    m.holder := t;
    WITH r = pthread_mutex_unlock(m.mutex) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_lock(t.mutex) DO <*ASSERT r=0*> END;
    <*ASSERT t.waitingOn = m.mutex*>
    t.nextWaiter := NIL;
    t.waitingOn := NIL;
    WITH r = pthread_cond_signal(t.cond) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock(t.mutex) DO <*ASSERT r=0*> END;
  END UnlockMutex;

(*---------------------------------------- Condition variables and Alerts ---*)

PROCEDURE CleanCondition (r: REFANY) =
  VAR c := NARROW(r, Condition);
  BEGIN
    pthread_mutex_delete(c.mutex);
    c.mutex := NIL;
  END CleanCondition;

PROCEDURE XWait (self: Activation; m: Mutex; c: Condition; alertable: BOOLEAN)
  RAISES {Alerted} =
  (* LL = m *)
  VAR next, prev: Activation;
  BEGIN
    IF perfOn THEN PerfChanged(State.waiting) END;
    IF c.mutex = NIL THEN InitMutex(c.mutex, c, CleanCondition) END;
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    <*ASSERT self.waitingOn = NIL*>
    <*ASSERT self.nextWaiter = NIL*>

    WITH r = pthread_mutex_lock(c.mutex) DO <*ASSERT r=0*> END;
    self.waitingOn := c.mutex;
    self.nextWaiter := c.waiters;
    c.waiters := self;
    WITH r = pthread_mutex_unlock(c.mutex) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;

    m.release();

    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    LOOP
      IF alertable AND self.alerted THEN
        self.alerted := FALSE;
        <*ASSERT self.waitingOn = c.mutex*>
        WITH r = pthread_mutex_lock(c.mutex) DO <*ASSERT r=0*> END;
        next := c.waiters; prev := NIL;
        WHILE next # self DO
          <*ASSERT next # NIL*>
          prev := next; next := next.nextWaiter;
        END;
        IF prev = NIL
          THEN c.waiters := self.nextWaiter;
          ELSE prev.nextWaiter := self.nextWaiter;
        END;
        WITH r = pthread_mutex_unlock(c.mutex) DO <*ASSERT r=0*> END;
        self.nextWaiter := NIL;
        self.waitingOn := NIL;
        WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
        m.acquire();
        RAISE Alerted;
      ELSIF self.waitingOn = NIL THEN
        <*ASSERT self.nextWaiter = NIL*>
        WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
        m.acquire();
        RETURN;
      END;
      WITH r = pthread_cond_wait(self.cond, self.mutex) DO <*ASSERT r=0*> END;
    END;
  END XWait;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := GetActivation();
  BEGIN
    XWait(self, m, c, alertable := TRUE);
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  <*FATAL Alerted*>
  (* LL = m *)
  VAR self := GetActivation();
  BEGIN
    XWait(self, m, c, alertable := FALSE);
  END Wait;

PROCEDURE Signal (c: Condition) =
  VAR
    self := GetActivation();
    t: Activation;
  BEGIN
    IF c.mutex = NIL THEN InitMutex(c.mutex, c, CleanCondition) END;
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_lock(c.mutex) DO <*ASSERT r=0*> END;
    t := c.waiters;
    IF t # NIL THEN
      c.waiters := t.nextWaiter;
    END;
    WITH r = pthread_mutex_unlock(c.mutex) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
    IF t # NIL THEN
      WITH r = pthread_mutex_lock(t.mutex) DO <*ASSERT r=0*> END;
      t.nextWaiter := NIL;
      t.waitingOn := NIL;
      WITH r = pthread_cond_signal(t.cond) DO <*ASSERT r=0*> END;
      WITH r = pthread_mutex_unlock(t.mutex) DO <*ASSERT r=0*> END;
    END;
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  VAR
    self := GetActivation();
    t, next: Activation;
  BEGIN
    IF c.mutex = NIL THEN InitMutex(c.mutex, c, CleanCondition) END;
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_lock(c.mutex) DO <*ASSERT r=0*> END;
    t := c.waiters;
    c.waiters := NIL;
    WITH r = pthread_mutex_unlock(c.mutex) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
    WHILE t # NIL DO
      WITH r = pthread_mutex_lock(t.mutex) DO <*ASSERT r=0*> END;
      next := t.nextWaiter;
      t.nextWaiter := NIL;
      t.waitingOn := NIL;
      WITH r = pthread_cond_signal(t.cond) DO <*ASSERT r=0*> END;
      WITH r = pthread_mutex_unlock(t.mutex) DO <*ASSERT r=0*> END;
      t := next;
    END;
  END Broadcast;

PROCEDURE Alert (thread: T) =
  VAR t := thread.act;
  BEGIN
    WITH r = pthread_mutex_lock(t.mutex) DO <*ASSERT r=0*> END;
    t.alerted := TRUE;
    WITH r = pthread_cond_signal(t.cond) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock(t.mutex) DO <*ASSERT r=0*> END;
  END Alert;

PROCEDURE XTestAlert (self: Activation): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    result := self.alerted;
    self.alerted := FALSE;
    WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
    RETURN result;
  END XTestAlert;

PROCEDURE TestAlert (): BOOLEAN =
  VAR self := GetActivation();
  BEGIN
    RETURN XTestAlert(self);
  END TestAlert;

(*------------------------------------------------------------------ Self ---*)

VAR (* LL = slotMu *)
  n_slotted: CARDINAL;
  next_slot: CARDINAL;      (* NOTE: we don't use slots[0] *)
  slots: REF ARRAY OF T;    (* NOTE: we don't use slots[0] *)

PROCEDURE InitActivations (me: Activation) =
  BEGIN
    me.handle := pthread_self();
    me.next := me;
    me.prev := me;
    SetActivation(me);
    (* Explicitly (re)initialize to handle fork(). *)
    next_slot := 1;     (* no threads created yet *)
    slots := NIL;       (* no threads created yet *)
    n_slotted := 0;     (* no threads created yet *)
    allThreads := me;
    FloatMode.InitThread(me.floatState);
  END InitActivations;

PROCEDURE Self (): T =
  (* If not the initial thread and not created by Fork, returns NIL *)
  VAR
    me := GetActivation();
    t: T;
  BEGIN
    IF me = NIL THEN Die(ThisLine(), "Thread primitive called from non-Modula-3 thread") END;
    WITH r = pthread_mutex_lock(slotsMu) DO <*ASSERT r=0*> END;
      t := slots[me.slot];
    WITH r = pthread_mutex_unlock(slotsMu) DO <*ASSERT r=0*> END;
    IF (t.act # me) THEN Die(ThisLine(), "thread with bad slot!") END;
    RETURN t;
  END Self;

PROCEDURE AssignSlot (t: T): INTEGER =
  (* LL = 0, cause we allocate stuff with NEW! *)
  VAR n: CARDINAL;  new_slots: REF ARRAY OF T;  slot: CARDINAL;
  BEGIN
    WITH r = pthread_mutex_lock(slotsMu) DO <*ASSERT r=0*> END;

      (* make sure we have room to register this guy *)
      IF (slots = NIL) THEN
        WITH r = pthread_mutex_unlock(slotsMu) DO <*ASSERT r=0*> END;
          slots := NEW (REF ARRAY OF T, 20);
        WITH r = pthread_mutex_lock(slotsMu) DO <*ASSERT r=0*> END;
      END;
      IF (n_slotted >= LAST (slots^)) THEN
        n := NUMBER (slots^);
        WITH r = pthread_mutex_unlock(slotsMu) DO <*ASSERT r=0*> END;
          new_slots := NEW (REF ARRAY OF T, n+n);
        WITH r = pthread_mutex_lock(slotsMu) DO <*ASSERT r=0*> END;
        IF (n = NUMBER (slots^)) THEN
          (* we won any races that may have occurred. *)
          SUBARRAY (new_slots^, 0, n) := slots^;
          slots := new_slots;
        ELSIF (n_slotted < LAST (slots^)) THEN
          (* we lost a race while allocating a new slot table,
             and the new table has room for us. *)
        ELSE
          (* ouch, the new table is full too!   Bail out and retry *)
          WITH r = pthread_mutex_unlock(slotsMu) DO <*ASSERT r=0*> END;
          RETURN AssignSlot (t);
        END;
      END;

      (* look for an empty slot *)
      WHILE (slots [next_slot] # NIL) DO
        INC (next_slot);
        IF (next_slot >= NUMBER (slots^)) THEN next_slot := 1; END;
      END;

      INC (n_slotted);
      slot := next_slot;
      slots [slot] := t;

    WITH r = pthread_mutex_unlock(slotsMu) DO <*ASSERT r=0*> END;
    RETURN slot;
  END AssignSlot;

PROCEDURE FreeSlot (self: T) =
  (* LL = 0 *)
  BEGIN
    WITH r = pthread_mutex_lock(slotsMu) DO <*ASSERT r=0*> END;

      DEC (n_slotted);
      WITH z = slots [self.act.slot] DO
        IF z # self THEN Die (ThisLine(), "unslotted thread!"); END;
        z := NIL;
      END;
      self.act.slot := 0;
    WITH r = pthread_mutex_unlock(slotsMu) DO <*ASSERT r=0*> END;
  END FreeSlot;

PROCEDURE DumpThread (t: Activation) =
  BEGIN
    RTIO.PutText("Activation:   "); RTIO.PutAddr(t);             RTIO.PutChar('\n');
    RTIO.PutText("  slot:       "); RTIO.PutInt(t.slot);         RTIO.PutChar('\n');
    RTIO.PutText("  mutex:      "); RTIO.PutAddr(t.mutex);       RTIO.PutChar('\n');
    RTIO.PutText("  cond:       "); RTIO.PutAddr(t.cond);        RTIO.PutChar('\n');
    RTIO.PutText("  alerted:    "); RTIO.PutInt(ORD(t.alerted)); RTIO.PutChar('\n');
    RTIO.PutText("  waitingOn:  "); RTIO.PutAddr(t.waitingOn);   RTIO.PutChar('\n');
    RTIO.PutText("  nextWaiter: "); RTIO.PutAddr(t.nextWaiter);  RTIO.PutChar('\n');
    RTIO.PutText("  frame:      "); RTIO.PutAddr(t.frame);       RTIO.PutChar('\n');
    RTIO.PutText("  next:       "); RTIO.PutAddr(t.next);        RTIO.PutChar('\n');
    RTIO.PutText("  prev:       "); RTIO.PutAddr(t.prev);        RTIO.PutChar('\n');
    RTIO.PutText("  handle:     "); RTIO.PutAddr(t.handle);      RTIO.PutChar('\n');
    RTIO.PutText("  stackbase:  "); RTIO.PutAddr(t.stackbase);   RTIO.PutChar('\n');
    RTIO.PutText("  context:    "); RTIO.PutAddr(t.context);     RTIO.PutChar('\n');
    RTIO.PutText("  state:      ");
    CASE t.state OF
    | ActState.Started => RTIO.PutText("Started\n");
    | ActState.Stopped => RTIO.PutText("Stopped\n");
    | ActState.Starting => RTIO.PutText("Starting\n");
    | ActState.Stopping => RTIO.PutText("Stopping\n");
    END;
    RTIO.Flush();
  END DumpThread;

PROCEDURE DumpThreads () =
  VAR t := allThreads;
  BEGIN
    REPEAT
      DumpThread(t);
      t := t.next
    UNTIL t = allThreads;
  END DumpThreads;

(*------------------------------------------------------------ Fork, Join ---*)

VAR (* LL=activeMu *)
  allThreads: Activation := NIL;            (* global list of active threads *)

PROCEDURE CleanThread (r: REFANY) =
  VAR t := NARROW(r, T);
  BEGIN
    pthread_mutex_delete(t.act.mutex);
    pthread_cond_delete(t.act.cond);
    DISPOSE(t.act);
  END CleanThread;

(* ThreadBase calls RunThread after finding (approximately) where
   its stack begins.  This dance ensures that all of ThreadMain's
   traced references are within the stack scanned by the collector. *)

PROCEDURE ThreadBase (param: ADDRESS): ADDRESS =
  VAR
    me: Activation := param;
  BEGIN
    SetActivation(me);
    me.stackbase := ADR(me); (* enable GC scanning of this stack *)
    me.handle := pthread_self();

    (* add to the list of active threads *)
    WITH r = pthread_mutex_lock(activeMu) DO <*ASSERT r=0*> END;
      me.next := allThreads;
      me.prev := allThreads.prev;
      allThreads.prev.next := me;
      allThreads.prev := me;
    WITH r = pthread_mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
    FloatMode.InitThread (me.floatState);

    RunThread(me);

    (* remove from the list of active threads *)
    WITH r = pthread_mutex_lock(activeMu) DO <*ASSERT r=0*> END;
      <*ASSERT allThreads # me*>
      me.stackbase := NIL; (* disable GC scanning of my stack *)
      me.next.prev := me.prev;
      me.prev.next := me.next;
      WITH r = pthread_detach_self() DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
    me.next := NIL;
    me.prev := NIL;
    RETURN NIL;
  END ThreadBase;

PROCEDURE RunThread (me: Activation) =
  VAR self: T;
  BEGIN
    IF perfOn THEN PerfChanged(State.alive) END;

    WITH r = pthread_mutex_lock(slotsMu) DO <*ASSERT r=0*> END;
      self := slots [me.slot];
    WITH r = pthread_mutex_unlock(slotsMu) DO <*ASSERT r=0*> END;

    IF perfOn THEN PerfRunning() END;

    (*** Run the user-level code. ***)
    self.result := self.closure.apply();

    IF perfOn THEN PerfChanged(State.dying) END;

    (* Join *)
    LOCK joinMu DO
      Broadcast(self.join);
      self.join := NIL;     (* mark me done *)
    END;

    IF perfOn THEN PerfChanged(State.dead) END;

    (* we're dying *)
    RTHeapRep.FlushThreadState(me.heapState);

    IF perfOn THEN PerfDeleted() END;
    FreeSlot(self);  (* note: needs self.act ! *)
    (* Since we're no longer slotted, we cannot touch traced refs. *)
  END RunThread;

VAR joinMu: MUTEX;

PROCEDURE Fork (closure: Closure): T =
  VAR
    act := NEW(Activation,
               mutex := pthread_mutex_new(),
               cond := pthread_cond_new());
    size := defaultStackSize;
    t: T := NIL;
  BEGIN
    TRY
      IF act.mutex = NIL OR act.cond = NIL THEN
        RTE.Raise(RTE.T.OutOfMemory);
      END;
      t := NEW(T, act := act, closure := closure, join := NEW(Condition));
      RTHeapRep.RegisterFinalCleanup(t, CleanThread);
      act.slot := AssignSlot(t);
    FINALLY
      IF act.slot = 0 THEN
        (* we failed, cleanup *)
        pthread_mutex_delete(act.mutex);
        pthread_cond_delete(act.cond);
        DISPOSE(act);
      END;
    END;
    (* determine the initial size of the stack for this thread *)
    TYPECASE closure OF
    | SizedClosure (scl) => size := scl.stackSize;
    ELSE (*skip*)
    END;
    WITH r = thread_create(size * ADRSIZE(Word.T), ThreadBase, act) DO
      IF r # 0 THEN DieI(ThisLine(), r) END;
    END;
    RETURN t;
  END Fork;

PROCEDURE XJoin (self: Activation; t: T; alertable: BOOLEAN):
  REFANY RAISES {Alerted} =
  BEGIN
    LOCK joinMu DO
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice") END;
      TRY
        t.joined := TRUE;
        WHILE t.join # NIL DO XWait(self, joinMu, t.join, alertable) END;
      FINALLY
        IF t.join # NIL THEN t.joined := FALSE END;
      END;
    END;
    RETURN t.result;
  END XJoin;

PROCEDURE Join (t: T): REFANY =
  <*FATAL Alerted*>
  VAR self := GetActivation();
  BEGIN
    RETURN XJoin(self, t, alertable := FALSE);
  END Join;

PROCEDURE AlertJoin (t: T): REFANY RAISES {Alerted} =
  VAR self := GetActivation();
  BEGIN
    RETURN XJoin(self, t, alertable := TRUE);
  END AlertJoin;

(*---------------------------------------------------- Scheduling support ---*)

PROCEDURE XPause (self: Activation; n: LONGREAL; alertable: BOOLEAN)
  RAISES {Alerted} =
  VAR until := Time.Now() + n;
  BEGIN
    IF perfOn THEN PerfChanged(State.pausing) END;
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    <*ASSERT self.waitingOn = NIL*>
    <*ASSERT self.nextWaiter = NIL*>

    LOOP
      IF alertable AND self.alerted THEN
        self.alerted := FALSE;
        WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
        IF perfOn THEN PerfRunning() END;
        RAISE Alerted;
      END;
      WITH r = pthread_cond_timedwait(self.cond, self.mutex, until) DO
        IF r = Uerror.ETIMEDOUT THEN
          WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
          IF perfOn THEN PerfRunning() END;
          RETURN;
        END;
        <*ASSERT r=0*>
      END;
    END;
  END XPause;

PROCEDURE Pause (n: LONGREAL) =
  <*FATAL Alerted*>
  VAR self := GetActivation();
  BEGIN
    XPause(self, n, alertable := FALSE);
  END Pause;

PROCEDURE AlertPause (n: LONGREAL) RAISES {Alerted} =
  VAR self := GetActivation();
  BEGIN
    XPause(self, n, alertable := TRUE);
  END AlertPause;

PROCEDURE Yield () =
  BEGIN
    WITH r = Usched.yield() DO
      IF r # 0 THEN DieI(ThisLine(), Cerrno.GetErrno()) END;
    END;
  END Yield;

PROCEDURE IOWait (fd: CARDINAL; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult =
  <*FATAL Alerted*>
  VAR self := GetActivation();
  BEGIN
    TRY
      IF perfOn THEN PerfChanged(State.blocking) END;
      RETURN XIOWait(self, fd, read, timeoutInterval, alertable := FALSE);
    FINALLY
      IF perfOn THEN PerfRunning() END;
    END;
  END IOWait;

PROCEDURE IOAlertWait (fd: CARDINAL; read: BOOLEAN;
                       timeoutInterval: LONGREAL := -1.0D0): WaitResult
  RAISES {Alerted} =
  VAR self := GetActivation();
  BEGIN
    TRY
      IF perfOn THEN PerfChanged(State.blocking) END;
      RETURN XIOWait(self, fd, read, timeoutInterval, alertable := TRUE);
    FINALLY
      IF perfOn THEN PerfRunning() END;
    END;
  END IOAlertWait;

PROCEDURE XIOWait (self: Activation; fd: CARDINAL; read: BOOLEAN;
                   interval: LONGREAL; alertable: BOOLEAN): WaitResult
  RAISES {Alerted} =
  VAR res: WaitResult;
      subInterval: LONGREAL := 1.0d0;
      err: int := 0;
      again := FALSE;
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
      res := VAL(Poll(fd, ORD(read), subInterval), WaitResult);

      IF alertable AND XTestAlert(self) THEN RAISE Alerted END;

      CASE res OF
        | WaitResult.FDError, WaitResult.Ready =>
          RETURN res;
        | WaitResult.Error =>
          err := Cerrno.GetErrno();
          IF err = Uerror.EINTR THEN
            (* spurious wakeups are OK *)
          ELSIF err = Uerror.EAGAIN AND NOT again THEN
            again := TRUE; (* try just once more *)
          ELSE
            RETURN WaitResult.Error;
          END;
        | WaitResult.Timeout =>
          interval := interval - subInterval;
          IF interval <= 0.0d0 THEN RETURN WaitResult.Timeout END;
          IF interval < subInterval THEN
            subInterval := interval;
          END;
      END;
    END;
  END XIOWait;

PROCEDURE WaitProcess (pid: int; VAR status: int): int =
  (* ThreadPThread.m3 and ThreadPosix.m3 are very similar. *)
  BEGIN
    LOOP
      WITH r = Uexec.waitpid(pid, ADR(status), 0) DO
        <*ASSERT r # 0*>
        IF r > 0 THEN RETURN r END;
        IF Cerrno.GetErrno() # Uerror.EINTR THEN RETURN r END;
      END;
    END;
  END WaitProcess;

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

   So, SuspendOthers does not grab "cm" before shutting down the other
   threads.  If the collector tries to use any of the thread functions
   that acquire "cm", it'll be deadlocked.
*)

VAR suspended: BOOLEAN := FALSE;        (* LL=activeMu *)

PROCEDURE SuspendOthers () =
  (* LL=0. Always bracketed with ResumeOthers which releases "activeMu" *)
  BEGIN
    WITH r = pthread_mutex_lock(activeMu) DO <*ASSERT r=0*> END;
    StopWorld();
    <*ASSERT NOT suspended*>
    suspended := TRUE;
  END SuspendOthers;

PROCEDURE ResumeOthers () =
  (* LL=activeMu.  Always preceded by SuspendOthers. *)
  BEGIN
    <*ASSERT suspended*>
    suspended := FALSE;
    StartWorld();
    WITH r = pthread_mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
  END ResumeOthers;

PROCEDURE ProcessStacks (p: PROCEDURE (start, limit: ADDRESS)) =
  (* LL=activeMu.  Only called within {SuspendOthers, ResumeOthers} *)
  VAR
    me := GetActivation();
    act: Activation;
  BEGIN
    ProcessMe(me, p);
    act := me.next;
    WHILE act # me DO
      ProcessOther(act, p);
      act := act.next;
    END;
  END ProcessStacks;

PROCEDURE ProcessEachStack (p: PROCEDURE (start, limit: ADDRESS)) =
  (* LL=0 *)
  VAR
    me := GetActivation();
    act: Activation;
    acks: int;
    nLive, nDead, newlySent: INTEGER := 0;
    wait_nsecs := RETRY_INTERVAL;
  BEGIN
    WITH r = pthread_mutex_lock(activeMu) DO <*ASSERT r=0*> END;

    ProcessMe(me, p);

    act := me.next;
    WHILE act # me DO
      (* stop *)
      LOOP
        <*ASSERT act.state = ActState.Started*>
        SetState(act, ActState.Stopping);
        IF SIG_SUSPEND = 0 THEN
          IF StopThread(act) THEN
            SetState(act, ActState.Stopped);
            EXIT;
          ELSE
            SetState(act, ActState.Started);
          END;
        ELSE
          SignalThread(act);
          INC(nLive);
          EXIT;
        END;
        Nanosleep(WAIT_UNIT);
      END;
      WHILE nLive > 0 DO
        <*ASSERT SIG_SUSPEND # 0*>
        WITH r = sem_getvalue(acks) DO <*ASSERT r=0*> END;
        IF acks = nLive THEN EXIT END;
        <*ASSERT acks < nLive*>
        IF wait_nsecs <= 0 THEN
          newlySent := 0;
          <*ASSERT act.state # ActState.Starting*>
          IF act.state # ActState.Stopped THEN
            SetState(act, ActState.Stopping);
            SignalThread(act);
            INC(newlySent);
          END;
          wait_nsecs := RETRY_INTERVAL;
        ELSE
          Nanosleep(WAIT_UNIT);
          DEC(wait_nsecs, WAIT_UNIT);
        END;
      END;
      FOR i := 0 TO nLive - 1 DO
        WHILE sem_wait() # 0 DO
          WITH r = Cerrno.GetErrno() DO
            IF r # Uerror.EINTR THEN DieI(ThisLine(), r) END;
          END;
          (*retry*)
        END;
      END;

      (* process *)
      ProcessOther(act, p);

      (* start *)
      nDead := 0;
      LOOP
        <*ASSERT act.state = ActState.Stopped*>
        SetState(act, ActState.Starting);
        IF SIG_SUSPEND = 0 THEN
          IF StartThread(act) THEN
            SetState(act, ActState.Started);
            EXIT;
          ELSE
            SetState(act, ActState.Stopped);
          END;
        ELSE
          SignalThread(act);
          INC(nDead);
          EXIT;
        END;
        Nanosleep(WAIT_UNIT);
      END;
      WHILE nDead > 0 DO
        <*ASSERT SIG_SUSPEND # 0*>
        WITH r = sem_getvalue(acks) DO <*ASSERT r=0*> END;
        IF acks = nDead THEN EXIT END;
        <*ASSERT acks < nDead*>
        IF wait_nsecs <= 0 THEN
          newlySent := 0;
          <*ASSERT act.state # ActState.Stopping*>
          IF act.state # ActState.Started THEN
            SignalThread(act);
            INC(newlySent);
          END;
          wait_nsecs := RETRY_INTERVAL;
        ELSE
          Nanosleep(WAIT_UNIT);
          DEC(wait_nsecs, WAIT_UNIT);
        END;
      END;
      FOR i := 0 TO nDead - 1 DO
        WHILE sem_wait() # 0 DO
          WITH r = Cerrno.GetErrno() DO
            IF r # Uerror.EINTR THEN DieI(ThisLine(), r) END;
          END;
          (*retry*)
        END;
      END;
    END;

    WITH r = pthread_mutex_unlock(activeMu) DO <*ASSERT r=0*> END;
  END ProcessEachStack;

PROCEDURE ProcessMe (me: Activation;  p: PROCEDURE (start, limit: ADDRESS)) =
  (* LL=activeMu *)
  BEGIN
    <*ASSERT me.state # ActState.Stopped*>
    IF DEBUG THEN
      RTIO.PutText("Processing act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;
    RTHeapRep.FlushThreadState(me.heapState);
    ProcessLive(me.stackbase, p);
  END ProcessMe;

PROCEDURE ProcessOther (act: Activation;  p: PROCEDURE (start, stop: ADDRESS)) =
  (* LL=activeMu *)
  BEGIN
    <*ASSERT act.state = ActState.Stopped*>
    IF DEBUG THEN
      RTIO.PutText("Processing act="); RTIO.PutAddr(act); RTIO.PutText("\n"); RTIO.Flush();
    END;
    RTHeapRep.FlushThreadState(act.heapState);
    IF act.stackbase # NIL THEN
      ProcessStopped(act.handle, act.stackbase, act.context, p);
    END;
  END ProcessOther;

(* Signal based suspend/resume *)

PROCEDURE SignalThread(act: Activation) =
  (* LL=activeMu *)
  BEGIN
    <*ASSERT SIG_SUSPEND # 0*>
    LOOP
      WITH z = pthread_kill(act.handle, SIG_SUSPEND) DO
        IF z = 0 THEN EXIT END;
        IF z # Uerror.EAGAIN THEN DieI(ThisLine(), z) END;
        (* try it again... *)
      END;
    END;
  END SignalThread;

PROCEDURE StopThread (act: Activation): BOOLEAN =
  (* LL=activeMu *)
  BEGIN
    <*ASSERT act.state = ActState.Stopping*>
    <*ASSERT SIG_SUSPEND = 0*>
    IF NOT SuspendThread(act.handle) THEN RETURN FALSE END;
    IF act.heapState.inCritical # 0 THEN
      IF NOT RestartThread(act.handle) THEN <*ASSERT FALSE*> END;
      RETURN FALSE;
    END;
    RETURN TRUE;
  END StopThread;

PROCEDURE StartThread (act: Activation): BOOLEAN =
  (* LL=activeMu *)
  BEGIN
    <*ASSERT act.state = ActState.Starting*>
    <*ASSERT SIG_SUSPEND = 0*>
    RETURN RestartThread(act.handle);
  END StartThread;

PROCEDURE StopWorld () =
  (* LL=activeMu *)
  VAR
    me := GetActivation();
    act: Activation;
    acks: int;
    nLive, newlySent: INTEGER;
    retry: BOOLEAN;
    wait_nsecs := RETRY_INTERVAL;
  BEGIN
    IF DEBUG THEN
      RTIO.PutText("Stopping from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;

    nLive := 0;
    LOOP
      retry := FALSE;
      act := me.next;
      WHILE act # me DO
        <*ASSERT act.state # ActState.Starting*>
        IF act.state = ActState.Started THEN
          SetState(act, ActState.Stopping);
          IF SIG_SUSPEND = 0 THEN
            IF StopThread(act) THEN
              SetState(act, ActState.Stopped);
            ELSE
              SetState(act, ActState.Started);
              retry := TRUE;
            END;
          ELSE
            SignalThread(act);
            INC(nLive);
          END;
        END;
        act := act.next;
      END;
      IF NOT retry THEN EXIT END;
      Nanosleep(WAIT_UNIT);
    END;
    WHILE nLive > 0 DO
      <*ASSERT SIG_SUSPEND # 0*>
      WITH r = sem_getvalue(acks) DO <*ASSERT r=0*> END;
      IF acks = nLive THEN EXIT END;
      <*ASSERT acks < nLive*>
      IF wait_nsecs <= 0 THEN
        newlySent := 0;
        act := me.next;
        WHILE act # me DO
          <*ASSERT act.state # ActState.Starting*>
          IF act.state # ActState.Stopped THEN
            SetState(act, ActState.Stopping);
            SignalThread(act);
            INC(newlySent);
          END;
          act := act.next;
        END;
        wait_nsecs := RETRY_INTERVAL;
      ELSE
        Nanosleep(WAIT_UNIT);
        DEC(wait_nsecs, WAIT_UNIT);
      END;
    END;
    (* drain semaphore *)
    FOR i := 0 TO nLive-1 DO
      WHILE sem_wait() # 0 DO
        WITH r = Cerrno.GetErrno() DO
          IF r # Uerror.EINTR THEN DieI(ThisLine(), r) END;
        END;
        (*retry*)
      END;
    END;

    IF DEBUG THEN
      RTIO.PutText("Stopped from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
      DumpThreads();
    END;
  END StopWorld;

PROCEDURE StartWorld () =
  (* LL=activeMu *)
  VAR
    me := GetActivation();
    act: Activation;
    acks: int;
    nDead, newlySent: INTEGER;
    retry: BOOLEAN;
    wait_nsecs := RETRY_INTERVAL;
  BEGIN
    IF DEBUG THEN
      RTIO.PutText("Starting from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;

    nDead := 0;
    LOOP
      retry := FALSE;
      act := me.next;
      WHILE act # me DO
        <*ASSERT act.state # ActState.Stopping*>
        IF act.state # ActState.Started THEN
          SetState(act, ActState.Starting);
          IF SIG_SUSPEND = 0 THEN
            IF StartThread(act) THEN
              SetState(act, ActState.Started);
            ELSE
              SetState(act, ActState.Stopped);
              retry := TRUE;
            END;
          ELSE
            SignalThread(act);
            INC(nDead);
          END;
        END;
        act := act.next;
      END;
      IF NOT retry THEN EXIT END;
      Nanosleep(WAIT_UNIT);
    END;
    WHILE nDead > 0 DO
      <*ASSERT SIG_SUSPEND # 0*>
      WITH r = sem_getvalue(acks) DO <*ASSERT r=0*> END;
      IF acks = nDead THEN EXIT END;
      <*ASSERT acks < nDead*>
      IF wait_nsecs <= 0 THEN
        newlySent := 0;
        act := me.next;
        WHILE act # me DO
          <*ASSERT act.state # ActState.Stopping*>
          IF act.state # ActState.Started THEN
            SignalThread(act);
            INC(newlySent);
          END;
          act := act.next;
        END;
        wait_nsecs := RETRY_INTERVAL;
      ELSE
        Nanosleep(WAIT_UNIT);
        DEC(wait_nsecs, WAIT_UNIT);
      END;
    END;
    (* drain semaphore *)
    FOR i := 0 TO nDead-1 DO
      WHILE sem_wait() # 0 DO
        WITH r = Cerrno.GetErrno() DO
          IF r # Uerror.EINTR THEN DieI(ThisLine(), r) END;
        END;
        (*retry*)
      END;
    END;

    IF DEBUG THEN
      RTIO.PutText("Started from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
      DumpThreads();
    END;
  END StartWorld;

PROCEDURE SignalHandler (sig: int; <*UNUSED*>info: ADDRESS; context: ADDRESS) =
  VAR
    errno := Cerrno.GetErrno();
    me := GetActivation();
  BEGIN
    <*ASSERT sig = SIG_SUSPEND*>
    IF me.state = ActState.Stopping THEN
      IF me.heapState.inCritical # 0 THEN
        me.state := ActState.Started;
        RETURN;
      END;
      me.state := ActState.Stopped;
      <*ASSERT me.context = NIL*>
      me.context := context;
      WITH r = sem_post() DO <*ASSERT r=0*> END;
      REPEAT sigsuspend() UNTIL me.state = ActState.Starting;
      me.context := NIL;
      me.state := ActState.Started;
      WITH r = sem_post() DO <*ASSERT r=0*> END;
    END;
    Cerrno.SetErrno(errno);
  END SignalHandler;

(*----------------------------------------------------------- misc. stuff ---*)

PROCEDURE MyId (): Id RAISES {} =
  VAR me := GetActivation();
  BEGIN
    IF me = NIL
      THEN RETURN 0
      ELSE RETURN me.slot;
    END;
  END MyId;

PROCEDURE MyFPState (): UNTRACED REF FloatMode.ThreadState =
  VAR me := GetActivation();
  BEGIN
    RETURN ADR(me.floatState);
  END MyFPState;

PROCEDURE MyHeapState (): UNTRACED REF RTHeapRep.ThreadState =
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

PROCEDURE DieI (lineno: INTEGER; i: INTEGER) =
  BEGIN
    RTError.MsgI (ThisFile(), lineno, "Thread client error: ", i);
  END DieI;

(*------------------------------------------------------ ShowThread hooks ---*)

VAR
  perfW : RTPerfTool.Handle;
  perfOn: BOOLEAN := FALSE;     (* LL = perfMu *)

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
  VAR
    e := ThreadEvent.T {kind := TE.Changed, id := MyId(), state := s};
  BEGIN
    WITH r = pthread_mutex_lock(perfMu) DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = pthread_mutex_unlock(perfMu) DO <*ASSERT r=0*> END;
  END PerfChanged;

PROCEDURE PerfDeleted () =
  VAR
    e := ThreadEvent.T {kind := TE.Deleted, id := MyId()};
  BEGIN
    WITH r = pthread_mutex_lock(perfMu) DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = pthread_mutex_unlock(perfMu) DO <*ASSERT r=0*> END;
  END PerfDeleted;

PROCEDURE PerfRunning () =
  VAR
    e := ThreadEvent.T {kind := TE.Running, id := MyId()};
  BEGIN
    WITH r = pthread_mutex_lock(perfMu) DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = pthread_mutex_unlock(perfMu) DO <*ASSERT r=0*> END;
  END PerfRunning;

(*-------------------------------------------------------- Initialization ---*)

PROCEDURE InitWithStackBase (stackbase: ADDRESS) =
  VAR
    self: T;
    me: Activation;
  BEGIN
    InitC(stackbase);

    me := NEW(Activation,
              mutex := pthread_mutex_new(),
              cond := pthread_cond_new());
    InitActivations(me);
    me.stackbase := stackbase;
    IF me.mutex = NIL OR me.cond = NIL THEN
      Die(ThisLine(), "Thread initialization failed.");
    END;
    self := NEW(T, act := me, closure := NIL, join := NIL);
    me.slot := AssignSlot(self);

    joinMu := NEW(MUTEX);

    PerfStart();
    IF perfOn THEN PerfRunning() END;
    IF RTParams.IsPresent("backgroundgc") THEN
      RTCollectorSRC.StartBackgroundCollection();
    END;
    IF RTParams.IsPresent("foregroundgc") THEN
      RTCollectorSRC.StartForegroundCollection();
    END;
  END InitWithStackBase;

PROCEDURE Init ()=
  VAR r: INTEGER;
  BEGIN
    r := RTProcess.RegisterForkHandlers(AtForkPrepare,
                                        AtForkParent,
                                        AtForkChild);
    IF r # 0 THEN DieI(ThisLine(), r) END;
    InitWithStackBase(ADR(r)); (* not quite accurate but hopefully ok *)
  END Init;

PROCEDURE PThreadLockMutex(mutex: pthread_mutex_t; line: INTEGER) =
  BEGIN
    IF mutex # NIL THEN
      WITH r = pthread_mutex_lock(mutex) DO
        IF r # 0 THEN DieI(line, r) END;
      END;
    END;
  END PThreadLockMutex;

PROCEDURE PThreadUnlockMutex(mutex: pthread_mutex_t;  line: INTEGER) =
  BEGIN
    IF mutex # NIL THEN
      WITH r = pthread_mutex_unlock(mutex) DO
        IF r # 0 THEN DieI(line, r) END;
      END;
    END;
  END PThreadUnlockMutex;

PROCEDURE AtForkPrepare() =
  VAR me := GetActivation();
      act: Activation;
  BEGIN
    PThreadLockMutex(slotsMu, ThisLine());
    PThreadLockMutex(perfMu, ThisLine());
    PThreadLockMutex(initMu, ThisLine()); (* InitMutex => RegisterFinalCleanup => LockHeap *)
    LockHeap();
    PThreadLockMutex(activeMu, ThisLine()); (* LockHeap => SuspendOthers => activeMu *)
    (* Walk activations and lock all threads.
     * NOTE: We have initMu, activeMu, so slots won't change, conditions and
     * mutexes won't be initialized on-demand.
     *)
    act := me;
    REPEAT
      PThreadLockMutex(act.mutex, ThisLine());
      act := act.next;
    UNTIL act = me;
  END AtForkPrepare;

PROCEDURE AtForkParent() =
  VAR me := GetActivation();
      act: Activation;
  BEGIN
    (* Walk activations and unlock all threads, conditions. *)
    act := me;
    REPEAT
      PThreadUnlockMutex(act.mutex, ThisLine());
      act := act.next;
    UNTIL act = me;
    PThreadUnlockMutex(activeMu, ThisLine());
    UnlockHeap();
    PThreadUnlockMutex(initMu, ThisLine());
    PThreadUnlockMutex(perfMu, ThisLine());
    PThreadUnlockMutex(slotsMu, ThisLine());
  END AtForkParent;

PROCEDURE AtForkChild() =
  BEGIN
    AtForkParent();
    InitWithStackBase(GetActivation().stackbase);
  END AtForkChild;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector. *)

VAR
  holder: pthread_t;
  inCritical := 0;

PROCEDURE LockHeap () =
  VAR self := pthread_self();
  BEGIN
    IF pthread_equal(holder, self) = 0 THEN
      WITH r = pthread_mutex_lock(heapMu) DO <*ASSERT r=0*> END;
      holder := self;
    END;
    INC(inCritical);
  END LockHeap;

PROCEDURE UnlockHeap () =
  BEGIN
    <*ASSERT pthread_equal(holder, pthread_self()) # 0*>
    DEC(inCritical);
    IF inCritical = 0 THEN
      holder := NIL;
      WITH r = pthread_mutex_unlock(heapMu) DO <*ASSERT r=0*> END;
    END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  VAR self := pthread_self();
  BEGIN
    <*ASSERT pthread_equal(holder, self) # 0*>
    DEC(inCritical);
    <*ASSERT inCritical = 0*>
    WITH r = pthread_cond_wait(heapCond, heapMu) DO <*ASSERT r=0*> END;
    holder := self;
    <*ASSERT inCritical = 0*>
    INC(inCritical);
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  BEGIN
    WITH r = pthread_cond_broadcast(heapCond) DO <*ASSERT r=0*> END;
  END BroadcastHeap;

(*--------------------------------------------- exception handling support --*)

PROCEDURE GetCurrentHandlers (): ADDRESS =
  VAR me := GetActivation();
  BEGIN
    RETURN me.frame;
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers (h: ADDRESS) =
  VAR me := GetActivation();
  BEGIN
    me.frame := h;
  END SetCurrentHandlers;

(*RTHooks.PushEFrame*)
PROCEDURE PushEFrame (frame: ADDRESS) =
  TYPE Frame = UNTRACED REF RECORD next: ADDRESS END;
  VAR
    me := GetActivation();
    f: Frame := frame;
  BEGIN
    f.next := me.frame;
    me.frame := f;
  END PushEFrame;

(*RTHooks.PopEFrame*)
PROCEDURE PopEFrame (frame: ADDRESS) =
  VAR me := GetActivation();
  BEGIN
    me.frame := frame;
  END PopEFrame;

VAR DEBUG := RTParams.IsPresent("debugthreads");

BEGIN
END ThreadPThread.
