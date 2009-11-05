(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

UNSAFE MODULE ThreadPThread EXPORTS Thread, ThreadF, ThreadInternal,
Scheduler, SchedulerPosix, RTOS, RTHooks, ThreadPThread;

IMPORT Cerrno, FloatMode, MutexRep,
       RTCollectorSRC, RTError, RTHeapRep, RTIO, RTParams,
       RTPerfTool, RTProcess, ThreadEvent, Time,
       Unix, Utime, Word, Upthread, Usched,
       Uerror, Uexec;
FROM Upthread IMPORT pthread_t;
FROM Compiler IMPORT ThisFile, ThisLine;
FROM Ctypes IMPORT int;
IMPORT RuntimeError AS RTE;

(*----------------------------------------------------- types and globals ---*)

CONST
  WAIT_UNIT = 1000000; (* one million nanoseconds, one thousandth of a second *)
  RETRY_INTERVAL = 10000000; (* 10 million nanoseconds, one hundredth of a second *)

VAR stack_grows_down: BOOLEAN;

REVEAL
  Mutex = MutexRep.Public BRANDED "Mutex Pthread-1.0" OBJECT
    mutex: pthread_mutex_t := NIL;
  OVERRIDES
    acquire := LockMutex;
    release := UnlockMutex;
  END;

  Condition = MUTEX BRANDED "Thread.Condition Pthread-1.0" OBJECT
    waiters: T := NIL;                   (* LL = mutex *)
  END;

  T = MUTEX BRANDED "Thread.T Pthread-1.6" OBJECT
    (* a place to park while waiting *)
    cond: pthread_cond_t := NIL;

    (* live thread data *)
    act: Activation := NIL;

    (* our work and its result *)
    closure: Closure := NIL;
    result: REFANY := NIL;

    (* CV that we're blocked on *)
    waitingOn: Condition := NIL;
    (* queue of threads waiting on the same CV *)
    nextWaiter: T := NIL;

    (* the alert flag *)
    alerted : BOOLEAN := FALSE;

    (* wait here to join *)
    join: Condition;			 (* NIL when result is set *)
    joined: BOOLEAN := FALSE;		 (* Is anyone waiting yet? *)
  END;

TYPE
  ActState = { Starting, Started, Stopping, Stopped };
  Activation = UNTRACED REF RECORD
    (* exception handling support *)
    frame: ADDRESS := NIL;
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

PROCEDURE InitMutex (m: Mutex) =
  VAR mutex := pthread_mutex_new();
  BEGIN
    WITH r = pthread_mutex_lock_init() DO <*ASSERT r=0*> END;
    IF m.mutex = NIL THEN (* We won the race. *)
      IF mutex = NIL THEN (* But we failed. *)
        WITH r = pthread_mutex_unlock_init() DO <*ASSERT r=0*> END;
        RTE.Raise (RTE.T.OutOfMemory);
      ELSE (* We won the race and succeeded. *)
        m.mutex := mutex;
        WITH r = pthread_mutex_unlock_init() DO <*ASSERT r=0*> END;
        RTHeapRep.RegisterFinalCleanup (m, CleanMutex);
      END;
    ELSE (* another thread beat us in the race, ok *)
      WITH r = pthread_mutex_unlock_init() DO <*ASSERT r=0*> END;
      pthread_mutex_delete(mutex);
    END;
  END InitMutex;

PROCEDURE LockMutex (m: Mutex) =
  BEGIN
    IF m.mutex = NIL THEN InitMutex(m) END;
    IF perfOn THEN PerfChanged(State.locking) END;
    WITH r = pthread_mutex_lock(m.mutex) DO
      IF r # 0 THEN DieI(ThisLine(), r) END;
    END;
    IF perfOn THEN PerfRunning() END;
  END LockMutex;

PROCEDURE UnlockMutex (m: Mutex) =
  (* LL = m *)
  BEGIN
    IF m.mutex = NIL THEN InitMutex(m) END;
    WITH r = pthread_mutex_unlock(m.mutex) DO
      IF r # 0 THEN DieI(ThisLine(), r) END;
    END;
  END UnlockMutex;

(*---------------------------------------- Condition variables and Alerts ---*)

PROCEDURE XWait (self: T; m: Mutex; c: Condition; alertable: BOOLEAN)
  RAISES {Alerted} =
  (* LL = m *)
  VAR next, prev: T;
  BEGIN
    IF c.mutex = NIL THEN InitMutex(c) END;
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    <*ASSERT self.waitingOn = NIL*>
    <*ASSERT self.nextWaiter = NIL*>

    WITH r = pthread_mutex_lock(c.mutex) DO <*ASSERT r=0*> END;
    self.waitingOn := c;
    self.nextWaiter := c.waiters;
    c.waiters := self;
    WITH r = pthread_mutex_unlock(c.mutex) DO <*ASSERT r=0*> END;

    m.release();
    IF perfOn THEN PerfChanged(State.waiting) END;
    LOOP
      IF alertable AND self.alerted THEN
        self.alerted := FALSE;
        <*ASSERT self.waitingOn = c*>
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
      END;
      WITH r = pthread_cond_wait(self.cond, self.mutex) DO <*ASSERT r=0*> END;
      IF self.waitingOn = NIL THEN
        <*ASSERT self.waitingOn = NIL*>
        <*ASSERT self.nextWaiter = NIL*>
        WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
        m.acquire();
        RETURN;
      END;
    END;
  END XWait;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    XWait(self, m, c, alertable := TRUE);
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  <*FATAL Alerted*>
  (* LL = m *)
  VAR self := Self();
  BEGIN
    XWait(self, m, c, alertable := FALSE);
  END Wait;

PROCEDURE DequeueHead(c: Condition) =
  (* LL = c *)
  VAR t := c.waiters;
  BEGIN
    WITH r = pthread_mutex_lock(t.mutex) DO <*ASSERT r=0*> END;
    c.waiters := t.nextWaiter;
    t.nextWaiter := NIL;
    t.waitingOn := NIL;
    WITH r = pthread_cond_signal(t.cond) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock(t.mutex) DO <*ASSERT r=0*> END;
  END DequeueHead;

PROCEDURE Signal (c: Condition) =
  BEGIN
    IF c.mutex = NIL THEN InitMutex(c) END;
    WITH r = pthread_mutex_lock(c.mutex) DO <*ASSERT r=0*> END;
    IF c.waiters # NIL THEN DequeueHead(c) END;
    WITH r = pthread_mutex_unlock(c.mutex) DO <*ASSERT r=0*> END;
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  BEGIN
    IF c.mutex = NIL THEN InitMutex(c) END;
    WITH r = pthread_mutex_lock(c.mutex) DO <*ASSERT r=0*> END;
    WHILE c.waiters # NIL DO DequeueHead(c) END;
    WITH r = pthread_mutex_unlock(c.mutex) DO <*ASSERT r=0*> END;
  END Broadcast;

PROCEDURE Alert (t: T) =
  BEGIN
    WITH r = pthread_mutex_lock(t.mutex) DO <*ASSERT r=0*> END;
    t.alerted := TRUE;
    WITH r = pthread_cond_signal(t.cond) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock(t.mutex) DO <*ASSERT r=0*> END;
  END Alert;

PROCEDURE XTestAlert (self: T): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    result := self.alerted;
    self.alerted := FALSE;
    WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;
    RETURN result;
  END XTestAlert;

PROCEDURE TestAlert (): BOOLEAN =
  VAR self := Self();
  BEGIN
    RETURN XTestAlert(self);
  END TestAlert;

(*------------------------------------------------------------------ Self ---*)

VAR (* LL = slotMu *)
  n_slotted := 0;
  next_slot := 1;
  slots: REF ARRAY OF T;		 (* NOTE: we don't use slots[0] *)

PROCEDURE InitActivations (): Activation =
  VAR me := NEW(Activation);
  BEGIN
    <* ASSERT me.frame = NIL *>
    <* ASSERT me.next = NIL *>
    <* ASSERT me.prev = NIL *>
    <* ASSERT me.stackbase = NIL *>
    <* ASSERT me.sp = NIL *>
    <* ASSERT me.state = ActState.Started *>
    me.handle := Upthread.self();
    me.next := me;
    me.prev := me;
    WITH r = pthread_key_create_activations() DO <*ASSERT r=0*> END;
    WITH r = pthread_setspecific_activations(me) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_lock_active() DO <*ASSERT r=0*> END;
      <* ASSERT next_slot = 1 *> (* no threads created yet *)
      <* ASSERT slots = NIL *> (* no threads created yet *)
      <* ASSERT n_slotted = 0 *> (* no threads created yet *)
      <* ASSERT allThreads = NIL *> (* no threads created yet *)
      allThreads := me;
    WITH r = pthread_mutex_unlock_active() DO <*ASSERT r=0*> END;
    RETURN me;
  END InitActivations;

PROCEDURE SetActivation (act: Activation) =
  (* LL = 0 *)
  VAR v: ADDRESS := act;
  BEGIN
    WITH r = pthread_setspecific_activations(v) DO <*ASSERT r=0*> END;
  END SetActivation;

PROCEDURE Self (): T =
  (* If not the initial thread and not created by Fork, returns NIL *)
  VAR
    me: Activation := pthread_getspecific_activations();
    t: T;
  BEGIN
    IF me = NIL THEN Die(ThisLine(), "Thread primitive called from non-Modula-3 thread") END;
    WITH r = pthread_mutex_lock_slots() DO <*ASSERT r=0*> END;
      t := slots[me.slot];
    WITH r = pthread_mutex_unlock_slots() DO <*ASSERT r=0*> END;
    IF (t.act # me) THEN Die(ThisLine(), "thread with bad slot!") END;
    RETURN t;
  END Self;

PROCEDURE AssignSlot (t: T) =
  (* LL = 0, cause we allocate stuff with NEW! *)
  VAR n: CARDINAL;  new_slots: REF ARRAY OF T;
  BEGIN
    WITH r = pthread_mutex_lock_slots() DO <*ASSERT r=0*> END;

      (* make sure we have room to register this guy *)
      IF (slots = NIL) THEN
        WITH r = pthread_mutex_unlock_slots() DO <*ASSERT r=0*> END;
          slots := NEW (REF ARRAY OF T, 20);
        WITH r = pthread_mutex_lock_slots() DO <*ASSERT r=0*> END;
      END;
      IF (n_slotted >= LAST (slots^)) THEN
        n := NUMBER (slots^);
        WITH r = pthread_mutex_unlock_slots() DO <*ASSERT r=0*> END;
          new_slots := NEW (REF ARRAY OF T, n+n);
        WITH r = pthread_mutex_lock_slots() DO <*ASSERT r=0*> END;
        IF (n = NUMBER (slots^)) THEN
          (* we won any races that may have occurred. *)
          SUBARRAY (new_slots^, 0, n) := slots^;
          slots := new_slots;
        ELSIF (n_slotted < LAST (slots^)) THEN
          (* we lost a race while allocating a new slot table,
             and the new table has room for us. *)
        ELSE
          (* ouch, the new table is full too!   Bail out and retry *)
          WITH r = pthread_mutex_unlock_slots() DO <*ASSERT r=0*> END;
          AssignSlot (t);
          RETURN;
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

    WITH r = pthread_mutex_unlock_slots() DO <*ASSERT r=0*> END;
  END AssignSlot;

PROCEDURE FreeSlot (t: T) =
  (* LL = 0 *)
  BEGIN
    WITH r = pthread_mutex_lock_slots() DO <*ASSERT r=0*> END;

      DEC (n_slotted);
      WITH z = slots [t.act.slot] DO
        IF (z # t) THEN Die (ThisLine(), "unslotted thread!"); END;
        z := NIL;
      END;
      t.act.slot := 0;

    WITH r = pthread_mutex_unlock_slots() DO <*ASSERT r=0*> END;
  END FreeSlot;

PROCEDURE DumpThread (t: T) =
  BEGIN
    RTIO.PutText("Thread: "); RTIO.PutAddr(LOOPHOLE(t, ADDRESS)); RTIO.PutChar('\n');
    RTIO.PutText(" mutex:       "); RTIO.PutAddr(LOOPHOLE(t.mutex, ADDRESS));          RTIO.PutChar('\n');
    RTIO.PutText("  cond:       "); RTIO.PutAddr(LOOPHOLE(t.cond, ADDRESS));          RTIO.PutChar('\n');
    RTIO.PutText("  act:        "); RTIO.PutAddr(LOOPHOLE(t.act, ADDRESS));           RTIO.PutChar('\n');
    RTIO.PutText("  closure:    "); RTIO.PutAddr(LOOPHOLE(t.closure, ADDRESS));       RTIO.PutChar('\n');
    RTIO.PutText("  result:     "); RTIO.PutAddr(LOOPHOLE(t.result, ADDRESS));        RTIO.PutChar('\n');
    RTIO.PutText("  waitingOn:  "); RTIO.PutAddr(LOOPHOLE(t.waitingOn, ADDRESS));     RTIO.PutChar('\n');
    RTIO.PutText("  nextWaiter: "); RTIO.PutAddr(LOOPHOLE(t.nextWaiter, ADDRESS));    RTIO.PutChar('\n');
    RTIO.PutText("  alerted:    "); RTIO.PutInt(ORD(t.alerted));   RTIO.PutChar('\n');
    RTIO.PutText("  joined:     "); RTIO.PutInt(ORD(t.joined)); RTIO.PutChar('\n');
    RTIO.PutText("  join:       "); RTIO.PutAddr(LOOPHOLE(t.join, ADDRESS)); RTIO.PutChar('\n');
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

PROCEDURE CleanThread (r: REFANY) =
  VAR t := NARROW(r, T);
  BEGIN
    pthread_mutex_delete(t.mutex);
    pthread_cond_delete(t.cond);
    t.mutex := NIL;
    t.cond := NIL;
  END CleanThread;

PROCEDURE CreateT (act: Activation): T =
  (* LL = 0, because allocating a traced reference may cause
     the allocator to start a collection which will call "SuspendOthers"
     which will try to acquire "activeMu". *)
  VAR
    t := NEW(T, act := act);
    mutex := pthread_mutex_new();
    cond := pthread_cond_new();
  BEGIN
    IF (mutex = NIL) OR (cond = NIL) THEN
      pthread_mutex_delete(mutex);
      pthread_cond_delete(cond);
      RTE.Raise(RTE.T.OutOfMemory);
    END;
    t.mutex := mutex;
    t.cond := cond;
    RTHeapRep.RegisterFinalCleanup (t, CleanThread);
    t.join := NEW(Condition);
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
    me: Activation := param;
  BEGIN
    SetActivation (me);
    (* We need to establish this binding before this thread touches any
       traced references.  Otherwise, it may trigger a heap page fault,
       which would call SuspendOthers, which requires an Activation. *)

    me.stackbase := ADR(xx);          (* enable GC scanning of this stack *)
    RunThread(me);
    me.stackbase := NIL;              (* disable GC scanning of my stack *)

    <*ASSERT allThreads # me*>
    DISPOSE (me);
    RETURN NIL;
  END ThreadBase;

PROCEDURE RunThread (me: Activation) =
  VAR self: T;
  BEGIN
    IF perfOn THEN PerfChanged(State.alive) END;

    WITH r = pthread_mutex_lock_slots() DO <*ASSERT r=0*> END;
      self := slots [me.slot];
    WITH r = pthread_mutex_unlock_slots() DO <*ASSERT r=0*> END;

    (* Run the user-level code. *)
    IF perfOn THEN PerfRunning() END;
    self.result := self.closure.apply();
    IF perfOn THEN PerfChanged(State.dying) END;

    (* Join *)
    WITH r = pthread_mutex_lock(self.mutex) DO <*ASSERT r=0*> END;
    Broadcast(self.join);
    self.join := NIL;
    WITH r = pthread_mutex_unlock(self.mutex) DO <*ASSERT r=0*> END;

    IF perfOn THEN PerfChanged(State.dead) END;

    (* we're dying *)
    RTHeapRep.FlushThreadState(me.heapState);

    IF perfOn THEN PerfDeleted() END;
    FreeSlot(self);  (* note: needs self.act ! *)
    (* Since we're no longer slotted, we cannot touch traced refs. *)

    (* remove ourself from the list of active threads *)
    WITH r = pthread_mutex_lock_active() DO <*ASSERT r=0*> END;
      <*ASSERT allThreads # me*>
      me.next.prev := me.prev;
      me.prev.next := me.next;
      me.next := NIL;
      me.prev := NIL;
      WITH r = Upthread.detach(me.handle) DO <*ASSERT r=0*> END;
    WITH r = pthread_mutex_unlock_active() DO <*ASSERT r=0*> END;
  END RunThread;

PROCEDURE Fork (closure: Closure): T =
  VAR
    act := NEW(Activation);
    t := CreateT(act);
    size := defaultStackSize;
  BEGIN
    t.closure := closure;
    (* determine the initial size of the stack for this thread *)
    TYPECASE closure OF
    | SizedClosure (scl) => size := scl.stackSize;
    ELSE (*skip*)
    END;

    WITH r = pthread_mutex_lock_active() DO <*ASSERT r=0*> END;
      act.next := allThreads;
      act.prev := allThreads.prev;
      act.size := size;
      allThreads.prev.next := act;
      allThreads.prev := act;
      WITH r = thread_create(act.handle, size * ADRSIZE(Word.T), ThreadBase, act) DO
        IF r # 0 THEN DieI(ThisLine(), r) END;
      END;
    WITH r = pthread_mutex_unlock_active() DO <*ASSERT r=0*> END;
    RETURN t;
  END Fork;

PROCEDURE XJoin (self, t: T; alertable: BOOLEAN): REFANY RAISES {Alerted} =
  BEGIN
    LOCK t DO
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice") END;
      TRY
        t.joined := TRUE;
        WHILE t.join # NIL DO XWait(self, t, t.join, alertable) END;
      FINALLY
        IF t.join # NIL THEN t.joined := FALSE END;
      END;
    END;
    RETURN t.result;
  END XJoin;

PROCEDURE Join (t: T): REFANY =
  <*FATAL Alerted*>
  VAR self := Self();
  BEGIN
    RETURN XJoin(self, t, alertable := FALSE);
  END Join;

PROCEDURE AlertJoin (t: T): REFANY RAISES {Alerted} =
  VAR self := Self();
  BEGIN
    RETURN XJoin(self, t, alertable := TRUE);
  END AlertJoin;

(*---------------------------------------------------- Scheduling support ---*)

PROCEDURE CommonSleep() =
  VAR wait, remaining: Utime.struct_timespec;
  BEGIN
    wait.tv_sec := 0;
    wait.tv_nsec := WAIT_UNIT;
    WHILE Nanosleep(wait, remaining) # 0 DO
      wait := remaining;
    END;
  END CommonSleep;

PROCEDURE ToNTime (n: LONGREAL; VAR ts: Utime.struct_timespec) =
  BEGIN
    ts.tv_sec := TRUNC(n);
    ts.tv_nsec := ROUND((n - FLOAT(ts.tv_sec, LONGREAL)) * 1.0D9);
  END ToNTime;

PROCEDURE XPause (self: T; n: LONGREAL; alertable: BOOLEAN) RAISES {Alerted} =
  VAR until: Utime.struct_timespec;
  BEGIN
    IF n <= 0.0d0 THEN RETURN END;
    ToNTime(Time.Now() + n, until);
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
  VAR self := Self();
  BEGIN
    XPause(self, n, alertable := FALSE);
  END Pause;

PROCEDURE AlertPause (n: LONGREAL) RAISES {Alerted} =
  VAR self := Self();
  BEGIN
    XPause(self, n, alertable := TRUE);
  END AlertPause;

PROCEDURE Yield () =
  BEGIN
    WITH r = Usched.yield() DO
      IF r # 0 THEN DieI(ThisLine(), Cerrno.GetErrno()) END;
    END;
  END Yield;

CONST FDSetSize = BITSIZE(INTEGER);

TYPE
  FDSet = SET OF [0 .. FDSetSize-1];
  FDS = REF ARRAY OF FDSet;

PROCEDURE IOWait (fd: CARDINAL; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult =
  <*FATAL Alerted*>
  VAR self := Self();
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
  VAR self := Self();
  BEGIN
    TRY
      IF perfOn THEN PerfChanged(State.blocking) END;
      RETURN XIOWait(self, fd, read, timeoutInterval, alertable := TRUE);
    FINALLY
      IF perfOn THEN PerfRunning() END;
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

VAR suspended: BOOLEAN := FALSE;	 (* LL=activeMu *)

PROCEDURE SuspendOthers () =
  (* LL=0. Always bracketed with ResumeOthers which releases "activeMu" *)
  BEGIN
    WITH r = pthread_mutex_lock_active() DO <*ASSERT r=0*> END;
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
    WITH r = pthread_mutex_unlock_active() DO <*ASSERT r=0*> END;
  END ResumeOthers;

PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS)) =
  (* LL=activeMu.  Only called within {SuspendOthers, ResumeOthers} *)
  VAR
    me: Activation := pthread_getspecific_activations();
    act: Activation;
  BEGIN
    ProcessMe(me, p);
    act := me.next;
    WHILE act # me DO
      ProcessOther(act, p);
      act := act.next;
    END;
  END ProcessStacks;

PROCEDURE ProcessEachStack (p: PROCEDURE (start, stop: ADDRESS)) =
  (* LL=0 *)
  VAR
    me: Activation := pthread_getspecific_activations();
    act: Activation;
    acks: int;
  BEGIN
    WITH r = pthread_mutex_lock_active() DO <*ASSERT r=0*> END;

    ProcessMe(me, p);

    act := me.next;
    WHILE act # me DO
      (* stop *)
      LOOP
        IF SIG_SUSPEND = 0 THEN
          IF StopThread(act) THEN EXIT END;
        ELSE
          SignalThread(act, ActState.Stopping);
          WITH r = sem_getvalue(acks) DO <*ASSERT r=0*> END;
          IF acks > 0 THEN
            WHILE sem_wait() # 0 DO
              WITH r = Cerrno.GetErrno() DO
                IF r # Uerror.EINTR THEN DieI(ThisLine(), r) END;
              END;
            END;
            EXIT;
          END;
          CommonSleep();
        END;
      END;
      (* process *)
      ProcessOther(act, p);
      (* start *)
      LOOP
        IF SIG_SUSPEND = 0 THEN
          IF StartThread(act) THEN EXIT END;
        ELSE
          SignalThread(act, ActState.Starting);
          WITH r = sem_getvalue(acks) DO <*ASSERT r=0*> END;
          IF acks > 0 THEN
            WHILE sem_wait() # 0 DO
              WITH r = Cerrno.GetErrno() DO
                IF r # Uerror.EINTR THEN DieI(ThisLine(), r) END;
              END;
            END;
            EXIT;
          END;
        END;
        CommonSleep();
      END;
      act := act.next;
    END;

    WITH r = pthread_mutex_unlock_active() DO <*ASSERT r=0*> END;
  END ProcessEachStack;

PROCEDURE ProcessMe (me: Activation;  p: PROCEDURE (start, stop: ADDRESS)) =
  (* LL=activeMu *)
  VAR
    sp: ADDRESS;
    xx: INTEGER;
  BEGIN
    <*ASSERT me.state # ActState.Stopped*>
    IF DEBUG THEN
      RTIO.PutText("Processing act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;
    RTHeapRep.FlushThreadState(me.heapState);
    (* process registers explicitly *)
    sp := ProcessRegisters(p);
    IF sp = NIL THEN sp := ADR(xx) END;
    (* or in my stack *)
    IF stack_grows_down
      THEN p(sp, me.stackbase);
      ELSE p(me.stackbase, sp);
    END;
  END ProcessMe;

PROCEDURE ProcessOther (act: Activation;  p: PROCEDURE (start, stop: ADDRESS)) =
  (* LL=activeMu *)
  VAR sp: ADDRESS;
  BEGIN
    <*ASSERT act.state = ActState.Stopped*>
    IF DEBUG THEN
      RTIO.PutText("Processing act="); RTIO.PutAddr(act); RTIO.PutText("\n"); RTIO.Flush();
    END;
    IF act.stackbase = NIL THEN RETURN END;
    RTHeapRep.FlushThreadState(act.heapState);
    (* process registers explicitly *)
    sp := ProcessState(act.handle, act.sp, p);
    (* or in my stack *)
    IF stack_grows_down
      THEN p(sp, act.stackbase);
      ELSE p(act.stackbase, sp);
    END;
  END ProcessOther;

(* Signal based suspend/resume *)

PROCEDURE SignalThread(act: Activation; state: ActState) =
  BEGIN
    <*ASSERT SIG_SUSPEND # 0*>
    SetState(act, state);
    LOOP
      WITH z = Upthread.kill(act.handle, SIG_SUSPEND) DO
        IF z = 0 THEN EXIT END;
        IF z # Uerror.EAGAIN THEN DieI(ThisLine(), z) END;
        (* try it again... *)
      END;
    END;
  END SignalThread;

PROCEDURE StopThread (act: Activation): BOOLEAN =
  BEGIN
    <*ASSERT act.state # ActState.Stopped*>
    <*ASSERT SIG_SUSPEND = 0*>
    SetState(act, ActState.Stopping);
    IF NOT SuspendThread(act.handle) THEN RETURN FALSE END;
    IF act.heapState.inCritical # 0 THEN
      IF NOT RestartThread(act.handle) THEN <*ASSERT FALSE*> END;
      RETURN FALSE;
    END;
    act.state := ActState.Stopped;
    RETURN TRUE;
  END StopThread;

PROCEDURE StartThread (act: Activation): BOOLEAN =
  BEGIN
    <*ASSERT act.state = ActState.Stopped*>
    <*ASSERT SIG_SUSPEND = 0*>
    SetState(act, ActState.Starting);
    IF NOT RestartThread(act.handle) THEN RETURN FALSE END;
    act.state := ActState.Started;
    RETURN TRUE;
  END StartThread;

PROCEDURE StopWorld () =
  (* LL=activeMu *)
  VAR
    me: Activation := pthread_getspecific_activations();
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
        IF act.state # ActState.Stopped THEN
          IF SIG_SUSPEND = 0 THEN
            retry := NOT StopThread(act);
          ELSE
            SignalThread(act, ActState.Stopping);
            INC(nLive);
          END;
        END;
        act := act.next;
      END;
      IF NOT retry THEN EXIT END;
      CommonSleep();
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
          IF act.state # ActState.Stopped THEN
            SignalThread(act, ActState.Stopping);
            INC(newlySent);
          END;
          act := act.next;
        END;
        IF newlySent < nLive - acks THEN
          (* how did we manage to lose some? *)
          nLive := acks + newlySent;
        END;
        wait_nsecs := RETRY_INTERVAL;
      ELSE
        CommonSleep();
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
    WITH r = sem_getvalue(acks) DO <*ASSERT r=0*> END;
    <*ASSERT acks = 0*>

    IF DEBUG THEN
      RTIO.PutText("Stopped from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;
  END StopWorld;

PROCEDURE StartWorld () =
  (* LL=activeMu *)
  VAR
    me: Activation := pthread_getspecific_activations();
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
        IF act.state # ActState.Started THEN
          IF SIG_SUSPEND = 0 THEN
            retry := NOT StartThread(act);
          ELSE
            SignalThread(act, ActState.Starting);
            INC(nDead);
          END;
        END;
        act := act.next;
      END;
      IF NOT retry THEN EXIT END;
      CommonSleep();
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
          IF act.state # ActState.Started THEN
            SignalThread(act, ActState.Starting);
            INC(newlySent);
          END;
          act := act.next;
        END;
        IF newlySent < nDead - acks THEN
          (* how did we manage to lose some? *)
          nDead := acks + newlySent;
        END;
        wait_nsecs := RETRY_INTERVAL;
      ELSE
        CommonSleep();
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
    WITH r = sem_getvalue(acks) DO <*ASSERT r=0*> END;
    <*ASSERT acks = 0*>

    IF DEBUG THEN
      RTIO.PutText("Started from act="); RTIO.PutAddr(me); RTIO.PutText("\n"); RTIO.Flush();
    END;
  END StartWorld;

PROCEDURE SignalHandler (sig: int) =
  VAR
    me: Activation := pthread_getspecific_activations();
    errno := Cerrno.GetErrno();
    xx: INTEGER;
  BEGIN
    <*ASSERT sig = SIG_SUSPEND*>
    IF me # NIL
      AND me.state = ActState.Stopping
      AND me.heapState.inCritical = 0 THEN
      me.sp := ProcessRegisters(NIL);
      IF me.sp = NIL THEN me.sp := ADR(xx) END;
      me.state := ActState.Stopped;
      WITH r = sem_post() DO <*ASSERT r=0*> END;
      REPEAT EVAL sigsuspend() UNTIL me.state = ActState.Starting;
      me.sp := NIL;
      me.state := ActState.Started;
      WITH r = sem_post() DO <*ASSERT r=0*> END;
    END;
    Cerrno.SetErrno(errno);
  END SignalHandler;

(*----------------------------------------------------------- misc. stuff ---*)

PROCEDURE MyId (): Id RAISES {} =
  VAR me: Activation := pthread_getspecific_activations();
  BEGIN
    IF me = NIL
      THEN RETURN 0
      ELSE RETURN me.slot;
    END;
  END MyId;

PROCEDURE MyFPState (): UNTRACED REF FloatMode.ThreadState =
  VAR me: Activation := pthread_getspecific_activations();
  BEGIN
    RETURN ADR(me.heapState);
  END MyFPState;

PROCEDURE MyHeapState (): UNTRACED REF RTHeapRep.ThreadState =
  VAR me: Activation := pthread_getspecific_activations();
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
  perfOn: BOOLEAN := FALSE;		 (* LL = perfMu *)

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
    WITH r = pthread_mutex_lock_perf() DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = pthread_mutex_unlock_perf() DO <*ASSERT r=0*> END;
  END PerfChanged;

PROCEDURE PerfDeleted () =
  VAR
    e := ThreadEvent.T {kind := TE.Deleted, id := MyId()};
  BEGIN
    WITH r = pthread_mutex_lock_perf() DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = pthread_mutex_unlock_perf() DO <*ASSERT r=0*> END;
  END PerfDeleted;

PROCEDURE PerfRunning () =
  VAR
    e := ThreadEvent.T {kind := TE.Running, id := MyId()};
  BEGIN
    WITH r = pthread_mutex_lock_perf() DO <*ASSERT r=0*> END;
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    WITH r = pthread_mutex_unlock_perf() DO <*ASSERT r=0*> END;
  END PerfRunning;

(*-------------------------------------------------------- Initialization ---*)

PROCEDURE Init ()=
  VAR
    xx: INTEGER;
    self: T;
    me := InitActivations();
  BEGIN
    IF SIG_SUSPEND # 0 THEN SetupHandlers() END;

    (* cm, activeMu, slotMu: initialized statically *)
    self := CreateT(me);

    stack_grows_down := ADR(xx) > XX();

    WITH r = pthread_mutex_lock_active() DO <*ASSERT r=0*> END;
      me.stackbase := ADR(xx);
    WITH r = pthread_mutex_unlock_active() DO <*ASSERT r=0*> END;
    PerfStart();
    IF perfOn THEN PerfRunning() END;
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
  holder: pthread_t;
  inCritical := 0;

PROCEDURE LockHeap () =
  VAR self := Upthread.self();
  BEGIN
    IF Upthread.equal(holder, self) = 0 THEN
      WITH r = pthread_mutex_lock_heap() DO <*ASSERT r=0*> END;
      holder := self;
    END;
    INC(inCritical);
  END LockHeap;

PROCEDURE UnlockHeap () =
  BEGIN
    <*ASSERT Upthread.equal(holder, Upthread.self()) # 0*>
    DEC(inCritical);
    IF inCritical = 0 THEN
      holder := NIL;
      WITH r = pthread_mutex_unlock_heap() DO <*ASSERT r=0*> END;
    END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  VAR self := Upthread.self();
  BEGIN
    <*ASSERT Upthread.equal(holder, self) # 0*>
    DEC(inCritical);
    <*ASSERT inCritical = 0*>
    WITH r = pthread_cond_wait_heap() DO <*ASSERT r=0*> END;
    holder := self;
    <*ASSERT inCritical = 0*>
    INC(inCritical);
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  BEGIN
    WITH r = pthread_cond_broadcast_heap() DO <*ASSERT r=0*> END;
  END BroadcastHeap;

(*--------------------------------------------- exception handling support --*)

PROCEDURE GetCurrentHandlers (): ADDRESS =
  VAR me: Activation := pthread_getspecific_activations();
  BEGIN
    RETURN me.frame;
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers (h: ADDRESS) =
  VAR me: Activation := pthread_getspecific_activations();
  BEGIN
    me.frame := h;
  END SetCurrentHandlers;

(*RTHooks.PushEFrame*)
PROCEDURE PushEFrame (frame: ADDRESS) =
  TYPE Frame = UNTRACED REF RECORD next: ADDRESS END;
  VAR
    me: Activation := pthread_getspecific_activations();
    f: Frame := frame;
  BEGIN
    f.next := me.frame;
    me.frame := f;
  END PushEFrame;

(*RTHooks.PopEFrame*)
PROCEDURE PopEFrame (frame: ADDRESS) =
  VAR me: Activation := pthread_getspecific_activations();
  BEGIN
    me.frame := frame;
  END PopEFrame;

VAR DEBUG := RTParams.IsPresent("debugthreads");

BEGIN
END ThreadPThread.
