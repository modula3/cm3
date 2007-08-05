(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

UNSAFE MODULE ThreadPThread
EXPORTS Thread, ThreadF, Scheduler, SchedulerPosix, RTOS, RTHooks;

IMPORT Cerrno, FloatMode, MutexRep,
       RTCollectorSRC, RTError,  RTHeapRep, RTIO, RTMachine, RTParams,
       RTPerfTool, RTProcess, ThreadEvent, Time,
       Unix, Utime, Word, Upthread, Usched, Usem, Usignal,
       Uucontext, Uerror, WeakRef;
FROM Upthread
IMPORT pthread_t, pthread_mutex_t, pthread_cond_t, pthread_key_t,
       pthread_attr_t, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER;
FROM Compiler IMPORT ThisFile, ThisLine;
IMPORT Ctypes, Utypes;

(*----------------------------------------------------- types and globals ---*)

VAR
  stack_grows_down: BOOLEAN;

  nextId: CARDINAL := 1;

  threadMu: Mutex;			 (* global lock for fields of T *)

  activeMu := PTHREAD_MUTEX_INITIALIZER; (* global lock for list of active threads *)
  slotMu   := PTHREAD_MUTEX_INITIALIZER; (* global lock for thread slot table *)
  initMu   := PTHREAD_MUTEX_INITIALIZER; (* global lock for initializers *)

REVEAL
  Mutex = MutexRep.Public BRANDED "Mutex Pthread-1.0" OBJECT
    mutex: UNTRACED REF pthread_mutex_t := NIL;
  OVERRIDES
    acquire := Acquire;
    release := Release;
  END;

  Condition = BRANDED "Thread.Condition Pthread-1.0" OBJECT
    cond: UNTRACED REF pthread_cond_t := NIL;
  END;

  T = BRANDED "Thread.T Pthread-1.6" OBJECT
    (* live thread data *)
    act: Activation := NIL;		 (* LL = threadMu *)

    (* our work and its result *)
    closure: Closure := NIL;		 (* LL = threadMu *)
    result: REFANY := NIL;		 (* LL = threadMu *)

    (* wait here to join *)
    cond: Condition := NIL;		 (* LL = threadMu *)

    (* the alert flag *)
    alerted : BOOLEAN := FALSE;		 (* LL = threadMu *)

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
    running := TRUE;
    (* index into global array of active, slotted threads *)
    slot: INTEGER;			 (* LL = slotMu *)

    (* state that is available to the floating point routines *)
    floatState : FloatMode.ThreadState;

    (* allocation pool *)
    newPool := RTHeapRep.NewPool;
  END;

(*----------------------------------------------------------------- Mutex ---*)

PROCEDURE CleanMutex (<*UNUSED*> READONLY wr: WeakRef.T; r: REFANY) =
  VAR m := NARROW(r, Mutex);
  BEGIN
    WITH r = Upthread.mutex_destroy (m.mutex^) DO <*ASSERT r=0*> END;
    DISPOSE(m.mutex);
  END CleanMutex;

PROCEDURE InitMutex (m: Mutex) =
  BEGIN
    TRY
      WITH r = Upthread.mutex_lock(initMu) DO <*ASSERT r=0*> END;
      IF m.mutex # NIL THEN RETURN END;
      m.mutex := NEW(UNTRACED REF pthread_mutex_t);
      WITH r = Upthread.mutex_init(m.mutex^, NIL) DO <*ASSERT r=0*> END;
    FINALLY
      WITH r = Upthread.mutex_unlock(initMu) DO <*ASSERT r=0*> END;
    END;
    EVAL WeakRef.FromRef (m, CleanMutex);
  END InitMutex;

PROCEDURE Acquire (m: Mutex) =
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "Acquire called from a non-Modula-3 thread");
    END;
    IF m.mutex = NIL THEN InitMutex(m) END;
    IF perfOn THEN PerfChanged(self.id, State.locking) END;
    WITH r = Upthread.mutex_lock(m.mutex^) DO
      IF r # 0 THEN
        RTError.MsgI(ThisFile(), ThisLine(),
                     "Thread client error: Acquire failed with error: ", r);
      END;
    END;
    IF perfOn THEN PerfRunning(self.id) END;
  END Acquire;

PROCEDURE Release (m: Mutex) =
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "Release called from a non-Modula-3 thread");
    END;
    WITH r = Upthread.mutex_unlock(m.mutex^) DO
      IF r # 0 THEN
        RTError.MsgI(ThisFile(), ThisLine(),
                     "Thread client error: Release failed with error: ", r);
      END;
    END;
  END Release;

(*---------------------------------------- Condition variables and Alerts ---*)

PROCEDURE CleanCondition (<*UNUSED*> READONLY wr: WeakRef.T; r: REFANY) =
  VAR c := NARROW(r, Condition);
  BEGIN
    WITH r = Upthread.cond_destroy (c.cond^) DO <*ASSERT r=0*> END;
    DISPOSE(c.cond);
  END CleanCondition;

PROCEDURE InitCondition (c: Condition) =
  BEGIN
    TRY
      WITH r = Upthread.mutex_lock(initMu) DO <*ASSERT r=0*> END;
      IF c.cond # NIL THEN RETURN END;
      c.cond := NEW(UNTRACED REF pthread_cond_t);
      WITH r = Upthread.cond_init(c.cond^, NIL) DO <*ASSERT r=0*> END;
    FINALLY
      WITH r = Upthread.mutex_unlock(initMu) DO <*ASSERT r=0*> END;
    END;
    EVAL WeakRef.FromRef(c, CleanCondition);
  END InitCondition;

PROCEDURE XWait (self: T; m: Mutex; c: Condition; alertable: BOOLEAN)
  RAISES {Alerted} =
  VAR until: Utime.struct_timespec;
  BEGIN
    IF c.cond = NIL THEN InitCondition(c) END;
    IF m.mutex = NIL THEN InitMutex(m) END;
    IF alertable THEN
      XTestAlert(self);
      LOOP
        ToNTime(Time.Now() + 1.0d0, until);
        WITH r = Upthread.cond_timedwait(c.cond^, m.mutex^, until) DO
          XTestAlert(self);
          IF r = 0 THEN EXIT END;
          <*ASSERT r=Uerror.ETIMEDOUT*>
        END;
      END;
    ELSE
      WITH r = Upthread.cond_wait(c.cond^, m.mutex^) DO <*ASSERT r=0*> END;
    END;
  END XWait;

PROCEDURE XTestAlert (self: T) RAISES {Alerted} =
  BEGIN
    LOCK threadMu DO
      IF self.alerted THEN
        self.alerted := FALSE;
        RAISE Alerted;
      END;
    END;
  END XTestAlert;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "AlertWait called from non-Modula-3 thread");
    END;
    TRY
      IF perfOn THEN PerfChanged(self.id, State.waiting) END;
      XWait(self, m, c, alertable := TRUE);
    FINALLY
      IF perfOn THEN PerfRunning(self.id) END;
    END;
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  <*FATAL Alerted*>
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "Wait called from non-Modula-3 thread");
    END;
    TRY
      IF perfOn THEN PerfChanged(self.id, State.waiting) END;
      XWait(self, m, c, alertable := FALSE);
    FINALLY
      IF perfOn THEN PerfRunning(self.id) END;
    END;
  END Wait;

PROCEDURE Signal (c: Condition) =
  BEGIN
    IF c.cond = NIL THEN InitCondition(c) END;
    WITH r = Upthread.cond_signal(c.cond^) DO
      IF r # 0 THEN
        RTError.MsgI(ThisFile(), ThisLine(),
                     "Thread client error: Signal failed with error: ", r);
      END;
    END;
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  BEGIN
    IF c.cond = NIL THEN InitCondition(c) END;
    WITH r = Upthread.cond_broadcast(c.cond^) DO
      IF r # 0 THEN
        RTError.MsgI(ThisFile(), ThisLine(),
                     "Thread client error: Broadcast failed with error: ", r);
      END;
    END;
  END Broadcast;

PROCEDURE Alert (t: T) =
  BEGIN
    LOCK threadMu DO
      t.alerted := TRUE;
    END;
  END Alert;

PROCEDURE TestAlert (): BOOLEAN =
  VAR self := Self();
  BEGIN
    IF self = NIL THEN
      Die(ThisLine(), "TestAlert called from non-Modula-3 thread");
    END;
    LOCK threadMu DO
      IF self.alerted THEN
        self.alerted := FALSE;
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
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

PROCEDURE DumpThread (t: T) =
  BEGIN
    RTIO.PutText("Thread: "); RTIO.PutAddr(LOOPHOLE(t, ADDRESS)); RTIO.PutChar('\n');
    RTIO.PutText("  act:        "); RTIO.PutAddr(LOOPHOLE(t.act, ADDRESS));           RTIO.PutChar('\n');
    RTIO.PutText("  closure:    "); RTIO.PutAddr(LOOPHOLE(t.closure, ADDRESS));       RTIO.PutChar('\n');
    RTIO.PutText("  result:     "); RTIO.PutAddr(LOOPHOLE(t.result, ADDRESS));        RTIO.PutChar('\n');
    RTIO.PutText("  cond:       "); RTIO.PutAddr(LOOPHOLE(t.cond, ADDRESS));          RTIO.PutChar('\n');
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
    t.cond := NEW(Condition);
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
    RTHeapRep.ClosePool(me.newPool);

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
    t: T;
    act: Activation := NIL;
    attr: pthread_attr_t;
    size := defaultStackSize;
    bytes: Utypes.size_t;
  BEGIN
    (* determine the initial size of the stack for this thread *)
    TYPECASE closure OF
    | SizedClosure (scl) => size := scl.stackSize;
    ELSE (*skip*)
    END;

    t := CreateT(NEW(Activation));
    act := t.act;
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

PROCEDURE XPause (self: T; n: LONGREAL; alertable: BOOLEAN) RAISES {Alerted} =
  VAR amount, remaining: Utime.struct_timespec;
  BEGIN
    IF alertable THEN XTestAlert(self) END;
    IF n <= 0.0d0 THEN RETURN END;
    ToNTime(n, amount);
    LOOP
      WITH r = Utime.nanosleep(amount, remaining) DO
        IF alertable THEN XTestAlert(self) END;
        IF r = 0 THEN EXIT END;
        amount := remaining;
      END;
    END;
  END XPause;

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

    IF alertable THEN XTestAlert(self) END;
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

      IF alertable THEN XTestAlert(self) END;

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
          <*ASSERT NOT act.running*>
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
  suspendAckSem: Usem.sem_t;

CONST SIG_SUSPEND = RTMachine.SIG_SUSPEND;

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
          IF act.running THEN
            IF RTMachine.SuspendThread(act.handle) THEN
              IF act.newPool.busy THEN
                RTMachine.RestartThread(act.handle);
                INC(nLive);
              ELSE
                act.running := FALSE;
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
      WITH r = Usem.init(suspendAckSem, 0, 0) DO <*ASSERT r=0*> END;
      WHILE act # me DO
        IF act.running THEN
          LOOP
            WITH r = Upthread.kill(act.handle, SIG_SUSPEND) DO
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
        <*ASSERT NOT act.running*>
        <*ASSERT NOT act.newPool.busy*>
        act.running := TRUE;
        RTMachine.RestartThread(act.handle);
        act := act.next;
      END;
    ELSE
      (* No native suspend routine so signal thread to suspend *)
      WHILE act # me DO
        <*ASSERT NOT act.running*>
        <*ASSERT NOT act.newPool.busy*>
        act.running := TRUE;
        LOOP
          WITH r = Upthread.kill(act.handle, SIG_SUSPEND) DO
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
    nLive: INTEGER;
    wait_nsecs := 0;
    wait, remaining: Utime.struct_timespec;
    acks: Ctypes.int;
  CONST
    WAIT_UNIT = 1000000;
    RETRY_INTERVAL = 10000000;
  BEGIN
    IF DEBUG THEN RTIO.PutText("stopping threads"); RTIO.Flush(); END;
    nLive := SuspendAll (me);
    IF nLive > 0 THEN
      LOOP
        WITH r = Usem.getvalue(suspendAckSem, acks) DO <*ASSERT r=0*> END;
        IF acks = nLive THEN IF DEBUG THEN RTIO.PutText("!"); END; EXIT END;
        <*ASSERT acks < nLive*>
        IF wait_nsecs > RETRY_INTERVAL THEN
          IF DEBUG THEN RTIO.PutText("."); RTIO.Flush(); END;
          IF SuspendAll(me) = 0 THEN EXIT END;
          wait_nsecs := 0;
        END;
        wait.tv_sec := 0;
        wait.tv_nsec := WAIT_UNIT;
        WHILE Utime.nanosleep(wait, remaining) # 0 DO
          wait := remaining;
        END;
        INC(wait_nsecs, WAIT_UNIT);
      END;
    END;
    IF DEBUG THEN RTIO.PutText("\n"); RTIO.Flush(); END;
  END StopWorld;

PROCEDURE StartWorld (me: Activation) =
  BEGIN
    RestartAll (me);
  END StartWorld;

PROCEDURE SignalHandler (sig: Ctypes.int;
                         <*UNUSED*> sip: Usignal.siginfo_t_star;
                         <*UNUSED*> uap: Uucontext.ucontext_t_star) =
  VAR
    errno := Cerrno.GetErrno();
    xx: INTEGER;
    me := GetActivation();
    m, om: Usignal.sigset_t;
  BEGIN
    <*ASSERT sig = SIG_SUSPEND*>
    IF me = NIL THEN RETURN END;
    <*ASSERT me.running*>
    IF me.newPool.busy THEN RETURN END;
    IF RTMachine.SaveRegsInStack # NIL THEN
      me.sp := RTMachine.SaveRegsInStack();
    ELSE
      me.sp := ADR(xx);
    END;
    me.running := FALSE;
    WITH r = Usem.post(suspendAckSem) DO <*ASSERT r=0*> END;

    WITH r = Usignal.sigemptyset(m) DO <*ASSERT r=0*> END;
    WITH r = Usignal.sigaddset(m, SIG_SUSPEND) DO <*ASSERT r=0*> END;
    WITH r = Upthread.sigmask(Usignal.SIG_BLOCK, m, om) DO <*ASSERT r=0*> END;
    REPEAT
      WITH r = Usignal.sigwait(m, sig) DO <*ASSERT r=0*> END;
      <*ASSERT sig = SIG_SUSPEND*>
    UNTIL me.running;

    Cerrno.SetErrno(errno);
  END SignalHandler;

PROCEDURE SetupHandlers () =
  VAR act, oact: Usignal.struct_sigaction;
  BEGIN
    IF RTMachine.SuspendThread # NIL THEN <*ASSERT SIG_SUSPEND = 0*> END;
    IF SIG_SUSPEND = 0 THEN RETURN END;

    act.sa_flags := Word.Or(Usignal.SA_RESTART, Usignal.SA_SIGINFO);
    WITH r = Usignal.sigfillset(act.sa_mask) DO <*ASSERT r=0*> END;
    act.sa_sigaction := SignalHandler;
    WITH r = Usignal.sigaction(SIG_SUSPEND, act, oact) DO <*ASSERT r=0*> END;
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
    self := CreateT(me);
    self.id := nextId;  INC(nextId);

    threadMu := NEW(Mutex, mutex := NEW(UNTRACED REF pthread_mutex_t));
    WITH r = Upthread.mutex_init(threadMu.mutex^, NIL) DO <*ASSERT r=0*> END;

    heapMu := NEW(Mutex, mutex := NEW(UNTRACED REF pthread_mutex_t));
    WITH r = Upthread.mutex_init(heapMu.mutex^, NIL) DO <*ASSERT r=0*> END;
    heapCond := NEW(Condition, cond := NEW(UNTRACED REF pthread_cond_t));
    WITH r = Upthread.cond_init(heapCond.cond^, NIL) DO <*ASSERT r=0*> END;

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
  lockers: CARDINAL := 0;
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
    IF lock_cnt = 0 THEN
      holder := self;
    ELSIF Upthread.equal(holder, self) = 0 THEN
      REPEAT
        INC(lockers);
        WITH r = Upthread.cond_wait(lockCond, lockMu) DO <*ASSERT r=0*> END;
        DEC(lockers);
      UNTIL lock_cnt = 0;
      holder := self;
    END;
    INC(lock_cnt);
    WITH r = Upthread.mutex_unlock(lockMu) DO <*ASSERT r=0*> END;
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
    WITH r = Upthread.mutex_lock(lockMu) DO <*ASSERT r=0*> END;
      DEC(lock_cnt);
      IF lock_cnt = 0 THEN
        IF lockers # 0 THEN
          WITH r = Upthread.cond_signal(lockCond) DO <*ASSERT r=0*> END;
        END;
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
