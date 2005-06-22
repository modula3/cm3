(*| Copyright (C) 1989, Digital Equipment Corporation        *)
(*| All rights reserved.                                     *)
(*| See the file COPYRIGHT for a full description.           *)
(*|                                                          *)
(*| Last modified on Fri Apr  7 09:06:48 PDT 1995 by kalsow  *)
(*|      modified on Mon Mar 21 17:40:31 PST 1994 by wobber  *)
(*|      modified on Sat Jun 26 17:07:03 PDT 1993 by gnelson     *)
(*|      modified on Fri May 14 16:15:26 PDT 1993 by mjordan *)
(*|      modified on Wed Apr 21 16:35:05 PDT 1993 by mcjones *)
(*|      modified On Mon Apr  5 14:51:30 PDT 1993 by muller  *)
(*|      modified on Mon Feb 22 10:08:49 PST 1993 by jdd     *)

UNSAFE MODULE ThreadPosix
EXPORTS Thread, ThreadF, Scheduler, SchedulerPosix, RTThreadInit, RTOS, RTHooks;

IMPORT Cerrno, Cstring, FloatMode, MutexRep,
       RTError, RTMisc, RTParams, RTPerfTool, RTProcedureSRC,
       RTProcess, RTThread, RTIO, ThreadEvent, Time, TimePosix,
       Unix, Utime, Word;

REVEAL
  (* Remember, the report (p 43-44) says that MUTEX is predeclared and <: ROOT;
     just pretend that we have "TYPE MUTEX <: ROOT" in our interface.
     The sem field is where we store the semaphore that implements the mutual
     exclusion and waitingForMe is the head of the list of threads that are
     waiting for the mutex to be released so that they can acquire it (the list
     is continued in the nextWaitingForMutex field of the threads) *)

  MUTEX = MutexRep.Public BRANDED "Mutex Posix-1.0" OBJECT
    holder       : T := NIL;
    waitingForMe : T := NIL;
  OVERRIDES
    acquire := LockMutex;
    release := UnlockMutex;
  END; 


  (* Threads that wait on a condition are inserted in the waitingForMe list,
     which is continued in the nextWaitingForCondition field of the waiting
     threads. *)

  Condition = BRANDED "Thread.Condition Posix-1.0" OBJECT
                waitingForMe: T := NIL; END;

TYPE SelectRec = RECORD
        fd: CARDINAL := 0;
        read: BOOLEAN := FALSE;
        waitResult: WaitResult := WaitResult.Ready;
        (* fields relevant for new and old implementation *)
        timeout := UTime{0, 0};
        hasTimeout: BOOLEAN := FALSE;
        errno: INTEGER := 0;
        index: CARDINAL := 0;
        set: FDSet := FDSet{};
      END;

(* The debugger, m3gdb, depends on these pieces of the thread implementation:

|      "Thread.T" is a record or object type with fields:
|         "id"                  an integer,
|         "state"               an enumeration (with fixed known values),
|         "next"                a pointer to a "Thread.T"
|         "waitingForCondition" a pointer
|         "waitingForMutex"     a pointer
|         "waitingForTime"      a "time"
|         "context"             a record with a field named "buf" which is
|                                  a jump buffer.

   Eric Muller, 3/16/94
*)
REVEAL
  T = BRANDED "Thread.T Posix-1.6" OBJECT
    state: State;
    id: Id;

    (* our work and its result *)
    closure : Closure;
    result : REFANY := NIL;

    (* the threads are organized in a circular list *)
    previous, next: T;

    (* next thread that waits for:
       CASE state OF
       | waiting  => the same condition;
       | locking  => the same mutex;
       | pausing  => a specified time;
       | blocking => some IO; *)
    nextWaiting: T;

    (* if state = waiting, the condition on which we wait *)
    waitingForCondition: Condition;
    waitingForMutex:     Mutex;

    (* if state = pausing, the time at which we can restart *)
    waitingForTime : UTime;

    (* true if we are waiting during an AlertWait or AlertJoin 
       or AlertPause *)
    alertable: BOOLEAN := FALSE;

    (* true if somebody alerted us and we did not TestAlert *)
    alertPending : BOOLEAN := FALSE;

    (* This condition is signaled then the thread terminates;
       other threads that want to join can just wait for it *)
    endCondition: Condition;

    (* where we carry our work. The first thread runs on the
       original C program stack and its context.stack is NIL *)
    context : Context;

    (* if state = blocking, the descriptors we are waiting on *)
    select : SelectRec := SelectRec{};

    (* state that is available to the floating point routines *)
    floatState : FloatMode.ThreadState;
  END;

TYPE
  IntPtr = UNTRACED REF INTEGER;

(*------------------------------------------------------- Unix time hack! ---*)

TYPE
  UTime = Utime.struct_timeval;
  TimeZone = Utime.struct_timezone;

PROCEDURE UTimeNow (): UTime =
  VAR tv: UTime;  tz: TimeZone;
  BEGIN
    EVAL Utime.gettimeofday (tv, tz);
    RETURN tv;
  END UTimeNow;

PROCEDURE Time_Add (READONLY t1, t2: UTime): UTime =
  VAR res: UTime;
  BEGIN
    res.tv_sec  := t1.tv_sec + t2.tv_sec;
    res.tv_usec := t1.tv_usec + t2.tv_usec;
    IF res.tv_usec > 1000000 THEN
      DEC (res.tv_usec, 1000000);
      INC (res.tv_sec, 1);
    END;
    RETURN res;
  END Time_Add;

PROCEDURE Time_Subtract (READONLY t1, t2: UTime): UTime =
  VAR res: UTime;
  BEGIN
    res.tv_sec := t1.tv_sec - t2.tv_sec;
    res.tv_usec := t1.tv_usec - t2.tv_usec;
    IF res.tv_usec < 0 THEN
      INC (res.tv_usec, 1000000);
      DEC (res.tv_sec, 1);
    END;
    RETURN res;
  END Time_Subtract;

PROCEDURE Time_Compare (READONLY t1, t2: UTime): [-1 .. 1] =
  BEGIN
    IF    t1.tv_sec > t2.tv_sec   THEN RETURN 1;
    ELSIF t1.tv_sec < t2.tv_sec   THEN RETURN -1;
    ELSIF t1.tv_usec = t2.tv_usec THEN RETURN 0;
    ELSIF t1.tv_usec > t2.tv_usec THEN RETURN 1;
    ELSE                               RETURN -1;
    END;
  END Time_Compare;

(*--------------------------------------------------------------- globals ---*)

VAR
  preemption: BOOLEAN;

  (* this is really a constant, but we need to take its address *)
  ZeroTimeout := UTime{0, 0};

VAR
  (* we start the heavy machinery only when we have more than one thread *)
  multipleThreads: BOOLEAN := FALSE;

  topThread: T;       (* the thread in which Main runs *)
  pausedThreads : T;
  selected_interval:= UTime{0, 100 * 1000};

  defaultStackSize := 3000;

  stack_grows_down: BOOLEAN;

VAR
  stats: RECORD
           n_forks := 0;
           n_dead  := 0;
           n_joins := 0;
         END;

EXCEPTION InternalError;
<*FATAL InternalError*>

CONST ForkYieldRatio = 5;

VAR
  nextId: Id := 1;

VAR
  dead_stacks: T := NIL;
  (* dead threads waiting to have their stacks disposed *)

(*------------------------------------------------- user-level procedures ---*)

PROCEDURE GetDefaultStackSize (): CARDINAL =
  BEGIN
    RETURN defaultStackSize;
  END GetDefaultStackSize;

PROCEDURE MinDefaultStackSize (new_min: CARDINAL) =
  BEGIN
    INC (inCritical);
      defaultStackSize := MAX (defaultStackSize, new_min);      
    DEC (inCritical);
  END MinDefaultStackSize;

PROCEDURE IncDefaultStackSize (inc: CARDINAL) =
  BEGIN
    INC (inCritical);
      INC (defaultStackSize, inc);
    DEC (inCritical);
  END IncDefaultStackSize;

PROCEDURE Fork (cl: Closure): T =
  VAR t: T;  stack_size: CARDINAL;
  BEGIN
    (* make sure that thread switching keeps up with thread creation *)
    INC (stats.n_forks);
    IF (stats.n_forks MOD ForkYieldRatio) = 0 THEN Yield () END;

    INC (inCritical);

      IF NOT multipleThreads THEN
        (* this is the first time we have more than one thread; we can start to
           consider switching *)
        multipleThreads := TRUE;
        StartSwitching ();
      END;

      t := NEW (T, closure := cl, id := nextId);
      INC (nextId);

      (* determine the size of the stack for this thread *)
      stack_size := defaultStackSize;
      TYPECASE cl OF
      | SizedClosure (scl) => IF scl.stackSize # 0 THEN
                                stack_size := scl.stackSize;
                              END;
      ELSE (*skip*)
      END;

      (* allocate a condition variable for this thread *)
      t.endCondition := NEW (Condition);

      (* link the thread into the global ring *)
      t.next := self.next;
      t.previous := self;
      self.next.previous := t;
      self.next := t;

      InitContext (t.context, stack_size);
      CanRun (t);

      IF hooks # NIL THEN hooks.fork (t) END;
    DEC (inCritical);
    RETURN t;
  END Fork;

PROCEDURE Join (t: T): REFANY RAISES {} =
  <*FATAL Alerted*>
  BEGIN
    self.alertable := FALSE;
    RETURN XJoin (t);
  END Join;

PROCEDURE AlertJoin (t: T): REFANY RAISES {Alerted} =
  BEGIN
    self.alertable := TRUE;
    RETURN XJoin (t);
  END AlertJoin;

PROCEDURE XJoin (t: T): REFANY RAISES {Alerted} =
  VAR c: Condition;
  BEGIN
    INC (inCritical);
      WHILE (t.state # State.dying) AND (t.state # State.dead) DO
        (*** INLINE Wait (inCritical, t.endCondition) ***)
        c := t.endCondition;
        ICannotRun (State.waiting);
        self.waitingForCondition := c;
        self.nextWaiting := c.waitingForMe;
        c.waitingForMe := self;
        DEC (inCritical);
          InternalYield ();
        INC (inCritical);
      END;
      t.state := State.dead;
      IF perfOn THEN PerfChanged (t.id, State.dead); END;
      INC (stats.n_joins);
    DEC (inCritical);
    RETURN t.result;
  END XJoin;

PROCEDURE Wait (m: Mutex; c: Condition) =
  <*FATAL Alerted*>
  BEGIN
    self.alertable := FALSE;
    XWait (m, c);
  END Wait;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  BEGIN
    self.alertable := TRUE;
    XWait (m, c);
  END AlertWait;

PROCEDURE XWait (m: Mutex; c: Condition) RAISES {Alerted} =
  BEGIN
    TRY
      INC (inCritical);
        EVAL XRelease (m);
        ICannotRun (State.waiting);
        self.waitingForCondition := c;
        self.nextWaiting := c.waitingForMe;
        c.waitingForMe := self;
      DEC (inCritical);
      InternalYield ();
    FINALLY
      LockMutex (m);
    END;
  END XWait;

PROCEDURE Signal (c: Condition) =
  BEGIN
    XSignal (c, 1);
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  BEGIN
    XSignal (c, -1);
  END Broadcast;

PROCEDURE XSignal (c: Condition; limit: INTEGER) =
  VAR t: T;
  BEGIN
    INC (inCritical);
      LOOP
        t := c.waitingForMe;
        IF (t = NIL) THEN EXIT END;
        c.waitingForMe := t.nextWaiting;
        CanRun (t);
        DEC (limit);
        IF limit = 0 THEN EXIT END;
      END;
    DEC (inCritical);
  END XSignal;

PROCEDURE Alert (t: T) =
  BEGIN
    INC (inCritical);
      t.alertPending := TRUE;
    DEC (inCritical);
  END Alert;

PROCEDURE TestAlert (): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    INC (inCritical);
      result := self.alertPending;
      self.alertPending := FALSE;
    DEC (inCritical);
    RETURN result; 
  END TestAlert;

PROCEDURE Yield () =
  <*FATAL Alerted*>
  BEGIN
    self.alertable := FALSE;
    InternalYield ();
  END Yield;

PROCEDURE Self (): T =
  BEGIN
    RETURN self;
  END Self;

(*--------------------------------------------------------------- MUTEXes ---*)
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
  <*FATAL Alerted*>
  BEGIN
    LOOP
      INC (inCritical);
        IF m.holder = NIL THEN
          <* ASSERT self # NIL *>
          m.holder := self;
          DEC (inCritical);
          RETURN;
        END;
        ICannotRun (State.locking);
        self.waitingForMutex := m;
        self.nextWaiting := m.waitingForMe;
        self.alertable := FALSE;
        m.waitingForMe := self;
        IF (m.holder = self) THEN ImpossibleAcquire (m); END;
      DEC (inCritical);
      InternalYield ();
    END;
  END LockMutex;

PROCEDURE ImpossibleAcquire (m: Mutex) =
  BEGIN
    DumpEverybody ();
    OutT ("*** Thread #");
    OutI (m.holder.id, 0);
    OutT (" is trying to reacquire mutex ");
    OutA (m, 0);
    OutT (" which it already holds.\n");
    RTError.Msg ("ThreadPosix.m3", 438, "impossible Thread.Acquire");
  END ImpossibleAcquire;

PROCEDURE UnlockMutex (m: Mutex) =
  <*FATAL Alerted*>
  VAR waiters: BOOLEAN;
  BEGIN
    INC (inCritical);
      waiters := XRelease (m);
    DEC (inCritical);
    IF waiters THEN
      self.alertable := FALSE;
      InternalYield ();
    END;
  END UnlockMutex;

PROCEDURE XRelease (m: Mutex): BOOLEAN =
  (* called while inCritical *)
  VAR t, last_t: T;
  BEGIN
    IF m.holder # self THEN SleazyRelease (m) END;
    m.holder := NIL;

    t := m.waitingForMe;
    IF (t = NIL) THEN RETURN FALSE END;

    (* search for the end:  t == last thread, last_t == second to last one *)
    last_t := NIL;
    WHILE (t.nextWaiting # NIL) DO last_t := t;  t := t.nextWaiting  END;

    IF (last_t # NIL)
      THEN last_t.nextWaiting := NIL; (* multiple threads are waiting *)
      ELSE m.waitingForMe := NIL;     (* only one thread is waiting *)
    END;
    t.nextWaiting := NIL;
    CanRun (t);
    RETURN TRUE;
  END XRelease;

PROCEDURE SleazyRelease (m: Mutex) =
  BEGIN
    DumpEverybody ();
    OutT ("*** Mutex ");
    OutA (m, 0);
    IF m.holder = NIL THEN
      OutT (" is not locked.\n");
    ELSE
      OutT (" is held by thread #");
      OutI (m.holder.id, 0);
      OutT (".\n");
    END;
    RTError.Msg ("Thread.m3", 489, "illegal Thread.Release");
  END SleazyRelease;

(*--------------------------------------------- garbage collector support ---*)

PROCEDURE SuspendOthers () =
  BEGIN
    INC(inCritical);
  END SuspendOthers;

PROCEDURE ResumeOthers () =
  BEGIN
    DEC(inCritical);
  END ResumeOthers;

PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS)) =
  VAR t:= self; start, stop: ADDRESS;
  BEGIN
    (* save my state *)
    EVAL RTThread.Save (self.context.buf);

    REPEAT 
      Tos (t.context, start, stop);	 (* process the stack *)
      p (start, stop);
      WITH z = t.context.buf DO		 (* process the registers *)
        p (ADR (z), ADR (z) + ADRSIZE (z))
      END;
      t := t.next;
    UNTIL t = self;
  END ProcessStacks;

(*------------------------------------------------- I/O and Timer support ---*)

PROCEDURE Pause(n: LONGREAL)=
  <*FATAL Alerted*>
  VAR until := TimePosix.ToUtime (n + Time.Now ());
  BEGIN
    XPause(until, FALSE);
  END Pause;

PROCEDURE AlertPause(n: LONGREAL) RAISES {Alerted}=
  VAR until := TimePosix.ToUtime (n + Time.Now ());
  BEGIN
    XPause(until, TRUE);
  END AlertPause;

PROCEDURE XPause (READONLY until: UTime; alertable := FALSE) RAISES {Alerted} =
  BEGIN
    INC (inCritical);
      self.waitingForTime := until;
      self.alertable := alertable;
      ICannotRun (State.pausing);
    DEC (inCritical);
    InternalYield ();
  END XPause;

CONST FDSetSize = BITSIZE(INTEGER);

TYPE
  FDSet = SET OF [0 .. FDSetSize-1];
  FDS = REF ARRAY OF FDSet;

VAR
  gMaxActiveFDSet, gMaxFDSet: CARDINAL := 1;
  gReadFDS, gWriteFDS, gExceptFDS: FDS := NEW(FDS, 1);

  (* gMaxFDSet is NUMBER(gReadFDS^) *)
  (* gReadFDS, gWriteFDS, and gExceptFDS all have the same length *)
  (* gMaxActiveFDSet <= gMaxFDSet, gMaxActiveFDSet * FDSetSize > nFD,
     where nFD is the maximum fd active in any call to XIOWait *)
  (* gMaxFDSet never decreases *)

  (* note that using a FD beyond the range of legal FDs produces
     a checked runtime error *)

PROCEDURE IOWait(fd: INTEGER; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult =
  <*FATAL Alerted*>
  BEGIN
    self.alertable := FALSE;
    RETURN XIOWait(fd, read, timeoutInterval);
  END IOWait;

PROCEDURE IOAlertWait(fd: INTEGER; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult
                  RAISES {Alerted} =
  BEGIN
    self.alertable := TRUE;
    RETURN XIOWait(fd, read, timeoutInterval);
  END IOAlertWait;

PROCEDURE XIOWait (fd: CARDINAL; read: BOOLEAN; interval: LONGREAL): WaitResult
    RAISES {Alerted} =
  VAR res: INTEGER;
      fdindex := fd DIV FDSetSize;
      fdset := FDSet{fd MOD FDSetSize};
  BEGIN
    (* If we are in a single-threaded program do just what the user wants *)
    IF NOT multipleThreads THEN
      self.alertable := FALSE;
      IF fdindex >= gMaxActiveFDSet THEN
        gMaxActiveFDSet := fdindex + 1;
        IF gMaxFDSet < gMaxActiveFDSet THEN
          gReadFDS := NEW(FDS, gMaxActiveFDSet);
          gWriteFDS := NEW(FDS, gMaxActiveFDSet);
          gExceptFDS := NEW(FDS, gMaxActiveFDSet);
          gMaxFDSet := gMaxActiveFDSet;
        END;
      END;
      ZeroFDS();
      IF read
        THEN gReadFDS[fdindex] := fdset;
        ELSE gWriteFDS[fdindex] := fdset;
      END;
      IF interval >= 0.0D0 THEN
        VAR utimeout := UTimeFromTime(interval); BEGIN
          res := CallSelect(fd+1, ADR(utimeout));
        END;
      ELSE
        res := CallSelect(fd+1, NIL);
      END;
      IF    res > 0 THEN RETURN TestFDS(fdindex, fdset, read);
      ELSIF res = 0 THEN RETURN WaitResult.Timeout;
      ELSE               RETURN WaitResult.Error;
      END;
    ELSE
      (* This thing blocks, schedule it for later *) 
      VAR newRead, newWrite, newExcept: FDS := NIL;
      BEGIN
        IF fdindex >= gMaxFDSet THEN
          (* must do alloc in non-critical *)
          newRead := NEW(FDS, fdindex+1);
          newWrite := NEW(FDS, fdindex+1);
          newExcept := NEW(FDS, fdindex+1);
        END;
        INC (inCritical);
          IF fdindex >= gMaxActiveFDSet THEN
            gMaxActiveFDSet := fdindex + 1;
            IF gMaxFDSet < gMaxActiveFDSet THEN
              gReadFDS := newRead;
              gWriteFDS := newWrite;
              gExceptFDS := newExcept;
              gMaxFDSet := gMaxActiveFDSet;
            END;
          END;
          self.select.fd := fd;
          self.select.read := read;
          self.select.index := fdindex;
          self.select.set := fdset;
          self.select.hasTimeout := (interval >= 0.0D0);
          IF interval >= 0.0D0 THEN
            self.select.timeout :=
                Time_Add(UTimeNow(), UTimeFromTime(interval));
          END;
          ICannotRun (State.blocking);
        DEC (inCritical);
      END;
      InternalYield ();
      Cerrno.SetErrno(self.select.errno);
      RETURN self.select.waitResult;
    END;
  END XIOWait;

PROCEDURE ZeroFDS() =
  BEGIN
    FOR i := 0 TO gMaxActiveFDSet-1 DO
      gReadFDS[i] := FDSet{};
      gWriteFDS[i] := FDSet{};
    END;
  END ZeroFDS;

(*
PROCEDURE InclFDS(fd: CARDINAL; read: BOOLEAN) =
  VAR set := fd DIV FDSetSize;
      sset := FDSet{fd MOD FDSetSize};
  BEGIN
    IF read
      THEN gReadFDS[set] := gReadFDS[set] + sset;
      ELSE gWriteFDS[set] := gWriteFDS[set] + sset;
    END;
  END InclFDS;
*)

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
    FOR i := 0 TO gMaxActiveFDSet-1 DO
      gExceptFDS[i] := gReadFDS[i] + gWriteFDS[i];
    END;
    res := Unix.select(nfd, LOOPHOLE (ADR(gReadFDS[0]), FDSPtr),
                            LOOPHOLE (ADR(gWriteFDS[0]), FDSPtr),
                            LOOPHOLE (ADR(gExceptFDS[0]), FDSPtr), timeout);
    IF res > 0 THEN
      FOR i := 0 TO gMaxActiveFDSet-1 DO
        gExceptFDS[i] := gExceptFDS[i] + gReadFDS[i] + gWriteFDS[i];
      END;
    END;
    RETURN res;
  END CallSelect;

PROCEDURE UTimeFromTime(time: Time.T): UTime =
  VAR floor := FLOOR(time);
  BEGIN
    RETURN UTime{floor, FLOOR(1.0D6 * (time - FLOAT(floor, LONGREAL)))};
  END UTimeFromTime;


(*------------------------------------------------ timer-based preemption ---*)

PROCEDURE DisableSwitching () =
  BEGIN
    INC (inCritical);
  END DisableSwitching;

PROCEDURE EnableSwitching () =
  BEGIN
    DEC (inCritical);
  END EnableSwitching;

PROCEDURE StartSwitching () =
(* set the SIGVTALRM timer and handler; can be called to change the 
   switching interval *)
  VAR it, oit: Utime.struct_itimerval;
  BEGIN
    IF preemption THEN
      RTThread.setup_sigvtalrm (switch_thread);
      it.it_interval := selected_interval;
      it.it_value    := selected_interval;
      IF Utime.setitimer (Utime.ITIMER_VIRTUAL, it, oit) # 0 THEN
        RAISE InternalError;
      END;
      RTThread.allow_sigvtalrm ();
    END;
  END StartSwitching;

PROCEDURE switch_thread (<*UNUSED*> sig: INTEGER) =
  BEGIN
    RTThread.allow_sigvtalrm ();
    IF inCritical = 0 THEN InternalYield () END;
  END switch_thread;

PROCEDURE SetSwitchingInterval (usec: CARDINAL) =
  BEGIN
    INC (inCritical);
      selected_interval.tv_sec  := usec DIV 1000000;
      selected_interval.tv_usec := usec MOD 1000000;
    DEC (inCritical);
    IF multipleThreads THEN StartSwitching () END;
  END SetSwitchingInterval;

(*------------------------------------------------------------- scheduler ---*)

PROCEDURE CanRun (t: T) = 
  BEGIN
    t.state := State.alive; 
    t.nextWaiting := NIL;
    t.waitingForCondition := NIL;
    t.waitingForMutex := NIL;
    IF perfOn THEN PerfChanged (t.id, State.alive); END;
  END CanRun;
  
PROCEDURE ICannotRun (newState: State) = 
  BEGIN
    self.state := newState;
    IF perfOn THEN PerfChanged (self.id, newState); END;
  END ICannotRun;


PROCEDURE InternalYield () RAISES {Alerted} =
VAR t, from: T;
    blockingNfds: CARDINAL;
    scanned := FALSE;
    (* scanned => gReadFDS, ... gExceptFDS have been
       set by a select call that includes the masks of all
       blocked threads in the prefix up the thread list
       up to the first runnable thread, or the whole list
       if there is no runnable thread.
       INVARIANT: scanned OR (selectResult = 0) *)

    somePausing, someBlocking: BOOLEAN;
    now          : UTime;
    earliest     : UTime;
    selectResult := 0;
    do_alert     : BOOLEAN;
    did_delete   : BOOLEAN;

BEGIN
  INC (inCritical);
  <*ASSERT inCritical = 1 *>

  from := self.next; (* remember where we started *)
  now            := UTimeNow ();

  LOOP
    t              := from;
    IF NOT scanned OR selectResult < 0 THEN ZeroFDS(); END;
    blockingNfds   := 0;
    did_delete     := FALSE;
    somePausing    := FALSE;
    someBlocking   := FALSE;

    LOOP
      CASE t.state OF 
        | State.waiting =>
            IF t.alertable AND t.alertPending THEN
              WITH c = t.waitingForCondition DO
                IF c.waitingForMe = t THEN
                  c.waitingForMe := t.nextWaiting;
                ELSE
                  VAR tt := c.waitingForMe; BEGIN
                    WHILE tt.nextWaiting # t DO tt := tt.nextWaiting; END;
                    tt.nextWaiting := t.nextWaiting; END; END; END;
              CanRun (t);
              EXIT; END;

        | State.locking =>
            <*ASSERT NOT t.alertable*>

        | State.pausing  =>
            IF t.alertable AND t.alertPending THEN
              CanRun (t);
              EXIT;

            ELSIF Time_Compare (t.waitingForTime, now) <= 0 THEN
              CanRun (t);
              EXIT;
  
            ELSIF NOT somePausing THEN
              earliest := t.waitingForTime;
              somePausing := TRUE;

            ELSIF Time_Compare (t.waitingForTime, earliest) < 0 THEN
              earliest := t.waitingForTime; END; 
  
        | State.blocking =>
            IF t.alertable AND t.alertPending THEN
              CanRun (t); 
              EXIT;

            ELSIF NOT scanned THEN
              blockingNfds := MAX (blockingNfds, t.select.fd + 1);
              IF t.select.read THEN
                gReadFDS[t.select.index] :=
                   gReadFDS[t.select.index] + t.select.set;
              ELSE
                gWriteFDS[t.select.index] :=
                   gWriteFDS[t.select.index] + t.select.set;
              END;
              someBlocking := TRUE
            ELSE
              (* scanned is TRUE *)
              IF selectResult < 0 THEN
                IF t.select.read THEN
                  gReadFDS[t.select.index] := t.select.set;
                ELSE
                  gWriteFDS[t.select.index] := t.select.set;
                END;
                VAR n := CallSelect(t.select.fd+1, ADR(ZeroTimeout)); BEGIN
                  IF n > 0 THEN
                    t.select.waitResult :=
                          TestFDS(t.select.index, t.select.set, t.select.read);
                    CanRun(t);
                    EXIT;
                  ELSIF n < 0 THEN
                    t.select.errno  := Cerrno.GetErrno();
                    t.select.waitResult := WaitResult.Error;
                    CanRun(t);
                    EXIT;
                  END;
                END;
              ELSIF selectResult > 0 THEN
                VAR
                  res := TestFDS(t.select.index, t.select.set, t.select.read);
                BEGIN
                  IF res # WaitResult.Timeout THEN
                    t.select.waitResult := res;
                    CanRun(t);
                    EXIT;
                  END;
                END;
              END;

              (* Not runnable, but its timer may have expired *)
              IF t.select.hasTimeout 
                  AND Time_Compare (t.select.timeout, now) <= 0 THEN
                t.select.errno  := 0;
                t.select.waitResult := WaitResult.Timeout;
                CanRun (t);
                EXIT;
              END;
            END;

            IF t.select.hasTimeout THEN
              IF NOT somePausing THEN
                earliest := t.select.timeout;
                somePausing := TRUE;
              ELSIF Time_Compare (t.select.timeout, earliest) < 0 THEN
                earliest := t.select.timeout; 
              END
            END
    
        | State.dying, State.dead =>
            (* remove this guy from the ring *)
            IF perfOn THEN PerfDeleted (t.id); END;
      	    IF hooks # NIL THEN hooks.die (t) END;
            VAR tmp := t.previous; BEGIN
              IF (t = from) THEN from := tmp END;
              t.next.previous := tmp;
              tmp.next := t.next;
              t.previous := NIL;
              t.next := dead_stacks;
              dead_stacks := t;
              t := tmp;
              did_delete := TRUE;
            END;
        | State.alive =>
            EXIT;
        END; (* case *)
  
      t := t.next;
      IF t = from THEN
        IF NOT scanned THEN
          gMaxActiveFDSet := 1 + ((blockingNfds-1) DIV FDSetSize);
        END;
        EXIT;
      END;
    END;

    IF t.state = State.alive AND (scanned OR NOT someBlocking) THEN
      IF perfOn THEN PerfRunning (t.id); END;
      (* At least one thread wants to run; transfer to it *)
      Transfer (self.context, t.context, t);
      IF (dead_stacks # NIL) THEN FreeDeadStacks () END;
      do_alert := self.alertable AND self.alertPending;
      self.alertable := FALSE;
      IF do_alert THEN self.alertPending := FALSE END;
      DEC (inCritical);
      IF do_alert THEN RAISE Alerted END;
      RETURN;

    ELSIF did_delete THEN
      (* run through the ring one more time before we block
         waiting for I/O, pause for a timer or declare deadlock. *)
      scanned := FALSE;
      selectResult := 0;

    ELSIF (selectResult < 0) THEN
      (* the initial select call failed (ie. erred or was interrupted)
         and none of the subsequent select calls is responsible.*)
      scanned := FALSE;
      selectResult := 0;

    ELSIF somePausing OR someBlocking THEN
      IF perfOn THEN PerfRunning (-1); END;
      IF t.state = State.alive OR 
               somePausing AND Time_Compare(earliest, now) <= 0 THEN
        selectResult := CallSelect(blockingNfds, ADR(ZeroTimeout));
      ELSIF somePausing THEN
        VAR timeout := Time_Subtract (earliest, now); BEGIN
          selectResult := CallSelect(blockingNfds, ADR(timeout));
        END;
      ELSE
        selectResult := CallSelect(blockingNfds, NIL);
      END;
      IF selectResult <= 0 THEN now := UTimeNow(); END;
      scanned := TRUE
    ELSE
      IF perfOn THEN PerfRunning (-1); END;
      DumpEverybody ();
      RTError.Msg (NIL, 0, "Deadlock !");
    END;
  END;
END InternalYield;

PROCEDURE FreeDeadStacks () =
  (* blow away any dead stacks *)
  VAR x: T;  t: T := dead_stacks;
  BEGIN
    WHILE (t # NIL) DO
      <*ASSERT t # self*>
      IF (t.context.stack.words # NIL) THEN
        RTThread.FreeStack (t.context.stack);
        t.context.stack.words := NIL;
      END;
      x := t;
      t := t.next;
      x.next := NIL;
    END;
  END FreeDeadStacks;


(*-------------------------------------------------- low-level coroutines ---*)

CONST
  seal = 123456;

TYPE
  Context = RECORD 
    stack:       RTThread.Stack;
    stackTop:    ADDRESS;
    stackBottom: ADDRESS;
    handlers:    ADDRESS;
    errno:       INTEGER;
    buf:         RTThread.State;
  END;

VAR
  self: T;  (* the currently running thread *)

VAR
  modelFrame: UNTRACED REF ARRAY OF Word.T;
  modelFrameLoc: ADDRESS;
  modelSP : ADDRESS;
  modelBuf : RTThread.State;
  
(* The general strategy is:
   - at initialization time, get an idea of what the stack frame and 
     environment for a routine is; this is done by 
     InitTopContext/DetermineContext. This context is stored in
     the  "model" variables.

   - when a new thread is forked, its stack is initialized from the 
     model stack, and the environment is restored after modifying the 
     entries that depend on the stack position (eg. SP, AP, FP)
     running in that new context will send us in DetermineContext that
     will execute the thread closure (actually a shell that runs that
     closure).
*)

PROCEDURE InitTopContext (VAR c: Context;  stackbase: ADDRESS) =
  CONST STACK_SLOP = 8 * ADRSIZE (INTEGER);
  VAR env: RTThread.State;
  BEGIN
    (* The first thread runs on the original stack, we don't want any checks *)
    c.stack.words := NIL;
    c.stack.first := NIL;
    c.stack.last  := NIL;
    c.handlers    := NIL;
    c.errno       := 0;
    IF stack_grows_down THEN
      c.stackTop    := NIL;
      c.stackBottom := stackbase + STACK_SLOP;
    ELSE
      c.stackTop    := LOOPHOLE (LAST (INTEGER), ADDRESS);
      c.stackBottom := stackbase - STACK_SLOP;
    END;
    
    (* determine what should go in the stack of future threads *)
    WITH i = RTThread.Save (env) DO
      <* ASSERT i = 0 *> END;
    DetermineContext (RTThread.SP (env));

  END InitTopContext;

PROCEDURE DetermineContext (oldSP: ADDRESS) =
  (* This routine looks at the stack frame for this call and takes
     it as a model for the frame to put in the stacks of forked
     threads. It also saves the jmp_buf at the beginning of the 
     call in a global; that jmp_buf will be (after updating the 
     stack pointer) for forked threads *)
  <*FATAL Alerted*>
  BEGIN
    
    IF (RTThread.Save (modelBuf) = 0) THEN
      (* first time through; this part is executed only once to determine
         the model *)
      
      modelSP := RTThread.SP (modelBuf);
      
      (* Copy the frame (plus pad) to modelStack and
         remember where that should go in the new stacks *)
      
      RTThread.FlushStackCache ();

      IF debug THEN
        OutAddr("modelSP:    ", modelSP);
        OutAddr("oldSP:      ", oldSP);
        OutInt("frame size: ", ABS (modelSP - oldSP));
      END;
      modelFrame := NEW (UNTRACED REF ARRAY OF Word.T,
                         ABS (modelSP - oldSP) DIV ADRSIZE (Word.T)
                         + 1 + RTThread.FramePadBottom + RTThread.FramePadTop);
                                               
      IF stack_grows_down THEN
        (* <* ASSERT oldSP > modelSP *> *)
        modelFrameLoc := modelSP - RTThread.FramePadTop * ADRSIZE(Word.T);
      ELSE
        (* <* ASSERT oldSP < modelSP *> *)
        modelFrameLoc := oldSP - RTThread.FramePadBottom * ADRSIZE (Word.T);
      END;

      EVAL Cstring.memcpy (ADR (modelFrame [0]), modelFrameLoc,
                           NUMBER (modelFrame^) * BYTESIZE (Word.T));
      
    ELSE 
      (* we are starting the execution of a forked thread *)
      handlerStack := self.context.handlers;
      Cerrno.SetErrno(self.context.errno);
      RTThread.allow_sigvtalrm ();
      DEC (inCritical);
      
      FloatMode.InitThread (self.floatState);
      self.result := self.closure.apply ();
      
      INC (inCritical);
      Broadcast (self.endCondition);
      ICannotRun (State.dying);
      INC (stats.n_dead);
      DEC (inCritical);
      InternalYield ();
      <* ASSERT FALSE *> END;
  END DetermineContext;


PROCEDURE InitContext (VAR c: Context;  size: INTEGER) =
  VAR
    offset, SPinFrame: INTEGER;
    frameLoad: ADDRESS;
  BEGIN
    (* allocate a new stack *)
    RTThread.GetStack (size, c.stack);
    
    (* initialize the context fields *)
    IF stack_grows_down THEN
      c.stackTop    := c.stack.first;
      c.stackBottom := c.stack.last - ADRSIZE (Word.T);
    ELSE
      c.stackTop    := c.stack.last - ADRSIZE (Word.T);
      c.stackBottom := c.stack.first;
    END;
    c.handlers    := NIL;
    c.errno       := Cerrno.GetErrno();
    
    (* mark the ends of the stack for a sanity check *)
    LOOPHOLE (c.stackTop, IntPtr)^ := seal;
    LOOPHOLE (c.stackBottom, IntPtr)^ := seal;

    IF stack_grows_down THEN
      SPinFrame := RTThread.FramePadTop * ADRSIZE (Word.T);
      frameLoad := RTMisc.Align (c.stack.last - 2 * ADRSIZE (Word.T)
                       - NUMBER (modelFrame^) * ADRSIZE (Word.T)
                       - RTThread.StackFrameAlignment + 1 + SPinFrame,
                       RTThread.StackFrameAlignment) - SPinFrame;
    ELSE
      SPinFrame := (NUMBER (modelFrame^) - RTThread.FramePadBottom)
                       * ADRSIZE (Word.T);
      frameLoad := RTMisc.Align (c.stack.first + ADRSIZE (Word.T) + SPinFrame,
                                 RTThread.StackFrameAlignment) - SPinFrame;
    END;
    offset := (frameLoad + SPinFrame) - modelSP;
    EVAL Cstring.memcpy (frameLoad, ADR (modelFrame [0]),
                         NUMBER (modelFrame^) * BYTESIZE (Word.T));
    RTThread.UpdateFrameForNewSP (frameLoad + SPinFrame, offset);

    c.buf := modelBuf;
    RTThread.UpdateStateForNewSP (c.buf, offset);
  END InitContext;

PROCEDURE Transfer (VAR from, to: Context;  new_self: T) =
  BEGIN
    IF (from.stack.words # NIL)
      AND (LOOPHOLE (from.stackTop, IntPtr)^ # seal
           OR LOOPHOLE (from.stackBottom, IntPtr)^ # seal) THEN
      SmashedStack (self);
    END;

    IF (to.stack.words # NIL)
      AND (LOOPHOLE (to.stackTop, IntPtr)^ # seal
           OR LOOPHOLE (to.stackBottom, IntPtr)^ # seal) THEN
      SmashedStack (new_self);
    END;

    (*********
    <* ASSERT (from.stack.words = NIL)
                OR (LOOPHOLE (from.stackTop, IntPtr)^ = seal
                AND LOOPHOLE (from.stackBottom, IntPtr)^ = seal) *>
    
    <* ASSERT (to.stack.words = NIL)
                OR (LOOPHOLE (to.stackTop, IntPtr)^ = seal
                AND LOOPHOLE (to.stackBottom, IntPtr)^ = seal) *>
    **********)

    IF (ADR (from) # ADR (to)) THEN
      RTThread.disallow_sigvtalrm ();
      from.handlers := handlerStack;
      from.errno := Cerrno.GetErrno();
      self := new_self;
      myId := new_self.id;
      RTThread.Transfer (from.buf, to.buf);
      handlerStack := from.handlers;
      Cerrno.SetErrno(from.errno);
      RTThread.allow_sigvtalrm ();
    END;
  END Transfer;

PROCEDURE SmashedStack (t: T) =
  BEGIN
    DumpEverybody ();
    OutT ("*** Thread #");
    OutI (t.id, 0);
    OutT ("'s stack overflowed its limits.\n");
    OutT ("*** Use Thread.IncDefaultStackSize to get bigger stacks.\n");
    RTError.Msg ("ThreadPosix.m3", 1215, "corrupt thread stack");
  END SmashedStack;

PROCEDURE Tos (READONLY c: Context; VAR start, stop: ADDRESS) =
  BEGIN
    IF stack_grows_down THEN
      start := RTThread.SP (c.buf);
      stop  := c.stackBottom - ADRSIZE (Word.T);
    ELSE
      start := c.stackBottom + ADRSIZE (Word.T);
      stop  := RTThread.SP (c.buf);
    END;
  END Tos;

PROCEDURE MyFPState (): UNTRACED REF FloatMode.ThreadState =
  BEGIN
    RETURN ADR (self.floatState);
  END MyFPState;

(*----------------------------------------------------- debugging support ---*)

CONST
  WaitTag = ARRAY State OF TEXT {
    "*ready*",
    "condition ",
    "mutex ",
    "timer ",
    "I/O ",
    "*dying*",
    "*dead*"
  };

PROCEDURE DumpEverybody () =
  VAR t: T;
  BEGIN
    INC (inCritical);
      OutT ("\n\n*****************************");
      OutT ("**********************************\n");
      OutT ("  id    Thread.T     closure root");
      OutT ("                A* waiting for\n");
      t := self;
      REPEAT
        IF (t = NIL) THEN
          OutT ("!!! NIL thread in ring !!!\n");
          EXIT;
        END;
        DumpThread (t);
        t := t.next;
      UNTIL (t = self);
      OutT ("*****************************");
      OutT ("**********************************\n");
      RTIO.Flush ();
    DEC (inCritical);
  END DumpEverybody;

PROCEDURE DumpThread (t: T) =
  TYPE ClosureMethods = UNTRACED REF ARRAY [0..1] OF ADDRESS;
  TYPE ClosureObject  = UNTRACED REF ClosureMethods;
  VAR
    pc, proc: ADDRESS;
    m: MUTEX;
    co: ClosureObject;
    name: RTProcedureSRC.Name;
    file: RTProcedureSRC.Name;
  BEGIN
    IF (t = self)
      THEN OutT (">");
      ELSE OutT (" ");
    END;

    (* thread ID *)
    OutI (t.id, 3);

    (* Thread.T *)
    OutA (t, 12);

    (* closure *)
    OutA (t.closure, 12);

    (* inital PC *)
    OutT (" ");
    pc := NIL;
    co := LOOPHOLE (t.closure, ClosureObject);
    IF (co # NIL) AND (co^ # NIL) THEN pc := co^^[0] END;
    IF (co = NIL) THEN
      OutT ("*main program*      ");
    ELSE
      RTProcedureSRC.FromPC (pc, proc, file, name);
      IF (proc = NIL) OR (proc # pc) THEN
        OutA (LOOPHOLE (pc, REFANY), 20);
      ELSE
        RTIO.PutString (name);
        Pad (20, Cstring.strlen (name));
      END;
    END;

    (* alert status *)
    IF (t.alertable)
      THEN OutT ("A");
      ELSE OutT (" ");
    END;
    IF (t.alertPending)
      THEN OutT ("* ");
      ELSE OutT ("  ");
    END;

    (* state *)
    OutT (WaitTag [t.state]);
    CASE t.state OF
    | State.alive =>
        (* nothing *)
    | State.waiting =>
        OutA (t.waitingForCondition, 0);
    | State.locking =>
        m := t.waitingForMutex;
        OutA (m, 0);
        IF (m # NIL) THEN
          IF (m.holder = NIL) THEN
            OutT (" (unlocked)");
          ELSE
            OutT (" (held by #");
            OutI (m.holder.id, 0);
            OutT (")");
          END;
        END;
    | State.blocking =>
        (* nothing *)
    | State.pausing =>
        (* nothing *)
    | State.dying =>
        (* nothing *)
    | State.dead =>
        (* nothing *)
    END;

    OutT ("\n");
  END DumpThread;

PROCEDURE OutT (t: TEXT) =
  BEGIN
    RTIO.PutText (t);
  END OutT;

PROCEDURE OutI (i: INTEGER;  width: INTEGER) =
  BEGIN
    RTIO.PutInt (i, width);
  END OutI;

PROCEDURE OutA (a: REFANY;  width: INTEGER) =
  BEGIN
    RTIO.PutHex (LOOPHOLE (a, INTEGER), width);
  END OutA;

VAR pad := ARRAY [0..20] OF CHAR { ' ', .. };

PROCEDURE Pad (min, used: INTEGER) =
  BEGIN
    IF (used < min) THEN
      RTIO.PutChars (ADR (pad[0]), min - used);
    END;
  END Pad;

(*------------------------------------------------------ ShowThread hooks ---*)

VAR
  perfW  : RTPerfTool.Handle;
  perfOn : BOOLEAN := FALSE;

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
    perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
  END PerfChanged;

PROCEDURE PerfDeleted (id: Id) =
  VAR e := ThreadEvent.T {kind := TE.Deleted, id := id};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
  END PerfDeleted;

PROCEDURE PerfRunning (id: Id) =
  VAR e := ThreadEvent.T {kind := TE.Running, id := id};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
  END PerfRunning;

(*--------------------------------------------------------- ThreadF hooks ---*)

VAR
  hooks: Hooks := NIL;

PROCEDURE RegisterHooks(h: Hooks; init := TRUE): Hooks RAISES {}=
  VAR
    oldHooks: Hooks; 
    t: T;
  BEGIN
    INC (inCritical);
      oldHooks := hooks;
      hooks := h;
      IF init AND hooks # NIL THEN
      	t := self;
      	REPEAT 
      	  hooks.fork (t);
          t := t.next;
      	UNTIL (t = self);
      END;
    DEC (inCritical);
    RETURN oldHooks;
  END RegisterHooks;

PROCEDURE MyId(): Id RAISES {}=
  BEGIN
    RETURN self.id;
  END MyId;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Init()=
  VAR xx: INTEGER;
  BEGIN
    inCritical := 1;
      topThread := NEW (T, state := State.alive, id := nextId);
      FloatMode.InitThread (topThread.floatState);

      INC (nextId);
    
      stack_grows_down := ADR (xx) > QQ();
      InitTopContext (topThread.context, ADR(xx));
      self := topThread;
      myId := self.id;

      pausedThreads := NIL;

      topThread.next := topThread;
      topThread.previous := topThread;
    inCritical := 0;

    PerfStart ();
    preemption := NOT RTParams.IsPresent ("nopreemption");
  END Init;

PROCEDURE QQ(): ADDRESS =
  VAR xx: INTEGER;
  BEGIN
    RETURN ADR (xx);
  END QQ;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector. *)

VAR
  lock_cnt  := 0;      (* LL = inCritical *)
  do_signal := FALSE;  (* LL = inCritical *)
  mutex     := NEW(MUTEX);
  condition := NEW(Condition);

PROCEDURE LockHeap () =
  BEGIN
    INC(inCritical);
    INC(lock_cnt);
  END LockHeap;

PROCEDURE UnlockHeap () =
  VAR sig := FALSE;
  BEGIN
    DEC(lock_cnt);
    IF (lock_cnt = 0) AND (do_signal) THEN sig := TRUE; do_signal := FALSE; END;
    DEC(inCritical);
    IF (sig) THEN Broadcast(condition); END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  (* LL = 0 *)
  BEGIN
    LOCK mutex DO Wait(mutex, condition); END;
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  (* LL = inCritical *)
  BEGIN
    do_signal := TRUE;
  END BroadcastHeap;

(*--------------------------------------------- exception handling support --*)

PROCEDURE GetCurrentHandlers (): ADDRESS=
  BEGIN
    RETURN handlerStack;
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers (h: ADDRESS)=
  BEGIN
    handlerStack := h;
  END SetCurrentHandlers;

(*RTHooks.PushEFrame*)
PROCEDURE PushEFrame (frame: ADDRESS) =
  TYPE Frame = UNTRACED REF RECORD next: ADDRESS END;
  VAR f := LOOPHOLE (frame, Frame);
  BEGIN
    f.next := handlerStack;
    handlerStack := f;
  END PushEFrame;

(*RTHooks.PopEFrame*)
PROCEDURE PopEFrame (frame: ADDRESS) =
  BEGIN
    handlerStack := frame;
  END PopEFrame;

(*------------------------------------------------------------- debugging ---*)

PROCEDURE OutAddr(s: TEXT; a: ADDRESS) =
  BEGIN
    IF NOT debug THEN RETURN END;
    RTIO.PutText(s);
    RTIO.PutAddr(a);
    RTIO.PutText("\r\n");
    RTIO.Flush();
  END OutAddr;

PROCEDURE OutInt(s: TEXT; a: INTEGER) =
  BEGIN 
    IF NOT debug THEN RETURN END;
    RTIO.PutText(s);
    RTIO.PutInt(a);
    RTIO.PutText("\r\n");
    RTIO.Flush();
  END OutInt;

VAR debug := FALSE;

BEGIN
END ThreadPosix.
