(*| Copyright (C) 1989, Digital Equipment Corporation        *)
(*| All rights reserved.                                     *)
(*| See the file COPYRIGHT for a full description.           *)
(*|                                                          *)
(*| Last modified on Fri Apr  7 09:06:48 PDT 1995 by kalsow  *)
(*|      modified on Mon Mar 21 17:40:31 PST 1994 by wobber  *)
(*|      modified on Sat Jun 26 17:07:03 PDT 1993 by gnelson *)
(*|      modified on Fri May 14 16:15:26 PDT 1993 by mjordan *)
(*|      modified on Wed Apr 21 16:35:05 PDT 1993 by mcjones *)
(*|      modified On Mon Apr  5 14:51:30 PDT 1993 by muller  *)
(*|      modified on Mon Feb 22 10:08:49 PST 1993 by jdd     *)

UNSAFE MODULE ThreadPosix EXPORTS Thread, ThreadF, RTThread,
Scheduler, SchedulerPosix, RTOS, RTHooks, ThreadPosix;

IMPORT Cerrno, Cstring, FloatMode, MutexRep, RTHeapRep, RTCollectorSRC,
       RTError, RTParams, RTPerfTool, RTProcedureSRC, RTProcess,
       RTIO, ThreadEvent, Time, Uexec, RuntimeError;
FROM Compiler IMPORT ThisFile, ThisLine;
FROM Ctypes IMPORT int;
FROM ThreadInternal IMPORT FDSet, FDSetSize, FDS, Select;
FROM Usignal IMPORT SIGCHLD, SIGSEGV;

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
        timeout: Time.T := 0.0D0;
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

(* Support for catching signals (other than alarm clock) added by
   Mika Nystrom <mika@alum.mit.edu>, February, 2011.

   The idea is to piggy-back general signal catching on the mechanism
   already used for thread switching.  Every thread that is "pausing"
   can now wait for the disjunction of an expired timer or the delivery
   of a specific signal.

   The main (only) client of pausing for signal delivery is
   Process.Wait which, under user threads, used to have a 0.1 second
   delay between polling for change of child-process status.  We
   now make it wait (WaitProcess) for 0.1 seconds OR the delivery
   of SIGCHLD.

   The changes are as follows:

   1. The system relies on changing ThreadPosix.XPause such that if a
   signal is allowed to wake up a threads, that fact is recorded in a
   new field in the thread's descriptor record (of type
   ThreadPosix.T).

   2. On receipt of a waited-for unix signal, a mask is set and
   control is passed to the thread scheduler which maintains the
   non-zero mask for exactly one iteration through the thread ring.

   3. If a thread is paused and waiting for EITHER a signal or some
   time, the thread is released for running and the thread's waiting
   state is cleared.

   A final subtlety is that signals are traditionally re-enabled
   in switch_thread (i.e., inside the signal handler itself).  This
   approach can't be used unchanged when catching SIGCHLD because
   it can lead to very deeply nested signal handlers if many child
   processes are exiting at around the same time.  Instead, we only
   re-enable SIGCHLD when inCritical is zero, i.e., in an unnested
   signal handler, so that these signals are taken from an unnested
   signal handler only.  This may also be important if longjmp is
   used (probably not anymore?)

   Implementation notes:

   1. a new field of type "int" to ThreadPosix.T, waitingForSig

   2. modifications to Pause, especially the new procedure SigPause

   3. received-signals mask

   4. signal handler has to be installed even if program is single-threaded

   The mechanism could be generalized to handle other signals as well
   as multiple types of signals.

   I have not carefully verified that no signals are ever "lost"
   (the 0.1-second delay serves as a backstop), but I believe this
   to be the case.

   One area of some concern is the use of longjmp in switching out
   of the signal handler.  This may not be legal in nested signal
   handlers and it should be verified it cannot occur.  I believe
   the check for inCritical = 0 before calling InternalYield has
   the desired effect.
*)

(* Support for catching SEGV added by Mika Nystrom <mika@alum.mit.edu>,
   February, 2011.

   The system is similar to the CHLD mechanism above and borrows the
   design of the mechanism for delivering Thread.Alerted.  The CHLD
   signal handler is registered for SEGV as well (see ThreadPosixC.c)
   and the old handler installation is removed from the file

   m3-libs/m3core/src/runtime/POSIX/RTSignalC.c

   SEGV is delivered here.  On a delivery of this signal all proceeds as
   a normal threadswitch except that the field self.runtimeError is updated
   to read RuntimeError.T.BadMemoryReference.  No other modifications
   are made to the thread handling except that if self is in state pausing
   it is nevertheless scheduleable (same as for waitingForSig)---not even
   sure this is necessary.

   The real trick is when the thread is switched to.  As with the case
   of delivering the exception Thread.Alerted, the manipulations span the
   end of the critical region, that is, they are prepared before the
   barrier DEC(inCritical) and executed after the barrier.  If a SEGV is
   pending, this is the point at which the exception is raised, same as
   for Thread.Alerted
*)

(* Note for people attempting to understand the code in this module:
   The real "magic" happens in the procedure Transfer.  It is vital that
   the magic only happens in one place as the procedure will appear to a
   thread to return from where it left off (i.e., it has exactly the
   semantics of a coroutine call).

   This also means that applications can only be linked with ONE
   version of Transfer (as they would otherwise return into the middle
   of a call to somewhere else, which is almost guaranteed to crash
   and burn) and that Transfer must never, never be inlined anywhere.

   Transfer should therefore be kept as small as possible and changes to
   it mean all libraries linked with m3core must be re-linked before
   linking any applications!
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
    waitingForTime: Time.T;

    (* if state = pausing, the signal that truncates the pause *)
    waitingForSig: int := -1;

    (* true if we are waiting during an AlertWait or AlertJoin
       or AlertPause *)
    alertable: BOOLEAN := FALSE;

    (* true if somebody alerted us and we did not TestAlert *)
    alertPending : BOOLEAN := FALSE;

    (* This condition is signaled then the thread terminates;
       other threads that want to join can just wait for it *)
    endCondition: Condition;
    joined: BOOLEAN := FALSE;

    (* where we carry our work *)
    handlers:    ADDRESS := NIL;
    errno:       INTEGER := 0;
    stackbase:   ADDRESS := NIL;
    sp:          ADDRESS := NIL;
    context:     ADDRESS := NIL;

    (* if state = blocking, the descriptors we are waiting on *)
    select : SelectRec := SelectRec{};

    (* state that is available to the floating point routines *)
    floatState : FloatMode.ThreadState;

    (* pending RuntimeError from signal handler *)
    runtimeError := NoXError;
  END;

TYPE XError = [ ORD(FIRST(RuntimeError.T))-1..ORD(LAST(RuntimeError.T)) ];

CONST NoXError = FIRST(XError);      (* FIRST is special *)

(*------------------------------------------------------ mutual exclusion ---*)

VAR inCritical: INTEGER;
(* inCritical provides low-level mutual exclusion between the thread
   runtime, garbage collector and the Unix signal that triggers thread
   preemption.  If inCritical is greater than zero, thread preemption
   is disabled.  We *ASSUME* that "INC(inCritical)" and "DEC(inCritical)"
   generate code that is atomic with respect to Unix signal delivery. *)

(*--------------------------------------------------------------- globals ---*)

VAR
  preemption: BOOLEAN;

CONST ZeroTimeout = 0.0D0;

VAR
  (* we start the heavy machinery only when we have more than one thread *)
  multipleThreads: BOOLEAN := FALSE;

  pausedThreads : T;

  defaultStackSize := 3000;

  (* note that even though the "heavy machinery" is only used for
     multipleThreads, we still need to set up the signal handler so
     that we can catch signals from other sources than thread switching.
     e.g., we use SIGCHLD to speed up Process.Wait *)

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

      t.context := MakeContext (RunThread, stack_size);
      IF t.context = NIL THEN
        DEC(inCritical);
        RuntimeError.Raise(RuntimeError.T.SystemError);
      END;
      CanRun (t);
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
      IF t.joined THEN
        DumpEverybody ();
        RTError.Msg(ThisFile(), ThisLine(), "attempt to join with thread twice");
      END;
      WHILE (t.state # State.dying) AND (t.state # State.dead) DO
        (*** INLINE Wait (inCritical, t.endCondition) ***)
        c := t.endCondition;
        ICannotRun (State.waiting);
        self.waitingForCondition := c;
        self.nextWaiting := c.waitingForMe;
        c.waitingForMe := self;
        TRY
          t.joined := TRUE;
          DEC (inCritical);
          InternalYield ();
          INC (inCritical);
        FINALLY
          IF inCritical = 0 THEN t.joined := FALSE END;
        END;
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
        m.release();
        ICannotRun (State.waiting);
        self.waitingForCondition := c;
        self.nextWaiting := c.waitingForMe;
        c.waitingForMe := self;
      DEC (inCritical);
      InternalYield ();
    FINALLY
      m.acquire();
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
    IF inCritical = 0 AND self # NIL THEN
      self.alertable := FALSE;
      InternalYield ();
    END;
  END Yield;

PROCEDURE Self (): T =
  BEGIN
    RETURN self;
  END Self;

(*--------------------------------------------------------------- MUTEXes ---*)

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
    RTError.Msg (ThisFile(), ThisLine(), "impossible Thread.Acquire");
  END ImpossibleAcquire;

PROCEDURE UnlockMutex (m: Mutex) =
  <*FATAL Alerted*>
  VAR waiters: BOOLEAN;
  BEGIN
    INC (inCritical);
      waiters := XRelease (m);
    DEC (inCritical);
    IF waiters AND inCritical = 0 THEN
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
    RTError.Msg (ThisFile(), ThisLine(), "illegal Thread.Release");
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

PROCEDURE ProcessStacks (p: PROCEDURE (start, limit: ADDRESS)) =
  (* we need to be careful to avoid read/write barriers here --
     hence the extensive use of LOOPHOLE. *)
  VAR
    me := LOOPHOLE(LOOPHOLE(ADR(self), UNTRACED REF ADDRESS)^, T);
    t: T;

  BEGIN
    (* flush thread state *)
    RTHeapRep.FlushThreadState(heapState);

    (* flag live thread to ProcessContext *)
    me.sp := NIL;
    <*ASSERT me.stackbase # NIL*>

    t := me;
    REPEAT
      (* t.stackbase = NIL means the thread isn't fully initialized
         yet, hasn't run, has nothing interesting on its stack. *)
      IF t.stackbase # NIL THEN
        ProcessContext(t.context, t.stackbase, t.sp, p);
      END;
      t := LOOPHOLE(LOOPHOLE(ADR(t.next), UNTRACED REF ADDRESS)^, T);
    UNTIL t = me;
  END ProcessStacks;

PROCEDURE ProcessEachStack (p: PROCEDURE (start, limit: ADDRESS)) =
  BEGIN
    ProcessStacks(p);
  END ProcessEachStack;

(*------------------------------------------------- I/O and Timer support ---*)

PROCEDURE Pause(n: LONGREAL)=
  <*FATAL Alerted*>
  VAR until := n + Time.Now ();
  BEGIN
    XPause(until, FALSE);
  END Pause;

PROCEDURE SigPause(n: LONGREAL; sig: int)=
  <*FATAL Alerted*>
  VAR until := n + Time.Now ();
  BEGIN
    XPause(until, FALSE, sig);
  END SigPause;

PROCEDURE AlertPause(n: LONGREAL) RAISES {Alerted}=
  VAR until := n + Time.Now ();
  BEGIN
    XPause(until, TRUE);
  END AlertPause;

PROCEDURE XPause (READONLY until: Time.T; alertable := FALSE; sig:int := -1)
  RAISES {Alerted} =
  BEGIN
    INC (inCritical);
      self.waitingForTime := until;
      self.alertable := alertable;
      IF sig # -1 THEN
        self.waitingForSig := sig
      END;
      ICannotRun (State.pausing);
    DEC (inCritical);
    InternalYield ();
  END XPause;

VAR
  gMaxActiveFDSet, gMaxFDSet: CARDINAL;
  gReadFDS, gWriteFDS, gExceptFDS: FDS;

  (* gMaxFDSet is NUMBER(gReadFDS^) *)
  (* gReadFDS, gWriteFDS, and gExceptFDS all have the same length *)
  (* gMaxActiveFDSet <= gMaxFDSet, gMaxActiveFDSet * FDSetSize > nFD,
     where nFD is the maximum fd active in any call to XIOWait *)
  (* gMaxFDSet never decreases *)

  (* note that using a FD beyond the range of legal FDs produces
     a checked runtime error *)

PROCEDURE IOWait(fd: CARDINAL; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult =
  <*FATAL Alerted*>
  BEGIN
    self.alertable := FALSE;
    RETURN XIOWait(fd, read, timeoutInterval);
  END IOWait;

PROCEDURE IOAlertWait(fd: CARDINAL; read: BOOLEAN;
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
      res := CallSelect(fd + 1, interval);
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
            self.select.timeout := Time.Now() + interval;
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
    FOR i := 0 TO gMaxActiveFDSet - 1 DO
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

PROCEDURE CallSelect(nfd: CARDINAL; timeout: Time.T): int =
  VAR res: int;
  BEGIN
    FOR i := 0 TO gMaxActiveFDSet - 1 DO
      gExceptFDS[i] := gReadFDS[i] + gWriteFDS[i];
    END;
    res := Select(nfd, gReadFDS[0], gWriteFDS[0], gExceptFDS[0], timeout);
    IF res > 0 THEN
      FOR i := 0 TO gMaxActiveFDSet - 1 DO
        gExceptFDS[i] := gExceptFDS[i] + gReadFDS[i] + gWriteFDS[i];
      END;
    END;
    RETURN res;
  END CallSelect;

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
  BEGIN
    IF preemption THEN
      setup_sigvtalrm (switch_thread);
      IF SetVirtualTimer() # 0 THEN
        RAISE InternalError;
      END;
      allow_sigvtalrm ();
      allow_othersigs ();
    END;
  END StartSwitching;

CONST MaxSigs = 64;
TYPE Sig = [ 0..MaxSigs-1 ];

VAR
    gotSigs := SET OF Sig { };

PROCEDURE switch_thread (sig: int) RAISES {Alerted} =
  BEGIN
    allow_sigvtalrm ();

    INC(inCritical);
    IF inCritical = 1 THEN allow_othersigs ();  END;

    (* mark signal as being delivered *)
    IF    sig = SIGSEGV THEN
      self.runtimeError := ORD(RuntimeError.T.BadMemoryReference)
    ELSIF sig >= 0 AND sig < MaxSigs THEN
      gotSigs := gotSigs + SET OF Sig { sig }
    END;
    DEC(inCritical);

    IF inCritical = 0 AND heapState.inCritical = 0 THEN
      InternalYield ()
    END;
  END switch_thread;

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
    now          : Time.T;
    earliest     : Time.T;
    selectResult := 0;
    do_alert     : BOOLEAN;
    did_delete   : BOOLEAN;
    do_error     : XError;

BEGIN
  INC (inCritical);
  <*ASSERT inCritical = 1 *>
  <*ASSERT heapState.inCritical = 0 *>

  from := self.next; (* remember where we started *)
  now            := Time.Now ();

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

            ELSIF t.waitingForSig IN gotSigs THEN
              t.waitingForSig := -1;
              CanRun(t);
              EXIT;

            ELSIF t.runtimeError # -1 THEN
              CanRun(t);
              EXIT;

            ELSIF t.waitingForTime <= now THEN
              CanRun (t);
              EXIT;

            ELSIF NOT somePausing THEN
              earliest := t.waitingForTime;
              somePausing := TRUE;

            ELSIF t.waitingForTime < earliest THEN
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
                VAR n := CallSelect(t.select.fd + 1, ZeroTimeout); BEGIN
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
              IF t.select.hasTimeout AND t.select.timeout <= now THEN
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
              ELSIF t.select.timeout < earliest THEN
                earliest := t.select.timeout;
              END
            END

        | State.dying, State.dead =>
            (* remove this guy from the ring *)
            IF perfOn THEN PerfDeleted (t.id); END;
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
          gMaxActiveFDSet := 1 + ((blockingNfds - 1) DIV FDSetSize);
        END;
        EXIT;
      END;
    END;

    gotSigs := SET OF Sig {};

    IF t.state = State.alive AND (scanned OR NOT someBlocking) THEN
      IF perfOn THEN PerfRunning (t.id); END;
      (* At least one thread wants to run; transfer to it *)
      Transfer (self, t);
      IF (dead_stacks # NIL) THEN FreeDeadStacks () END;
      do_alert := self.alertable AND self.alertPending;
      self.alertable := FALSE;
      IF do_alert THEN self.alertPending := FALSE END;

      IF NOT do_alert AND self.runtimeError # NoXError THEN
        do_error := self.runtimeError;
        self.runtimeError := NoXError;
      ELSE
        do_error := NoXError
      END;

      DEC (inCritical);
      IF do_alert THEN RAISE Alerted END;
      IF do_error # NoXError THEN
        RAISE RuntimeError.E(VAL(do_error, RuntimeError.T))
      END;
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
      (* Some parens please to disambiguate. *)
      IF t.state = State.alive OR somePausing AND earliest <= now THEN
        selectResult := CallSelect(blockingNfds, ZeroTimeout);
      ELSIF somePausing THEN
        VAR timeout := earliest - now; BEGIN
          selectResult := CallSelect(blockingNfds, timeout);
        END;
      ELSE
        selectResult := CallSelect(blockingNfds, -1.0D0 (* NIL *));
      END;
      IF selectResult <= 0 THEN now := Time.Now(); END;
      scanned := TRUE
    ELSE
      IF perfOn THEN PerfRunning (-1); END;
      DumpEverybody ();
      RTError.Msg (ThisFile(), ThisLine(), "Deadlock !");
    END;
  END;
END InternalYield;

PROCEDURE FreeDeadStacks () =
  (* blow away any dead stacks *)
  VAR x: T;  t: T := dead_stacks;
  BEGIN
    dead_stacks := NIL;
    WHILE (t # NIL) DO
      <*ASSERT t # self*>
      DisposeContext (t.context);
      x := t;
      t := t.next;
      x.next := NIL;
    END;
  END FreeDeadStacks;

PROCEDURE WaitProcess (pid: int; VAR status: int): int =
  (* ThreadPThread.m3 and ThreadPosix.m3 are very similar. *)
  CONST Delay = 0.1D0;
  BEGIN
    LOOP
      WITH r = Uexec.waitpid(pid, ADR(status), Uexec.WNOHANG) DO
        IF r # 0 THEN RETURN r END;
      END;
      IF SIGCHLD # 0 THEN
        SigPause(Delay, SIGCHLD);
      END;
    END;
  END WaitProcess;

(*-------------------------------------------------- low-level coroutines ---*)

VAR self: T;  (* the currently running thread *)

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

PROCEDURE RunThread () =
  <*FATAL Alerted*>
  VAR xx: INTEGER;
  BEGIN
    self.stackbase := ADR(xx);
    handlerStack := self.handlers;
    Cerrno.SetErrno(self.errno);
    allow_sigvtalrm ();
    allow_othersigs ();
    DEC (inCritical);

    IF debug THEN
      DumpThread(self);
    END;
    FloatMode.InitThread (self.floatState);
    self.result := self.closure.apply ();

    INC (inCritical);
    Broadcast (self.endCondition);
    RTHeapRep.FlushThreadState(heapState);
    ICannotRun (State.dying);
    INC (stats.n_dead);
    DEC (inCritical);
    IF debug THEN
      OutT("> DetermineContext: thread finished\n");
      DumpThread(self);
    END;
    InternalYield ();
    <* ASSERT FALSE *>
  END RunThread;

PROCEDURE Transfer (from, to: T) =
  VAR xx: INTEGER;
  BEGIN
    IF from # to THEN
      disallow_signals ();
      from.handlers := handlerStack;
      from.errno := Cerrno.GetErrno();
      self := to;
      from.sp := ADR(xx);
      SwapContext(from.context, to.context);
      handlerStack := from.handlers;
      Cerrno.SetErrno(from.errno);
      allow_sigvtalrm ();
      allow_othersigs ();
    END
  END Transfer;

PROCEDURE MyFPState (): UNTRACED REF FloatMode.ThreadState =
  BEGIN
    RETURN ADR(self.floatState);
  END MyFPState;

VAR heapState: RTHeapRep.ThreadState;

PROCEDURE MyHeapState (): UNTRACED REF RTHeapRep.ThreadState =
  BEGIN
    RETURN ADR(heapState);
  END MyHeapState;

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

(*--------------------------------------------------------------- ThreadF ---*)

PROCEDURE MyId(): Id RAISES {}=
  BEGIN
    RETURN self.id;
  END MyId;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Init() =
  VAR xx: INTEGER;
  BEGIN
    inCritical := 1;
      WITH r = RTProcess.RegisterForkHandlers(AtForkPrepare,
                                              AtForkParent,
                                              AtForkChild) DO
        <* ASSERT r = 0 *>
      END;

      self := NEW (T, state := State.alive, id := nextId,
                   stackbase := ADR(xx),
                   context := MakeContext(NIL, 0));
      IF self.context = NIL THEN
        RuntimeError.Raise(RuntimeError.T.SystemError);
      END;
      heapCond := NEW (Condition);
      FloatMode.InitThread (self.floatState);

      INC (nextId);

      pausedThreads := NIL;

      self.next := self;
      self.previous := self;

      gMaxActiveFDSet := 1;
      gMaxFDSet := 1;
      gReadFDS := NEW(FDS, 1);
      gWriteFDS := NEW(FDS, 1);
      gExceptFDS := NEW(FDS, 1);
    inCritical := 0;

    PerfStart ();
    preemption := NOT RTParams.IsPresent ("nopreemption");
    IF RTParams.IsPresent("backgroundgc") THEN
      RTCollectorSRC.StartBackgroundCollection();
    END;
    IF RTParams.IsPresent("foregroundgc") THEN
      RTCollectorSRC.StartForegroundCollection();
    END;
  END Init;

PROCEDURE AtForkPrepare() =
  BEGIN
    INC(inCritical);
  END AtForkPrepare;

PROCEDURE AtForkParent() =
  BEGIN
    DEC(inCritical);
  END AtForkParent;

PROCEDURE AtForkChild() =
  BEGIN
    pausedThreads := NIL;
    self.next := self;
    self.previous := self;
    multipleThreads := FALSE;
    AtForkParent();
  END AtForkChild;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector. *)

VAR
  heapCond: Condition;

PROCEDURE LockHeap () =
  BEGIN
    INC(inCritical);
  END LockHeap;

PROCEDURE UnlockHeap () =
  BEGIN
    DEC(inCritical);
  END UnlockHeap;

PROCEDURE WaitHeap () =
  <*FATAL Alerted*>
  BEGIN
    self.alertable := FALSE;
    ICannotRun (State.waiting);
    self.waitingForCondition := heapCond;
    self.nextWaiting := heapCond.waitingForMe;
    heapCond.waitingForMe := self;
    DEC(inCritical);
    <*ASSERT inCritical = 0*>
    InternalYield ();
    <*ASSERT inCritical = 0*>
    INC(inCritical);
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  BEGIN
    Broadcast(heapCond);
  END BroadcastHeap;

(*--------------------------------------------- exception handling support --*)

VAR handlerStack: ADDRESS;

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

VAR debug := RTParams.IsPresent ("debugthreads");

BEGIN
  (* we need to call set up the signal handler for other reasons than
     just thread switching now *)
  setup_sigvtalrm (switch_thread);
END ThreadPosix.
