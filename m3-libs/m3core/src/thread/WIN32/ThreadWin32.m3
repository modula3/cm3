(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE MODULE ThreadWin32 EXPORTS
Thread, ThreadF, Scheduler, RTThread, RTOS, RTHooks, ThreadWin32;

IMPORT RTError, WinGDI, RTParams, FloatMode, RuntimeError;
IMPORT MutexRep, RTHeapRep, RTCollectorSRC, RTIO, WinBase;
IMPORT ThreadEvent, RTPerfTool, RTProcess, ThreadDebug;
FROM Compiler IMPORT ThisFile, ThisLine;
FROM WinNT IMPORT DWORD, HANDLE, SIZE_T;
FROM WinBase IMPORT CloseHandle, CreateEvent, CreateThread,
    DuplicateHandle, EnterCriticalSection, GetCurrentProcess, GetCurrentThread,
    GetCurrentThreadId, GetLastError, GetThreadContext,
    LeaveCriticalSection, PCRITICAL_SECTION, ResetEvent, ResumeThread, SetEvent,
    Sleep, SuspendThread, TlsAlloc, TlsGetValue, TlsSetValue,
    WaitForMultipleObjects, WaitForSingleObject;
FROM ThreadContext IMPORT PCONTEXT;

(*----------------------------------------- Exceptions, types and globals ---*)

VAR default_stack: DWORD := 8192;

REVEAL
  Mutex = MutexRep.Public BRANDED "MUTEX Win32-1.0" OBJECT
      lock: PCRITICAL_SECTION := NIL;
      held: BOOLEAN := FALSE; (* LL = mutex.lock *) (* Because critical sections are thread re-entrant *)
    OVERRIDES
      acquire := LockMutex;
      release := UnlockMutex;
    END;

  Condition = BRANDED "Thread.Condition Win32-1.0" OBJECT
  (* see C:\src\jdk-6u14-ea-src-b05-jrl-23_apr_2009\hotspot\agent\src\os\win32\Monitor.cpp
   * http://www.cs.wustl.edu/~schmidt/win32-cv-1.html
   *   "3.3. The Generation Count Solution"
   *)
 
      lock: PCRITICAL_SECTION := NIL;
      waitEvent: HANDLE := NIL;
      counter := 0; (* LL = condition.lock *)
      tickets := 0; (* LL = condition.lock *)
      waiters := 0; (* LL = condition.lock *)
    END;

  T = MUTEX BRANDED "Thread.T Win32-1.0" OBJECT
      act: Activation := NIL;       (* LL = Self(); live (untraced) thread data *)
      closure: Closure := NIL;      (* our work and its result *)
      result: REFANY := NIL;        (* our work and its result *)
      joined (*BOOLEAN*) := 0;      (* "Join" or "AlertJoin" has already returned *)
    END;

  TYPE ActState = { Starting, Started, Stopping, Stopped };
  TYPE Activation = UNTRACED BRANDED REF RECORD
      frame: ADDRESS := NIL;            (* exception handling support; this field is accessed MANY times
                                         * so perhaps therefore should be first *)
      next, prev: Activation := NIL;    (* LL = activeLock; global doubly-linked, circular list of all active threads *)
      handle: HANDLE := NIL;            (* thread handle in Windows *)
      stackStart: ADDRESS := NIL;       (* stack bounds for use by GC; the lowest address in a growing-down stack *)
      stackEnd: ADDRESS := NIL;         (* stack bounds for use by GC; just past the highest address in a growing-down stack *)
      slot := 0;                        (* LL = slotLock;  index into global array of active, slotted threads *)
      suspendCount := 0;                (* LL = activeLock *)
      context: PCONTEXT := NIL;         (* registers of suspended thread *)
      stackPointer: ADDRESS := NIL;     (* context->Esp etc. (processor dependent) *)
      waitEvent: HANDLE := NIL;         (* event for blocking during "Wait", "AlertWait", "AlertPause", etc. *)
      alertEvent: HANDLE := NIL;        (* event for blocking during "Wait", "AlertWait", "AlertPause", etc. *)
      state := ActState.Started;        (* LL = activeMu *)
      heapState: RTHeapRep.ThreadState; (* thread state *)
      floatState: FloatMode.ThreadState; (* thread state *)
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

PROCEDURE CleanMutex (r: REFANY) =
  VAR m := NARROW(r, Mutex);
  BEGIN
    DelCriticalSection(m.lock);
  END CleanMutex;

PROCEDURE DelHandle(VAR a: HANDLE; line: INTEGER) =
  BEGIN
    IF a # NIL THEN
      IF CloseHandle(a) = 0 THEN Choke(line) END;
      a := NIL;
    END;
  END DelHandle;

PROCEDURE InitMutex (mutex: Mutex) =
  VAR lock := NewCriticalSection();
  BEGIN
    IF lock = NIL THEN
      IF mutex.lock # NIL THEN (* Someone else won the race. *)
        RETURN;
      END;
      RuntimeError.Raise (RuntimeError.T.OutOfMemory)
    END;
    EnterCriticalSection(ADR(initLock));
    IF mutex.lock # NIL THEN
      (* Someone else won the race. *)
      DelCriticalSection(lock);
      LeaveCriticalSection(ADR(initLock));
      RETURN;
    END;
    (* We won the race. *)
    TRY
      RTHeapRep.RegisterFinalCleanup (mutex, CleanMutex);
      mutex.lock := lock;
      lock := NIL;
    FINALLY
      LeaveCriticalSection(ADR(initLock));
      DelCriticalSection(lock);
    END;
  END InitMutex;      

PROCEDURE CleanCondition (r: REFANY) =
  VAR c := NARROW(r, Condition);
  BEGIN
    DelCriticalSection(c.lock);
    DelHandle(c.waitEvent, ThisLine());
  END CleanCondition;

PROCEDURE InitCondition (c: Condition) =
  VAR
    lock := NewCriticalSection();
    event := CreateEvent(NIL, 1, 0, NIL);
  BEGIN
    IF lock = NIL OR event = NIL THEN
      DelCriticalSection(lock);
      DelHandle(event, ThisLine());
      IF c.lock # NIL AND c.waitEvent # NIL THEN (* Someone else won the race. *)
        RETURN;
      END;
      RuntimeError.Raise (RuntimeError.T.OutOfMemory);
    END;
    EnterCriticalSection(ADR(initLock));
    IF c.lock # NIL THEN
      (* Someone else won the race. *)
      LeaveCriticalSection(ADR(initLock));
      DelCriticalSection(lock);
      DelHandle(event, ThisLine());
      RETURN;
    END;
    (* We won the race. *)
    TRY
      RTHeapRep.RegisterFinalCleanup (c, CleanCondition);
      c.lock := lock;
      c.waitEvent := event;
      lock := NIL;
      event := NIL;
    FINALLY
      LeaveCriticalSection(ADR(initLock));
      DelCriticalSection(lock);
      DelHandle(event, ThisLine());
    END;
  END InitCondition;

PROCEDURE LockMutex (m: Mutex) =
  BEGIN
    IF perfOn THEN PerfChanged(State.locking) END;
    IF m.lock = NIL THEN InitMutex(m) END;
    EnterCriticalSection(m.lock);
    IF m.held THEN Die(ThisLine(), "attempt to lock mutex already locked by self") END;
    m.held := TRUE;
    IF perfOn THEN PerfRunning() END;
  END LockMutex;

PROCEDURE UnlockMutex(m: Mutex) =
  BEGIN
    IF NOT m.held THEN Die(ThisLine(), "attempt to release an unlocked mutex") END;
    m.held := FALSE;
    LeaveCriticalSection(m.lock);
  END UnlockMutex;

(**********
PROCEDURE DumpSlots () =
  VAR me := GetActivation();
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

PROCEDURE XWait(m: Mutex; c: Condition; act: Activation;
                alertable: BOOLEAN) RAISES {Alerted} =
  (* LL = m on entry and exit, but not for the duration
   * see C:\src\jdk-6u14-ea-src-b05-jrl-23_apr_2009\hotspot\agent\src\os\win32\Monitor.cpp
   * NOTE that they merge the user lock and the condition lock.
   * http://www.cs.wustl.edu/~schmidt/win32-cv-1.html
   *   "3.3. The Generation Count Solution"
   *)
  VAR (* The order of the handles is important.
       * - They affect computing if we are alerted.
       * - If we are alerted and signaled, we take the signaled path
       *   in order to maintain the condition variable correctly.
       *   If both events get signaled, WaitForMultipleObjects returns the smaller index.
       *)
      handles := ARRAY [0..1] OF HANDLE {NIL(*c.waitEvent*), act.alertEvent};
      count: INTEGER;
      retry := FALSE;
      wait: DWORD;
      conditionLock := c.lock;
      waitDone := FALSE;
      alerted := FALSE;
      lastWaiter := FALSE;
  BEGIN

    IF DEBUG THEN ThreadDebug.XWait(m, c, act); END;

    IF conditionLock = NIL THEN
      InitCondition(c);
      conditionLock := c.lock;
    END;
    <* ASSERT conditionLock # NIL *>
    <* ASSERT c.waitEvent # NIL *>
    <* ASSERT act.alertEvent # NIL *>

    handles[0] := c.waitEvent;
    EnterCriticalSection(conditionLock);

      (* Capture the value of the counter before we start waiting.
       * We will not stop waiting until the counter changes.
       * That is, we will not stop waiting until a signal
       * comes in after we start waiting.
       *)

      count := c.counter;
      INC(c.waiters);

    LeaveCriticalSection(conditionLock);
    m.release(); (* can this be moved to before taking conditionLock? *)

    (* Loop until condition variable is signaled. The event object is
     * set whenever the condition variable is signaled, and tickets will
     * reflect the number of threads which have been notified. The counter
     * field is used to make sure we don't respond to notifications that
     * have occurred *before* we started waiting, and is incremented each
     * time the condition variable is signaled.
     *)

    WHILE (NOT alerted) AND (NOT waitDone) DO

      IF perfOn THEN PerfChanged(State.waiting) END;

      (* If this is a retry, let other low-priority threads run.
       * Be sure to sleep outside of critical section.
       *)

      IF retry THEN
        Sleep(1);
      ELSE
        retry := TRUE;
      END;

      wait := WaitForMultipleObjects(1 + ORD(alertable), ADR(handles[0]), 0, INFINITE);
      <* ASSERT wait = WAIT_OBJECT_0 OR wait = (WAIT_OBJECT_0 + 1) *>

      IF perfOn THEN PerfRunning() END;

      alerted := (wait = (WAIT_OBJECT_0 + 1));
      EnterCriticalSection(conditionLock);
        waitDone := (c.tickets # 0 AND c.counter # count);
      LeaveCriticalSection(conditionLock);

    END; (* WHILE *)

    IF waitDone THEN
      alerted := FALSE;
    END;

    m.acquire();

    EnterCriticalSection(conditionLock);
      DEC(c.waiters);
      IF waitDone THEN
        DEC(c.tickets);
        lastWaiter := (c.tickets = 0);
      END;
    LeaveCriticalSection(conditionLock);

    IF alerted THEN
      RAISE Alerted;
    END;

    (* If this was the last thread to be notified, then reset event
     * so no further threads are woken, for now.
     *)

    IF lastWaiter THEN
      IF ResetEvent(c.waitEvent) = 0 THEN Choke(ThisLine()) END;
    END;

  END XWait;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := GetActivation();
  BEGIN
    IF DEBUG THEN ThreadDebug.AlertWait(m, c); END;
    IF self = NIL THEN Die(ThisLine(), "AlertWait called from non-Modula-3 thread") END;
    XWait(m, c, self, alertable := TRUE);
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  <* FATAL Alerted *>
  (* LL = m *)
  VAR self := GetActivation();
  BEGIN
    IF DEBUG THEN ThreadDebug.Wait(m, c); END;
    IF self = NIL THEN Die(ThisLine(), "Wait called from non-Modula-3 thread") END;
    XWait(m, c, self, alertable := FALSE);
  END Wait;

PROCEDURE Signal (c: Condition) =
  VAR conditionLock := c.lock;
  BEGIN
    IF DEBUG THEN ThreadDebug.Signal(c); END;

    IF conditionLock = NIL THEN
      InitCondition(c);
      conditionLock := c.lock;
    END;
    <* ASSERT conditionLock # NIL *>
    <* ASSERT c.waitEvent # NIL *>

    EnterCriticalSection(conditionLock);

      IF c.waiters > c.tickets THEN
        IF SetEvent(c.waitEvent) = 0 THEN Choke(ThisLine()) END;
        INC(c.tickets);
        INC(c.counter);
      END;

    LeaveCriticalSection(conditionLock);

  END Signal;

PROCEDURE Broadcast (c: Condition) =
  VAR conditionLock := c.lock;
  BEGIN
    IF DEBUG THEN ThreadDebug.Broadcast(c); END;

    IF conditionLock = NIL THEN
      InitCondition(c);
      conditionLock := c.lock;
    END;
    <* ASSERT conditionLock # NIL *>
    <* ASSERT c.waitEvent # NIL *>

    EnterCriticalSection(conditionLock);

      IF c.waiters > 0 THEN
        IF SetEvent(c.waitEvent) = 0 THEN Choke(ThisLine()) END;
        c.tickets := c.waiters;
        INC(c.counter);
      END;

    LeaveCriticalSection(conditionLock);

  END Broadcast;

PROCEDURE Alert(t: T) =
  BEGIN
    IF DEBUG THEN ThreadDebug.Alert(t); END;
    IF t = NIL THEN Die(ThisLine(), "Alert called from non-Modula-3 thread") END;
    IF SetEvent(t.act.alertEvent) = 0 THEN Choke(ThisLine()) END;
  END Alert;

PROCEDURE XTestAlert(self: Activation): BOOLEAN =
  VAR wait: DWORD;
  BEGIN
    IF self = NIL THEN
      (* Not created by Fork; not alertable *)
      RETURN FALSE
    ELSE
      <* ASSERT self.alertEvent # NIL *>
      wait := WaitForSingleObject(self.alertEvent, 0);
      <* ASSERT wait = WAIT_TIMEOUT OR wait = WAIT_OBJECT_0 *>
      RETURN (wait = WAIT_OBJECT_0);
    END;
  END XTestAlert;

PROCEDURE TestAlert(): BOOLEAN =
  BEGIN
    IF DEBUG THEN ThreadDebug.TestAlert(); END;
    RETURN XTestAlert(GetActivation());
  END TestAlert;

(*------------------------------------------------------------------ Self ---*)

VAR (* LL = slotLock *)
  n_slotted := 0;
  next_slot := 1;
  slots     : REF ARRAY OF T;  (* NOTE: we don't use slots[0]. *)

PROCEDURE Self (): T =
  (* If not the initial thread and not created by Fork, returns NIL *)
  (* LL = 0 *)
  VAR me := GetActivation();
      t: T;
  BEGIN
    IF me = NIL THEN RETURN NIL; END;
    EnterCriticalSection(ADR(slotLock));
      t := slots[me.slot];
      IF t.act # me THEN Die (ThisLine(), "thread with bad slot!"); END;
    LeaveCriticalSection(ADR(slotLock));
    RETURN t;
  END Self;

PROCEDURE AssignSlot (t: T) =
  (* LL = 0, cause we allocate stuff with NEW! *)
  VAR n: CARDINAL;  old_slots, new_slots: REF ARRAY OF T;
      retry := TRUE;
  BEGIN
    EnterCriticalSection(ADR(slotLock));
      WHILE retry DO
        retry := FALSE;

        (* make sure we have room to register this guy *)
        IF slots = NIL THEN
          LeaveCriticalSection(ADR(slotLock));
            slots := NEW (REF ARRAY OF T, 20);
          EnterCriticalSection(ADR(slotLock));
        END;
        IF n_slotted >= LAST (slots^) THEN
          old_slots := slots;
          n := NUMBER (old_slots^);
          LeaveCriticalSection(ADR(slotLock));
            new_slots := NEW (REF ARRAY OF T, n+n);
          EnterCriticalSection(ADR(slotLock));
          IF old_slots = slots THEN
            (* we won any races that may have occurred. *)
            SUBARRAY (new_slots^, 0, n) := slots^;
            slots := new_slots;
          ELSIF n_slotted < LAST (slots^) THEN
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

      INC (n_slotted);
      t.act.slot := next_slot;
      slots [next_slot] := t;

    LeaveCriticalSection(ADR(slotLock));
  END AssignSlot;

PROCEDURE FreeSlot (self: T) =
  (* LL = 0 *)
  BEGIN
    EnterCriticalSection(ADR(slotLock));
      <* ASSERT self.act.slot > 0 *>
      <* ASSERT n_slotted > 0 *>
      DEC(n_slotted);
      WITH z = slots [self.act.slot] DO
        IF z # self THEN Die (ThisLine(), "unslotted thread!"); END;
        z := NIL;
      END;
      self.act.slot := 0;
    LeaveCriticalSection(ADR(slotLock));
  END FreeSlot;

(*------------------------------------------------------------ Fork, Join ---*)

VAR (* LL=activeLock *)
  allThreads: Activation := NIL;  (* global list of active threads *)

PROCEDURE CreateT (act: Activation): T =
  (* LL = 0, because allocating a traced reference may cause
     the allocator to start a collection which will call "SuspendOthers"
     which will try to acquire "activeLock". *)
  VAR t: T := NEW(T, act := act);
  BEGIN
    TRY
      RTHeapRep.RegisterFinalCleanup (t, CleanThread);
      act := NIL;
    FINALLY
      DISPOSE(act);
    END;
    t.act.context := NewContext();
    t.act.waitEvent := CreateEvent(NIL, 0, 0, NIL);
    t.act.alertEvent := CreateEvent(NIL, 0, 0, NIL);
    IF t.act.context = NIL OR t.act.waitEvent = NIL OR t.act.alertEvent = NIL THEN
      (* we could just let the registered cleanup run, but memory is tight *)
      CleanThread(t);
      RuntimeError.Raise(RuntimeError.T.SystemError);
    END;
    AssignSlot (t);
    RETURN t;
  END CreateT;

PROCEDURE CleanThread(r: REFANY) =
  VAR t: T;
  BEGIN
    IF r # NIL THEN
      t := NARROW(r, T);
      DeleteContext(t.act.context);
      DelHandle(t.act.waitEvent, ThisLine());
      DelHandle(t.act.alertEvent, ThisLine());
      DelHandle(t.act.handle, ThisLine());
      DISPOSE(t.act);
    END;
  END CleanThread;

<* WINAPI *>
PROCEDURE ThreadBase (param: ADDRESS): DWORD =
  VAR me: Activation := param;
      self: T := NIL;
  BEGIN
    SetActivation (me);
    (* We need to establish this binding before this thread touches any
       traced references.  Otherwise, it may trigger a heap page fault,
       which would call SuspendOthers, which requires an Activation. *)

    IF DEBUG THEN
      RTIO.PutText("threadbase act="); RTIO.PutAddr(me.handle); RTIO.PutText("\n"); RTIO.Flush();
    END;

    GetStackBounds(me.stackStart, me.stackEnd); (* enable GC scanning of this stack *)

    (* add to the list of active threads *)
    <* ASSERT allThreads # NIL *>
    EnterCriticalSection(ADR(activeLock));
      me.next := allThreads;
      me.prev := allThreads.prev;
      allThreads.prev.next := me;
      allThreads.prev := me;
    LeaveCriticalSection(ADR(activeLock));

    (* begin "RunThread" *)

        IF DEBUG THEN ThreadDebug.RunThread(); END;
        IF perfOn THEN PerfChanged(State.alive) END;

        self := slots [me.slot]; (* produce traced references (slots briefly and self) *)

        IF perfOn THEN PerfRunning() END;

        (*** Run the user-level code. ***)
        self.result := self.closure.apply();

        IF perfOn THEN PerfChanged(State.dying) END;
        (* What is the point of this in-between state? *)
        IF perfOn THEN PerfChanged(State.dead) END;

        (* we're dying *)
        RTHeapRep.FlushThreadState(me.heapState);

        IF perfOn THEN PerfDeleted() END;
        FreeSlot(self);  (* note: needs self.act ! *)
        (* Since we're no longer slotted, we cannot touch traced refs. *)

        self := NIL; (* drop traced reference *)
        me.stackStart := NIL; (* disable GC scanning of my stack *)
        me.stackEnd := NIL;

    (* end "RunThread" *)

    (* remove from the list of active threads *)
    <* ASSERT allThreads # NIL *>
    <* ASSERT allThreads # me *>
    EnterCriticalSection(ADR(activeLock));
      me.next.prev := me.prev;
      me.prev.next := me.next;
    LeaveCriticalSection(ADR(activeLock));
    me.next := NIL;
    me.prev := NIL;

    EVAL WinGDI.GdiFlush ();  (* help out Trestle *)

    SetActivation(NIL);
    RETURN 0;
  END ThreadBase;

PROCEDURE Fork(closure: Closure): T =
  VAR act := NEW(Activation);
      t := CreateT(act);
      stack_size := default_stack;
      id: DWORD;
  BEGIN
    IF DEBUG THEN ThreadDebug.Fork(); END;

    <* ASSERT allThreads # NIL *>
    <* ASSERT allThreads.next # NIL *>
    <* ASSERT allThreads.prev # NIL *>

    (* determine the initial size of the stack for this thread *)
    TYPECASE closure OF
    | SizedClosure (scl) => IF scl.stackSize # 0 THEN 
                              stack_size := scl.stackSize * BYTESIZE(INTEGER);
                            END;
    ELSE (*skip*)
    END;

    t.closure := closure;

    (* create suspended just so we can store act.handle before it runs;
     * act.handle := CreateThread() is not good enough. *)

    act.handle := CreateThread(NIL, stack_size, ThreadBase, act, CREATE_SUSPENDED, ADR(id));
    IF act.handle = NIL THEN
      RuntimeError.Raise(RuntimeError.T.SystemError);
    END;
    IF ResumeThread(t.act.handle) = -1 THEN Choke(ThisLine()) END;
    RETURN t;
  END Fork;

PROCEDURE XJoin (t: T; act: Activation; alertable: BOOLEAN): REFANY RAISES {Alerted} =
  VAR (* The order of the handles is important. *)
      handles := ARRAY [0..1] OF HANDLE {t.act.handle, NIL(*alertEvent*)};
      wait: DWORD;
  BEGIN
    IF t.joined # 0 THEN Die(ThisLine(), "attempt to join with thread twice") END;
    <* ASSERT handles[0] # NIL *>
    IF alertable THEN
      <* ASSERT act # NIL *>
      <* ASSERT act.alertEvent # NIL *>
      handles[1] := act.alertEvent;
      <* ASSERT handles[1] # NIL *>
    END;
    wait := WaitForMultipleObjects(1 + ORD(alertable), ADR(handles[0]), 0, INFINITE);
    <* ASSERT wait = WAIT_OBJECT_0 OR wait = (WAIT_OBJECT_0 + 1) *>
    IF wait = WAIT_OBJECT_0 THEN
      t.joined := 1;
      RETURN t.result;
    ELSE
      <* ASSERT alertable *>
      RAISE Alerted;
    END;
  END XJoin;

PROCEDURE Join(t: T): REFANY =
  <* FATAL Alerted *>
  BEGIN
    IF DEBUG THEN ThreadDebug.Join(t); END;
    RETURN XJoin(t, NIL, alertable := FALSE);
  END Join;

PROCEDURE AlertJoin(t: T): REFANY RAISES {Alerted} =
  VAR self := GetActivation();
  BEGIN
    IF DEBUG THEN ThreadDebug.AlertJoin(t); END;
    IF self = NIL THEN Die(ThisLine(), "AlertJoin called from a non-Modula-3 thread") END;
    RETURN XJoin(t, self, alertable := TRUE);
  END AlertJoin;

(*---------------------------------------------------- Scheduling support ---*)

PROCEDURE Pause(n: LONGREAL) =
  <* FATAL Alerted *>
  BEGIN
    XPause(NIL, n, alertable := FALSE);
  END Pause;

PROCEDURE AlertPause(n: LONGREAL) RAISES {Alerted} =
  VAR self := GetActivation();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "AlertPause called from a non-Modula-3 thread") END;
    XPause(self, n, alertable := TRUE);
  END AlertPause;

PROCEDURE XPause(self: Activation; n: LONGREAL; alertable: BOOLEAN) RAISES {Alerted} =
  VAR amount := n;
      thisTime: LONGREAL;
      wait: DWORD;
      alerted := FALSE;
  CONST LAST_CARDINAL32 = 16_7FFFFFFF;
        Limit = FLOAT(LAST_CARDINAL32, LONGREAL) / 1000.0D0 - 1.0D0;
  BEGIN

    IF DEBUG THEN ThreadDebug.XPause(self, n, alertable); END;

    IF alertable THEN
      <* ASSERT self # NIL *>
      <* ASSERT self.alertEvent # NIL *>
    END;

    IF amount <= 0.0d0 THEN
      IF alertable THEN
        IF XTestAlert(self) THEN
          RAISE Alerted;
        END;
      END;
      RETURN;
    END;

    IF perfOn THEN PerfChanged(State.pausing) END;

    (* Loop to handle waiting more than 4 billion milliseconds. *)

    WHILE (NOT alerted) AND (amount > 0.0D0) DO
      thisTime := MIN (Limit, amount);
      amount := amount - thisTime;
      IF alertable THEN
        wait := WaitForSingleObject(self.alertEvent, ROUND(thisTime*1000.0D0));
        <* ASSERT wait = WAIT_TIMEOUT OR wait = WAIT_OBJECT_0 *>
        alerted := (wait = WAIT_OBJECT_0);
      ELSE
        Sleep(ROUND(thisTime*1000.0D0));
      END;
    END;
    IF perfOn THEN PerfRunning() END;
    IF alerted THEN
      RAISE Alerted;
    END;
  END XPause;

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

VAR suspend_cnt: CARDINAL := 0; (* LL = activeLock *)

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

  IF GetThreadContext(act.handle, act.context) = 0 THEN Choke(ThisLine()) END;
  act.stackPointer := StackPointerFromContext(act.context);
  RETURN (act.stackPointer >= act.stackStart AND act.stackPointer < act.stackEnd);

END GetContextAndCheckStack;

PROCEDURE SuspendOthers () =
  (* LL=0. Always bracketed with ResumeOthers which releases "activeLock". *)
  VAR me: Activation;
      act: Activation;
      retry: BOOLEAN;
  BEGIN
    EnterCriticalSection(ADR(activeLock));

    <* ASSERT suspend_cnt = 0 *>
    INC (suspend_cnt);
    IF suspend_cnt # 1 THEN
      RETURN
    END;
    me := GetActivation();
    LOOP
      act := me.next;
      retry := FALSE;
      WHILE act # me DO
        <* ASSERT act.suspendCount = 0 OR act.suspendCount = 1 *>
        IF act.suspendCount = 0 THEN
          SetState(act, ActState.Stopping);
          IF act.stackStart # NIL AND act.stackEnd # NIL THEN
            IF SuspendThread(act.handle) = -1 THEN Choke(ThisLine()) END;
            IF act.heapState.inCritical # 0 OR NOT GetContextAndCheckStack(act) THEN
              IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
              retry := TRUE;
              SetState(act, ActState.Started);
            ELSE
              INC(act.suspendCount);
              <* ASSERT act.suspendCount = 1 *>
              SetState(act, ActState.Stopped);
            END;
          END;
        END;
        act := act.next;
      END;
      IF NOT retry THEN
        RETURN;
      END;
      Sleep(1);
    END;
  END SuspendOthers;

PROCEDURE ResumeOthers () =
  (* LL=activeLock.  Always preceded by SuspendOthers. *)
  VAR act: Activation;
      me: Activation;
  BEGIN
    <* ASSERT suspend_cnt = 1 *>
    DEC (suspend_cnt);
    IF suspend_cnt = 0 THEN
      me := GetActivation();
      act := me.next;
      WHILE act # me DO
        IF DEBUG THEN
          RTIO.PutText("resuming act="); RTIO.PutAddr(act.handle); RTIO.PutText("\n"); RTIO.Flush();
        END;
        <* ASSERT (act.suspendCount > 0 AND act.stackPointer # NIL) OR (act.stackStart = NIL AND act.stackEnd = NIL) *>
        <* ASSERT act.suspendCount = 0 OR act.suspendCount = 1 *>
        act.stackPointer := NIL;
        IF act.suspendCount > 0 THEN
          SetState(act, ActState.Starting);
          IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
          DEC(act.suspendCount);
          <* ASSERT act.suspendCount = 0 *>
          SetState(act, ActState.Started);
        END;
        act := act.next;
      END;
    END;

    LeaveCriticalSection(ADR(activeLock));
  END ResumeOthers;

PROCEDURE ProcessStacks (p: PROCEDURE (start, limit: ADDRESS)) =
  (* LL=activeLock.  Only called within {SuspendOthers, ResumeOthers} *)
  VAR me := GetActivation();
      act := me.next;
  BEGIN
    ProcessMe(me, p);
    WHILE act # me DO
      ProcessOther(act, p);
      act := act.next;
    END;
  END ProcessStacks;

PROCEDURE ProcessMe (me: Activation;  p: PROCEDURE (start, limit: ADDRESS)) =
  (* LL=activeLock *)
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
    RTHeapRep.FlushThreadState(act.heapState);
    ProcessStopped(act.stackStart, act.stackEnd, act.context, p);
  END ProcessOther;

PROCEDURE ProcessEachStack (<* UNUSED *>p: PROCEDURE (start, limit: ADDRESS)) =
  BEGIN
    (* experimental, unimplemented here *)
    <* ASSERT FALSE *>
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

VAR perfW : RTPerfTool.Handle;
    (* perfOn: BOOLEAN := FALSE;    (* LL = perfLock *) *)

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
  VAR e := ThreadEvent.T {kind := TE.Changed, id := GetCurrentThreadId(), state := s};
  BEGIN
    EnterCriticalSection(ADR(perfLock));
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    LeaveCriticalSection(ADR(perfLock));
  END PerfChanged;

PROCEDURE PerfDeleted () =
  VAR e := ThreadEvent.T {kind := TE.Deleted, id := GetCurrentThreadId()};
  BEGIN
    EnterCriticalSection(ADR(perfLock));
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    LeaveCriticalSection(ADR(perfLock));
  END PerfDeleted;

PROCEDURE PerfRunning () =
  VAR e := ThreadEvent.T {kind := TE.Running, id := GetCurrentThreadId()};
  BEGIN
    EnterCriticalSection(ADR(perfLock));
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    LeaveCriticalSection(ADR(perfLock));
  END PerfRunning;

(*-------------------------------------------------------- Initialization ---*)

PROCEDURE SetActivation(act: Activation) =
  (* LL = 0 *)
  VAR success := (threadIndex # TLS_OUT_OF_INDEXES);
  BEGIN
    <* ASSERT success *>
    success := (0 # TlsSetValue(threadIndex, LOOPHOLE(act, SIZE_T))); (* NOTE: This CAN fail. *)
    <* ASSERT success *>
  END SetActivation;

PROCEDURE GetActivation(): Activation =
  (* If not the initial thread and not created by Fork, returns NIL *)
  (* LL = 0 *)
  (* This function is called VERY frequently. *)
  BEGIN
    <* ASSERT threadIndex # TLS_OUT_OF_INDEXES *>
    RETURN LOOPHOLE(TlsGetValue(threadIndex), Activation);
  END GetActivation;

PROCEDURE Init() =
(* NOTE: We never cleanup; this would be a job for DllMain(DLL_PROCESS_DETACH)
 *        if we implemented it.
 * NOTE: Be careful not use <* ASSERT *> too early.
 *       Assertion failures depend on some of Init having run. (e.g. SetActivation)
 *       Test by making the ASSERT fail.
 *)
  VAR self: T;
      me := NEW(Activation);
  BEGIN
    WinBase.InitializeCriticalSection(ADR(activeLock));
    WinBase.InitializeCriticalSection(ADR(heapLock));
    WinBase.InitializeCriticalSection(ADR(perfLock));
    WinBase.InitializeCriticalSection(ADR(slotLock));
    WinBase.InitializeCriticalSection(ADR(initLock));

    (* This CAN fail under low resources. *)
    threadIndex := TlsAlloc();
    IF threadIndex = TLS_OUT_OF_INDEXES THEN Choke(ThisLine()) END;

    (* This CAN fail under very low resources? *)
    IF DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
            GetCurrentProcess(), ADR(me.handle), 0, 0, DUPLICATE_SAME_ACCESS) = 0 THEN
      Choke(ThisLine());
    END;

    me.next := me;
    me.prev := me;
    SetActivation(me);
    <* ASSERT allThreads = NIL *>
    allThreads := me;
    IF me.handle = NIL THEN
      Choke(ThisLine());
    END;

    self := CreateT(me);

    HeapWaitMutex := NEW(MUTEX);
    HeapWaitCondition := NEW(Condition);

    GetStackBounds(me.stackStart, me.stackEnd);
    IF me.stackStart = NIL OR me.stackEnd = NIL THEN Choke(ThisLine()); END;

    IF DEBUG THEN
      RTIO.PutText("created initial act="); RTIO.PutAddr(me.handle); RTIO.PutText("\n"); RTIO.Flush();
    END;

    <* ASSERT HeapInCritical = 1 *>
    HeapInCritical := 0;

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

VAR HeapInCritical := 1;   (* LL = heap *)
    HeapDoSignal: BOOLEAN; (* LL = heap *)
    HeapWaitMutex: MUTEX;
    HeapWaitCondition: Condition;

PROCEDURE LockHeap () =
  BEGIN
    IF DEBUG THEN ThreadDebug.LockHeap(); END;
    EnterCriticalSection(ADR(heapLock));
    INC(HeapInCritical);
  END LockHeap;

PROCEDURE UnlockHeap () =
  VAR sig := FALSE;
  BEGIN   
    IF DEBUG THEN ThreadDebug.UnlockHeap(); END;
    DEC(HeapInCritical);
    IF HeapInCritical = 0 AND HeapDoSignal THEN
      HeapDoSignal := FALSE;
      sig := TRUE;
    END;
    LeaveCriticalSection(ADR(heapLock));
    IF sig THEN Broadcast(HeapWaitCondition); END;
  END UnlockHeap;

PROCEDURE WaitHeap () =
  BEGIN
    IF DEBUG THEN ThreadDebug.WaitHeap(); END;
    LOCK HeapWaitMutex DO
      DEC(HeapInCritical);
      <* ASSERT HeapInCritical = 0 *>
      LeaveCriticalSection(ADR(heapLock));
      Wait(HeapWaitMutex, HeapWaitCondition);
      EnterCriticalSection(ADR(heapLock));
      <* ASSERT HeapInCritical = 0 *>
      INC(HeapInCritical);
    END;
  END WaitHeap;

PROCEDURE BroadcastHeap () =
  (* LL >= RTOS.LockHeap *)
  BEGIN
    IF DEBUG THEN ThreadDebug.BroadcastHeap(); END;
    <* ASSERT HeapInCritical # 0 *>
    HeapDoSignal := TRUE;
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

(* RTHooks.PopEFrame *)
PROCEDURE PopEFrame (frame: ADDRESS) =
  BEGIN
    GetActivation().frame := frame;
  END PopEFrame;

VAR DEBUG := RTParams.IsPresent("debugthreads");

BEGIN
END ThreadWin32.
