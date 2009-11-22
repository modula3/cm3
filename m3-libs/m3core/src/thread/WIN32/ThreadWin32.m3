(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE MODULE ThreadWin32 EXPORTS RTHooks, RTOS, Scheduler, Thread,
ThreadF, ThreadInternal, ThreadWin32;

IMPORT RTError, WinGDI, RTParams, RuntimeError;
IMPORT ThreadContext, Word, MutexRep, RTHeapRep, RTCollectorSRC;
IMPORT ThreadEvent, RTPerfTool, RTProcess;
FROM Compiler IMPORT ThisFile, ThisLine;
FROM WinNT IMPORT HANDLE, DWORD, SIZE_T, DUPLICATE_SAME_ACCESS,
    MEMORY_BASIC_INFORMATION, PAGE_READWRITE, PAGE_READONLY;
FROM WinBase IMPORT WaitForSingleObject, INFINITE, ReleaseSemaphore,
    GetCurrentProcess, DuplicateHandle, GetCurrentThread, CreateSemaphore,
    CloseHandle, CreateThread, ResumeThread, Sleep, SuspendThread,
    GetThreadContext, VirtualQuery, GetLastError, CREATE_SUSPENDED,
    GetCurrentThreadId;

(*----------------------------------------- Exceptions, types and globals ---*)

VAR
  default_stack: DWORD := 8192;

REVEAL
  Mutex = MutexRep.Public BRANDED "MUTEX Win32-1.0" OBJECT
      waiters: T := NIL;
        (* LL = giant; List of threads waiting on this mutex. *)
      holder: T := NIL;
        (* LL = giant; The thread currently holding this mutex. *)
    OVERRIDES
      acquire := LockMutex;
      release := UnlockMutex;
    END;

  Condition = BRANDED "Thread.Condition Win32-1.0" OBJECT
      waiters: T := NIL;
        (* LL = giant; List of threads waiting on this CV. *)
    END;

  T = MUTEX BRANDED "Thread.T Win32-1.0" OBJECT
      act: Activation := NIL;
        (* LL = Self();  live thread data *)
      closure: Closure := NIL;
        (* LL = Self() *)
      result: REFANY := NIL;
        (* LL = Self();  if not self.completed, used only by self;
           if self.completed, read-only. *)
      join: Condition;
        (* LL = Self(); wait here to join *)
      waitingOn: Condition := NIL;
        (* LL = giant; CV that we're blocked on *)
      nextWaiter: T := NIL;
        (* LL = giant; queue of threads waiting on the same CV *)
      waitSema: HANDLE := NIL;
        (* binary semaphore for blocking during "Wait" *)
      alertable: BOOLEAN := FALSE;
        (* LL = giant; distinguishes between "Wait" and "AlertWait" *)
      alerted: BOOLEAN := FALSE;
        (* LL = giant; the alert flag, of course *)
      completed: BOOLEAN := FALSE;
        (* LL = Self(); indicates that "result" is set *)
      joined: BOOLEAN := FALSE;
        (* LL = Self(); "Join" or "AlertJoin" has already returned *)
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
      suspendCount := 1;

      (* thread state *)
      heapState: RTHeapRep.ThreadState;
      floatState: FloatMode.ThreadState;
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
    IF perfOn THEN PerfChanged(State.locking) END;

    EnterCriticalSection_giant();

      self.alertable := FALSE;
      IF m.holder = NIL THEN
        m.holder := self;  (* I get it! *)
      ELSIF m.holder = self THEN
        Die(ThisLine(), "Attempt to lock mutex already locked by self");
      ELSE
        (* somebody else already has the mutex locked.  We'll need to wait *)
        wait := TRUE;
        self.nextWaiter := NIL;
        next := m.waiters;
        IF next = NIL THEN
          m.waiters := self;
        ELSE
          (* put me at the end of the list of waiters.*)
          prev := NIL;
          WHILE (next # NIL) DO  prev := next;  next := next.nextWaiter; END;
          prev.nextWaiter := self;
        END;
      END;

    LeaveCriticalSection_giant();

    IF wait THEN
      (* I didn't get the mutex, I need to wait for my turn... *)
      IF WaitForSingleObject(self.waitSema, INFINITE) # 0 THEN
        Choke(ThisLine());
      END;
    END;

    IF perfOn THEN PerfRunning() END;

  END LockMutex;

PROCEDURE UnlockMutex(m: Mutex) =
  VAR self := Self(); next: T;
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "UnlockMutex called from non-Modula-3 thread") END;
    EnterCriticalSection_giant();

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
        IF ReleaseSemaphore(next.waitSema, 1, NIL) = 0 THEN
          Choke(ThisLine());
        END;
      END;

    LeaveCriticalSection_giant();
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
    (* LL = giant+m on entry; LL = m on exit *)
  BEGIN
    <* ASSERT( (self.waitingOn=NIL) AND (self.nextWaiter=NIL) ) *>
    self.waitingOn := c;
    self.nextWaiter := c.waiters;
    c.waiters := self;
    LeaveCriticalSection_giant();
    UnlockMutex(m);
    IF WaitForSingleObject(self.waitSema, INFINITE) # 0 THEN
      Choke(ThisLine());
    END;
    LockMutex(m);
  END InnerWait;

PROCEDURE InnerTestAlert(self: T) RAISES {Alerted} =
  (* LL = giant on entry; LL = giant on normal exit, 0 on exception exit *)
  (* If self.alerted, clear "alerted", leave giant and raise
     "Alerted". *)
  BEGIN
    IF self.alerted THEN
      self.alerted := FALSE;
      LeaveCriticalSection_giant();
      RAISE Alerted
    END;
  END InnerTestAlert;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "AlertWait called from non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(State.waiting) END;
    EnterCriticalSection_giant();
    InnerTestAlert(self);
    self.alertable := TRUE;
    InnerWait(m, c, self);
    EnterCriticalSection_giant();
    InnerTestAlert(self);
    LeaveCriticalSection_giant();
    IF perfOn THEN PerfChanged(State.alive) END;
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die(ThisLine(), "Wait called from non-Modula-3 thread") END;
    IF perfOn THEN PerfChanged(State.waiting) END;
    EnterCriticalSection_giant();
    InnerWait(m, c, self);
    IF perfOn THEN PerfChanged(State.alive) END;
  END Wait;

PROCEDURE DequeueHead(c: Condition) =
  (* LL = giant *)
  VAR t: T;
  BEGIN
    t := c.waiters; c.waiters := t.nextWaiter;
    t.nextWaiter := NIL;
    t.waitingOn := NIL;
    t.alertable := FALSE;
    IF ReleaseSemaphore(t.waitSema, 1, NIL) = 0 THEN
      Choke(ThisLine());
    END;
  END DequeueHead;

PROCEDURE Signal (c: Condition) =
  BEGIN
    EnterCriticalSection_giant();
    IF c.waiters # NIL THEN DequeueHead(c) END;
    LeaveCriticalSection_giant();
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  BEGIN
    EnterCriticalSection_giant();
    WHILE c.waiters # NIL DO DequeueHead(c) END;
    LeaveCriticalSection_giant();
  END Broadcast;

PROCEDURE Alert(t: T) =
    VAR prev, next: T;
  BEGIN
    IF t = NIL THEN Die(ThisLine(), "Alert called from non-Modula-3 thread") END;
    EnterCriticalSection_giant();
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
      IF ReleaseSemaphore(t.waitSema, 1, NIL) = 0 THEN
        Choke(ThisLine());
      END;
    END;
    LeaveCriticalSection_giant();
  END Alert;

PROCEDURE XTestAlert (self: T): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    EnterCriticalSection_giant();
    result := self.alerted; IF result THEN self.alerted := FALSE END;
    LeaveCriticalSection_giant();
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
    IF t.act # me THEN Die (ThisLine(), "thread with bad slot!"); END;
    RETURN t;
  END Self;

PROCEDURE AssignSlot (t: T) =
  (* LL = 0, cause we allocate stuff with NEW! *)
  VAR n: CARDINAL;  new_slots: REF ARRAY OF T;
  BEGIN
    EnterCriticalSection_slotMu();

      (* make sure we have room to register this guy *)
      IF slots = NIL THEN
        LeaveCriticalSection_slotMu();
          slots := NEW (REF ARRAY OF T, 20);
        EnterCriticalSection_slotMu();
      END;
      IF n_slotted >= LAST (slots^) THEN
        n := NUMBER (slots^);
        LeaveCriticalSection_slotMu();
          new_slots := NEW (REF ARRAY OF T, n+n);
        EnterCriticalSection_slotMu();
        IF n = NUMBER (slots^) THEN
          (* we won any races that may have occurred. *)
          SUBARRAY (new_slots^, 0, n) := slots^;
          slots := new_slots;
        ELSIF n_slotted < LAST (slots^) THEN
          (* we lost a race while allocating a new slot table,
             and the new table has room for us. *)
        ELSE
          (* ouch, the new table is full too!   Bail out and retry *)
          LeaveCriticalSection_slotMu();
          AssignSlot (t);
          RETURN;
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

    LeaveCriticalSection_slotMu();
  END AssignSlot;

PROCEDURE FreeSlot (t: T) =
  (* LL = 0 *)
  BEGIN
    EnterCriticalSection_slotMu();
    
      DEC (n_slotted);
      WITH z = slots [t.act.slot] DO
        IF z # t THEN Die (ThisLine(), "unslotted thread!"); END;
        z := NIL;
      END;
      t.act.slot := 0;

    LeaveCriticalSection_slotMu();
  END FreeSlot;

(*------------------------------------------------------------ Fork, Join ---*)

VAR (* LL=activeMu *)
  allThreads  : Activation := NIL;  (* global list of active threads *)

PROCEDURE CreateT (act: Activation): T =
  (* LL = 0, because allocating a traced reference may cause
     the allocator to start a collection which will call "SuspendOthers"
     which will try to acquire "activeMu". *)
  VAR t := NEW(T, act := act);
  BEGIN
    t.waitSema := CreateSemaphore(NIL, 0, 1, NIL);
    t.join     := NEW(Condition);
    AssignSlot (t);
    RETURN t;
  END CreateT;

(* ThreadBase calls RunThread after finding (approximately) where
   its stack begins.  This dance ensures that all of ThreadMain's
   traced references are within the stack scanned by the collector.
*)

<*WINAPI*>
PROCEDURE ThreadBase (param: ADDRESS): DWORD =
  VAR
    me       : Activation   := param;
  BEGIN
    SetActivation (me);
    (* We need to establish this binding before this thread touches any
       traced references.  Otherwise, it may trigger a heap page fault,
       which would call SuspendOthers, which requires an Activation. *)

    me.stackbase := ADR (me); (* enable GC scanning of this stack *)
    RunThread (me);
    me.stackbase := NIL; (* disable GC scanning of my stack *)
    EVAL WinGDI.GdiFlush ();  (* help out Trestle *)

    <*ASSERT me # allThreads*>
    DISPOSE (me);
    RETURN 0;
  END ThreadBase;

PROCEDURE RunThread (me: Activation) =
  VAR self: T;
  BEGIN
    IF perfOn THEN PerfChanged(State.alive) END;
    EnterCriticalSection_slotMu();
      self := slots [me.slot];
    LeaveCriticalSection_slotMu();

    (* Run the user-level code. *)
    IF perfOn THEN PerfRunning() END;
    self.result := self.closure.apply();
    IF perfOn THEN PerfChanged(State.dying) END;

    LOCK self DO
      (* mark "self" done and clean it up a bit *)
      self.completed := TRUE;
      Broadcast(self.join); (* let everybody know that "self" is done *)
    END;

    IF perfOn THEN PerfDeleted() END;

    (* we're dying *)
    RTHeapRep.FlushThreadState(me.heapState);

    IF CloseHandle(self.waitSema) = 0 THEN Choke(ThisLine()) END;
    self.waitSema := NIL;

    FreeSlot(self);  (* note: needs self.act ! *)
    (* Since we're no longer slotted, we cannot touch traced refs. *)

    (* remove ourself from the list of active threads *)
    EnterCriticalSection_activeMu();
      IF allThreads = me THEN allThreads := me.next; END;
      me.next.prev := me.prev;
      me.prev.next := me.next;
    LeaveCriticalSection_activeMu();

    me.next := NIL;
    me.prev := NIL;
    IF CloseHandle(me.handle) = 0 THEN Choke(ThisLine()) END;
    me.handle := NIL;

  END RunThread;

PROCEDURE Fork(closure: Closure): T =
  VAR t: T;
      stack_size: DWORD;
      act: Activation;
      id: DWORD;
      handle: HANDLE;
  BEGIN

    <*ASSERT allThreads # NIL*>
    <*ASSERT allThreads.next # NIL*>
    <*ASSERT allThreads.prev # NIL*>

    (* determine the initial size of the stack for this thread *)
    stack_size := default_stack;
    TYPECASE closure OF
    | SizedClosure (scl) => IF scl.stackSize # 0 THEN 
                              stack_size := scl.stackSize * BYTESIZE(INTEGER);
                            END;
    ELSE (*skip*)
    END;

    act := NEW(Activation);
    handle := CreateThread(NIL, stack_size, ThreadBase, act, CREATE_SUSPENDED, ADR(id));
    IF handle = NIL THEN
      RuntimeError.Raise(RuntimeError.T.SystemError);
    END;
    act.handle := handle;
    t := CreateT(act);
    t.closure := closure;
    EnterCriticalSection_activeMu();
      act.next := allThreads;
      act.prev := allThreads.prev;
      allThreads.prev.next := act;
      allThreads.prev := act;
      IF ResumeThread(t.act.handle) = -1 THEN Choke(ThisLine()) END;
      DEC(act.suspendCount);
    LeaveCriticalSection_activeMu();
    RETURN t;
  END Fork;

PROCEDURE Join(t: T): REFANY =
  VAR res: REFANY;
  BEGIN
    LOCK t DO
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice"); END;
      WHILE NOT t.completed DO Wait(t, t.join) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
      t.join := NIL;
    END;
    RETURN res;
  END Join;

PROCEDURE AlertJoin(t: T): REFANY RAISES {Alerted} =
  VAR res: REFANY;
  BEGIN
    LOCK t DO
      IF t.joined THEN Die(ThisLine(), "attempt to join with thread twice"); END;
      WHILE NOT t.completed DO AlertWait(t, t.join) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
      t.join := NIL;
    END;
    RETURN res;
  END AlertJoin;

(*------------------------------------------------ timer-based preemption ---*)

PROCEDURE DisableSwitching () =
  BEGIN
    (* do nothing *)
  END DisableSwitching;

PROCEDURE EnableSwitching () =
  BEGIN
    (* do nothing *)
  END EnableSwitching;

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
    IF perfOn THEN PerfChanged(State.pausing) END;
    amount := n;
    WHILE amount > 0.0D0 DO
      thisTime := MIN (Limit, amount);
      amount := amount - thisTime;
      EnterCriticalSection_giant();
      InnerTestAlert(self);
      self.alertable := TRUE;
      <* ASSERT(self.waitingOn = NIL) *>
      LeaveCriticalSection_giant();
      EVAL WaitForSingleObject(self.waitSema, ROUND(thisTime*1000.0D0));
      EnterCriticalSection_giant();
      self.alertable := FALSE;
      IF self.alerted THEN
        (* Sadly, the alert might have happened after we timed out on the
           semaphore and before we entered "giant". In that case, we need to
           decrement the semaphore's count *)
        EVAL WaitForSingleObject(self.waitSema, 0);
        InnerTestAlert(self);
      END;
      LeaveCriticalSection_giant();
    END;
    IF perfOn THEN PerfChanged(State.alive) END;
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
   collector, it acquired 'giant' to guarantee that no suspended thread held it.
   That way when the collector tried to acquire a mutex or signal a
   condition, it wouldn't deadlock with the suspended thread that held giant.
   
   With the VM-synchronized, incremental collector this design is inadequate.
   Here's a deadlock that occurred:
      Thread.Broadcast held giant,
      then it touched its condition argument,
      the page containing the condition was protected by the collector,
      another thread started running the page fault handler,
      the handler called SuspendOthers,
      SuspendOthers tried to acquire giant.

   So, SuspendOthers doesn't grab "giant" before shutting down the other
   threads.  If the collector tries to use any of the thread functions
   that acquire "giant", it'll be deadlocked.
*)

VAR
  suspend_cnt: CARDINAL := 0;  (* LL = giant *)

PROCEDURE SuspendOthers () =
  (* LL=0. Always bracketed with ResumeOthers which releases "activeMu". *)
  VAR me := GetActivation();
  BEGIN
    <*ASSERT me # NIL*>
    EnterCriticalSection_activeMu();

    INC (suspend_cnt);
    IF suspend_cnt = 1 THEN StopWorld(me) END;
  END SuspendOthers;

PROCEDURE StopWorld (me: Activation) =
  (* LL=activeMu *)
  VAR nLive := 0;
      act := me.next;
  BEGIN
    LOOP
      WHILE act # me DO
        IF act.suspendCount = 0 THEN
          IF SuspendThread(act.handle) = -1 THEN Choke(ThisLine()) END;
          IF act.heapState.inCritical # 0 THEN
            IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
            INC(nLive);
          ELSE
            INC(act.suspendCount);
          END;
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
    IF suspend_cnt = 0 THEN
      act := me.next;
      WHILE act # me DO
        <*ASSERT act.suspendCount > 0*>
        IF ResumeThread(act.handle) = -1 THEN Choke(ThisLine()) END;
        DEC(act.suspendCount);
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
      IF act.stackbase # NIL THEN
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

PROCEDURE ProcessEachStack (<*UNUSED*>p: PROCEDURE (start, stop: ADDRESS)) =
  BEGIN
    (* experimental, unimplemented here *)
    <*ASSERT FALSE*>
  END ProcessEachStack;

PROCEDURE VerifySP (start, stop: ADDRESS): ADDRESS =
  (* Apparently, Win95 will lie about a thread's stack pointer! *)
  (* Verify that the claimed stack pages are really readable... *)
  CONST PageSize = 4096;
  CONST N = BYTESIZE (info);
  VAR info: MEMORY_BASIC_INFORMATION;
  BEGIN
    info.BaseAddress := LOOPHOLE (stop-1, ADDRESS);
    LOOP
      IF info.BaseAddress <= start THEN
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

PROCEDURE PerfChanged (s: State) =
  (* LL = Self() *)
  VAR e := ThreadEvent.T {kind := TE.Changed, id := GetCurrentThreadId(), state := s};
  BEGIN
    EnterCriticalSection_perfMu();
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    LeaveCriticalSection_perfMu();
  END PerfChanged;

PROCEDURE PerfDeleted () =
  (* LL = Self() *)
  VAR e := ThreadEvent.T {kind := TE.Deleted, id := GetCurrentThreadId()};
  BEGIN
    EnterCriticalSection_perfMu();
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    LeaveCriticalSection_perfMu();
  END PerfDeleted;

PROCEDURE PerfRunning () =
  (* LL = Self() *)
  VAR e := ThreadEvent.T {kind := TE.Running, id := GetCurrentThreadId()};
  BEGIN
    EnterCriticalSection_perfMu();
      perfOn := RTPerfTool.Send (perfW, ADR (e), EventSize);
    LeaveCriticalSection_perfMu();
  END PerfRunning;

(*-------------------------------------------------------- Initialization ---*)

PROCEDURE Init() =
  VAR
    self: T;
    me := NEW(Activation);
  BEGIN
    InitC();
    SetActivation(me);
    IF DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(),
            ADR(me.handle), 0, 0, DUPLICATE_SAME_ACCESS) = 0 THEN
      Choke(ThisLine());
    END;
    me.next := me;
    me.prev := me;
    <* ASSERT allThreads = NIL *>
    allThreads := me;

    self := CreateT(me);

    heapCond := NEW(Condition);

    me.stackbase := InitialStackBase (ADR (self));
    IF me.stackbase = NIL THEN Choke(ThisLine()); END;

    <*ASSERT inCritical = 1*>
    DEC(inCritical);

    PerfStart();
    IF perfOn THEN PerfChanged(State.alive) END;

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
  inCritical := 1;      (* LL = heap *)
  heapCond: Condition;

PROCEDURE LockHeap () =
  BEGIN
    EnterCriticalSection_heap();
    INC(inCritical);
  END LockHeap;

PROCEDURE UnlockHeap () =
  BEGIN
    DEC(inCritical);
    LeaveCriticalSection_heap();
  END UnlockHeap;

PROCEDURE WaitHeap () =
  VAR self := Self();
  BEGIN
    EnterCriticalSection_giant();

    <* ASSERT( (self.waitingOn=NIL) AND (self.nextWaiter=NIL) ) *>
    self.waitingOn := heapCond;
    self.nextWaiter := heapCond.waiters;
    heapCond.waiters := self;
    LeaveCriticalSection_giant();

    DEC(inCritical);
    <*ASSERT inCritical = 0*>
    LeaveCriticalSection_heap();

    IF WaitForSingleObject(self.waitSema, INFINITE) # 0 THEN
      Choke(ThisLine());
    END;

    EnterCriticalSection_heap();
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
