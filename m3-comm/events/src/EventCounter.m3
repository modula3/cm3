(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Thu May 11 11:19:21 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 12:30:02 1997
 * Update Count    : 119
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.3  1997/08/04 20:15:08  bm
 * Fixed BRANDs
 *
 * Revision 1.2  1997/01/23 15:26:35  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 * We use a Priority Queue to hold the blocked threads.  Each thread
 * has its own entry in the queue, and its own condition variable.
 * Threads blocked because of Acquire use priority "value", wait := FALSE.
 * Threads blocked because of Wait use priority "value+1", wait := TRUE.
 * Equal priorities are ordered with wait before non-wait.  
 * Thus, they don't get in each others way, and are obtained in the
 * correct order.
 *)

MODULE EventCounter;

IMPORT Thread, EventPQ, EventPQRep, EventNumber, Fmt, Process, Text,
       RdWrMutex; 
(* IMPORT IO; *)
TYPE 
  ThreadWaiting = EventPQ.Elt OBJECT cv: Thread.Condition;  END;
  EventWaiting = EventPQ.Elt OBJECT handler: Handler  END;

REVEAL
  T = Public BRANDED "EventCounter.T" OBJECT
        locker : Thread.T;
        mu     : RdWrMutex.T;
        wmu    : Thread.Mutex;
        val    : EventNumWait;
        waiting: EventPQ.Default;
      OVERRIDES
        init    := Init;
        tryAcquire := TryAcquire;
        enqueueAction := EnqueueAction;
        acquire := Acquire;
        release := Release;
        wait    := Wait;
        value   := Value;
        set     := Set;
      END;

TYPE 
  EventNumWait = EventNumber.T OBJECT 
    wait: BOOLEAN := FALSE;
  OVERRIDES
    compare := Compare;
    fmt := Format;
  END;

PROCEDURE Compare(self: EventNumWait; en: EventNumber.T): [-1..1] =
  VAR cmp := EventNumber.Compare(self, en);
  BEGIN
    (* IO.Put("Comparing " & self.fmt() & " and " & en.fmt() & "\n"); *)
    (* If the EventNumber part is different, they are different! *)
    IF cmp # 0 THEN
      RETURN cmp;
    END;
    TYPECASE en OF
    | EventNumWait(p2) =>
      (* "wait" is greater than non-"wait" values. *) 
      IF self.wait = p2.wait THEN
        RETURN 0;
      ELSIF self.wait THEN
        RETURN -1;
      ELSE
        RETURN 1;
      END;
    ELSE
      (* "wait" is greater than normal EventNumber.T, non-"wait" is
         equal. *)
      IF self.wait THEN
        RETURN -1;
      ELSE
        RETURN 0;
      END;
    END;
  END Compare;

PROCEDURE Format(self: EventNumWait; base: Fmt.Base): Text.T =
  BEGIN
    RETURN EventNumber.T.fmt(self, base) & "." & Fmt.Bool(self.wait);
  END Format;

PROCEDURE Init (self: T; mu: RdWrMutex.T; value: EventNumber.T): T =
  BEGIN
    self.val := NEW(EventNumWait, wait := FALSE).init(value);
    self.waiting := NEW(EventPQ.Default).init();
    self.mu := mu;
    self.wmu := NEW(Thread.Mutex);
    self.locker := NIL;
    RETURN self;
  END Init;

PROCEDURE New(mu: RdWrMutex.T; t: EventNumber.T): T =
  BEGIN
    RETURN NEW(T).init(mu, t);
  END New;

PROCEDURE TryAcquire (self: T; value: EventNumber.T): BOOLEAN 
  RAISES {Duplicate} =
  BEGIN
    LOCK self.wmu DO
      (* Make sure we don't try to recursively grab the counter. *)
      IF self.locker = Thread.Self() THEN
        Process.Crash(
          "Thread #" (* & Fmt.Int(ThreadF.MyId()) *)
            & " is trying to acquire an event counter it already holds.\n");
      END;
      (* If the counter has already passed this value, raise Duplicate. *)
      CASE value.compare(self.val) OF
      | -1 => RAISE Duplicate;
      |  1 => RETURN FALSE;
      ELSE (* drop through *)
      END;
    END;

    self.mu.acquireWrite();
    Thread.Acquire(self.wmu);
    (* Need to check again after acquiring the lock. 
       If the counter has already passed this value, raise Duplicate. *)
    IF value.compare(self.val) = -1 THEN
      Thread.Release(self.wmu);
      self.mu.releaseWrite();
      RAISE Duplicate;
    END;
    self.locker := Thread.Self();
    Thread.Release(self.wmu);
    RETURN TRUE;
  END TryAcquire;

PROCEDURE Acquire (self: T; value: EventNumber.T) RAISES {Duplicate} =
  BEGIN
    LOCK self.wmu DO
      (* Make sure we don't try to recursively grab the counter. *)
      IF self.locker = Thread.Self() THEN
        Process.Crash(
          "Thread #" (* & Fmt.Int(ThreadF.MyId()) *)
            & " is trying to acquire an event counter it already holds.\n");
      END;
      (* If the counter has already passed this value, raise Duplicate. *)
      IF value.compare(self.val) = -1 THEN
        RAISE Duplicate;
      END;
    END;

    self.mu.acquireWrite();
    Thread.Acquire(self.wmu);
    (* Need to check again after acquiring the lock. 
       If the counter has already passed this value, raise Duplicate. *)
    IF value.compare(self.val) = -1 THEN
      Thread.Release(self.wmu);
      self.mu.releaseWrite();
      RAISE Duplicate;
    ELSIF value.compare(self.val) = 1 THEN
      WITH newSleeper = NEW(ThreadWaiting, cv := NEW(Thread.Condition),
                            priority := NEW(EventNumWait, 
                                            wait := FALSE).init(value)) DO
        self.waiting.insert(newSleeper);
        self.mu.wait(self.wmu, newSleeper.cv);
        
        (* If multiple threads block trying to acquire the same value, one
           of them will return from wait first and eventually release the
           counter (incrementing it).  All others will see self.val >
           value when they eventually get to run.  These release the lock
           and raise Duplicate. *)
        IF value.compare(self.val) = 0 THEN
          self.locker := Thread.Self();
          Thread.Release(self.wmu);
          RETURN;
        END;
        Thread.Release(self.wmu);
        self.mu.releaseWrite();
        RAISE Duplicate;
      END;
    END;
    self.locker := Thread.Self();
    Thread.Release(self.wmu);
  END Acquire;

(* The algorithm is this:
   Acquire self.wmu.   Now, check the eventnumber. 
   - If we could run right now, release the lock, try to Acquire the
     counter, execute the update, Release the counter and return. 
   - If not, enqueue it.
*)
PROCEDURE EnqueueAction(self: T; value: EventNumber.T; 
                        handler: Handler) RAISES {Duplicate} =
  BEGIN
    LOCK self.wmu DO
      IF self.locker = Thread.Self() THEN
        Process.Crash("Thread #" (* & Fmt.Int(ThreadF.MyId()) *)
            & " is trying to wait for an event counter it already holds.\n");
      END;
      CASE value.compare(self.val) OF
      | -1 => RAISE Duplicate;
      |  1 => 
        (* we really can't execute this action yet, so enqueue it *)
        WITH newEvent = NEW(EventWaiting, handler := handler,
                            priority := NEW(EventNumWait, 
                                            wait := FALSE).init(value)) DO
          self.waiting.insert(newEvent);
        END;
        RETURN;
      ELSE (* drop through *)
      END;
    END;

    (* We are actually next in line, so just try to acquire the lock
       and execute the event *)
    Acquire(self, value);
    handler.handle();
    Release(self);
  END EnqueueAction;

PROCEDURE Wait (self: T; value: EventNumber.T) =
  BEGIN
    LOCK self.wmu DO
      (* Make sure only the owner releases the counter. *)
      IF self.locker = Thread.Self() THEN
        Process.Crash(
          "Thread #" (* & Fmt.Int(ThreadF.MyId()) *)
            & " is trying to wait for an event counter it already holds.\n");
      END;
      IF value.compare(self.val) >= 0 THEN
        WITH newSleeper = NEW(ThreadWaiting, cv := NEW(Thread.Condition),
                              priority := NEW(EventNumWait, 
                                              wait := TRUE).init(value)) DO
          (* Increment this so it comes after the blocked sleepers trying
             to acquire the lock. *)
          TRY
            newSleeper.priority.inc();
          EXCEPT
          | EventNumber.Overflow => 
            Process.Crash("EventCounter overflowed on wait().");
          END;
          self.waiting.insert(newSleeper);
          (* IO.Put("EC.Wait(" & Fmt.Int(value) & ") blocked\n"); *)
          Thread.Wait(self.wmu, newSleeper.cv);
        END;
      END;
    END;
  END Wait;

(* Advance as far through the queue as possible, waking up
   waiters and anyone attempting to acquire the lock, and
   executing actions. *)
PROCEDURE AdvanceQueue(self: T): ThreadWaiting =
  VAR
    sleeper   : ThreadWaiting := NIL;
  BEGIN
    LOOP
      IF self.waiting.size() = 0 THEN RETURN sleeper END;

      WITH minElt = NARROW(self.waiting.min(), EventPQ.Elt) DO
        IF minElt.priority.compare(self.val) > 0 THEN
          RETURN sleeper;
        END;

        TYPECASE self.waiting.deleteMin() OF
        | ThreadWaiting(minSleeper) =>
          IF sleeper = NIL THEN
            (* Wake this one up after we leave loop. *)
            sleeper := minSleeper;
          ELSE
            (* Wake up any more. *)
            Thread.Signal(minSleeper.cv);
          END;
        | EventWaiting(minEvent) =>
          (* If this event is the current one we are waiting for,
             process it.  If we've already hit this number, then
             call the duplicate method. *)
          IF minEvent.priority.compare(self.val) = 0 THEN
            (* an event we can process! *)
            minEvent.handler.handle();

            (* after processing, increment our counter *)
            TRY
              self.val.inc();
            EXCEPT
            | EventNumber.Overflow => 
              Process.Crash("EventCounter overflowed on release().");
            END;

            (* If we had a sleeper, wake it since it's now going to
               just raise Duplicate when it's turn comes, and we may
               want to give someone else a chance to be the sleeper *)
            IF sleeper # NIL THEN 
              Thread.Signal(sleeper.cv);
              sleeper := NIL;
            END;
          ELSE
            minEvent.handler.duplicate();
          END;
        ELSE
          <*ASSERT FALSE*>
        END;
      END;
    END;
  END AdvanceQueue;

(* These should never happen. *)
<* FATAL EventPQ.Empty *>

PROCEDURE Release (self: T) =
  VAR
    sleeper   : ThreadWaiting := NIL;
  BEGIN
    LOCK self.wmu DO
      (* Make sure only the owner releases the counter. *)
      IF self.locker = NIL THEN
        Process.Crash(
          "Thread #" (* & Fmt.Int(ThreadF.MyId()) *)
            & " is trying to release an event counter that is not held.\n");
      END;

      (* Increment the value of the event counter. *)
      TRY
        self.val.inc();
      EXCEPT
      | EventNumber.Overflow => 
        Process.Crash("EventCounter overflowed on release().");
      END;
      sleeper := AdvanceQueue(self);

      self.locker := NIL;
    END;

    (* Release the lock, signal a sleeper if one should wake. *)
    self.mu.releaseWrite();
    IF sleeper # NIL THEN Thread.Signal(sleeper.cv); END;
  END Release;

PROCEDURE Set (self: T; val: EventNumber.T) RAISES {Invalid} =
  BEGIN
    LOCK self.wmu DO
      (* Make sure only the owner releases the counter. *)
      IF self.locker = Thread.Self() THEN
        Process.Crash(
          "Thread #" (* & Fmt.Int(ThreadF.MyId()) *)
            & " is trying to set an event counter it already holds.\n");
      END;
    END;
    self.mu.acquireWrite();
    Thread.Acquire(self.wmu);
    (* If the counter is at, or has already passed, this value, raise
       Invalid. *) 
    IF val.compare(self.val) <= 0 THEN
      Thread.Release(self.wmu);
      self.mu.releaseWrite();
      RAISE Invalid;
    END;

    (* Set the value of the event counter minus 1, so release will
       leave it at the correct value. *)
    TRY
      self.val := NEW(EventNumWait, wait := FALSE).init(val);
      self.val.dec();
    EXCEPT
    | EventNumber.Overflow => RAISE Invalid;
    END;

    self.locker := Thread.Self();
    Thread.Release(self.wmu);

    (*  we used to lock, set and then unlock.  We now treat set
        similarly to acquire, and require that the programmer call
        release as well.  This stuff is no longer needed:
    sleeper := AdvanceQueue(self);

    Thread.Release(self.wmu);
    self.mu.releaseWrite();
    IF sleeper # NIL THEN Thread.Signal(sleeper.cv); END;
    *)
  END Set;

PROCEDURE Value(self: T): EventNumber.T =
  BEGIN
    LOCK self.wmu DO
      RETURN NEW(EventNumber.T).init(self.val);
    END;
  END Value;

PROCEDURE DefaultHandlerHandle(<*UNUSED*>self: Handler) =
  BEGIN
  END DefaultHandlerHandle;

PROCEDURE DefaultHandlerDuplicate(<*UNUSED*>self: Handler) =
  BEGIN
  END DefaultHandlerDuplicate;

PROCEDURE ToText(self: T): TEXT =
  VAR t := "{";
  BEGIN
    LOCK self.wmu DO
      IF self.locker # NIL THEN t := t & "(locked)" END;
      t := t & "curr=" & self.val.fmt(10) & ",";
      IF self.waiting.size() = 0 THEN
        t := t & "<none blocked>";
      ELSE
        WITH minElt = NARROW(self.waiting.min(), EventPQ.Elt) DO
          t := t & "queue=" & minElt.priority.fmt(10) & "+" & 
                   Fmt.Int(self.waiting.size()) & "[";
          WITH arr = self.waiting.heap DO
            FOR i := 1 TO self.waiting.sz DO
              TYPECASE arr[i] OF
              | ThreadWaiting(tw) =>
                t := t & "waiting(" & tw.priority.fmt(10) & ")";
              | EventWaiting(tw) =>  
                t := t & "event(" & tw.priority.fmt(10) & ")";
              ELSE <*ASSERT FALSE*>
              END;
            END;
          END;
        END;
      END;
    END;
    RETURN t & "}";
  END ToText;

BEGIN
END EventCounter.
