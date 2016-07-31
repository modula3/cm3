(* Copyright (C) 1989, 1993 Digital Equipment Corporation          *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Fri Aug 11 11:46:39 PDT 1995 by detlefs        *)
(*      modified on Fri May  7 11:35:32 PDT 1993 by mcjones        *)
(*      modified on Thu Jan 28 10:26:11 PST 1993 by mjordan        *)
(*      modified on Mon Dec 14 09:13:01 PST 1992 by kalsow         *)
(*      modified on Tue Jun 16 21:06:18 PDT 1992 by muller         *)

INTERFACE Thread;

TYPE
  T <: ROOT;
  Mutex = MUTEX;
  Condition <: ROOT;

(* A "Thread.T" is a handle on a thread.  A "Mutex" is locked by some
   thread, or unlocked.  A "Condition" is a set of waiting threads.  A
   newly-allocated "Mutex" is unlocked; a newly-allocated "Condition"
   is empty.  It is a checked runtime error to pass the "NIL" "Mutex",
   "Condition", or "T" to any procedure in this interface. *)

TYPE Closure = OBJECT METHODS apply(): REFANY END;

PROCEDURE Fork(cl: Closure): T;
(* Return a handle on a newly-created thread executing "cl.apply()". *)

PROCEDURE Join(t: T): REFANY;
(* Wait until "t" has terminated and return its result. It is a
   checked runtime error to call this more than once for any "t". *)

PROCEDURE Wait(m: Mutex; c: Condition);
(* The calling thread must have "m" locked. Atomically unlocks "m" and
   waits on "c".  Then relocks "m" and returns. *)

PROCEDURE Acquire(m: Mutex);
(* Wait until "m" is unlocked and then lock it. *)

PROCEDURE Release(m: Mutex);
(* The calling thread must have "m" locked.  Unlocks "m". *)

PROCEDURE Broadcast(c: Condition);
(* All threads waiting on "c" become eligible to run. *)

PROCEDURE Signal(c: Condition);
(* One or more threads waiting on "c" become eligible to run. *)

PROCEDURE Pause(n: LONGREAL);
(* Wait for "n" seconds to elapse. *)

(* To wait until a specified point in time in the future, say "t",
   you can use the call

| Pause(t - Time.Now())
*)

PROCEDURE Self(): T;
(* Return the handle of the calling thread. *)

EXCEPTION Alerted;
(* Used to approximate asynchronous interrupts. *)

PROCEDURE Alert(t: T);
(* Mark "t" as an alerted thread. *)

PROCEDURE TestAlert(): BOOLEAN;
(* If the calling thread has been marked alerted, return "TRUE" and
   unmark it. *)

PROCEDURE AlertWait(m: Mutex; c: Condition) RAISES {Alerted};
(* Like "Wait", but if the thread is marked alerted at the time of
   call or sometime during the wait, lock "m" and raise "Alerted". *)

PROCEDURE AlertJoin(t: T): REFANY RAISES {Alerted};
(* Like "Join", but if the thread is marked alerted at the time of
   call or sometime during the wait, raise "Alerted". *)

PROCEDURE AlertPause(n: LONGREAL) RAISES {Alerted};
(* Like "Pause", but if the thread is marked alerted at the time of
   the call or sometime during the wait, raise "Alerted". *)

(* \paragraph*{Specifying thread stack size.} Normally "Fork" uses a
   default value for the size of the stack of the new thread.  It is
   possible to change the default value, and also to specify the value
   used for a particular call to "Fork" by supplying a "SizedClosure"
   rather than a "Closure".  Stack sizes are given as a number of
   "Word.T"s.
*)

PROCEDURE GetDefaultStackSize(): CARDINAL;
(* Return the current default stack size for new threads. *)

PROCEDURE MinDefaultStackSize(min: CARDINAL);
(* Change the default stack size for newly forked threads to the
   greater of "min" and the current default stack size. *)

PROCEDURE IncDefaultStackSize(inc: CARDINAL);
(* Increment the default stack size for newly forked threads by "inc". *)

TYPE
  SizedClosure = Closure OBJECT stackSize: CARDINAL := 0 END;

<*PRAGMA SPEC*>

<* SPEC FUNC MaxLL(m: MUTEX): BOOLEAN *>
<* SPEC AXIOM (ALL [m1, m2: MUTEX]
                   (NOT MaxLL(m1) AND MaxLL(m2) AND m1 # NIL AND m2 # NIL)
                   IMPLIES m1 < m2) *>
(* Any mutex in MaxLL is greater than every mutex not in MaxLL. *)

<* SPEC Acquire(m) MODIFIES LL
                   REQUIRES sup(LL) < m
                   ENSURES LL' = INSERT(LL, m) *>

<* SPEC Release(m) MODIFIES LL
                   REQUIRES MEMBER(m, LL)
                   ENSURES LL' = DELETE(LL, m) *>

END Thread.

