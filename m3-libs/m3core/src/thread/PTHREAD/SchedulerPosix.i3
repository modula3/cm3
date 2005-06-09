(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jun 22 16:42:22 PDT 1994 by kalsow     *)
(*      modified on Wed Mar 16 14:19:17 PST 1994 by wobber     *)
(*      modified on Thu Jan 28 10:14:15 PST 1993 by mjordan    *)
(*      modified on Mon Nov 18 14:41:43 PST 1991 by muller     *)

INTERFACE SchedulerPosix;

IMPORT Thread;

TYPE
  WaitResult = {Ready, Error, FDError, Timeout};

PROCEDURE IOWait(fd: INTEGER; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult;

PROCEDURE IOAlertWait(fd: INTEGER; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult
                  RAISES {Thread.Alerted};

(* These procedures are functionally equivalent to calling "select(2)"
   with a single file descriptor.  The major difference is that calls
   on "IOWait" and "IOAlertWait" do not prevent other threads from
   running.  If "read" is "TRUE", then the "readfds" and "exceptfds"
   arguments to "select" are the singleton set containing "fd", and
   "writefds" is the empty set.  Otherwise, the "writefds" and
   "exceptfds" are non-empty, and "readfds" is empty.

   The return value from "IOWait" and "IOAlertWait" indicates that
   "fd" was found to be ready for I/O, or that the caller's timeout
   interval expired, or that an error occured:

     -- "Ready" indicates that "fd" was found to be ready for I/O
        (according to the value of the "read" argument).  In other
        words, a subsequent I/O operation might succeed.  If "fd" is
        persistently in such a state, then "IOWait" and "IOAlertWait"
        will return "Ready".

     -- "Error" indicates that a "select" call executed on behalf
        of the caller failed, for instance the supplied "fd" is not
        valid.

     -- "FDError" indicates that "fd" is valid, but it exhibits an
        exceptional condition.

     -- "Timeout" indicates that the caller's timeout expired.  The
        file descriptor will have been tested at least once before
        this result is returned.

   "IOWait" and "IOAlertWait" block until the argument "fd" is in a
   reportable state, or until "timeoutInterval" seconds have passed.
   If "timeoutInterval" is negative, an indefinite wait is indicated,

   As usual, "IOAlertWait" is the alertable version, "IOWait" is the
   non-alertable version.   

   A "Ready" result from "IOWait" and "IOAlertWait" does not guarantee
   that I/O is currently possible on "fd".  For example, any other
   thread may preempt during the return sequence and issue a read on
   "fd".

   A standard technique for using these procedures is as follows.
   First make reads non-blocking on the file descriptor "fd", and then
   use a loop of the form:

      LOOP
	status := read (fd, buf, n);
        IF status = -1 AND errno # EWOULDBLOCK THEN
	  (* error to be handled *)
        ELSIF status = 0 THEN
          (* eof reached *)
        ELSIF status > 0 THEN
          (* status chars available in buf *)
        END;

        EVAL SchedulerPosix.IOWait(fd, TRUE);
      END;
*)

PROCEDURE DisableSwitching ();
PROCEDURE EnableSwitching ();
(* Increment/decrement the counter that controls whether thread
   switches may occur.  The counter is initialized to zero.  If
   the counter is greater than zero, switching is disabled.  It's
   a checked runtime error to use any thread primitives (e.g. LOCK,
   Wait, Signal, Yield, ...) that would result in thread switches
   while switching is disabled. *)

END SchedulerPosix.
