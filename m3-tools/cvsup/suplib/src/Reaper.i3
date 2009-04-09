(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$ *)

(* The "Reaper" interface provides facilities for controlling and
   harvesting threads. *)

INTERFACE Reaper;

IMPORT Thread;

TYPE
  T <: Public;

  Public = Private OBJECT METHODS
    init(): T;
  END;

  Private <: ROOT;

(* A "Reaper.T" manages a set of threads.  It allows you to fork several
   threads, and then wait until any one of them has terminated.  You do not
   need to know a priori which thread will terminate first.

   After one of the threads has terminated (or, actually, at any time),
   you can alert all of the remaining threads, for example, to terminate
   all of them prematurely.

   We say that the threads managed by the reaper are registered with it.
   A thread is registered with the reaper at the time it is forked.  To
   achieve that, use the following procedure to fork the thread, instead
   of "Thread.Fork". *)

PROCEDURE Fork(reaper: T; cl: Thread.Closure): Thread.T;
(* Fork a thread, and register it with the reaper. *)

(* When a registered thread is about to terminate, it should notify its
   reaper via the following procedure. *)

PROCEDURE Dying(reaper: T);
(* Called by a thread to inform the reaper that it is about to exit. *)

(* The following procedures block until the next registered thread has
   terminated.  Then they perform the equivalent of "Thread.Join" on the
   terminated thread, and pass back the results. *)

PROCEDURE JoinNext(reaper: T;
                   VAR thr: Thread.T;
                   VAR ret: REFANY): BOOLEAN;
(* If there are still registered threads that have not died, wait for
   the next one to die.  When that occurs, return "TRUE", passing back
   the thread's handle and return value.  If there are no more registered
   threads alive, return "FALSE". *)

PROCEDURE AlertJoinNext(reaper: T;
                        VAR thr: Thread.T;
                        VAR ret: REFANY): BOOLEAN
  RAISES {Thread.Alerted};
(* An alertable version of "JoinNext". *)

(* To alert all registered threads that have not yet terminated, use
   the procedure below. *)

PROCEDURE AlertAll(reaper: T);
(* Alerts all the registered threads that are still alive. *)

END Reaper.
