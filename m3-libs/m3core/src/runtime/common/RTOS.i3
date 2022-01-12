(*| Copyright (C) 1990, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Fri Nov 18 15:44:19 PST 1994 by kalsow     *)
(*|      modified on Sun Feb 21 14:15:00 PST 1993 by jdd        *)
(*|      modified on Wed Jan 27 22:27:27 PST 1993 by mjordan    *)

(* "RTOS" is a private interface that provides the low-level,
   OS-specific memory allocation and shutdown routines. *)

INTERFACE RTOS;

PROCEDURE Exit (n: INTEGER);
(* Terminate current process with return code "n". *)

PROCEDURE Crash ();
(* Terminate current process with a crash *)

<* EXTERNAL "RTOS__GetMemory" *>
PROCEDURE GetMemory (size: INTEGER): ADDRESS;
(* Return the address of "size" bytes of unused storage *)

<* EXTERNAL "RTOS__Cygwin"*>
PROCEDURE Cygwin (): BOOLEAN;

PROCEDURE LockHeap ();
(* Enters an allocator/collector critical section; the same thread may
   enter the critical section multiple times.  *)

PROCEDURE UnlockHeap ();
(* Leaves the critical section.  *)

PROCEDURE BroadcastHeap ();
(* Restarts all threads that called "WaitHeap" sometime after the
   allocator/collector critical section is released. *)

PROCEDURE WaitHeap ();
(* Blocks the caller until "BroadcastHeap" has been called and
   the allocator/collector critical section is released.   The
   caller must be in the critical section. *)

PROCEDURE Write (a: ADDRESS;  n: INTEGER);
(* Write the "n" bytes beginning at address "a" to the standard
   error output file or console. *)

END RTOS.
