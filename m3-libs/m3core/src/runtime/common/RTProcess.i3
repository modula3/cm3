(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Tue Aug  9 09:54:25 PDT 1994 by kalsow     *)
(*      modified on Wed Mar 13 01:21:53 1991 by muller         *)

INTERFACE RTProcess;

(* This interface provides the runtime shutdown routines. *)

PROCEDURE RegisterExitor (p: PROCEDURE ());
(* Registers the procedure p to be executed when Exit is called.
   The registered procedures are executed in reverse of the order
   they were registered. *)

PROCEDURE Exit (n: INTEGER);
(* call the registered exitors and terminate the program with status "n".
   Terminating a Modula-3 program by ``falling off the end'' is like
   calling "Exit(0)".*)

PROCEDURE Crash (msg: TEXT);
(* Call the registered exitors and terminate the program with the
   error message "msg".  If possible, invoke a debugger or generate a
   core dump. *)

PROCEDURE InvokeExitors ();
(* Run the registered exit procedures, at most once. *)

PROCEDURE OnInterrupt (p: InterruptHandler): InterruptHandler;
(* Register 'p' as the handler to be called on the next "interrupt" signal
   and return the currently registered handler.  Note that 'p' must be NIL
   or a top-level procedure. *)

TYPE InterruptHandler = PROCEDURE ();

PROCEDURE RegisterInterruptSetup (enable, disable: PROCEDURE ());
(* Registers "enable" and "disable" as the machine-specific setup procedures
   to enable and disable respectively control-C handling. *)

TYPE ForkHandler = PROCEDURE();

(* RegisterForkHandlers:
  pthreads: pthread_atfork
  Win32: just return 0 -- success but it doesn't do anything
  user threads: maintains its own globals
                no dependence on -pthread/-pthreads/-lpthread
                because they are apparently broken on some systems *)
<* EXTERNAL RTProcess__RegisterForkHandlers *>
PROCEDURE RegisterForkHandlers(prep, parent, child: ForkHandler): INTEGER;

(* Fork:
  pthreads: fork (fork1 on Solaris)
  Win32: non-existant
  user threads: fork but handles RegisterForkHandlers *)
<* EXTERNAL RTProcess__Fork *>
PROCEDURE Fork(): INTEGER;
 
END RTProcess.
