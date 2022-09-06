(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.           *)
(* See file COPYRIGHT-CMASS for details.                       *)

(* This interface provides access to the runtime machinery that
   raises exceptions.  *)

UNSAFE INTERFACE RTException;

IMPORT RT0;

PROCEDURE Raise (VAR a: RT0.RaiseActivation) RAISES ANY;
(* raise the exception described by "a" *)

PROCEDURE ResumeRaise (VAR a: RT0.RaiseActivation) RAISES ANY;
(* after a TRY-FINALLY handler has been executed, restart the processing
   of the execption. It is assumed that there is a handler for this exception *)

PROCEDURE SetBackstop (h: Backstop): Backstop;
(* Sets the current backstop to "h" and returns the previous handler. *)

PROCEDURE InvokeBackstop (VAR a: RT0.RaiseActivation;
                          raises: BOOLEAN) RAISES ANY;
(* Invokes the current backstop. *)

TYPE Backstop = PROCEDURE (VAR a: RT0.RaiseActivation;
                           raises: BOOLEAN) RAISES ANY;

PROCEDURE DumpStack ();
(* If possible, produce a diagnostic stack dump on stderr *)

END RTException.

(* Implementation notes:

Safety:
   The "arg" and "aux_info" fields of a "RaiseActivation" are
   gigantic loopholes.  They may contain traced references or
   any other data.  As long as the activation stays on the stack,
   the collector will handle the traced references properly.
   Code that copies the activation data off the stack is
   <<EM>>very<<EM>> dangerous.

Unhandled Exceptions:

   When an exception is raised but not handled, the current
   backstop handler is invoked, "h(a,r)".  The current activation
   information is in "a" and the flag "r" indicating whether the failure
   was due to a restrictive "RAISES" clause. 

   There is only one backstop handler per process.

   The default backstop handler (represented by "NIL") converts the
   failing exception to

|      RuntimeError.E(RuntimeError.T.UnhandledException)  "or"
|      RuntimeError.E(RuntimeError.T.BlockedException)

   If the unhandled exception is one of these, the program is terminated
   with a diagnostic message.

   The default backstop preserves the module, line number, and pc
   information in the activation.  It sets "aux_info[0]" to the exception
   that was not handled and sets "aux_info[1]" to the argument passed to
   that exception.
*)
