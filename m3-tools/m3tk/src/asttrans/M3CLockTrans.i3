(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CLockTrans;

IMPORT M3Context;

EXCEPTION NoThread;

PROCEDURE Run(c: M3Context.T) RAISES {NoThread};
(* Perform the LOCK desugaring on all primary source modules in 'c',
   as defined in the Modula-3 report. If the "Thread" interface
   is not found in "c" then "NoThread" will be raised. *)

END M3CLockTrans.
