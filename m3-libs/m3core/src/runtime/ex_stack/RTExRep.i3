(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue May  2 11:30:19 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 25 18:20:47 PDT 1992 by muller     *)

UNSAFE INTERFACE RTExRep;

(* This interface defines the low-level data structures
   used by the exception runtime's stack walker.
*)

(*----------------------------------------- compiler generated descriptors --*)

TYPE
  ScopeKind = { Except, ExceptElse,
                Finally, FinallyProc,
                Raises, RaisesNone,
                Lock };

TYPE
  Scope = UNTRACED REF RECORD
    kind        : CHAR;    (* ScopeKind *)
    outermost   : CHAR;    (* BOOLEAN => last scope that covers [start..stop]*)
    end_of_list : CHAR;    (* BOOLEAN => last scope in module list *)
    pad         : CHAR;
    start       : ADDRESS; (* first PC of the handled scope *)
    stop        : ADDRESS; (* last PC of the handled scope *)
    excepts     : ADDRESS; (* NIL-terminated list of handled exceptions *)
    offset      : INTEGER; (* frame offset of ExceptionInfo *)
  END;

TYPE
  ExceptionInfo = RECORD
    exception : ADDRESS;
    arg       : ADDRESS;
  END;

END RTExRep.

