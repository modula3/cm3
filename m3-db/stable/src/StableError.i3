(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:11:04 PST 1995 by kalsow     *)
(*      modified on Mon Sep 19 16:22:56 PDT 1994 by weich      *)

(* Interface for stable object exceptions *)

INTERFACE StableError;

IMPORT AtomList;

TYPE Code = AtomList.T;

EXCEPTION E(Code);

PROCEDURE ToText(code: Code): TEXT;
(* Convert a "StableError.E" into a text usable for error messages *)

PROCEDURE Halt(msg: TEXT);
(* Print out a runtime error message "msg" to standard error and
   halt the program. "Stable object error" will be prepended to the
   message "msg". *)

END StableError.

(* A "StableError.E" is raised by methods of the generic interface "Stable.ig".
   It is used to unify all exceptions raised by internal methods of stable
   objects. *)

