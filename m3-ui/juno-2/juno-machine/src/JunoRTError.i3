(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 30 19:49:36 PDT 1995 by heydon                   *)

INTERFACE JunoRTError;

(* Defines types for the run-time error return codes. *)

TYPE Code = {
  None, Abort, Halt, IfFailure, UndefTerm, FailedExtProc, UsedUninitialized };

(* The integer value corresponding to an error code is the order of the error
   code within the enumerated type. For example, the "Code.Abort" error has
   ordinal value 1. *)

VAR (*CONST*) names: ARRAY Code OF TEXT;

(* "names[code]" is a descriptive text describing error "code", to be printed
   in error messages. *)

END JunoRTError.
