(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 30 19:48:20 PDT 1995 by heydon                   *)

MODULE JunoRTError;

BEGIN
  names := ARRAY Code OF TEXT{
    "No error",
    "Executed ABORT",
    "Executed HALT",
    "Guard failed",
    "Undefined term",
    "Built-in procedure failed",
    "Use of an uninitialized variable"}
END JunoRTError.
