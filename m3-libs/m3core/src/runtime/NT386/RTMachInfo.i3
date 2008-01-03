(* Copyright 1996-2000, Critical Mass, Inc.   All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE RTMachInfo;

PROCEDURE DumpStack (pc, fp: ADDRESS);
(* Produce a stack trace for the stack rooted at "fp". *)

END RTMachInfo.
