(* Copyright 1996, Critical Mass, Inc.   All rights reserved. *)

INTERFACE RTMachInfo;

PROCEDURE DumpStack (pc, fp: ADDRESS);
(* Produce a stack trace for the stack rooted at "fp". *)

END RTMachInfo.
