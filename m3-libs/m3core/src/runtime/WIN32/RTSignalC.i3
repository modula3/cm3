(* Copyright 1996-2000, Critical Mass, Inc.   All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE RTSignalC;
IMPORT RuntimeError, ThreadContext;

TYPE
  RTE = RuntimeError.T;

<* EXTERNAL "RTSignalC_Raise" *>
PROCEDURE Raise (rte: RTE;  err, pc: INTEGER;
                 ctxt: ThreadContext.PCONTEXT): BOOLEAN;

END RTSignalC.
