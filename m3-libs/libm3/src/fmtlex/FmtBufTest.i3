(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Mar 14 10:48:18 PST 1994 by heydon     *)

INTERFACE FmtBufTest;

(* By default, the procedures in the "FmtBuf" interface for formatting real
   numbers use the current rounding mode to round the result when fewer
   digits of precision are requested than are provided automatically by
   "Float.ToDecimal". In order to test the implementation of the "FmtBuf"
   interface, we would like to be able to exercise it under each rounding
   mode. Unfortunately, not all rounding modes are necessarily supported by
   each architecture. In particular, the "FloatMode.SetRounding" procedure
   may raise the exception "FloatMode.Failure" if the requested rounding
   mode is not supported by the underlying architecture.

   This interface provides a mechanism for test programs to override this
   default behavior and fix the current rounding mode, even if it is not
   supported by the underlying architecture. *)

IMPORT FloatMode;

VAR
  useCurrentRounding := TRUE;
  testRoundingMode: FloatMode.RoundingMode;

(* If "useCurrentRounding" is "TRUE", then the procedures in "FmtBuf" for
   formatting real numbers use the current rounding mode. In this case, the
   value of the global variable "testRoundingMode" is ignored.

   If "useCurrentRounding" is "FALSE", then these procedures use
   "testRoundingMode" as the rounding mode, even if it is not a rounding
   mode supported by the underlying architecture. *)

END FmtBufTest.
