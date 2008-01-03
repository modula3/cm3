(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:20 PDT 1992 by muller                   *)

INTERFACE UnFmt;

IMPORT Text;

PROCEDURE ToInt(t: Text.T): INTEGER;
(* Converts an unsigned string of digits to an INTEGER number. No
   error checks *)

PROCEDURE ToReal(t: Text.T): REAL;

PROCEDURE ToBool(t: Text.T): BOOLEAN;

END UnFmt.
