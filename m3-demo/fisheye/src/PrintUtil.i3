(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:28 PDT 1992 by muller                   *)

INTERFACE PrintUtil;

IMPORT Text;

PROCEDURE PrintRealPair(text: Text.T; x: REAL; y:REAL);

PROCEDURE PrintReal(text: Text.T; r: REAL);

PROCEDURE PrintIntPair(text: Text.T; i: INTEGER; j: INTEGER);

PROCEDURE PrintInt(text: Text.T; i: INTEGER);

PROCEDURE NewLine();

PROCEDURE PrintText(text: Text.T);

END PrintUtil.
