(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Ctypes;

PROCEDURE P(chars: ARRAY OF CHAR) RAISES {}=
  BEGIN
    EVAL NUMBER (chars);
  END P;

VAR a: ARRAY [0..9] OF Ctypes.char;

BEGIN
  P(a);
END Main.
