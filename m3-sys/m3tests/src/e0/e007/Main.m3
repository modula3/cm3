(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Mon Nov  4 10:55:20 PST 1991 by kalsow    *)
(*      modified on Wed Apr 25 07:14:04 1990 by muller        *)

MODULE Main;
IMPORT A;

PROCEDURE foofoo (y: INTEGER): INTEGER =
  BEGIN
    RETURN y;
  END foofoo;

CONST
  bar = A.foo;
  barbar = foofoo;

BEGIN
  EVAL bar (3);
  EVAL barbar (3);
END Main.
