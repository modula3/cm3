(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE A = RECORD x, y: INTEGER END;
TYPE B = BITS 64 FOR A;
TYPE C = RECORD i: INTEGER;  b: B;  j: INTEGER END;
TYPE F = BITS 32 FOR INTEGER;
TYPE G = BITS 8 FOR CHAR;

VAR c: C;
BEGIN
  EVAL BITSIZE (c);
  EVAL BITSIZE (F);
  EVAL BITSIZE (G);
END Main.
