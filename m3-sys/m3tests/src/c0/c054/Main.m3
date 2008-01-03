(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: coverage of Word*.mod *)

MODULE Main;

IMPORT Word;
FROM Word IMPORT T;

VAR
  a, b, c: T;
  z : BOOLEAN;

BEGIN

a := Word.Plus (b, c);
a := Word.Times (b, c);
a := Word.Minus (b, c);
z := Word.LT (b, c);
z := Word.LE (b, c);
z := Word.GT (b, c);
z := Word.GE (b, c);
a := Word.And (b, c);
a := Word.Or (b, c);
a := Word.Xor (b, c);
a := Word.Not (b);
a := Word.Shift (b, 10);
a := Word.Rotate (b, 10);
a := Word.Extract (b, 5, 10);
a := Word.Insert (b, c, 3, 4);

END Main.
  
