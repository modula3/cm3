(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TWord.i3                                              *)
(* Last Modified On Fri Nov 19 09:32:50 PST 1993 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TWord;

(*  Modula-3 target description

    This interface provides simulations of the target machine's
    unsigned integer operations.

    Unless otherwise specified, the arithmetic operations defined
    below return TRUE if they succeed in producing a new target value,
    otherwise they return FALSE.
*)

FROM Target IMPORT Int;

PROCEDURE New (READONLY chars: ARRAY OF CHAR;  base: [2..16];  n: CARDINAL;
               VAR i: Int): BOOLEAN;
(* converts the string of characters in 'chars' representing a base 'base'
   number to an integer value in 'i' *)

PROCEDURE Add (READONLY a, b: Int;  VAR i: Int);
(* returns 'Word.Plus (a, b)' *)

PROCEDURE Subtract (READONLY a, b: Int;  VAR i: Int);
(* returns 'Word.Minus (a, b)' *)

PROCEDURE Multiply (READONLY a, b: Int;  VAR i: Int);
(* returns 'Word.Times (a, b)' *)

PROCEDURE Div (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'Word.Divide (a, b)' unless b is zero. *)

PROCEDURE Mod (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'Word.Mod (a, b)' unless b is zero. *)

PROCEDURE DivMod (READONLY x, y: Int;  VAR q, r: Int);
(* returns 'q = x DIV y', and 'r = x MOD y', but assumes that 'y # 0' *)

PROCEDURE LT (READONLY a, b: Int): BOOLEAN; (* a < b *)
PROCEDURE LE (READONLY a, b: Int): BOOLEAN; (* a <= b *)
PROCEDURE EQ (READONLY a, b: Int): BOOLEAN; (* a = b *)
PROCEDURE NE (READONLY a, b: Int): BOOLEAN; (* a # b *)
PROCEDURE GE (READONLY a, b: Int): BOOLEAN; (* a >= b *)
PROCEDURE GT (READONLY a, b: Int): BOOLEAN; (* a > b *)

PROCEDURE And (READONLY a, b: Int;  VAR i: Int);
(* returns 'Word.And (a, b)' *)

PROCEDURE Or (READONLY a, b: Int;  VAR i: Int);
(* returns 'Word.Or (a, b)' *)

PROCEDURE Xor (READONLY a, b: Int;  VAR i: Int);
(* returns 'Word.Xor (a, b)' *)

PROCEDURE Not (READONLY a: Int;  VAR i: Int);
(* returns 'Word.Not (a)' *)

PROCEDURE Shift (READONLY x: Int;  n: INTEGER;  VAR r: Int);
(* returns 'Word.Shift (x, n)' *)

PROCEDURE LeftShift (READONLY x: Int;  n: CARDINAL;  VAR r: Int);
(* returns 'Word.LeftShift (x, n)' *)

PROCEDURE RightShift (READONLY x: Int;  n: CARDINAL;  VAR r: Int);
(* returns 'Word.RightShift (x, n)' *)

PROCEDURE Rotate (READONLY x: Int;  n: INTEGER;  VAR r: Int);
(* returns 'Word.Rotate (x, n)' *)

PROCEDURE Extract (READONLY x: Int;  i, n: CARDINAL;  VAR r: Int): BOOLEAN;
(* returns 'Word.Extract (x, i, n)' *)

PROCEDURE Insert (READONLY x, y: Int;  i, n: CARDINAL;  VAR r: Int): BOOLEAN;
(* returns 'Word.Insert (x, y, i, n)' *)

END TWord.
