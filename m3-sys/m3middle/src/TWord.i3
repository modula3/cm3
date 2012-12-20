(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TWord.i3                                              *)
(* Last Modified On Fri Nov 19 09:32:50 PST 1993 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TWord;

(*  Modula-3 target description

    This interface provides simulations of unsigned integer operations, at the
    maximum precision supported by any target.

    Unless otherwise specified, the arithmetic operations defined
    below return TRUE if they succeed in producing a new target value,
    otherwise they return FALSE.
*)

FROM TInt IMPORT Int;

CONST Size = BITSIZE(Int);

CONST
  Max8  = Int{16_FF, 0, ..};
  Max16 = Int{16_FF, 16_FF, 0, ..};
  Max32 = Int{16_FF, 16_FF, 16_FF, 16_FF, 0, ..};
  Max64 = Int{16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_FF};

PROCEDURE New (READONLY chars: ARRAY OF CHAR;  base: [2..16];
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

PROCEDURE LT (READONLY a, b: Int): BOOLEAN;
(* returns 'Word.LT (a, b)' *)

PROCEDURE GT (READONLY a, b: Int): BOOLEAN;
(* returns 'Word.GT (a, b)' *)

PROCEDURE LE (READONLY a, b: Int): BOOLEAN;
(* returns 'Word.LE (a, b)' *)

PROCEDURE GE (READONLY a, b: Int): BOOLEAN;
(* returns 'Word.GE (a, b)' *)

PROCEDURE And (READONLY a, b: Int;  VAR i: Int);
(* returns 'Word.And (a, b)' *)

PROCEDURE Or (READONLY a, b: Int;  VAR i: Int);
(* returns 'Word.Or (a, b)' *)

PROCEDURE Xor (READONLY a, b: Int;  VAR i: Int);
(* returns 'Word.Xor (a, b)' *)

PROCEDURE Not (READONLY a: Int;  VAR i: Int);
(* returns 'Word.Not (a)' *)

PROCEDURE Shift (READONLY a: Int;  b: INTEGER;  VAR r: Int);
(* returns 'Word.Shift (a, b)' *)

PROCEDURE LeftShift (READONLY a: Int;  b: [0..Size-1];  VAR r: Int);
(* returns 'Word.LeftShift (a, b)' *)

PROCEDURE RightShift (READONLY a: Int;  b: [0..Size-1];  VAR r: Int);
(* returns 'Word.RightShift (a, b)' *)

PROCEDURE Rotate (READONLY a: Int;  b: INTEGER;  n: CARDINAL;  VAR r: Int);
(* returns 'Word.Rotate (a, b)' *)

PROCEDURE Extract (READONLY a: Int;  b, c: CARDINAL;  VAR r: Int): BOOLEAN;
(* returns 'Word.Extract (a, b, c)' *)

PROCEDURE Insert (READONLY a, b: Int;  c, d: CARDINAL;  VAR r: Int): BOOLEAN;
(* returns 'Word.Insert (a, b, c, d)' *)

PROCEDURE Truncate (READONLY a: Int;  n: CARDINAL;  VAR r: Int): BOOLEAN;
(* truncates to the low-order 'n' bytes of 'i'.
   Returns TRUE if 'i' has at most 'n' significant bytes, FALSE otherwise. *)

END TWord.
