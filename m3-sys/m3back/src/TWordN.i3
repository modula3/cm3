(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TWordN.i3                                             *)
(* Last Modified On Fri Nov 19 09:32:50 PST 1993 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TWordN;

(*  Modula-3 target description

    This interface provides simulations of the target machine's
    unsigned integer operations.

    Unless otherwise specified, the arithmetic operations defined
    below return TRUE if they succeed in producing a new target value,
    otherwise they return FALSE.
*)

IMPORT TIntN, TWord;
FROM TIntN IMPORT T;

CONST
  Max8  = T{x := TWord.Max8};
  Max16 = T{x := TWord.Max16};
  Max32 = T{x := TWord.Max32};
  (*Max64 = T{x := TWord.Max64};*)
  Size = TWord.Size;

PROCEDURE Add (READONLY a, b: T;  VAR i: T);
(* returns 'Word.Plus (a, b)' *)

PROCEDURE Subtract (READONLY a, b: T;  VAR i: T);
(* returns 'Word.Minus (a, b)' *)

PROCEDURE Multiply (READONLY a, b: T;  VAR i: T);
(* returns 'Word.Times (a, b)' *)

PROCEDURE Div (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'Word.Divide (a, b)' unless b is zero. *)

PROCEDURE Mod (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'Word.Mod (a, b)' unless b is zero. *)

PROCEDURE LT (READONLY a, b: T): BOOLEAN; (* a < b *)
PROCEDURE LE (READONLY a, b: T): BOOLEAN; (* a <= b *)
PROCEDURE EQ (READONLY a, b: T): BOOLEAN; (* a = b *)
PROCEDURE NE (READONLY a, b: T): BOOLEAN; (* a # b *)
PROCEDURE GE (READONLY a, b: T): BOOLEAN; (* a >= b *)
PROCEDURE GT (READONLY a, b: T): BOOLEAN; (* a > b *)

PROCEDURE And (READONLY a, b: T;  VAR i: T);
(* returns 'Word.And (a, b)' *)

PROCEDURE Or (READONLY a, b: T;  VAR i: T);
(* returns 'Word.Or (a, b)' *)

PROCEDURE Xor (READONLY a, b: T;  VAR i: T);
(* returns 'Word.Xor (a, b)' *)

PROCEDURE Not (READONLY a: T;  VAR i: T);
(* returns 'Word.Not (a)' *)

PROCEDURE Shift (READONLY x: T;  n: INTEGER;  VAR r: T);
(* returns 'Word.Shift (x, n)' *)

PROCEDURE LeftShift (READONLY x: T;  n: [0..Size - 1];  VAR r: T);
(* returns 'Word.LeftShift (x, n)' *)

PROCEDURE RightShift (READONLY x: T;  n: [0..Size - 1];  VAR r: T);
(* returns 'Word.RightShift (x, n)' *)

PROCEDURE Rotate (READONLY x: T;  n: INTEGER;  VAR r: T);
(* returns 'Word.Rotate (x, n)' *)

PROCEDURE Extract (READONLY x: T;  i, n: CARDINAL;  VAR r: T): BOOLEAN;
(* returns 'Word.Extract (x, i, n)' *)

PROCEDURE Insert (READONLY x, y: T;  i, n: CARDINAL;  VAR r: T): BOOLEAN;
(* returns 'Word.Insert (x, y, i, n)' *)

END TWordN.
