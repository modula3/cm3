(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TCardinal.i3                                          *)
(* Last Modified On Fri Nov 19 09:32:50 PST 1993 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TCardinal;

IMPORT Target;
TYPE T = Target.Int;
CONST Size = BITSIZE(T);

CONST
  Zero = TInt.Zero;
  Max8  = TInt.Max8;
  Max16 = TInt.Max16;
  Max32 = TInt.Max32;
  Max64 = TInt.Max64;

PROCEDURE New (READONLY chars: ARRAY OF CHAR;  base: [2..16];
               VAR i: T): BOOLEAN;
(* converts the string of characters in 'chars' representing a base 'base'
   number to an integer value in 'i' *)

PROCEDURE Inc (VAR i: T);
(* returns 'i + 1' *)

PROCEDURE Dec (VAR i: T);
(* returns 'i - 1' *)

PROCEDURE Add (READONLY a, b: T;  VAR i: T);
(* returns 'a + b' *)

PROCEDURE Subtract (READONLY a, b: T;  VAR i: T);
(* returns 'a - b' *)

PROCEDURE Multiply (READONLY a, b: T;  VAR i: T);
(* returns 'a * b' *)

PROCEDURE Div (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'a / b' *)

PROCEDURE Mod (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'a % b' *)

PROCEDURE LT (READONLY a, b: T): BOOLEAN;
(* returns 'a < b' *)

PROCEDURE GT (READONLY a, b: T): BOOLEAN;
(* returns 'a > b' *)

PROCEDURE LE (READONLY a, b: T): BOOLEAN;
(* returns 'a <= b' *)

PROCEDURE GE (READONLY a, b: T): BOOLEAN;
(* returns 'a >= b' *)

END TCardinal.
