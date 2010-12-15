(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TCardinal.i3                                          *)

INTERFACE TCardinal;

IMPORT Target, TInt;
TYPE T = Target.Int; (* plus assertions *)

CONST
  Zero = TInt.Zero;
  Max8  = TInt.Max8;
  Max16 = TInt.Max16;
  Max32 = TInt.Max32;
  Max64 = TInt.Max64;

PROCEDURE FromCardinal (x: CARDINAL;  VAR r: T): BOOLEAN;

PROCEDURE Inc (VAR i: T): BOOLEAN;
(* returns 'i + 1' *)

PROCEDURE Dec (VAR i: T): BOOLEAN;
(* returns 'i - 1' *)

PROCEDURE Add (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'a + b' *)

PROCEDURE Subtract (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'a - b' *)

PROCEDURE Multiply (READONLY a, b: T;  VAR i: T): BOOLEAN;
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
