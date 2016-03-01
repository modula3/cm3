(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TInt.i3                                               *)
(* Last Modified On Thu Mar 10 13:42:53 PST 1994 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TInt;

(*  Modula-3 target description

    This interface provides simulations of signed integer operations,
    at the maximum precision supported by any target.

    Unless otherwise specified, the arithmetic operations defined
    below return TRUE if they succeed in producing a new value,
    otherwise they return FALSE.
*)

CONST Size = BITSIZE(Int);
TYPE Int = (* OPAQUE *) ARRAY [0..8] OF IByte;
TYPE IByte = BITS 8 FOR [0..16_ff];

CONST
  Zero   = Int{ 0, 0,..};
  One    = Int{ 1, 0,..};
  Two    = Int{ 2, 0,..};
  MOne   = Int{16_FF,..};

  Min8   = Int{16_80, 16_FF,..};
  Max8   = Int{16_7f, 16_00,..};
  Max8U  = Int{16_FF, 16_00,..};
  Min16  = Int{16_00, 16_80, 16_FF,..};
  Max16  = Int{16_FF, 16_7f, 16_00,..};
  Max16U = Int{16_FF, 16_FF, 16_00,..};
  Min32  = Int{16_00, 16_00, 16_00, 16_80, 16_FF,..};
  Max32  = Int{16_FF, 16_FF, 16_FF, 16_7f, 16_00,..};
  Max32U = Int{16_FF, 16_FF, 16_FF, 16_FF, 16_00,..};
  Min64  = Int{16_00, 16_00, 16_00, 16_00, 16_00, 16_00, 16_00, 16_80, 16_FF};
  Max64  = Int{16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_7f, 16_00};
  Max64U = Int{16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_FF, 16_00};

PROCEDURE FromInt (x: INTEGER;  VAR i: Int): BOOLEAN;
(* converts a host integer 'x' to a target integer 'i' *)

PROCEDURE ToInt (READONLY i: Int;  VAR x: INTEGER): BOOLEAN;
(* converts a target integer 'i' to a host integer 'x' *)

PROCEDURE New (READONLY chars: ARRAY OF CHAR;  VAR i: Int): BOOLEAN;
(* converts the string of decimal characters in 'chars' to an integer
   value in 'i' *)

PROCEDURE Add (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'a + b' unless there's an overflow *)

PROCEDURE Inc (VAR i: Int): BOOLEAN;
(* returns 'a + 1' unless there's an overflow *)

PROCEDURE Dec (VAR i: Int): BOOLEAN;
(* returns 'a - 1' unless there's an overflow *)

PROCEDURE Subtract (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'a - b' unless there's an overflow *)

PROCEDURE Multiply (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'a * b' unless there's an overflow *)

PROCEDURE Div (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'a DIV b' unless there's an overflow *)

PROCEDURE Mod (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'a MOD b' unless there's an overflow *)

PROCEDURE EQ (READONLY a, b: Int): BOOLEAN;
(* returns 'a = b' *)

PROCEDURE LT (READONLY a, b: Int): BOOLEAN;
(* returns 'a < b' *)

PROCEDURE LE (READONLY a, b: Int): BOOLEAN;
(* returns 'a <= b' *)

PROCEDURE NE (READONLY a, b: Int): BOOLEAN;
(* 'a # b' *)

PROCEDURE GT (READONLY a, b: Int): BOOLEAN;
(* 'a > b' *)

PROCEDURE GE (READONLY a, b: Int): BOOLEAN;
(* 'a >= b' *)

PROCEDURE Abs (READONLY a: Int;  VAR r: Int): BOOLEAN;
(* ABS(a) or FALSE if overflow *)

PROCEDURE Negate (READONLY a: Int;  VAR r: Int): BOOLEAN;
(* '-a' or FALSE if overflow *)

PROCEDURE ToText (READONLY i: Int): TEXT;
(* converts 'i' to a printable string. *)

PROCEDURE ToChars (READONLY i: Int;  VAR buf: ARRAY OF CHAR): INTEGER;
(* converts 'i' to a printable string in 'buf'.  Returns the
   number of characters in the string.  Returns -1 if 'buf' is too short. *)

PROCEDURE ToBytes (READONLY i: Int;  VAR buf: ARRAY OF [0..255]): CARDINAL;
(* converts 'i' to the shortest sequence of bytes in little-endian order
   which when sign-extended equal 'i'.  Returns the number of
   significant bytes in the result.  Returns 0 if 'buf' is too short. *)

PROCEDURE Extend (READONLY i: Int;  n: CARDINAL;  VAR r: Int): BOOLEAN;
(* sign-extends from the low-order 'n' bytes of 'i'.
   Returns TRUE if 'i' has at most 'n' significant bytes, FALSE otherwise. *)

END TInt.
