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

FROM Target IMPORT Int;

CONST Size = BITSIZE(Int);

CONST
  Zero   = Int{ 0, 0,..};
  One    = Int{ 1, 0,..};
  MOne   = Int{16_FF,..};

  Two       = Int{ 2,0,..};
  Three     = Int{ 3,0,..};
  Four      = Int{ 4,0,..};
  Eight     = Int{ 8,0,..};
  ThirtyOne = Int{31,0,..};
  ThirtyTwo = Int{32,0,..};
  SixtyThree= Int{63,0,..};
  F3FF      = Int{16_FF,16_F3,0,..};
  x0400     = Int{0,4,0,..};
  x0800     = Int{0,8,0,..};
  x0F00     = Int{0,16_F,0,..};

  (* 'M' for Minus (negative) *)

  MThirtyOne = Int{16_E1,16_FF,..};
  MSixtyThree= Int{16_C1,16_FF,..};

PROCEDURE FromInt (x: INTEGER;  VAR i: Int): BOOLEAN;
(* converts a host integer 'x' to a target integer 'i' *)

PROCEDURE ToInt (READONLY i: Int;  VAR x: INTEGER): BOOLEAN;
(* converts a target integer 'i' to a host integer 'x' *)

PROCEDURE New (READONLY chars: ARRAY OF CHAR;  VAR i: Int): BOOLEAN;
(* converts the string of decimal characters in 'chars' to an integer
   value in 'i' *)

PROCEDURE Add (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'a + b' unless there's an overflow *)

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

PROCEDURE Chop (VAR i: Int;  n: CARDINAL);
(* Extract the low-order 'n' bytes of 'i', sign extended. *)

PROCEDURE SignExtend(VAR a: Int; n: CARDINAL);
(* sign extend from n to the precision of Int *)

PROCEDURE SignedTruncate(VAR a: Int; n: CARDINAL): BOOLEAN;
(* truncate to n bytes; return FALSE if the value did not previously fit *)

PROCEDURE ZeroExtend(VAR a: Int; n: CARDINAL);
(* zero extend from n bytes to the precision of Int *)

PROCEDURE UnsignedTruncate(VAR a: Int; n: CARDINAL): BOOLEAN;
(* truncate to n bytes; return FALSE if the value did not previously fit *)

END TInt.
