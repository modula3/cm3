(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TInt.i3                                               *)
(* Last Modified On Thu Mar 10 13:42:53 PST 1994 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TInt;

(*  Modula-3 target description

    This interface provides simulations of the target machine's
    signed integer operations.

    Unless otherwise specified, the arithmetic operations defined
    below return TRUE if they succeed in producing a new target value,
    otherwise they return FALSE.
*)

FROM Target IMPORT Int, IBytes;

CONST
  FF = 16_FF;

  Zero  = Int{NUMBER (IBytes), IBytes{0,..}};
  One   = Int{NUMBER (IBytes), IBytes{1,0,..}};
  Two   = Int{NUMBER (IBytes), IBytes{2,0,..}};
  Three = Int{NUMBER (IBytes), IBytes{3,0,..}};
  Four  = Int{NUMBER (IBytes), IBytes{4,0,..}};
  Ten   = Int{NUMBER (IBytes), IBytes{10,0,..}};

  (* 'M' for Minus (negative) *)

  MOne  = Int{NUMBER (IBytes), IBytes{FF,..}};

  (* 'S' for Signed *)

  MinS32 = Int{NUMBER (IBytes), IBytes{0,0,0,16_80,FF,..}};
  MaxS32 = Int{NUMBER (IBytes), IBytes{FF,FF,FF,16_7F,0,..}};
  MinS64 = Int{NUMBER (IBytes), IBytes{0,0,0,0,0,0,0,16_80}};
  MaxS64 = Int{NUMBER (IBytes), IBytes{FF,FF,FF,FF,FF,FF,FF,16_7F}};

  (* 'U' for Unsigned *)

  MinU32 = Zero;
  MaxU32 = Int{NUMBER (IBytes), IBytes{FF,FF,FF,FF,0,..}};
  MinU64 = Zero;
  MaxU64 = Int{NUMBER (IBytes), IBytes{FF,FF,FF,FF,FF,FF,FF,FF}};

PROCEDURE FromInt (x: INTEGER;  n: CARDINAL;  VAR i: Int): BOOLEAN;
(* converts a host integer 'x' to a target integer 'i' *)

PROCEDURE IntI (READONLY x: Int;  n: CARDINAL;  VAR i: Int): BOOLEAN;
(* converts a target integer 'x' to a target integer 'i' *)

PROCEDURE ToInt (READONLY i: Int;  VAR x: INTEGER): BOOLEAN;
(* converts a target integer 'i' to a host integer 'x' *)

PROCEDURE New (READONLY chars: ARRAY OF CHAR;  n: CARDINAL;
               VAR i: Int): BOOLEAN;
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

PROCEDURE NE (READONLY a, b: Int): BOOLEAN;
(* returns 'a # b' *)

PROCEDURE LT (READONLY a, b: Int): BOOLEAN;
(* returns 'a < b' *)

PROCEDURE GT (READONLY a, b: Int): BOOLEAN;
(* returns 'a > b' *)

PROCEDURE LE (READONLY a, b: Int): BOOLEAN;
(* returns 'a <= b' *)

PROCEDURE GE (READONLY a, b: Int): BOOLEAN;
(* returns 'a >= b' *)

PROCEDURE ToChars (READONLY i: Int;  VAR buf: ARRAY OF CHAR): INTEGER;
(* converts 'i' to a printable string in 'buf'.  Returns the
   number of characters in the string.  Returns -1 if 'buf' is too short. *)

PROCEDURE ToBytes (READONLY i: Int;  VAR buf: ARRAY OF [0..255]): INTEGER;
(* converts 'i' to the shortest sequence of bytes in little-endian order
   which when sign-extended equal 'i'.  Returns the number of
   significant bytes in the result.  Returns -1 if 'buf' is too short. *)

END TInt.
