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

FROM Target IMPORT Int, IChunks;

CONST
  Zero = Int { IChunks {16_0000, 16_0000, 16_0000, 16_0000}};
  One  = Int { IChunks {16_0001, 16_0000, 16_0000, 16_0000}};
  MOne = Int { IChunks {16_ffff, 16_ffff, 16_ffff, 16_ffff}};

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

PROCEDURE ToChars (READONLY i: Int;  VAR buf: ARRAY OF CHAR): INTEGER;
(* converts 'i' to a printable string in 'buf'.  Returns the
   number of characters in the string.  Returns -1 if 'buf' is too short. *)

TYPE ByteArray = ARRAY [0..7] OF [0..255];
PROCEDURE ToBytes (READONLY i: Int;  VAR buf: ByteArray): INTEGER;
(* converts 'i' to the shortest sequence of bytes in little-endian order
   which when sign-extended equal 'i'.  Returns the number of
   significant bytes in the result. *)

END TInt.
