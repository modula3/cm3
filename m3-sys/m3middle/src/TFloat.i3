(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TFloat.i3                                             *)
(* Last Modified On Thu Mar 10 14:34:10 PST 1994 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TFloat;

(*  Modula-3 target description

    This interface provides simulations of the target machine's
    floating-point operations.

    Unless otherwise specified, the arithmetic operations defined
    below return TRUE if they succeed in producing a new target value,
    otherwise they return FALSE.
*)

FROM Target IMPORT Int, Float, Precision;

CONST
  ZeroR = Float { Precision.Short,    0, 0.0x0 };
  ZeroL = Float { Precision.Long,     0, 0.0x0 };
  ZeroX = Float { Precision.Extended, 0, 0.0x0 };

PROCEDURE New (READONLY chars: ARRAY OF CHAR;  pre: Precision;
                                                      VAR f: Float): BOOLEAN;
(* converts 'chars' to a floating point value with the specified
   precision.  Note: regardless of the precision, the exponent must
   be started by the character 'e'. *)

PROCEDURE Prec (READONLY f: Float): Precision;
(* returns the precision of 'f' *)

PROCEDURE Add (READONLY a, b: Float;  VAR f: Float): BOOLEAN;
(* returns 'a + b' unless there's an overflow *)

PROCEDURE Subtract (READONLY a, b: Float;  VAR f: Float): BOOLEAN;
(* returns 'a - b' unless there's an overflow *)

PROCEDURE Multiply (READONLY a, b: Float;  VAR f: Float): BOOLEAN;
(* returns 'a * b' unless there's an overflow *)

PROCEDURE Divide (READONLY a, b: Float;  VAR f: Float): BOOLEAN;
(* returns 'a / b' unless there's an overflow *)

PROCEDURE Mod (READONLY a, b: Float;  VAR f: Float): BOOLEAN;
(* returns 'a MOD b' unless there's an overflow *)

PROCEDURE EQ (READONLY a, b: Float): BOOLEAN;
(* returns 'a = b' *)

PROCEDURE LT (READONLY a, b: Float): BOOLEAN;
(* returns 'a < b' unless there's an overflow *)

PROCEDURE LE (READONLY a, b: Float): BOOLEAN;
(* returns 'a <= b' unless there's an overflow *)

PROCEDURE FloatF (READONLY a: Float;  p: Precision;  VAR f: Float): BOOLEAN;
(* returns 'FLOAT (a, p)' unless there's an overflow *)

PROCEDURE FloatI (READONLY i: Int;  p: Precision;  VAR f: Float): BOOLEAN;
(* returns 'FLOAT (i, p)' unless there's an overflow *)

PROCEDURE Trunc (READONLY a: Float;  VAR i: Int): BOOLEAN;
(* returns 'TRUNC(a)' unless there's an overflow *)

PROCEDURE Round (READONLY a: Float;  VAR i: Int): BOOLEAN;
(* returns 'ROUND(a)' unless there's an overflow *)

PROCEDURE Floor (READONLY a: Float;  VAR i: Int): BOOLEAN;
(* returns 'FLOOR(a)' unless there's an overflow *)

PROCEDURE Ceiling (READONLY a: Float;  VAR i: Int): BOOLEAN;
(* returns 'CEILING(a)' unless there's an overflow *)

PROCEDURE ToChars (READONLY f: Float;  VAR buf: ARRAY OF CHAR): INTEGER;
(* converts 'f' to a printable string in 'buf'.  Returns the
   number of characters in the string.  Returns -1 if 'buf' is too short. *)

TYPE Byte = [0..255];

PROCEDURE ToBytes (READONLY f: Float;  VAR buf: ARRAY OF Byte): INTEGER;
(* converts 'f' to an array of bytes in 'buf'.  Returns the
   number of bytes in 'buf'.  Returns -1 if 'buf' is too short. *)

PROCEDURE FromBytes (READONLY buf: ARRAY OF Byte; p: Precision; VAR f: Float);
(* converts the array of bytes in 'buf' to a float 'f' with
   precision 'p'. *)

END TFloat.
