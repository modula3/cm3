(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3BackInt.i3                                          *)
(* Last Modified On Thu Mar 10 13:42:53 PST 1994 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE M3BackInt; (* also known as TInt *)

(*  Modula-3 target description

    This interface provides simulations of the target machine's
    signed integer operations.

    Unless otherwise specified, the arithmetic operations defined
    below return TRUE if they succeed in producing a new target value,
    otherwise they return FALSE.
*)

IMPORT TInt, Target;

TYPE
  Int = (* OPAQUE *) RECORD
    n: CARDINAL;          (* only bytes [0..n-1] contain valid bits *)
    x := IBytes{0,..};    (* default is Zero *)
  END;
  TargetInt = Target.Int;
  IBytes = ARRAY [0..7] OF IByte;
  IByte = BITS 8 FOR [0..16_ff];

CONST
  Zero      = Int{NUMBER (IBytes), IBytes{ 0,0,..}};
  One       = Int{NUMBER (IBytes), IBytes{ 1,0,..}};
  Two       = Int{NUMBER (IBytes), IBytes{ 2,0,..}};
  Three     = Int{NUMBER (IBytes), IBytes{ 3,0,..}};
  Four      = Int{NUMBER (IBytes), IBytes{ 4,0,..}};
  Eight     = Int{NUMBER (IBytes), IBytes{ 8,0,..}};
  Ten       = Int{NUMBER (IBytes), IBytes{10,0,..}};
  ThirtyOne = Int{NUMBER (IBytes), IBytes{31,0,..}};
  ThirtyTwo = Int{NUMBER (IBytes), IBytes{32,0,..}};
  SixtyThree= Int{NUMBER (IBytes), IBytes{63,0,..}};
  SixtyFour = Int{NUMBER (IBytes), IBytes{64,0,..}};
  F3FF      = Int{NUMBER (IBytes), IBytes{16_FF,16_F3,0,..}};
  x0400     = Int{NUMBER (IBytes), IBytes{0,4,0,..}};
  x0800     = Int{NUMBER (IBytes), IBytes{0,8,0,..}};
  x0F00     = Int{NUMBER (IBytes), IBytes{0,16_F,0,..}};

  (* 'M' for Minus (negative) *)

  MOne  = Int{NUMBER (IBytes), IBytes{16_FF,..}};
  MThirtyOne = Int{NUMBER (IBytes), IBytes{16_E1,16_FF,..}};
  MSixtyThree= Int{NUMBER (IBytes), IBytes{16_C1,16_FF,..}};

PROCEDURE FromInt (x: INTEGER;  n: CARDINAL;  VAR i: Int): BOOLEAN;
(* converts a host integer 'x' to a target integer 'i' *)

PROCEDURE ToInt (READONLY i: Int;  VAR x: INTEGER): BOOLEAN;
(* converts a target integer 'i' to a host integer 'x' *)

PROCEDURE Abs (READONLY a: Int;  VAR r: Int): BOOLEAN;
(* returns a if a >= 0, -a if a < 0, or overflow *)

PROCEDURE Add (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'a + b' unless there's an overflow *)

PROCEDURE Subtract (READONLY a, b: Int;  VAR i: Int): BOOLEAN;
(* returns 'a - b' unless there's an overflow *)

PROCEDURE Negate (READONLY a: Int;  VAR r: Int): BOOLEAN;
(* returns '-a' unless there's an overflow *)

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

PROCEDURE ToText (READONLY i: Int): TEXT;
(* converts 'i' to a printable string. *)

PROCEDURE ToChars (READONLY i: Int;  VAR buf: ARRAY OF CHAR): INTEGER;
(* converts 'i' to a printable string in 'buf'.  Returns the
   number of characters in the string.  Returns -1 if 'buf' is too short. *)

PROCEDURE ToBytes (READONLY i: Int;  VAR buf: ARRAY OF [0..255]): INTEGER;
(* converts 'i' to the shortest sequence of bytes in little-endian order
   which when sign-extended equal 'i'.  Returns the number of
   significant bytes in the result.  Returns -1 if 'buf' is too short. *)

PROCEDURE FromTargetInt (READONLY i: Target.Int): Int;

TYPE
  Int_type = RECORD (* Like Target.Int_type *)
    size    : CARDINAL;  (* bit size *)
    bytes   : CARDINAL;  (* byte size *)
    min     : Int;       (* minimum value of this type *)
    max     : Int;       (* maximum value of this type *)
  END;

VAR (* CONST *)
  Integer: Int_type;
  Int8: Int_type;
  Int16: Int_type;
  Int32: Int_type;
  Int64: Int_type;
  Word8: Int_type;
  Word16: Int_type;
  Word32: Int_type;
  Word64: Int_type;

PROCEDURE Init();

(* renaming for diff minimization *)
CONST TypeNames = Target.TypeNames;
CONST FindConvention = Target.FindConvention;
CONST IntToTargetInt = TInt.FromInt;
CONST TargetIntZero = TInt.Zero;
TYPE Float = Target.Float;
TYPE Precision = Target.Precision;
VAR Extended: Target.Float_type;
CONST FloatType = Target.FloatType;
PROCEDURE TargetIntToDiagnosticText(a: Int): TEXT;

END M3BackInt.
