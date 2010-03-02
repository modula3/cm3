(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TIntN.i3                                              *)
(* Last Modified On Thu Mar 10 13:42:53 PST 1994 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TIntN;

(*  Modula-3 target description

    This interface provides simulations of the target machine's
    signed integer operations.

    Unless otherwise specified, the arithmetic operations defined
    below return TRUE if they succeed in producing a new target value,
    otherwise they return FALSE.
*)

IMPORT TInt;
FROM Target IMPORT Int;

TYPE
 (* Int but with user-specified precision *)
  T = (* OPAQUE *) RECORD
    n: CARDINAL := NUMBER (Int);    (* only bytes [0..n-1] contain valid bits *)
    x: Int;                         (* default is Zero *)
  END;

CONST
  Zero      = T{x := TInt.Zero};
  One       = T{x := TInt.One};
  MOne      = T{x := TInt.MOne};

  Min8      = T{x := TInt.Min8};
  Max8      = T{x := TInt.Max8};
  (*Min16     = T{x := TInt.Min16};*)
  Max16     = T{x := TInt.Max16};
  (*Min32     = T{x := TInt.Min32};*)
  Max32     = T{x := TInt.Max32};
  (*Min64     = T{x := TInt.Min64};*)
  (*Max64     = T{x := TInt.Max64};*)

  Two       = T{x := Int{ 2,0,..}};
  Three     = T{x := Int{ 3,0,..}};
  Four      = T{x := Int{ 4,0,..}};
  Eight     = T{x := Int{ 8,0,..}};
  ThirtyOne = T{x := Int{31,0,..}};
  ThirtyTwo = T{x := Int{32,0,..}};
  SixtyThree= T{x := Int{63,0,..}};
  SixtyFour = T{x := Int{64,0,..}};
  F3FF      = T{x := Int{16_FF,16_F3,16_00,..}};
  x0400     = T{x := Int{16_00,16_04,16_00,..}};
  x0800     = T{x := Int{16_00,16_08,16_00,..}};
  x0F00     = T{x := Int{16_00,16_0F,16_00,..}};

  MThirtyOne = T{x := Int{16_E1,16_FF,..}};
  MSixtyThree= T{x := Int{16_C1,16_FF,..}};

PROCEDURE SignExtend(VAR a: Int; n: CARDINAL);
(* sign extend from n to the precision of Int *)

PROCEDURE SignedTruncate(VAR a: Int; n: CARDINAL): BOOLEAN;
(* truncate to n bytes; return FALSE if the value did not previously fit *)

PROCEDURE ZeroExtend(VAR a: Int; n: CARDINAL);
(* zero extend from n bytes to the precision of Int *)

PROCEDURE UnsignedTruncate(VAR a: Int; n: CARDINAL): BOOLEAN;
(* truncate to n bytes; return FALSE if the value did not previously fit *)

PROCEDURE FromHostInteger (x: INTEGER;  n: CARDINAL;  VAR i: T): BOOLEAN;
(* converts a host integer 'x' to a target integer 'i' *)

PROCEDURE ToHostInteger (READONLY i: T;  VAR x: INTEGER): BOOLEAN;
(* converts a target integer 'i' to a host integer 'x' *)

PROCEDURE Abs (READONLY a: T;  VAR r: T): BOOLEAN;
(* returns a if a >= 0, -a if a < 0, or overflow *)

PROCEDURE Add (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'a + b' unless there's an overflow *)

PROCEDURE Subtract (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'a - b' unless there's an overflow *)

PROCEDURE Negate (READONLY a: T;  VAR r: T): BOOLEAN;
(* returns '-a' unless there's an overflow *)

PROCEDURE Multiply (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'a * b' unless there's an overflow *)

PROCEDURE Div (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'a DIV b' unless there's an overflow *)

PROCEDURE Mod (READONLY a, b: T;  VAR i: T): BOOLEAN;
(* returns 'a MOD b' unless there's an overflow *)

PROCEDURE EQ (READONLY a, b: T): BOOLEAN;
(* returns 'a = b' *)

PROCEDURE NE (READONLY a, b: T): BOOLEAN;
(* returns 'a # b' *)

PROCEDURE LT (READONLY a, b: T): BOOLEAN;
(* returns 'a < b' *)

PROCEDURE GT (READONLY a, b: T): BOOLEAN;
(* returns 'a > b' *)

PROCEDURE LE (READONLY a, b: T): BOOLEAN;
(* returns 'a <= b' *)

PROCEDURE GE (READONLY a, b: T): BOOLEAN;
(* returns 'a >= b' *)

PROCEDURE ToText (READONLY i: T): TEXT;
(* converts 'i' to a printable string. *)

PROCEDURE ToChars (READONLY i: T;  VAR buf: ARRAY OF CHAR): INTEGER;
(* converts 'i' to a printable string in 'buf'.  Returns the
   number of characters in the string.  Returns -1 if 'buf' is too short. *)

PROCEDURE FromTargetInt (READONLY i: Int; byteSize: CARDINAL): T;

PROCEDURE ToDiagnosticText(a: T): TEXT;

VAR (*CONST*) TargetIntegerMin: T;
VAR (*CONST*) TargetIntegerMax: T;

PROCEDURE Init();

END TIntN.
