(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TIntN.i3                                              *)
(* Last Modified On Thu Mar 10 13:42:53 PST 1994 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TIntN; (* also known as TInt *)

(*  Modula-3 target description

    This interface provides simulations of the target machine's
    signed integer operations.

    Unless otherwise specified, the arithmetic operations defined
    below return TRUE if they succeed in producing a new target value,
    otherwise they return FALSE.
*)

IMPORT Target, TInt;
FROM Target IMPORT Int, IntN;

CONST
  Zero      = IntN{x := TInt.Zero};
  One       = IntN{x := TInt.One};
  Two       = IntN{x := TInt.Two};
  Three     = IntN{x := TInt.Three};
  Four      = IntN{x := TInt.Four};
  Eight     = IntN{x := TInt.Eight};
  ThirtyOne = IntN{x := TInt.ThirtyOne};
  ThirtyTwo = IntN{x := TInt.ThirtyTwo};
  SixtyThree= IntN{x := TInt.SixtyThree};
  F3FF      = IntN{x := TInt.F3FF};
  x0400     = IntN{x := TInt.x0400};
  x0800     = IntN{x := TInt.x0800};
  x0F00     = IntN{x := TInt.x0F00};

  (* 'M' for Minus (negative) *)

  MOne       = IntN{x := TInt.MOne};
  MThirtyOne = IntN{x := TInt.MThirtyOne};
  MSixtyThree= IntN{x := TInt.MSixtyThree};

PROCEDURE FromHostInteger (x: INTEGER;  n: CARDINAL;  VAR i: IntN): BOOLEAN;
(* converts a host integer 'x' to a target integer 'i' *)

PROCEDURE ToHostInteger (READONLY i: IntN;  VAR x: INTEGER): BOOLEAN;
(* converts a target integer 'i' to a host integer 'x' *)

PROCEDURE Abs (READONLY a: IntN;  VAR r: IntN): BOOLEAN;
(* returns a if a >= 0, -a if a < 0, or overflow *)

PROCEDURE Add (READONLY a, b: IntN;  VAR i: IntN): BOOLEAN;
(* returns 'a + b' unless there's an overflow *)

PROCEDURE Subtract (READONLY a, b: IntN;  VAR i: IntN): BOOLEAN;
(* returns 'a - b' unless there's an overflow *)

PROCEDURE Negate (READONLY a: IntN;  VAR r: IntN): BOOLEAN;
(* returns '-a' unless there's an overflow *)

PROCEDURE Multiply (READONLY a, b: IntN;  VAR i: IntN): BOOLEAN;
(* returns 'a * b' unless there's an overflow *)

PROCEDURE Div (READONLY a, b: IntN;  VAR i: IntN): BOOLEAN;
(* returns 'a DIV b' unless there's an overflow *)

PROCEDURE Mod (READONLY a, b: IntN;  VAR i: IntN): BOOLEAN;
(* returns 'a MOD b' unless there's an overflow *)

PROCEDURE EQ (READONLY a, b: IntN): BOOLEAN;
(* returns 'a = b' *)

PROCEDURE NE (READONLY a, b: IntN): BOOLEAN;
(* returns 'a # b' *)

PROCEDURE LT (READONLY a, b: IntN): BOOLEAN;
(* returns 'a < b' *)

PROCEDURE GT (READONLY a, b: IntN): BOOLEAN;
(* returns 'a > b' *)

PROCEDURE LE (READONLY a, b: IntN): BOOLEAN;
(* returns 'a <= b' *)

PROCEDURE GE (READONLY a, b: IntN): BOOLEAN;
(* returns 'a >= b' *)

PROCEDURE ToText (READONLY i: IntN): TEXT;
(* converts 'i' to a printable string. *)

PROCEDURE ToChars (READONLY i: IntN;  VAR buf: ARRAY OF CHAR): INTEGER;
(* converts 'i' to a printable string in 'buf'.  Returns the
   number of characters in the string.  Returns -1 if 'buf' is too short. *)

PROCEDURE FromTargetInt (READONLY i: Int; byteSize: CARDINAL): IntN;

PROCEDURE ToDiagnosticText(a: IntN): TEXT;

END TIntN.
