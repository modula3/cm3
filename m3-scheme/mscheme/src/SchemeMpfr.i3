(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Arbitrary-precision inexact numbers via MPFR.
   Opt-in: created via (make-mpfr value bits).
   Contagious: once present, result stays SchemeMpfr.T.
   Precision = max of operand precisions. *)

INTERFACE SchemeMpfr;
IMPORT SchemeObject, Mpfr;

TYPE T = BRANDED "SchemeMpfr" REF RECORD val: Mpfr.T; prec: CARDINAL END;

(* Construction *)
PROCEDURE New(prec: CARDINAL): T;
  (* Allocate zeroed Mpfr at given precision *)
PROCEDURE FromLR(v: LONGREAL; prec: CARDINAL): T;
  (* Convert LONGREAL to Mpfr *)
PROCEDURE FromExact(v: SchemeObject.T; prec: CARDINAL): T;
  (* Convert any exact number (integer or rational) to Mpfr *)
PROCEDURE FromMpfr(v: Mpfr.T): T;
  (* Wrap an existing Mpfr.T, inferring precision from the value *)

(* Extraction *)
PROCEDURE ToLR(v: T): LONGREAL;
  (* Truncate to LONGREAL; overflow -> +/-Inf *)
PROCEDURE GetPrec(v: T): CARDINAL;

(* Arithmetic — precision = max(a.prec, b.prec) *)
PROCEDURE Add(a, b: T): T;
PROCEDURE Sub(a, b: T): T;
PROCEDURE Mul(a, b: T): T;
PROCEDURE Div(a, b: T): T;
PROCEDURE Neg(a: T): T;
PROCEDURE Abs(a: T): T;
PROCEDURE Sqrt(a: T): T;

(* Transcendentals *)
PROCEDURE Sin(a: T): T;
PROCEDURE Cos(a: T): T;
PROCEDURE Tan(a: T): T;
PROCEDURE Exp(a: T): T;
PROCEDURE Log(a: T): T;

(* Comparison *)
PROCEDURE Compare(a, b: T): INTEGER;
  (* Returns -1, 0, or 1 *)

(* Formatting *)
PROCEDURE Format(v: T): TEXT;

END SchemeMpfr.
