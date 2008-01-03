(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Wed Aug  5 08:12:18 PDT 1992 by kalsow *)

MODULE ParseColor;

IMPORT PaintOp;

(* These are the "standard" colors for the parsing views.
   They are read only. *)

BEGIN
  Virgin     := PaintOp.FromRGB( 0.0, 1.0, 0.0 );  (* green *)
  Current    := PaintOp.FromRGB( 0.0, 0.0, 1.0 );  (* blue *)
  Accepted   := PaintOp.FromRGB( 0.0, 0.6, 0.4 );
  Error      := PaintOp.FromRGB( 1.0, 0.0, 0.0 );  (* red *)
  Passive    := PaintOp.FromRGB( 0.1, 0.4, 0.5 );
  Active     := PaintOp.FromRGB( 0.7, 0.2, 0.4 );
  Highlight  := PaintOp.FromRGB( 0.2, 0.0, 0.0 );
  Clear      := PaintOp.Bg;
END ParseColor.

