(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Aug  4 17:16:00 PDT 1992 by kalsow *)

INTERFACE ParseColor;

IMPORT PaintOp;

(* These are the "standard" colors for the parsing views.
   They are read only. *)

VAR (* CONST *)
  Virgin    : PaintOp.T;
  Current   : PaintOp.T;
  Accepted  : PaintOp.T;
  Error     : PaintOp.T;
  Passive   : PaintOp.T;
  Active    : PaintOp.T;
  Highlight : PaintOp.T;
  Clear     : PaintOp.T;

END ParseColor.

