(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE WYSIWYGify;
(* this interface was created on painful discovery that PS text sizes and
   Trestle text sizes do not mean the same thing. *)

PROCEDURE FormatTextSizeForPS(a: REAL): TEXT;
PROCEDURE ScanTextSizeFromPS(t: TEXT): REAL;
END WYSIWYGify.
