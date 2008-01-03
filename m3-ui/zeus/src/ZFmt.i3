(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jun 16 14:18:42 PDT 1994 by mhb                      *)
(*      modified on Sun Jun  5 16:59:05 PDT 1994 by najork                   *)
(*       Created on Sun Jun  5 16:53:32 PDT 1994 by najork                   *)

(* The procedure in the "ZFmt" interface are intended for use
   as the ``formatting functions'' in "m3zume" event-specification
   files. The locking-level on all procedures in this interface 
   is arbitrary. *)

INTERFACE ZFmt;

PROCEDURE Int (n: INTEGER): TEXT;
(* "ZFmt.Int(n)" returns "Fmt.Int(n)" *)

PROCEDURE Bool(b: BOOLEAN): TEXT;
(* "ZFmt.Bool(b)" returns "true" if "b=TRUE", and "false" otherwise *)

PROCEDURE Char(c: CHAR): TEXT;
(* "ZFmt.Char(c)" returns "Fmt.Char(c)" *)

PROCEDURE Real (r: REAL): TEXT;
(* "ZFmt.Real(r)" returns 
     "Fmt.Real(r, style := Fmt.Style.Fix, literal := TRUE)" *)

PROCEDURE LongReal (r: LONGREAL): TEXT;
(* "ZFmt.LongReal(r)" returns 
     "Fmt.LongReal(r, style := Fmt.Style.Fix, literal := TRUE)" *)

PROCEDURE Text (t: TEXT): TEXT;
(* "ZFmt.Text(t) returns "TextConv.Encode(t)" *)

END ZFmt.



