(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id: SIsuffix.i3,v 1.2 2001-09-19 14:07:43 wagner Exp $ *)

INTERFACE SIsuffix;
IMPORT FloatMode, Lex;

EXCEPTION 
  OutOfRange;
  UnknownSuffix;

(* parse numbers that have appended SI suffixes. (or dont) *)

(* If the routines are called with a mode of Base10, the standard SI *)
(* suffixes will be used.  If Base2 is used, then k is 1024, etc.    *)

TYPE Mode = { Base10, Base2 };

PROCEDURE Int(text : TEXT; mode := Mode.Base10) : INTEGER RAISES { OutOfRange, UnknownSuffix, FloatMode.Trap, Lex.Error } ;

PROCEDURE LongReal(text : TEXT; mode := Mode.Base10) : LONGREAL RAISES { UnknownSuffix, FloatMode.Trap, Lex.Error };

PROCEDURE Real(text : TEXT; mode := Mode.Base10) : REAL RAISES { UnknownSuffix, FloatMode.Trap, Lex.Error };

TYPE
  T = Suffix;

  Suffix = RECORD
    char : CHAR;
    size : LONGREAL;
    name : TEXT;
    geeky : LONGREAL := 0.0d0
  END;

(* n.b. deka is non-standard.  should be "da" according to SI *)
(* also micro should be "mu" (Greek letter) *)
(* kilo could perhaps be "K" as well *)

CONST
  KiloGeek = FLOAT(1024, LONGREAL);

VAR  List : ARRAY [0..15] OF T;  

CONST
  Brand = "SIsuffix";

END SIsuffix.

