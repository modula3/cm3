(*                                                                           *)
(*  SopLiteral.i3                                                            *)
(*                                                                           *)
(*  Data structure used as literal in S-O-P expressions.                     *)
(*                                                                           *)
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
(* $Id$ *)

GENERIC INTERFACE SopLiteralG(Bool);
IMPORT Word;

TYPE 
  T = RECORD var : Bool.T; mode : BOOLEAN; END;

(* These things are needed for generics: *)

PROCEDURE Compare(a, b : T) : [-1..1];
PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "SopLiteral" & Bool.Brand;

END SopLiteralG.
