(*                                                                           *)
(*  SopLiteral.m3                                                            *)
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

GENERIC MODULE SopLiteralG(Bool);
IMPORT Word;

PROCEDURE Compare(a, b : T) : [-1..1] =
  BEGIN WITH ai = Bool.GetId(a.var), bi = Bool.GetId(b.var) DO
    IF ai < bi THEN RETURN -1
    ELSIF ai = bi THEN RETURN 0
    ELSE RETURN 1
    END
  END END Compare;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN Bool.Hash(a.var) END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = 
  BEGIN RETURN Bool.Equal(a.var, b.var) AND a.mode = b.mode END Equal;

BEGIN END SopLiteralG.
