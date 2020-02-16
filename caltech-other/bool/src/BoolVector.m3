(*                                                                           *)
(*  BoolVector.m3                                                            *)
(*                                                                           *)
(*  Vectors of Bool.T's.                                                     *)
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

MODULE BoolVector EXPORTS BoolVector;
IMPORT Bool, Word;

(* these are the non-private BoolVector routines *)
(* the private implementation is revealed in BoolVectorImpl.m3 *)

PROCEDURE Max(a, b : INTEGER) : INTEGER = 
  BEGIN IF a > b THEN RETURN a ELSE RETURN b END END Max;

(* pre-operate on a BoolVector with scalar *op* a_i *) 
PROCEDURE ScalarPreOp(op : PROCEDURE (a, b : Bool.T) : Bool.T; 
                      scalar : Bool.T; 
                      a : T) : T =
  <* FATAL IndexOutOfBounds *>
  VAR
    res := NEW(T); (* FALSE *)
  BEGIN
    FOR i := 0 TO a.getWidth() - 1 DO
      res.setBit(i,op(scalar,a.getBit(i)))
    END;
    RETURN res
  END ScalarPreOp;

(* post-operate on a BoolVector with scalar *op* a_i *) 
PROCEDURE ScalarPostOp(op : PROCEDURE (a, b : Bool.T) : Bool.T; 
                      a : T;
                      scalar : Bool.T) : T =
  <* FATAL IndexOutOfBounds *>
  VAR
    res := NEW(T); (* FALSE *)
  BEGIN
    FOR i := 0 TO a.getWidth() - 1 DO
      res.setBit(i,op(a.getBit(i),scalar))
    END;
    RETURN res
  END ScalarPostOp;

PROCEDURE BitwiseOp(op : PROCEDURE (a, b : Bool.T) : Bool.T;
                    a, b : T) : T =
  <* FATAL IndexOutOfBounds *>
  VAR res := NEW(T); BEGIN
    FOR i := 0 TO Max(a.getWidth(),b.getWidth()) - 1 DO
      res.setBit(i,op(a.getBit(i),b.getBit(i)))
    END;
    RETURN res
  END BitwiseOp;

PROCEDURE AggregateOp(op : PROCEDURE (a, b : Bool.T) : Bool.T;
                      a : T; initially : Bool.T) : Bool.T =
  <* FATAL IndexOutOfBounds *>
  BEGIN
    FOR i := 0 TO a.getWidth() - 1 DO
      initially := op(initially,a.getBit(i))
    END;
    RETURN initially
  END AggregateOp;

PROCEDURE Equivalent(a, b : T) : Bool.T =
  VAR bitwise := BitwiseOp(Bool.Equivalent, a, b); BEGIN
    RETURN AggregateOp(Bool.And, bitwise, Bool.True())
  END Equivalent;

PROCEDURE Integer(x : Word.T) : T =
  <* FATAL IndexOutOfBounds *>
  VAR res := NEW(T); i := 0; BEGIN
    WHILE x > 0 DO
      IF Word.And(x,1) = 1 THEN res.setBit(i,Bool.True()) END;
      x := Word.RightShift(x,1);
      INC(i)
    END;
    RETURN res
  END Integer;

BEGIN END BoolVector.
