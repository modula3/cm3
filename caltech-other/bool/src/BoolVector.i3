(*                                                                           *)
(*  BoolVector.i3                                                            *)
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

INTERFACE BoolVector;
IMPORT Bool, Word;

EXCEPTION
  IndexOutOfBounds;

(* This is the Modula-3 replacement for expr.c *)
(* a BoolVector is a vector of Bools.  If a bool has not been specified, *)
(* it is assumed to be the constant FALSE. *)

(* When a BoolVector is first NEWed, it is all FALSE *)
(* A BoolVector.T is dynamically resized---we can think of it as being *)
(* infinitely wide.  Thus, operations on a BoolVector.T will never raise *)
(* IndexOutOfBounds.  getBit and setBit can, however, raise the exception on *)
(* a BoolVector.FixedWidth. *)
TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    getBit(idx : CARDINAL) : Bool.T RAISES { IndexOutOfBounds };
    setBit(idx : CARDINAL; bool : Bool.T) RAISES { IndexOutOfBounds }; 
    getWidth() : CARDINAL; (* some number > highest non-FALSE bit *)
    toWidth(width : CARDINAL) : FixedWidth; (* truncate to width *)
  END;

(* To specify that a BoolVector is to have a fixed with (e.g., to implement *)
(* a statically sized datatype, use the FixedWidth type, initializing the *)
(* width thru setWidth(). (Use setWidth() as you would init().) *)

  FixedWidth <: FixedWidthPublic;
  
  FixedWidthPublic = T OBJECT
  METHODS 
    setMaxWidth(width : CARDINAL) : FixedWidth;
    getMaxWidth() : CARDINAL;
  END;

(* carry out bitwise operation, matching a and b bit for bit *)
PROCEDURE BitwiseOp(op : PROCEDURE (a, b : Bool.T) : Bool.T; a, b : T) : T;

(* post-operate on a BoolVector with scalar *op* a_i *) 
PROCEDURE ScalarPostOp(op : PROCEDURE (a, b : Bool.T) : Bool.T; a : T;
                       scalar : Bool.T) : T;

(* pre-operate on a BoolVector with scalar *op* a_i *) 
PROCEDURE ScalarPreOp(op : PROCEDURE (a, b : Bool.T) : Bool.T; scalar :Bool.T; 
                      a : T) : T;

(* AggregateOp: with the initial value initially, apply 
   op(initially,a.bit[i]) *)
PROCEDURE AggregateOp(op : PROCEDURE (a, b : Bool.T) : Bool.T;
                      a : T; initially : Bool.T) : Bool.T;

(* This extends Bool.Equivalent out to whole BoolVectors *)
PROCEDURE Equivalent(a, b : T) : Bool.T;

(* here we could do arithmetic, etc... like in expr.c *)

PROCEDURE Integer(x : Word.T) : T;

END BoolVector.
