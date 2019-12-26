(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ArrayType.i3                                          *)
(* Last Modified On Fri Jun 24 09:23:44 PDT 1994 By kalsow     *)
(*      Modified On Sat Nov 10 01:11:37 1990 By muller         *)

INTERFACE ArrayType;

IMPORT Type;

PROCEDURE Parse (): Type.T;

PROCEDURE New (index, element: Type.T): Type.T;

PROCEDURE Is (t: Type.T): BOOLEAN;
(* An array type, open or fixed. *)

PROCEDURE IsFixed (t: Type.T): BOOLEAN;
(* A fixed array type. *)

PROCEDURE Split (array: Type.T; VAR index, element: Type.T): BOOLEAN;
(* If 'array' is a fixed array type, returns TRUE and sets index and
   element to the appropriate types.  If 'array' is an open array type,
   returns TRUE, sets index to NIL, and sets element to the appropriate
   type.  Otherwise, returns FALSE *)

PROCEDURE EltType (array: Type.T): Type.T;
(* If 'array' is a fixed array type, its element type, otherwise NIL.
   *BUT NOTE*: If elements are packed, this is the alignment of the
   unpacked element type !!! *)

PROCEDURE EltPack (array: Type.T): INTEGER;
(* If 'array' is an array type, returns the packed size in bits of
   its elements.  If 'array' is an open array type, this is for the
   outermost fixed array dimension, if such exists.  Otherwise,
   returns 0. *)

PROCEDURE EltAlign (array: Type.T): INTEGER;
(* If 'array' is an array type, returns the bit alignment of its
   elements.  If t is open, returns the first non-open or non-array
   alignment. Otherwise, returns Target.Byte. *)

PROCEDURE OpenCousin (t: Type.T): Type.T;
(* If 't' is an 'ARRAY I OF X', returns 'ARRAY OF X', otherwise
   returns 't'. *)

PROCEDURE EltsAreBitAddressed (t: Type.T): BOOLEAN;
(* Returns TRUE if the array's elements are not guaranteed to be byte aligned.
   Works on a fixed or open array. *)

PROCEDURE TotalDepth (t: Type.T): INTEGER;
(* Total number of dimensions of t, open + fixed.
   Works on a fixed or open array. *)

PROCEDURE GenIndex (t: Type.T);
(* Given "ADR(a)" and "index" on the stack, generate code to replace them
   by "ADR(a[index])" on the stack.  Beginning and ending "addresses" are
   CG 'ValRec's and may include a 'bits' expression. Generates no bounds
   checks. *)
(* Works for an open array too, but uses elt_pack of the first nonopen element
   type, so, for an open array of depth > 1, 'index' must have been already
   multiplied by the product of element counts of inner open dimensions. *)

END ArrayType.
