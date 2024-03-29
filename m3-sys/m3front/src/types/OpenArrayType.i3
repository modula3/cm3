(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OpenArrayType.i3                                      *)
(* Last Modified On Thu Jun 17 16:13:36 PDT 1993 By kalsow     *)
(*      Modified On Sat Nov 10 01:11:37 1990 By muller         *)

INTERFACE OpenArrayType;

IMPORT Type, CG;

PROCEDURE New (element: Type.T): Type.T;

PROCEDURE Is (t: Type.T): BOOLEAN;
(* Return TRUE iff 't' is an open array type *)

PROCEDURE Split (t: Type.T;  VAR element: Type.T): BOOLEAN;
(* If 't' is an open array type, returns TRUE and sets 'element'
   to the appropriate type.  Otherwise, returns FALSE *)

PROCEDURE EltPack (array: Type.T): INTEGER;
(* If 'array' is an array type, returns the packed size in bits of
   its elements.  If 'array' is an open array type, this is for the
   nearest fixed array dimension, if such exists.  If t is not an
   array type, returns 0. *)

PROCEDURE EltAlign (array: Type.T): INTEGER;
(* If 'array' is an open array type, returns the bit alignment of
   the nearest non-open or non-array elements.
   *BUT NOTE*: If elements are packed, this is the alignment of the
   unpacked element type !!!
   Otherwise, returns Target.Byte. *)
(* PRE: t is Checked. *)

PROCEDURE EltsAreBitAddressed (t: Type.T): BOOLEAN;
(* Returns TRUE if t is an open array whose nearest non-open elements are not
   at least byte-aligned. *)

PROCEDURE OpenDepth (t: Type.T): INTEGER;
(* If 't' is an n-dimensional open array, returns n else returns 0 *)

PROCEDURE NonopenEltType (t: Type.T): Type.T;
(* If 't' is an n-dimensional open array, returns the type of the non-open
   elements; otherwise, returns t. That is, strip all the ARRAY OF in 
   front of t *)

PROCEDURE DeclareDopeTemp (t: Type.T): CG.Var;
(* If 't' is an open array type, declare and return a temporary variable to
   hold its dope vector, otherwise abort. *)

END OpenArrayType.
