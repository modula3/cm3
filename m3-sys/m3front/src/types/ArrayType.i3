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

PROCEDURE Split (array: Type.T; VAR index, element: Type.T): BOOLEAN;
(* If 'array' is an array type, returns TRUE and sets index and element
   to the appropriate types.  Otherwise, returns FALSE *)

PROCEDURE EltPack (array: Type.T): INTEGER;
(* If 'array' is an array type, returns the packed size in bits of
   its elements.  Otherwise, returns 0. *)

PROCEDURE EltAlign (array: Type.T): INTEGER;
(* If 'array' is an array type, returns the bit alignment of
   its elements.  Otherwise, returns Target.Byte. *)

PROCEDURE OpenCousin (t: Type.T): Type.T;
(* If 't' is an 'ARRAY I OF X', returns 'ARRAY OF X', otherwise
   returns 't'. *)

PROCEDURE IsBitAddressed (t: Type.T): BOOLEAN;
(* Returns TRUE if the array's elements are not byte aligned. *)

PROCEDURE GenIndex (t: Type.T);
(* Given "ADR(a)" and "x" on the stack,
   generate code to compute "ADR(a[x])". *)

END ArrayType.
