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
(* If 'array' is a fixed array type, returns TRUE and sets index and
   element to the appropriate types.  If 'array' is an open array type,
   returns TRUE, sets index to NIL, and sets element to the appropriate
   type.  Otherwise, returns FALSE *)

PROCEDURE EltPack (array: Type.T): INTEGER;
(* If 'array' is an array type, returns the packed size in bits of
   its elements.  Otherwise, returns 0. *)

PROCEDURE EltAlign (array: Type.T): INTEGER;
(* If 'array' is an array type, returns the bit alignment of its
   elements.  If t is open, returns the first non-open or non-array
   alignment. Otherwise, returns Target.Byte. *)

PROCEDURE OpenCousin (t: Type.T): Type.T;
(* If 't' is an 'ARRAY I OF X', returns 'ARRAY OF X', otherwise
   returns 't'. *)

PROCEDURE EltsAreBitAddressed (t: Type.T): BOOLEAN;
(* Returns TRUE if the array's elements are not guaranteed to be byte aligned.
   Works on an open array too. *)

PROCEDURE TotalDepth (t: Type.T): INTEGER;
(* Total number of dimensions of t, open + fixed.
   Works on an open array too. *) 

PROCEDURE GenIndex (t: Type.T);
(* Given "ADR(a)" and "x" on the stack, generate code to compute "ADR(a[x])". *)
(* Works for an open array too. *)

END ArrayType.
