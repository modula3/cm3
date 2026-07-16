(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: QualifyExpr.i3                                        *)
(* Last Modified On Fri Jun 24 08:57:39 PDT 1994 By kalsow     *)
(*      Modified On Sat Aug 18 01:13:38 1990 By muller         *)

INTERFACE QualifyExpr;

IMPORT M3ID, Expr, Value, Type, MSIR;

PROCEDURE New (a: Expr.T;  id: M3ID.T): Expr.T;

PROCEDURE Split (e: Expr.T; VAR v: Value.T): BOOLEAN;

PROCEDURE SplitQID (e: Expr.T; VAR module, item: M3ID.T): BOOLEAN;

PROCEDURE PassObject (e: Expr.T): BOOLEAN;

PROCEDURE MethodType (e: Expr.T): Type.T;

(* If e is a QualifyExpr for an object method call (class=objMethod),
   return the left-hand object expression.  Otherwise return NIL.
   Used by the MSIR path to compile virtual dispatch. *)
PROCEDURE LhsExpr (e: Expr.T): Expr.T;

(* If e is a QualifyExpr for an object method (class=objMethod), return the
   bit-offset of the holder type's own methods within the vtable (i.e.
   ObjectType.MethodOffset(p.holder)).  Returns -1 if the offset is not known
   at compile time (opaque supertype).  Used by the MSIR path to compute the
   absolute vtable slot index: (MethodSlotBase + method.offset) / ptr_size. *)
PROCEDURE MethodSlotBase (e: Expr.T): INTEGER;

(* If e is a type-qualified method reference T.m (class=objTypeMethod), return
   TRUE and set objType := T and holder := the type that introduces m.  Used by
   the MSIR path to emit a static supercall: load m's proc from T's typecell
   OTC_defaultMethods table and call it indirectly.  Returns FALSE otherwise. *)
PROCEDURE ObjTypeMethod (e: Expr.T;  VAR objType, holder: Type.T): BOOLEAN;

(* Return the holder type of a method reference (the object type that declares
   the method — p.holder in QualifyExpr's private P type).  NIL if not a
   method or not resolved.  Used for dynamic vtable dispatch when the holder's
   methodOffset is not statically known (opaque supertype). *)
PROCEDURE MethodHolder (e: Expr.T): Type.T;

(* If e is a QualifyExpr whose field is sub-byte, emit a read-modify-write
   bit-insertion of rhs and return TRUE.  Otherwise return FALSE immediately. *)
PROCEDURE SubByteStoreMSIR (e: Expr.T;  rhs: MSIR.Value): BOOLEAN;

(* If e is a sub-byte/bit-field rec/obj field, return its containing storage
   base pointer (evaluated once) + bit offset/width/raw type; else FALSE.  Used
   to bind a bit-field WITH alias by reference. *)
PROCEDURE BitFieldBaseMSIR (e: Expr.T;  VAR base: MSIR.Value;
                            VAR bitOff, width: INTEGER;
                            VAR ftype: Type.T): BOOLEAN;

END QualifyExpr.
