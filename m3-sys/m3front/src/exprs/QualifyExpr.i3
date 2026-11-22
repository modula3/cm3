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

TYPE ArgsMSIR = OBJECT METHODS compile (): REF ARRAY OF MSIR.Value END;
(* The caller's explicit actuals for CompileMethodCallMSIR; compile returns
   NIL after an Abandon. *)

PROCEDURE CompileMethodCallMSIR (e: Expr.T;  args: ArgsMSIR;
                                 resultType: Type.T;
                                 VAR(*OUT*) result: MSIR.Value): BOOLEAN;
(* If e is a method designator, obj.m or the static T.m, emit the call and
   return TRUE with its raw result (NIL after an Abandon): evaluate the
   receiver (obj.m), then args.compile(), then dispatch through the vtable
   (obj.m) or T's default methods (T.m, whose receiver is the first explicit
   actual).  The MSIR counterpart of Prep, Compile and PassObject for the
   two method classes.  FALSE, emitting nothing, if e is not a method. *)

PROCEDURE FieldByteAddrMSIR (e: Expr.T): MSIR.Value;
(* The raw byte address of a record-field access whose BIT OFFSET is
   byte-aligned, regardless of the field's bit WIDTH — for ADR of a packed
   field (ADR(r.f) where f: BITS 23 FOR ... starts on a byte boundary),
   which has a well-defined address but no loadable/storable lvalue.
   Returns NIL when e is not such an access. *)

END QualifyExpr.
