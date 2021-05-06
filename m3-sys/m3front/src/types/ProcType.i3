(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ProcType.i3                                           *)
(* Last Modified On Tue Dec 20 14:39:09 PST 1994 By kalsow     *)
(*      Modified On Wed Aug 29 02:55:06 1990 By muller         *)

INTERFACE ProcType;

IMPORT M3, M3ID, CG, Type, Value, CallExpr;

PROCEDURE Parse          (): Type.T;
PROCEDURE ParseSignature (name: M3ID.T;  cc: CG.CallingConvention): Type.T;

PROCEDURE MethodSigAsProcSig (sig, objType: Type.T): Type.T;

PROCEDURE Is        (t: Type.T): BOOLEAN;
PROCEDURE NFormals  (t: Type.T): INTEGER;
PROCEDURE Formals   (t: Type.T): Value.T (*list*);
PROCEDURE Result    (t: Type.T): Type.T;
PROCEDURE ResultQid (t: Type.T): M3.QID;
PROCEDURE CGResult  (t: Type.T): CG.Type;
PROCEDURE Raises    (t: Type.T): M3.ExSet;
PROCEDURE Methods   (t: Type.T): CallExpr.MethodList;
PROCEDURE CallConv  (t: Type.T): CG.CallingConvention;

PROCEDURE LargeResult (t: Type.T): BOOLEAN;
PROCEDURE IsCompatible (procSig, objectType, methodSig: Type.T): BOOLEAN;

PROCEDURE New (result: Type.T;  f0, f1, f2, f3, f4: Value.T := NIL): Type.T;
PROCEDURE SetMethods (t: Type.T;  m: CallExpr.MethodList);

END ProcType.
