(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RecordExpr.i3                                         *)
(* Last Modified On Fri Jun 24 08:48:36 PDT 1994 By kalsow         *)

INTERFACE RecordExpr;
(* A record constructor. *) 

IMPORT M3ID, Type, Expr, CG, MSIR;

PROCEDURE New (type: Type.T;  args: Expr.List): Expr.T;

PROCEDURE Is (e: Expr.T): BOOLEAN;
(* Purely syntactic. Will not look through a ConsExpr. *)

PROCEDURE Qualify (record: Expr.T;  id: M3ID.T;  VAR e: Expr.T): BOOLEAN;

PROCEDURE CheckStaticRTErrEval
  (expr: Expr.T; VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT);
(* Set Code and Msg if they are not set and expr is known to produce a
   statically unconditional runtime error when evaluated. *)

PROCEDURE TryCompileConstMSIR(e: Expr.T; VAR v: MSIR.Value): BOOLEAN;
(* If e is a checked, all-constant RecordExpr whose fields are all integer or
   float constants, set v to a ConstStruct Value and return TRUE.
   Otherwise return FALSE. *)

(* General compile-time-constant lowering for a value of MSIR type ft: handles
   integer/enum/BOOLEAN, float, TEXT literal, procedure ref, fixed array, and
   nested record constants (recursing via TryCompileConstMSIR).  Emits NO IR
   (safe at module-declare time).  Returns NIL if e is not such a constant.
   Used both for record fields and for whole module-global initializers. *)
PROCEDURE TryConstFieldMSIR(e: Expr.T;  ft: MSIR.T): MSIR.Value;

PROCEDURE EmitUseFailureMSIR(e: Expr.T);
(* MSIR analogue of CheckUseFailure's CG.Abort: if e is a record constructor
   whose use is a statically-detected runtime error (per AssignStmt.DoGenRTAbort,
   e.g. an out-of-range field), emit an unconditional ReportFault.  Used at
   const-fold sites (QualifyExpr) that extract a field value without going
   through CompileLValueMSIR, which emits the fault itself.  No-op otherwise. *)

END RecordExpr.
