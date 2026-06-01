(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CheckExpr.i3                                          *)
(* Last Modified On Sun Aug 23 15:26:38 PDT 1992 By kalsow     *)

INTERFACE CheckExpr;
(* Range checks for ordinal types. *)

IMPORT Expr, Target, CG, MSIR;

(* Check pass: *)

PROCEDURE New      (a: Expr.T;  READONLY min, max: Target.Int;
                    err: CG.RuntimeError): Expr.T;
PROCEDURE NewLower (a: Expr.T;  READONLY min: Target.Int;
                    err: CG.RuntimeError): Expr.T;
PROCEDURE NewUpper (a: Expr.T;  READONLY max: Target.Int;
                    err: CG.RuntimeError): Expr.T;

(* New, NewLower, and NewUpper insert a CheckExpr.T node that, when
   compiled, will emit conditional RT range checks on its operand expression.
   They do NOT emit warnings on statically detectable failures.
   Callers must do that.  Nor do they simplify generated code
   to unconditional aborts on statically detectable failures. *)

(* Compile pass: *)

PROCEDURE EmitChecks (e: Expr.T;  READONLY min, max: Target.Int;
                      err: CG.RuntimeError);
(* Compiles 'e' and ensures that it's contained in [min..max].
   Does not construct any expression node.
   Does emit warnings on statically detectable RT failures.
   Does not simplify generated code to unconditional aborts
   on statically detectable failures. *)

PROCEDURE EmitChecksMSIR (v: MSIR.Value;  e: Expr.T;
                          READONLY min, max: Target.Int;
                          err: CG.RuntimeError): MSIR.Value;
(* MSIR analogue of EmitChecks operating on an ALREADY-compiled value 'v'
   (the MSIR result of expression 'e'): emits ReportFault range guards so v is
   contained in [min..max], with the same bound-elision as EmitChecks (only the
   bounds that 'e's static range can violate, gated by Host.doRangeChk).
   Returns v unchanged.  No-op for non-integer values or outside a proc. *)

END CheckExpr.
