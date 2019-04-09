(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AssignStmt.i3                                         *)
(* Last Modified On Tue Jun 20 15:23:26 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 11:26:34 PDT 1995 By ericv      *)
(*      Modified On Tue Mar 20 01:30:09 1990 By muller         *)

INTERFACE AssignStmt;

IMPORT Expr, Stmt, Target, Type;

PROCEDURE Parse (): Stmt.T;

PROCEDURE Check (tlhs: Type.T;  rhs: Expr.T;  VAR cs: Stmt.CheckState);
(* check that rhs is assignable to a variable of type tlhs. *)

PROCEDURE PrepForEmit (tlhs: Type.T;  rhs: Expr.T;  initializing: BOOLEAN);
(* An alternative to calling Expr.Prep(rhs) before calling Emit() below,
   that tries to avoid unnecessary structure copying on assignments
   by passing the final destination to operations that can assign
   the result directly.  "initializing" is TRUE if the lhs is
   uninitialized storage (i.e. contains no user data). *)

PROCEDURE DoEmit (tlhs: Type.T;  rhs: Expr.T; lhs_align := Target.Byte);
(* tlhs is the repType. *)
(* emit code to assign  (s0.A).tlhs := rhs.
   Note that Emit assumes that TypeOf(rhs) is assignable to tlhs
   and that Expr.Prep(rhs) or preferably PrepForEmit(tlhs,rhs,initializing)
   has been called. *)

PROCEDURE DoEmitCheck (tlhs: Type.T;  rhs: Expr.T);
(* Emit code to evaluate "rhs" and generate any runtime 
   runtime checks that would be needed if it were assigned to
   a value of type 'tlhs'.  Leave the rhs value on the stack.
   PRE: TypeOf(rhs) is assignable to tlhs.
   PRE: Expr.Prep(rhs) has been called.
   PRE: 'tlhs' is not an open array type.  *)

END AssignStmt.
