(* -----------------------------------------------------------------------1- *)
(* File ArrayExpr.i3                                                         *)
(* Modula-3 source code.                                                     *)
(* Copyright 2020, 2021, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *) 
(* -----------------------------------------------------------------------2- *)

INTERFACE ArrayExpr;
(* An array constructor. *) 

IMPORT Type, Expr, Target, CG;

TYPE T <: Expr.T;

PROCEDURE New
  (type: Type.T; args: Expr.List; dots: BOOLEAN): Expr.T;
(* PRE: type is a fixed or open array type. *)

PROCEDURE ArrayConstrExpr (expr: Expr.T): T;
(* Look through a ConsExpr for an ArrayExpr.  NIL if not. *)

PROCEDURE NoteNested (constr: T);
(* PRE: constr has not been checked. *)
(* Mark constr as nested (ArrayExpr nested inside an ArrayExpr, directly,
   except for a possible ConsExpr in between.  In particular, must not
   be accessed by the outer ArrayExpr through a named constant. *)

PROCEDURE NoteTargetType (expr: Expr.T; type: Type.T);
(* PRE: If expr is an array constructor, it is top-level.
   If so, arrange for it to be compiled having type 'type'. *)
(* Will look through a ConsExpr. *)

PROCEDURE NoteUseTargetVar (expr: Expr.T);
(* NOOP if expr is not an array constructor.  Otherwise: *)
(* PRE: expr is top-level *)
(* Arrange to use LHS from the CG stack to set nonstatic shape components. *)
(* Will look through a ConsExpr. *)

PROCEDURE ShapeCheckNeeded (expr: Expr.T): BOOLEAN;
(* PRE: If expr is an array constructor, it is top-level and Checked. *)
(* If expr is an array constructor, assigning expr will require CT or RT array
   shape check. (Otherwise, ArrayExpr will take care of shape checks.) *)
(* Will look through a ConsExpr. *)

PROCEDURE Is (expr: Expr.T): BOOLEAN;
(* Purely syntactic. Will not look through a ConsExpr. *)

PROCEDURE IsAnon (expr: Expr.T): BOOLEAN;
(* expr is an anonymous array constructor. Will look thru' a ConsExpr. *)

PROCEDURE StaticSize (expr: Expr.T): INTEGER;
(* < 0, if nonstatic.  Can be static, even if open array repType.
   Does not include dope. *)

PROCEDURE GetBounds
  (expr: Expr.T;  VAR min, max: Target.Int): (* Success *) BOOLEAN;
(* Will look through a ConsExpr. *)

PROCEDURE ConstSubscript (array, index: Expr.T;  VAR expr: Expr.T): BOOLEAN;
(* Will look through a ConsExpr. *)

PROCEDURE CheckStaticRTErrEval
  (expr: Expr.T; VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT);
(* Set Code and Msg if they are not set and expr is known to produce a
   statically unconditional runtime error when evaluated. *)

PROCEDURE CheckStaticRTErrAssign
  (lhsType: Type.T; expr: Expr.T;
   VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT);
(* PRE: expr has been Checked. *)
(* Set Code and Msg if they are not set and expr is known to produce a
   statically unconditional runtime error when assigned to a variable
   of lhsType. *)

END ArrayExpr.
