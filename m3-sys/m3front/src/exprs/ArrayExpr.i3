(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ArrayExpr.i3                                          *)
(* Last Modified On Thu Aug 27 09:10:06 PDT 1992 By kalsow     *)
(*      Modified On Fri Dec 21 01:19:11 1990 By muller         *)

INTERFACE ArrayExpr;
(* An array constructor. *) 

IMPORT Type, Expr, Target, CG;

TYPE T <: Expr.T;

PROCEDURE New
  (type: Type.T; args: Expr.List; dots: BOOLEAN): Expr.T;
(* PRE: type is a fixed or open array type. *)

PROCEDURE ArrayConstrExpr (e: Expr.T): T;
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

PROCEDURE Is (e: Expr.T): BOOLEAN;
(* Will look through a ConsExpr. *)

PROCEDURE GetBounds
  (expr: Expr.T;  VAR min, max: Target.Int): (* Success *) BOOLEAN;
(* Will look through a ConsExpr. *)

PROCEDURE ConstSubscript (array, index: Expr.T;  VAR e: Expr.T): BOOLEAN;
(* Will look through a ConsExpr. *)

PROCEDURE CheckRT
  (expr: Expr.T; VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT);
END ArrayExpr.
