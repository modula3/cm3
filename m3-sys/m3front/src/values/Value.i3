(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Value.i3                                              *)
(* Last modified on Tue Feb 28 17:00:17 PST 1995 by kalsow     *)
(*      modified on Fri Dec  7 05:12:46 1990 by muller         *)

INTERFACE Value;

IMPORT M3, M3ID;

TYPE
  T = M3.Value;
  CheckState = M3.CheckState;

TYPE
  Class = {Expr, Var, Type, Exception, Procedure,
           Module, Field, Method, Formal, Error};

PROCEDURE TypeCheck (t: T;  VAR cs: CheckState);

PROCEDURE TypeOf (t: T): M3.Type;
(* returns the type of 't' *)

PROCEDURE SetGlobals (t: T);
(* assign offsets to any needed global data. *)

PROCEDURE Load (t: T);
(* generate code to load 't' on the evaluation stack *)

PROCEDURE Declare (t: T);
(* generate the C declaration for t *)

PROCEDURE ConstInit (t: T);
(* generate the static initializations needed by 't' *)

PROCEDURE NeedsInit (t: T): BOOLEAN;
(* returns TRUE if 't' needs runtime code to be initialized *)

PROCEDURE LangInit (t: T);
(* generate language required initialization for t if Declare & ConstInit
   did nothing *)

PROCEDURE UserInit (t: T);
(* generate the user specified initialization for t *)

PROCEDURE ClassOf          (t: T): Class;
PROCEDURE IsExternal       (t: T): BOOLEAN;
PROCEDURE IsImported       (t: T): BOOLEAN;
PROCEDURE IsWritable       (t: T): BOOLEAN;
PROCEDURE ToExpr           (t: T): M3.Expr;
PROCEDURE ToType           (t: T): M3.Type;
PROCEDURE Base             (t: T): T;
PROCEDURE CName            (t: T): M3ID.T;
PROCEDURE GlobalName       (t: T;  dots, with_module: BOOLEAN): TEXT;
PROCEDURE IllegalRecursion (t: T);

PROCEDURE AddFPTag   (t: T;  VAR x: M3.FPInfo): CARDINAL;
PROCEDURE AddFPEdges (t: T;  VAR x: M3.FPInfo;  n: CARDINAL): CARDINAL;

PROCEDURE Reset ();

PROCEDURE SetModule (t: T): T;
(* sets the current module's list of values to 't' and
   returns the previouse module's value list.  This routine
   is only called when the "current" module changes. *)

PROCEDURE Reuse (t: T);
(* prepares the values on list 't' for reuse in another
   compilation ==> reset a bunch of flags *)

END Value.
