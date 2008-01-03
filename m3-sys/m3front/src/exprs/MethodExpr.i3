(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MethodExpr.i3                                         *)
(* Last Modified On Fri Jun 24 08:47:03 PDT 1994 By kalsow     *)

INTERFACE MethodExpr;

IMPORT M3ID, Expr, Type, Value;

PROCEDURE New (object: Type.T;  name: M3ID.T;
                  method: Value.T;  holder: Type.T): Expr.T;
(* evaluates to the default for the given method *)

END MethodExpr.
