(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MethodExpr.i3                                         *)
(* Last Modified On Fri Jun 24 08:47:03 PDT 1994 By kalsow     *)

INTERFACE MethodExpr;

IMPORT M3ID, Expr, Type, Value, MSIR;

PROCEDURE New (object: Type.T;  name: M3ID.T;
                  method: Value.T;  holder: Type.T): Expr.T;

PROCEDURE LoadMSIR (object: Type.T;  method: Value.T;  holder: Type.T): MSIR.Value;
(* object.method as a procedure value: object's binding of the method, from
   the defaultMethods table of its typecell (holder is the type that declares
   the method).  As Compile.  NIL after MSIRBuilder.Abandon. *)
(* evaluates to the default for the given method *)

END MethodExpr.
