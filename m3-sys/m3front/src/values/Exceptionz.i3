(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Exceptionz.i3                                         *)
(* Last Modified On Tue Dec 20 14:57:54 PST 1994 By kalsow     *)
(*      Modified On Fri Feb 23 03:41:38 1990 By muller         *)

INTERFACE Exceptionz;

IMPORT M3, Type, Value, Decl, Expr;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes);

PROCEDURE ArgType (t: Value.T): Type.T;

PROCEDURE ArgByReference (type: Type.T): BOOLEAN;

PROCEDURE EmitRaise (t: Value.T;  arg: Expr.T);

PROCEDURE UID (t: Value.T): INTEGER;

PROCEDURE IsImplicit (t: Value.T): BOOLEAN;

PROCEDURE AddFPSetTag (t: Value.T;  VAR x: M3.FPInfo): CARDINAL;
(* called for RAISES sets, doesn't include the interface record offset *)

END Exceptionz.
