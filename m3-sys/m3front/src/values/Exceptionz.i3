(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Exceptionz.i3                                         *)
(* Last Modified On Tue Dec 20 14:57:54 PST 1994 By kalsow     *)
(*      Modified On Fri Feb 23 03:41:38 1990 By muller         *)

INTERFACE Exceptionz;

IMPORT CG, M3, Type, Value, Decl, Expr;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes);

PROCEDURE ArgType (t: Value.T): Type.T;

PROCEDURE ArgByReference (type: Type.T): BOOLEAN;

PROCEDURE ArgToWord (t: CG.Type);
PROCEDURE ArgFromWord (t: CG.Type);
(* An argument passed by value travels in the activation's argument word.
   ArgToWord turns the argument on the stack, of stack type t, into that
   word; ArgFromWord turns the word back into a value of type t.  A float
   goes through an integer of its own size, so its bits are carried, not
   its value converted. *)

PROCEDURE EmitRaise (t: Value.T;  arg: Expr.T);

PROCEDURE UID (t: Value.T): INTEGER;

PROCEDURE IsImplicit (t: Value.T): BOOLEAN;

PROCEDURE AddFPSetTag (t: Value.T;  VAR x: M3.FPInfo): CARDINAL;
(* called for RAISES sets, doesn't include the interface record offset *)

END Exceptionz.
