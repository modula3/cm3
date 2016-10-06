(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Procedure.i3                                          *)
(* Last Modified On Tue Dec 20 14:55:57 PST 1994 By kalsow     *)
(*      Modified On Tue Oct  9 23:46:24 1990 By muller         *)

INTERFACE Procedure;

IMPORT CG, Value, Type, CallExpr, Decl;

TYPE T <: Value.T;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes;
                     headerOnly: BOOLEAN := FALSE);

PROCEDURE Signature   (t: T): Type.T;
PROCEDURE HasBody     (t: T): BOOLEAN;
PROCEDURE IsNested    (t: T): BOOLEAN;
PROCEDURE StaticLevel (t: T): INTEGER;

PROCEDURE DefinePredefined
                 (name      : TEXT;
                  methods   : CallExpr.MethodList;
                  reserved  : BOOLEAN;
                  signature : Type.T := NIL;
                  assignable: BOOLEAN := FALSE);

PROCEDURE CheckBody (t: T;  VAR cs: Value.CheckState);

PROCEDURE IsEqual    (a, b: Value.T): BOOLEAN;
PROCEDURE NoteExport (impl, intf: Value.T);

PROCEDURE CGName (t: T): CG.Proc;
(* return a back-end reference to the procedure 't'. *)

PROCEDURE LoadStaticLink (t: T);
(* generate code to load the static link needed to call 't' *)

PROCEDURE StartCall (t: T);
(* generate code the start a procedure call *)

PROCEDURE EmitValueCall (t: T): CG.Val;
(* generate code to finish a procedure call and return the procedure's result
   in a temporary. *)

PROCEDURE EmitCall (t: T);
(* generate code to finish a procedure call and leave any return result
   on the stack. *)
   
PROCEDURE Reset ();

END Procedure.
