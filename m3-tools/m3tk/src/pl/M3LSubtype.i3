(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3LSubtype;

IMPORT AST, M3AST_AS, ASTWalk;

PROCEDURE SetNode(
    cl: ASTWalk.Closure;
    n: AST.NODE;
    vm: ASTWalk.VisitMode)
    RAISES {};
(* If ISTYPE(n, M3AST_AS.Object_type), compute the pl_subtype_s attribute *)

PROCEDURE Set(cu: M3AST_AS.Compilation_Unit) RAISES {};
(*
    ASTWalk.VisitNodes(cu,
        NEW(ASTWalk.Closure, callback := SetNode));
*)

END M3LSubtype.
