(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "ASTNodeArray" provides access to the children of a node viewed as
an array indexed from "0" to "Number(n) - 1". This is not an efficient
way to iterate children, use the "AST_Iter" iterator if this is
important.  *)

INTERFACE ASTNodeArray;

IMPORT AST;

PROCEDURE Number(n: AST.NODE): CARDINAL;
(* returns the number of children in node "n". *)

PROCEDURE High(n: AST.NODE): INTEGER;
(* returns "Number(n) - 1" *)

PROCEDURE Ith(n: AST.NODE; i: CARDINAL): AST.NODE;
(* returns "n[i]", provided that "0 <= i <= High(n)", else a checked
run-time error. *)

END ASTNodeArray.

