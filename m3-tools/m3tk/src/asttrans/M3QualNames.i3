INTERFACE M3QualNames;

(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST_AS;

PROCEDURE Set(cu: M3AST_AS.Compilation_Unit) RAISES {};
(* transform 'cu' such that all imported unqualified names are replaced
 * by their qualified form and all FROM I IMPORT clauses are deleted.
 *) 

END M3QualNames.
