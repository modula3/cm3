(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CImportASTrans;

IMPORT M3AST_AS;

PROCEDURE Set(cu: M3AST_AS.Compilation_Unit; genericFormalsOnly := FALSE);
(* Transform 'cu' so that statements of the form "IMPORT A AS B"
   are eliminated, and all instances of "B.x" are replaced by
   "A.x". This is destructive, naturally. 

   If 'genericFormalsOnly =TRUE', then 'cu' must be a generic instantiation, 
   and the transformation is restricted to values of B in "IMPORT A AS B",
   such that B was a generic formal parameter.
*)


END M3CImportASTrans.
