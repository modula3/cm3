(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE AstToVal;

IMPORT Value, M3AST_AS;

PROCEDURE ProcessExp(exp: M3AST_AS.EXP): Value.T;

END AstToVal.
