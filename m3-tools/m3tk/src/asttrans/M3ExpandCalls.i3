INTERFACE M3ExpandCalls;

(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST_AS;

PROCEDURE Set(cu: M3AST_AS.Compilation_Unit) RAISES {};
(* transform 'cu' such that all calls are expanded to include
   default parameters, and with keywords (formal names) removed.
*) 

END M3ExpandCalls.
