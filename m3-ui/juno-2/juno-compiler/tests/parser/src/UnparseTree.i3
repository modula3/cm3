(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jun 26 14:46:51 PDT 1992 by heydon                   *)

INTERFACE UnparseTree;

(* Provides procedures for unparsing Juno units (interfaces and modules),
   commands, and expressions in the form of ugly lisp-like lists for debugging
   purposes. Any "dead-ends" in the tree (due to the fact that the parser
   never reached that portion of the input) are rendered as "EMPTY".
*)

IMPORT Wr, JunoAST;

PROCEDURE Unit(wr: Wr.T; READONLY ast: JunoAST.Unit; cnt: CARDINAL; in := 0);
(* Unparse "ast" to "wr" at the initial indentation level of "in". Report that
   a total of "cnt" tokens were parsed.
*)

PROCEDURE Cmd(wr: Wr.T; READONLY ast: JunoAST.Cmd; cnt: CARDINAL; in := 0);
(* Unparse "ast" to "wr" at the initial indentation level of "in". Report that
   a total of "cnt" tokens were parsed.
*)

PROCEDURE Expr(wr: Wr.T; READONLY ast: JunoAST.Expr; cnt: CARDINAL; in := 0);
(* Unparse "ast" to "wr" at the initial indentation level of "in". Report that
   a total of "cnt" tokens were parsed.
*)

END UnparseTree.
