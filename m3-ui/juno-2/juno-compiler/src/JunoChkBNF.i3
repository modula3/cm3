(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Aug  5 12:55:19 PDT 1994 by heydon                   *)
(*      modified on Fri Aug  7 21:54:00 PDT 1992 by myers                    *)

INTERFACE JunoChkBNF;

(* Checks that a "JunoAST.T" produced by the parser conforms to the Juno BNF
   grammar described at the end of the Juno language definintion.

   See the "JunoAST" interface for a description of the ways in which a
   JunoAST.T may not conform to the Juno grammar. *)

IMPORT JunoAST;
FROM JunoCompileErr IMPORT Error;

(* Procedures in this module may raise the exception "JunoCompile.Error" to
   indicate that the JunoAST.T to be checked does not conform to the Juno
   grammar. When "JunoCompile.Error" is raised by procedures in this module,
   it is with an argument of type "JunoCompile.ErrVal". The "ast" field of the
   error value is the subtree of the argument "JunoAST.T" that violates the
   Juno grammar. *)

PROCEDURE Expr(expr: JunoAST.Expr) RAISES {Error};
(* Check that the expression "expr" produced by the Juno parser conforms to
   the non-terminal named "Expr" in the Juno grammar. Raises "Error" if there
   is a grammar violation. *)

PROCEDURE Formula(form: JunoAST.Formula) RAISES {Error};
(* Check that the formula "form" produced by the Juno parser conforms to the
   non-terminal named "Formula" in the Juno grammar. Raises "Error" if there
   is a grammar violation. *)

PROCEDURE TotalCmd(cmd: JunoAST.Cmd) RAISES {Error};
(* Check that the command "cmd" produced by the Juno parser conforms to the
   non-terminal named "TotalCmd" in the Juno grammar. Raises "Error" if there
   is a grammar violation. *)

END JunoChkBNF.
