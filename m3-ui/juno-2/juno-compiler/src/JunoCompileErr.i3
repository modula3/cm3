(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 10 09:06:23 PDT 1994 by heydon                   *)

INTERFACE JunoCompileErr;

IMPORT JunoAST;

TYPE
  ErrVal = OBJECT
    msg: TEXT;
    ast: JunoAST.T;
  END;

EXCEPTION Error(ErrVal);

(* Procedures may raise the exception Error to indicate that a compilation
   error has occurred. The "ErrVal" contains the text of the error message and
   the bad AST node of the original AST. *)

PROCEDURE Raise(msg: TEXT; ast: JunoAST.T) RAISES {Error};
(* Raise "Error" with an error value whose message is "msg" and whose error
   AST is the earliest AST reachable from "ast" by following back pointers. *)

END JunoCompileErr.
