(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 10 09:06:22 PDT 1994 by heydon                   *)

MODULE JunoCompileErr;

IMPORT JunoAST;

PROCEDURE Raise(msg: TEXT; ast: JunoAST.T) RAISES {Error} =
  VAR err := NEW(ErrVal, msg := msg); BEGIN
    IF ast # NIL THEN
      (* find predecessor; crash if predecessor undefined *)
      WHILE ast.bp # JunoAST.End DO ast := ast.bp END;
    END;
    err.ast := ast;
    RAISE Error(err)
  END Raise;

BEGIN END JunoCompileErr.
