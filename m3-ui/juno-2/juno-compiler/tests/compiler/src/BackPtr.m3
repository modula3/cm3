(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jul 23 18:42:53 PDT 1993 by heydon                   *)
(*      modified on Tue Jul 16 16:23:33 PDT 1993 by lin                      *)

MODULE BackPtr;

IMPORT JunoAST, JunoUnparse;
IMPORT Wr, Thread;

<* FATAL Wr.Failure, Thread.Alerted *>

PROCEDURE ShowAll(ast: JunoAST.T; wr: Wr.T) = 
  PROCEDURE Show(ast: JunoAST.T) =
  (* Unparse "ast" and its predecessor to "wr". *)
    VAR node: JunoAST.T := JunoAST.Predecessor(ast); BEGIN
      IF node # NIL AND node # ast THEN
        Wr.PutText(wr, "Result Node:\n");
        JunoUnparse.P(wr, ast, debug := TRUE);
        Wr.PutText(wr, "\nOriginal Node:\n");
        JunoUnparse.P(wr, node, debug := TRUE);
        Wr.PutText(wr, "\n\n")
      END
    END Show;
  PROCEDURE ShowAll2(ast: JunoAST.T) =
  (* Same as "ShowAll", but implicitly unparse to "wr". *)
    VAR it := ast.iterator(); child: JunoAST.T; BEGIN
      Show(ast);
      WHILE it.next(child) AND child # NIL DO ShowAll2(child) END
    END ShowAll2;
  BEGIN
    ShowAll2(ast)
  END ShowAll;

BEGIN
END BackPtr.
