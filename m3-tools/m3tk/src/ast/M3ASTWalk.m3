(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3ASTWalk;

IMPORT AST, ASTWalk;



PROCEDURE VisitNodes(n: AST.NODE; vc: Closure) RAISES ANY=
  BEGIN
    TRY
      ASTWalk.VisitNodes(n, vc)
    EXCEPT
    | ASTWalk.Aborted => RAISE Aborted
    END;
  END VisitNodes;


PROCEDURE ModeVisitNodes(n: AST.NODE; vc: Closure; vm: VisitModeControl) RAISES ANY=
  BEGIN
    TRY
      ASTWalk.ModeVisitNodes(n, vc, vm)
    EXCEPT
    | ASTWalk.Aborted => RAISE Aborted
    END;
  END ModeVisitNodes;


PROCEDURE NodeProcClosure(p: NodeCallbackProc): Closure RAISES {}=
  BEGIN
    RETURN ASTWalk.NodeProcClosure(p);
  END NodeProcClosure;


PROCEDURE IgnoreChildren(vc: Closure) RAISES {}=
  BEGIN
    ASTWalk.IgnoreChildren(vc);
  END IgnoreChildren;


PROCEDURE Abort() RAISES {ASTWalk.Aborted}=
  BEGIN
    RAISE ASTWalk.Aborted;
  END Abort;

BEGIN
END M3ASTWalk.
