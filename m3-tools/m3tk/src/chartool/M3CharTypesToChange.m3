(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

MODULE M3CharTypesToChange;

IMPORT AST, ASTWalk;
IMPORT M3AST_AS;
IMPORT M3Error;
IMPORT M3CharPreds;

REVEAL
  Handle = Public BRANDED OBJECT OVERRIDES callback := Node; END;

PROCEDURE NewHandle (): Handle RAISES {} =
  BEGIN
    RETURN NEW(Handle).init();
  END NewHandle;

PROCEDURE Node (<*UNUSED*> h : Handle;
                           n : AST.NODE;
                           vm: ASTWalk.VisitMode) RAISES {} =
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      TYPECASE n OF
      | M3AST_AS.TYPE_SPEC (ts) =>
          IF M3CharPreds.Tr(n) THEN
            M3Error.Warn(ts, "type needs changing"); END; (* if *)
      ELSE END;                  (* typecase *)
      END;                       (* if *)
  END Node;


BEGIN

END M3CharTypesToChange.
