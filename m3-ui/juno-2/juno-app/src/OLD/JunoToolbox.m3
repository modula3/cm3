(* Copyright 1992 Digital Equipment Corporation                              *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct  5 16:26:26 PDT 1992 by heydon                   *)
(*      modified on Fri Aug 7 21:51:53 PDT 1992 by myers                     *)

MODULE JunoToolbox;

IMPORT Drawing;
IMPORT JunoScope AS Scope, JunoAST AS AST;
IMPORT VBT, ButtonVBT, Split, TextVBT, PackSplit, FVTypes, PaintOp;
IMPORT Atom;

REVEAL
  T = PackSplit.T BRANDED OBJECT END;

TYPE
  Button = ButtonVBT.T OBJECT
    sel    : VBT.Selection;
    closure: Closure;
    tool   : Drawing.Tool;
  END;

VAR fvBg := PaintOp.FromRGB(0.8, 0.8, 0.8);

PROCEDURE New (module_name: Atom.T;
               md         : Scope.Mod;
               cl         : Closure;
               sel        : VBT.Selection): T =
  VAR
    res  : T := NEW(T).init(bg := fvBg);
    names    := Scope.Names(md.scp);
  BEGIN
    FOR i := FIRST(names) TO LAST(names) DO
      VAR qid: AST.QId;
      BEGIN
        IF module_name = NIL
          THEN qid := NEW(AST.QId, id0 := names[i], id1 := AST.NilId)
          ELSE qid := NEW(AST.QId, id0 := module_name, id1 := names[i])
        END;
        VAR ent := Scope.Lookup(scp, names[i], localOnly := TRUE); BEGIN
          TYPECASE ent OF
            Scope.Proc (proc) =>
              IF proc.inout_cnt = 0 AND proc.out_cnt = 0 THEN
                VAR
                  tool := Drawing.Tool{kind := Drawing.ToolKind.Procedure,
                                       name := qid, arity := proc.in_cnt};
                BEGIN
                  Split.AddChild(res, NEW(Button, sel := sel, tool := tool,
                                          closure := closure).init(
                                        NEW(TextVBT.T).init(
                                          "PROC " & Atom.Name(names[i])),
                                        action := ButtonProc))
                END
              END
          | Scope.Pred (pred) =>
              VAR
                tool := Drawing.Tool{kind := Drawing.ToolKind.Predicate,
                                     name := qid, arity := pred.in_cnt};
              BEGIN
                Split.AddChild(res, NEW(Button, sel := sel, tool := tool,
                  closure := closure).init(NEW(TextVBT.T).init(
                    "PRED " & Atom.Name(names[i])), action := ButtonProc))
              END
          ELSE
            (* SKIP *)
          END
        END
      END
    END;
    RETURN res
  END New;

PROCEDURE ButtonProc (self_: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  VAR self: Button := self_; BEGIN
    IF self.tool.arity # 0 THEN
      TRY
        VBT.Acquire(self, sel, cd.time)
      EXCEPT
        VBT.Error => (* SKIP *)
      END
    END;
    self.closure.apply(self.tool)
  END ButtonProc;

BEGIN
END JunoToolbox.
