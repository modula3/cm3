MODULE M3CChkReturn;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT Text;

IMPORT AST, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_STM;
IMPORT ASTWalk;
IMPORT M3CPragma, M3Error;


TYPE
  Stack = BRANDED REF RECORD
    next: Stack;
    procDecl: M3AST_AS.Proc_decl;
    stm: M3AST_AS.STM;
    ignoring := TRUE;
    inLoop := 0;
    returnInLoop := FALSE;
  END; (* record *)


REVEAL
  Handle = ASTWalk.Closure BRANDED OBJECT
    cu: M3AST_AS.Compilation_Unit;
    stack: Stack := NIL;
  OVERRIDES callback := Node;
  END; (* record *)

<*INLINE*> PROCEDURE DoWarning(n: M3Error.ERROR_NODE; m: TEXT) RAISES {}=
  BEGIN
    M3Error.Warn(n, m);
  END DoWarning;

PROCEDURE NeedsReturnCheck(s: Stack) RAISES {}=
  BEGIN
    WHILE s # NIL DO
      IF s.procDecl # NIL THEN
        s.procDecl.tmp_needs_return_check := TRUE;
        RETURN;
      END;
      s := s.next;
    END;
  END NeedsReturnCheck;


PROCEDURE NotReachedAfterSrcNode(
    h: Handle;
    srcNode: M3AST_AS.SRC_NODE)
    : BOOLEAN
    RAISES {}=
  VAR
    iter := M3CPragma.AfterNode(h.cu.lx_pragmas, srcNode);
    pragma: M3CPragma.T;
    args: Text.T;
  BEGIN
    WHILE M3CPragma.Next(iter, pragma) AND
        M3CPragma.PrecedingNode(pragma) = srcNode DO
      IF M3CPragma.Match(pragma, "NOTREACHED", args) AND args = NIL THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END NotReachedAfterSrcNode;


PROCEDURE PushLastStm(
    h: Handle;
    srcNode: M3AST_AS.SRC_NODE;
    seqSTM: SeqM3AST_AS_STM.T;
    procDecl: M3AST_AS.Proc_decl := NIL)
    RAISES {}=
  VAR
    last, stm: M3AST_AS.STM := NIL;
    iter := SeqM3AST_AS_STM.NewIter(seqSTM);
  BEGIN
    WHILE SeqM3AST_AS_STM.Next(iter, stm) DO last := stm END;
    IF last = NIL THEN
      IF NOT NotReachedAfterSrcNode(h, srcNode) THEN
        DoWarning(srcNode, "last statement in function is not a RETURN");
      END;
      IF procDecl # NIL THEN
        procDecl.tmp_needs_return_check := TRUE;
      ELSE
        NeedsReturnCheck(h.stack);
      END;
    ELSE
      h.stack := NEW(Stack, next := h.stack, procDecl := procDecl, stm := last);
    END;
  END PushLastStm;


PROCEDURE NotReachedAfterStm(h: Handle; stm: M3AST_AS.STM): BOOLEAN RAISES {}=
  VAR
    iter := M3CPragma.AfterStmOrDecl(h.cu.lx_pragmas, stm);
    pragma: M3CPragma.T;
    args: Text.T;
  BEGIN
    WHILE M3CPragma.Next(iter, pragma) AND
        M3CPragma.PrecedingStmOrDecl(pragma) = stm DO
      IF M3CPragma.Match(pragma, "NOTREACHED", args) AND args = NIL THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END NotReachedAfterStm;


PROCEDURE Node(
    h: Handle;
    n: AST.NODE;
    vm: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      TYPECASE n OF
      | M3AST_AS.Proc_decl(procDecl) =>
          VAR
            body := procDecl.as_body;
          BEGIN
            IF body # NIL AND procDecl.as_type.as_result_type # NIL THEN
              PushLastStm(h, body, body.as_stm_s, procDecl);
            END;
          END;
      ELSE
        IF h.stack # NIL THEN
          IF h.stack.ignoring THEN
            IF h.stack.stm # n THEN RETURN END;
            TYPECASE n OF
            | M3AST_AS.Assign_st, M3AST_AS.Call_st,
              M3AST_AS.Exit_st, M3AST_AS.Eval_st =>
                NeedsReturnCheck(h.stack);
                IF NOT NotReachedAfterStm(h, h.stack.stm) THEN
                  DoWarning(h.stack.stm,
                      "last statement in function is not a RETURN");
                END;
            | M3AST_AS.Return_st, M3AST_AS.Raise_st =>
            ELSE
              (* Complex statement, containing sub statements *)
              h.stack.ignoring := FALSE;
              TYPECASE n OF
              | M3AST_AS.While_st, M3AST_AS.Repeat_st,
                M3AST_AS.Loop_st, M3AST_AS.For_st =>
                  INC(h.stack.inLoop);
                  NeedsReturnCheck(h.stack);
              | M3AST_AS.If_st(ifSt) =>
                  IF ifSt.as_else = NIL THEN
                    NeedsReturnCheck(h.stack);
                    IF NOT NotReachedAfterStm(h, ifSt) THEN
                      DoWarning(ifSt,
                          "last statement in function is IF with no ELSE");
                    END;
                  ELSE
                    PushLastStm(h, ifSt, ifSt.as_stm_s);
                  END;
              | M3AST_AS.STM_WSS(stmWSS) =>
                  PushLastStm(h, stmWSS, stmWSS.as_stm_s);
              ELSE
              END;
            END;
            RETURN;
          ELSE
            IF h.stack.inLoop > 0 THEN
              TYPECASE n OF
              | M3AST_AS.Return_st =>
                  h.stack.returnInLoop := TRUE;
              | M3AST_AS.Exit_st(exitSt) =>
                  IF h.stack.inLoop = 1 THEN
                    DoWarning(exitSt,
                        "EXIT will leave function without returning value");
                  END;
              | M3AST_AS.While_st, M3AST_AS.Repeat_st,
                M3AST_AS.Loop_st, M3AST_AS.For_st =>
                  INC(h.stack.inLoop);
              ELSE
              END;
            ELSE
              TYPECASE n OF
              | M3AST_AS.Try_except =>
                  (* AST problem; not really a SUBSTM_WSS at all.. *)
              | M3AST_AS.SUBSTM_WSS(subStmWSS) =>
                  PushLastStm(h, subStmWSS, subStmWSS.as_stm_s);
              ELSE
              END;
            END;
          END;
        END;
      END; (* typecase *)
    ELSE
      IF h.stack # NIL THEN
        IF h.stack.stm = n THEN
          IF h.stack.inLoop # 0 AND NOT h.stack.returnInLoop THEN
            IF NOT NotReachedAfterStm(h, h.stack.stm) THEN
              DoWarning(h.stack.stm,
                  "loop at end of function does not contain RETURN");
            END;
          END;
          h.stack := h.stack.next;
        ELSIF h.stack.inLoop > 1 THEN
          TYPECASE n OF
          | M3AST_AS.While_st, M3AST_AS.Repeat_st,
            M3AST_AS.Loop_st, M3AST_AS.For_st =>
                DEC(h.stack.inLoop);
          ELSE
          END;
        END;
      END;
    END;
  END Node;


PROCEDURE NewHandle(cu: M3AST_AS.Compilation_Unit): Handle RAISES {}=
  BEGIN
    RETURN NEW(Handle, cu := cu);
  END NewHandle;


BEGIN

END M3CChkReturn.
