MODULE M3CDuplicate;

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

IMPORT M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;


IMPORT M3Error, M3ASTNext, M3CBackEnd, M3COrdinal;


TYPE
  Tree = OBJECT
    left, right: Tree := NIL;
    count: INTEGER;
    low, high: M3AST_SM.Exp_value;
  END;


PROCEDURE NewLeaf(
    count: INTEGER;
    low, high: M3AST_SM.Exp_value)
    : Tree
    RAISES {}=
  BEGIN
    RETURN NEW(Tree, count := count, low := low, high := high);
  END NewLeaf;


PROCEDURE AddRange(
    low, high: M3AST_SM.Exp_value;
    count: INTEGER;
    VAR tree: Tree)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF tree = NIL THEN
      tree := NewLeaf(count, low, high);
      RETURN TRUE;
    ELSIF M3CBackEnd.Compare(low, tree.high) > 0 THEN
      RETURN AddRange(low, high, count, tree.right);
    ELSIF M3CBackEnd.Compare(high, tree.low) < 0 THEN
      RETURN AddRange(low, high, count, tree.left);
    ELSE
      IF M3CBackEnd.Compare(low, tree.low) < 0 THEN tree.low := low END;
      IF M3CBackEnd.Compare(high, tree.high) > 0 THEN tree.high := high END;
      WITH result = count = tree.count DO
        tree.count := count;
        RETURN result;
      END;
    END; (* if *)
  END AddRange;


PROCEDURE CheckExp(
    exp: M3AST_AS.EXP;
    VAR val: M3AST_SM.Exp_value)
    : BOOLEAN
    RAISES {}=
  VAR
    type := exp.sm_exp_type_spec;
    baseType: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF M3COrdinal.Is(type, baseType) AND
        (type # NIL) AND (exp.sm_exp_value # NIL) THEN
      val := exp.sm_exp_value;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END CheckExp;


PROCEDURE CaseLabels(caseSt: M3AST_AS.Case_st) RAISES {}=
  VAR
    tree: Tree := NIL;
    iter := M3ASTNext.NewIterCaseLabel(caseSt.as_case_s);
    count := 0;
    oldCase, case: M3AST_AS.Case := NIL;
    label: M3AST_AS.RANGE_EXP;
    val1, val2: M3AST_SM.Exp_value;
  BEGIN
    WHILE M3ASTNext.CaseLabel(iter, case, label) DO
      IF case # oldCase THEN INC(count); oldCase := case END;
      TYPECASE label OF <*NOWARN*>
      | M3AST_AS.Range(range) =>
          IF CheckExp(range.as_exp1, val1) AND
              CheckExp(range.as_exp2, val2) THEN
            IF M3CBackEnd.Compare(val1, val2) <= 0 THEN
              IF NOT AddRange(val1, val2, count, tree) THEN
                M3Error.Report(range.as_exp1, "duplicated case label");
              END; (* if *)
            END; (* if *)
          END; (* if *)
      | M3AST_AS.Range_EXP(rangeExp) =>
          IF CheckExp(rangeExp.as_exp, val1) AND
              (NOT AddRange(val1, val1, count, tree)) THEN
            M3Error.Report(label, "duplicated case label");
          END; (* if *)
      END; (* if *)
    END; (* while *)
  END CaseLabels;


TYPE
  List = RECORD
    number: INTEGER;
    ids: ARRAY [0..31] OF RECORD id: M3AST_AS.DEF_ID; count: INTEGER END;
    next: REF List;
  END;


PROCEDURE AddId(
    id: M3AST_AS.DEF_ID;
    count: INTEGER;
    VAR list: List)
    : BOOLEAN
    RAISES {}=
  BEGIN
    FOR i := 0 TO list.number - 1 DO
      WITH compare = list.ids[i] DO
        IF id = compare.id THEN
          IF compare.count = count THEN
            RETURN TRUE;
          ELSE
            compare.count := count;
            RETURN FALSE;
          END;
        END;
      END;
    END;
    IF list.number = NUMBER(list.ids) AND list.next = NIL THEN
      list.next := NEW(REF List, number := 0, next := NIL);
    END;
    IF list.next # NIL THEN
      RETURN AddId(id, count, list.next^);
    ELSE
      WITH add = list.ids[list.number] DO
        add.id := id;
        add.count := count;
      END;
      INC(list.number);
      RETURN TRUE
    END;
  END AddId;


PROCEDURE HandlerExceptions(except: M3AST_AS.Try_except) RAISES {}=
  VAR
    list: List;
    iter := M3ASTNext.NewIterHandlerLabel(except.as_handler_s);
    count := 0;
    oldHandler, handler: M3AST_AS.Handler;
    qId: M3AST_AS.Qual_used_id;
    defId: M3AST_AS.DEF_ID;
  BEGIN
    list.number := 0;
    list.next := NIL;
    oldHandler := NIL;
    WHILE M3ASTNext.HandlerLabel(iter, handler, qId) DO
      IF handler # oldHandler THEN INC(count); oldHandler := handler END;
      defId := qId.as_id.sm_def;
      IF defId # NIL AND NOT AddId(defId, count, list) THEN
        M3Error.ReportWithId(qId, "duplicate exception handler label \'%s\'",
            defId.lx_symrep);
      END; (* if *)
    END; (* while *)
  END HandlerExceptions;


BEGIN
END M3CDuplicate.
