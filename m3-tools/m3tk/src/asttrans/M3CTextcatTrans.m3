(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CTextcatTrans;

IMPORT AST, AST_Iter;
IMPORT M3Context, M3CId, M3CStdTypes, M3Assert, M3CUnit, M3CSearch;
IMPORT M3AST_AS;
IMPORT SeqM3AST_AS_Actual, SeqM3AST_AS_EXP;
IMPORT M3AST_AS_F, M3AST_SM_F;

PROCEDURE Set(c: M3Context.T; cu: M3AST_AS.Compilation_Unit)=
  VAR text_cu: M3AST_AS.Compilation_Unit; void: M3AST_AS.EXP;
  BEGIN
    M3Assert.Check(M3Context.Find(c, "Text", M3CUnit.Type.Interface, text_cu));
    WITH cat = M3AST_AS.NewUSED_ID() DO
      cat.lx_symrep := M3CId.Enter("Cat");
      M3CSearch.Export(text_cu.as_root, cat);
      M3Assert.Check(cat.sm_def # NIL);
      EVAL Visit(cu, text_cu.as_root, cat.sm_def, NIL, NIL, void);
    END
  END Set;


PROCEDURE Visit(n: AST.NODE;  text_intf: M3AST_AS.Interface;
                textcat: M3AST_AS.Proc_id;
                parent: AST.NODE; parent_iter: AST_Iter.T;
                VAR (* out*) old_exp: M3AST_AS.EXP): M3AST_AS.Call=
  BEGIN
    (* iterate children *)
    VAR
      iter := n.newIter();
      iter2 := n.newIter();
      child, void: AST.NODE;
      child_old_exp: M3AST_AS.EXP;
    BEGIN
      WHILE iter.next(child) DO
      	IF child # NIL THEN
	  WITH r = Visit(child, text_intf, textcat, n, iter2, child_old_exp) DO
            IF r # NIL THEN
              (* If we are a "Actual" node and our "as_exp_type" child
                 was updated, we must also fix up the "sm_actual_s"
                 entry of the parent "Call" node. *)
              TYPECASE n OF
              | M3AST_AS.Actual =>
                  VAR
                    call := NARROW(parent, M3AST_AS.Call);
                    exp_iter := SeqM3AST_AS_EXP.NewIter(call.sm_actual_s);
                    exp: M3AST_AS.EXP;
                    ns := SeqM3AST_AS_EXP.Null;
                  BEGIN
                    WHILE SeqM3AST_AS_EXP.Next(exp_iter, exp) DO
                      IF exp = child_old_exp THEN exp := r; END;
                      SeqM3AST_AS_EXP.AddRear(ns, exp);
                    END;
                    call.sm_actual_s := ns;
                  END;
              ELSE
              END;
            END
          END
	END; (* if *)
	EVAL iter2.next(void);
      END; (* while *)
    END;
    TYPECASE n OF
    | M3AST_AS.Binary(b) =>
        IF ISTYPE(b.as_binary_op, M3AST_AS.Textcat) THEN
          WITH c = TextDotCatCall(text_intf, textcat, b.as_exp1, b.as_exp2) DO
            parent.update(parent_iter, c);
            old_exp := b;
            RETURN c;
          END;
        END;
    ELSE
    END;
    RETURN NIL;
  END Visit;


PROCEDURE TextDotCatCall(
    text_intf: M3AST_AS.Interface;
    textcat: M3AST_AS.Proc_id;
    arg1, arg2: M3AST_AS.EXP): M3AST_AS.EXP=
  VAR
    exp_used_id1, exp_used_id2 := M3AST_AS.NewExp_used_id();
    call := M3AST_AS.NewCall();
    select := M3AST_AS.NewBinary();
  BEGIN
    select.as_binary_op := M3AST_AS.NewSelect();
    select.as_exp1 := exp_used_id1;
    select.as_exp2 := exp_used_id2;
    exp_used_id1.vUSED_ID.lx_symrep := M3CId.Enter("Text");
    exp_used_id1.vUSED_ID.sm_def := text_intf.as_id;
    exp_used_id1.sm_exp_type_spec := M3CStdTypes.Void();
    exp_used_id2.vUSED_ID.lx_symrep := M3CId.Enter("Cat");
    exp_used_id2.vUSED_ID.sm_def := textcat;
    exp_used_id2.sm_exp_type_spec := textcat.sm_type_spec;
    call.as_callexp := select;
    MkActual(call, arg1); MkActual(call, arg2);
    select.sm_exp_type_spec := M3CStdTypes.Text();
    call.sm_exp_type_spec := select.sm_exp_type_spec;   
    RETURN call;
  END TextDotCatCall;

PROCEDURE MkActual(call: M3AST_AS.Call; arg: M3AST_AS.EXP)=
  VAR
    actual := M3AST_AS.NewActual();
  BEGIN
    actual.as_id := NIL; actual.as_exp_type := arg;
    SeqM3AST_AS_Actual.AddRear(call.as_param_s, actual);
    SeqM3AST_AS_EXP.AddRear(call.sm_actual_s, arg);
  END MkActual;

BEGIN
END M3CTextcatTrans.
