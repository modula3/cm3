MODULE M3ExpandCalls;

(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


IMPORT AST, M3AST_AS;
IMPORT M3Context;
IMPORT ASTWalk;
IMPORT M3CStdProcs;
IMPORT SeqM3AST_AS_Actual, SeqM3AST_AS_EXP;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

TYPE
  ExpandClosure = ASTWalk.Closure OBJECT
    unit_id: M3AST_AS.UNIT_ID;
    standard_id: M3AST_AS.UNIT_ID;
  OVERRIDES callback := Expand;
  END;

PROCEDURE Set(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    ASTWalk.VisitNodes(cu,
        NEW(ExpandClosure, unit_id := cu.as_root.as_id,
            standard_id := M3Context.Standard().as_root.as_id));
  END Set;

PROCEDURE Expand(
    cl: ExpandClosure;
    n: AST.NODE;
    vm: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Call(call) =>
        VAR st_call: M3CStdProcs.T;
            proc_id: M3AST_AS.Proc_id;
        BEGIN
          IF M3CStdProcs.IsStandardCall(call, st_call) THEN
            (* ignore *)
          ELSE
            ReplaceActuals(cl, call);
          END; (* if *)
        END; (* begin *)
    ELSE (* ignore *) 	
    END; (* typecase *)
  END Expand;

PROCEDURE ReplaceActuals(cl: ExpandClosure; call: M3AST_AS.Call) RAISES {}=
  VAR iter := SeqM3AST_AS_EXP.NewIter(call.sm_actual_s);
    exp: M3AST_AS.EXP;
    actual_s := SeqM3AST_AS_Actual.Null;
  BEGIN
    WHILE SeqM3AST_AS_EXP.Next(iter, exp) DO
      WITH a = M3AST_AS.NewActual() DO
        TYPECASE exp OF
        | M3AST_AS.Exp_used_id(e) =>
           WITH tmp_unit_id = e.vUSED_ID.sm_def.tmp_unit_id DO
            IF tmp_unit_id # cl.unit_id AND
               tmp_unit_id # cl.standard_id THEN
              (* must convert to a Select *)
              WITH b = M3AST_AS.NewBinary() DO
                b.lx_srcpos := e.lx_srcpos;
                b.as_binary_op := M3AST_AS.NewSelect();
                WITH e1 = M3AST_AS.NewExp_used_id() DO
                  e1.vUSED_ID.lx_symrep := tmp_unit_id.lx_symrep;
                  e1.vUSED_ID.sm_def := tmp_unit_id;
                  b.as_exp1 := e1;
                END; (* with *)
                b.as_exp2 := e;
                b.sm_exp_type_spec := e.sm_exp_type_spec;
                b.sm_exp_value := e.sm_exp_value;
                exp := b;
              END; (* with *)
            END; (* if *)
           END;
        ELSE
        END; (* typecase *)
        a.as_exp_type := exp;
        SeqM3AST_AS_Actual.AddRear(actual_s, a);
      END;
    END; (* while *)
    call.as_param_s := actual_s;
  END ReplaceActuals;


BEGIN

END M3ExpandCalls.
