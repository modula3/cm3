(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CLockTrans;

IMPORT AST, ASTWalk;
IMPORT M3Context, M3CUnit, M3Conventions, M3CId;
IMPORT M3AST_AS, M3CStdTypes;
IMPORT M3CSearch;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_FE_F, M3AST_TM_F;
IMPORT SeqM3AST_AS_STM, SeqM3AST_AS_Actual, SeqM3AST_AS_EXP,
       SeqM3AST_AS_Binding, SeqM3AST_AS_STM_rep;

CONST
  ThreadT = "Thread";
  AcquireT = "Acquire";
  ReleaseT = "Release";
  MT = "t__mutex__4678361";

TYPE
  SetUnitClosure = M3Context.Closure OBJECT
  OVERRIDES callback := SetUnit;
  END;

TYPE
 SetNodeClosure = ASTWalk.Closure OBJECT 
   c: M3Context.T;
   cu: M3AST_AS.Compilation_Unit;
 OVERRIDES
   callback := SetNode;
 END;

PROCEDURE Run(c: M3Context.T) RAISES {NoThread}=
  BEGIN
    M3Context.ApplyToSet(c, NEW(SetUnitClosure),
                         M3CUnit.TypeSet{M3CUnit.Type.Module,
                                         M3CUnit.Type.Module_gen_ins});
  END Run;

PROCEDURE SetUnit(cl: SetUnitClosure; ut: M3CUnit.Type; name: TEXT;
    cu: M3AST_AS.Compilation_Unit) RAISES {NoThread}=
  BEGIN
    ASTWalk.VisitNodes(cu, NEW(SetNodeClosure, cu := cu, c := cl.context));
  END SetUnit;

PROCEDURE SetNode(cl: SetNodeClosure; n: AST.NODE; 
    vm: ASTWalk.VisitMode) RAISES {NoThread}=
  BEGIN
    TYPECASE n OF
    | NULL =>
    | M3AST_AS.STM_WSS(t) =>  CheckForLock(cl, t.as_stm_s);
    | M3AST_AS.SUBSTM_WSS(t) => CheckForLock(cl, t.as_stm_s);
    ELSE
    END; (* typecase *)
  END SetNode;

PROCEDURE CheckForLock(cl: SetNodeClosure;
                       s: SeqM3AST_AS_STM.T) RAISES {NoThread}=
  VAR
    iter := SeqM3AST_AS_STM.NewIter(s);
    stm: M3AST_AS.STM;
    try_st: M3AST_AS.Try_st;
    with_st: M3AST_AS.With_st;
    try_finally: M3AST_AS.Try_finally;
    m_used_id := M3AST_AS.NewExp_used_id();
    binding: M3AST_AS.Binding;
  BEGIN 
    WHILE SeqM3AST_AS_STM.Next(iter, stm) DO
      TYPECASE stm OF
      | M3AST_AS.Lock_st(lock_st) =>
          try_st := M3AST_AS.NewTry_st();
          try_st.as_stm_s := lock_st.as_stm_s;
          with_st := M3AST_AS.NewWith_st();
          binding := M3AST_AS.NewBinding();
          binding.as_id := M3AST_AS.NewWith_id();
          binding.as_id.lx_symrep := M3CId.Enter(MT);
          binding.as_id.sm_type_spec := M3CStdTypes.Mutex();
          binding.as_id.tmp_unit_id := cl.cu.as_root.as_id;
          binding.as_exp := lock_st.as_exp;
          SeqM3AST_AS_Binding.AddFront(with_st.as_binding_s, binding);
          m_used_id.vUSED_ID.lx_symrep := binding.as_id.lx_symrep;
          m_used_id.vUSED_ID.sm_def := binding.as_id;
          m_used_id.sm_exp_type_spec := binding.as_id.sm_type_spec;
          with_st.as_stm_s := SingleStm(ThreadCall(cl, AcquireT, m_used_id));
          SeqM3AST_AS_STM.AddRear(with_st.as_stm_s, try_st);
          try_finally := M3AST_AS.NewTry_finally();
          try_finally.as_stm_s := SingleStm(ThreadCall(cl, ReleaseT, m_used_id));
          try_st.as_try_tail := try_finally;
          ReplaceInSeqSTM(s, stm, with_st);
      ELSE
      END; (* typecase *)
    END; (* while *)
  END CheckForLock;

PROCEDURE ThreadCall(cl: SetNodeClosure;
          t: TEXT; arg: M3AST_AS.EXP): M3AST_AS.STM RAISES {NoThread}=
  VAR
    call_st := M3AST_AS.NewCall_st();
    exp_used_id1, exp_used_id2 := M3AST_AS.NewExp_used_id();
    call := M3AST_AS.NewCall();
    actual := M3AST_AS.NewActual();
    select := M3AST_AS.NewBinary();
  BEGIN
    select.as_binary_op := M3AST_AS.NewSelect();
    select.as_exp1 := exp_used_id1;
    select.as_exp2 := exp_used_id2;
    exp_used_id1.vUSED_ID.lx_symrep := M3CId.Enter(ThreadT);
    exp_used_id2.vUSED_ID.lx_symrep := M3CId.Enter(t);
    call.as_callexp := select;
    actual.as_id := NIL; actual.as_exp_type := arg;
    SeqM3AST_AS_Actual.AddFront(call.as_param_s, actual);
    SeqM3AST_AS_EXP.AddFront(call.sm_actual_s, arg);
    FindProc(cl, exp_used_id1.vUSED_ID, exp_used_id2.vUSED_ID);
    select.sm_exp_type_spec := 
        NARROW(exp_used_id2.vUSED_ID.sm_def, M3AST_AS.Proc_id).sm_type_spec;
    exp_used_id2.sm_exp_type_spec := select.sm_exp_type_spec;
    call.sm_exp_type_spec := M3CStdTypes.Void();   
    call_st.as_call := call;
    RETURN call_st;
  END ThreadCall;

PROCEDURE FindProc(cl: SetNodeClosure; 
    id1, id2: M3AST_AS.USED_ID) RAISES {NoThread}=
  VAR
    thread_cu: M3AST_AS.Compilation_Unit;
  BEGIN
    IF M3Context.Find(cl.c, ThreadT, M3CUnit.Type.Interface, thread_cu) THEN
      id1.sm_def := thread_cu.as_root.as_id;
      M3CSearch.Export(thread_cu.as_root, id2)
    ELSE
      RAISE NoThread
    END;
  END FindProc;

PROCEDURE SingleStm(s: M3AST_AS.STM): SeqM3AST_AS_STM.T RAISES {}=
  VAR seq := SeqM3AST_AS_STM.Null;
  BEGIN
    SeqM3AST_AS_STM.AddFront(seq, s);
    RETURN seq;
  END SingleStm;

PROCEDURE ReplaceInSeqSTM(s: SeqM3AST_AS_STM.T; 
    old, new: M3AST_AS.STM) RAISES {}=
  BEGIN
    (* here we are using the _priv interfaces to do a replace *)
    WHILE s # NIL DO
      IF s.elem = old THEN s.elem := new; RETURN 
      ELSE s := s.next;
      END;
    END; (* while *)
  END ReplaceInSeqSTM;


BEGIN
END M3CLockTrans.
