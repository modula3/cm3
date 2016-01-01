(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Marker.m3                                             *)
(* Last modified on Tue Jun 20 09:14:19 PDT 1995 by kalsow     *)
(* Last modified on Fri Jun 16 17:35:38 PDT 1995 by ericv      *)
(*      modified on Fri Feb 15 03:21:08 1991 by muller         *)

MODULE Marker;

IMPORT CG, Error, Type, Variable, ProcType, ESet, Expr, AssignStmt;
IMPORT M3ID, M3RT, Target, Module, RunTyme, Procedure, Host;

TYPE
  Kind = { zFINALLY, zFINALLYPROC, zLOCK, zEXIT, zTRY, zTRYELSE,
           zRAISES, zPROC};

  FramePtr = REF Frame;

  Frame = RECORD
    kind       : Kind;
    outermost  : BOOLEAN;
    saved      : BOOLEAN;
    returnSeen : BOOLEAN;
    exitSeen   : BOOLEAN;
    info       : CG.Var;
    start      : CG.Label;
    stop       : CG.Label;
    type       : Type.T;     (* kind = PROC *)
    variable   : Variable.T; (* kind = PROC *)
    tmp_result : CG.Var;     (* kind = PROC *)
    e_set      : ESet.T;     (* kind = RAISES, TRY *)
    next       : FramePtr;
    callConv   : CG.CallingConvention;
    handler    : CG.Proc;
    h_level    : INTEGER;
  END;

CONST
  RT_Kind = ARRAY Kind OF INTEGER {
    ORD (M3RT.HandlerClass.Finally),
    ORD (M3RT.HandlerClass.FinallyProc),
    ORD (M3RT.HandlerClass.Lock),
    -1, (* exit *)
    ORD (M3RT.HandlerClass.Except),
    ORD (M3RT.HandlerClass.ExceptElse),
    ORD (M3RT.HandlerClass.Raises),
    -1 (* proc *)
  };

VAR
  all_frames   : FramePtr := NIL;
  n_frames     : INTEGER  := 0;
  save_depth   : INTEGER  := 0;
  tos          : INTEGER  := 0;
  stack        : ARRAY [0..50] OF Frame;

(*---------------------------------------------------------- marker stack ---*)

PROCEDURE SaveFrame () =
  VAR p := NEW (FramePtr);
  BEGIN
    <*ASSERT save_depth >= 0*>
    WITH z = stack [tos-1] DO
      z.saved := TRUE;  INC (save_depth);
      p^ := z;
      (*******
      p.outermost := (save_depth <= 1);
          - this only works if the front-end doesn't inline
             nested procedures and the back-end doesn't screw
             around reordering labels.
      ********)
      p.next := all_frames;
      all_frames := p;
      INC (n_frames);
    END;
  END SaveFrame;

<*INLINE*> PROCEDURE Pop () =
  BEGIN
    DEC (tos);
    IF (stack[tos].saved) THEN DEC (save_depth) END;
    <*ASSERT save_depth >= 0*>
  END Pop;

PROCEDURE PushFinally (l_start, l_stop: CG.Label;  info: CG.Var) =
  BEGIN
    Push (Kind.zFINALLY, l_start, l_stop, info);
  END PushFinally;

PROCEDURE PushFinallyProc (l_start, l_stop: CG.Label;  info: CG.Var;
                           handler: CG.Proc;  h_level: INTEGER) =
  BEGIN
    Push (Kind.zFINALLYPROC, l_start, l_stop, info);
    WITH z = stack[tos - 1] DO
      z.handler := handler;
      z.h_level := h_level;
    END;
  END PushFinallyProc;

PROCEDURE PopFinally (VAR(*OUT*) returnSeen, exitSeen: BOOLEAN) =
  BEGIN
    Pop ();
    returnSeen := stack[tos].returnSeen;
    exitSeen   := stack[tos].exitSeen;
  END PopFinally;

PROCEDURE PushLock (l_start, l_stop: CG.Label;  mutex: CG.Var) =
  BEGIN
    Push (Kind.zLOCK, l_start, l_stop, mutex);
  END PushLock;

PROCEDURE PushTry (l_start, l_stop: CG.Label;  info: CG.Var;  ex: ESet.T) =
  BEGIN
    Push (Kind.zTRY, l_start, l_stop, info, ex);
  END PushTry;

PROCEDURE PushTryElse (l_start, l_stop: CG.Label;  info: CG.Var) =
  BEGIN
    Push (Kind.zTRYELSE, l_start, l_stop, info);
  END PushTryElse;

PROCEDURE PushExit (l_stop: CG.Label) =
  BEGIN
    Push (Kind.zEXIT, l_stop := l_stop);
  END PushExit;

PROCEDURE PushRaises (l_start, l_stop: CG.Label;  ex: ESet.T;  info: CG.Var) =
  BEGIN
    Push (Kind.zRAISES, l_start, l_stop, info, ex);
  END PushRaises;

PROCEDURE PushProcedure (t: Type.T; v: Variable.T; cc: CG.CallingConvention) =
  BEGIN
    <* ASSERT (t = NIL) = (v = NIL) *>
    Push (Kind.zPROC);
    WITH z = stack[tos - 1] DO
      z.type     := t;
      z.variable := v;
      z.callConv := cc;
    END;
  END PushProcedure;

PROCEDURE Push (k: Kind;  l_start, l_stop: CG.Label := CG.No_label;
                info: CG.Var := NIL;  ex: ESet.T := NIL) =
  BEGIN
    WITH z = stack[tos] DO
      z.kind       := k;
      z.saved      := FALSE;
      z.outermost  := FALSE;
      z.returnSeen := FALSE;
      z.exitSeen   := FALSE;
      z.start      := l_start;
      z.stop       := l_stop;
      z.info       := info;
      z.type       := NIL;
      z.variable   := NIL;
      z.tmp_result := NIL;
      z.e_set      := ex;
      z.next       := NIL;
      z.callConv   := NIL;
      z.handler    := NIL;
      z.h_level    := 0;
    END;
    INC (tos);
  END Push;

(*--------------------------------------------- explicit frame operations ---*)

PROCEDURE PushFrame (frame: CG.Var;  class: M3RT.HandlerClass) =
  VAR push: Procedure.T;
  BEGIN
    CG.Load_intt (ORD (class));
    CG.Store_int (Target.Integer.cg_type, frame, M3RT.EF_class);
    push := RunTyme.LookUpProc (RunTyme.Hook.PushEFrame);
    Procedure.StartCall (push);
    CG.Load_addr_of (frame, 0, Target.Address.align);
    CG.Pop_param (CG.Type.Addr);
    Procedure.EmitCall (push);
  END PushFrame;

PROCEDURE PopFrame (frame: CG.Var) =
  VAR pop: Procedure.T;
  BEGIN
    pop := RunTyme.LookUpProc (RunTyme.Hook.PopEFrame);
    Procedure.StartCall (pop);
    CG.Load_addr (frame, M3RT.EF_next);
    CG.Pop_param (CG.Type.Addr);
    Procedure.EmitCall (pop);
  END PopFrame;

PROCEDURE SetLock (acquire: BOOLEAN;  var: CG.Var;  offset: INTEGER) =
  VAR method_offset: INTEGER;
  BEGIN
    IF acquire
      THEN method_offset := M3RT.MUTEX_acquire;
      ELSE method_offset := M3RT.MUTEX_release;
    END;

    CG.Start_call_indirect (CG.Type.Void, Target.DefaultCall);

    CG.Load_addr (var, offset); (* mutext object *)
    CG.Pop_param (CG.Type.Addr);

    CG.Load_addr (var, offset); (* mutex object *)
    CG.Boost_alignment (Target.Address.align);
    CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);  (* method list *)
    CG.Boost_alignment (Target.Address.align);
    CG.Load_indirect (CG.Type.Addr, method_offset, Target.Address.size); (* proc *)
    CG.Boost_alignment (Target.Address.align);

    CG.Gen_Call_indirect (CG.Type.Void, Target.DefaultCall);    
  END SetLock;

PROCEDURE CallFinallyHandler (info: CG.Var;
                              handler: CG.Proc;  h_level: INTEGER) =
  BEGIN
    IF (handler # NIL) THEN
      CG.Start_call_direct (handler, h_level, CG.Type.Void);
      CG.Call_direct (handler, CG.Type.Void);
    ELSE
      CG.Start_call_indirect (CG.Type.Void, Target.DefaultCall);
      CG.Load_addr (info, M3RT.EF2_frame);
      CG.Pop_static_link ();
      CG.Load_addr (info, M3RT.EF2_handler);
      CG.Gen_Call_indirect (CG.Type.Void, Target.DefaultCall);
    END;
  END CallFinallyHandler;

PROCEDURE CaptureState (frame: CG.Var;  jmpbuf: CG.Var;  handler: CG.Label) =
  VAR setjmp := Module.GetSetjmp (Module.Current ());
  BEGIN

    IF Target.Alloca_jmpbuf THEN
      CG.Load_addr (jmpbuf);
      CG.Store_addr (frame, M3RT.EF1_jmpbuf);
    END;

    CG.Start_call_direct (setjmp, 0, Target.Integer.cg_type);

    IF Target.Alloca_jmpbuf THEN
      CG.Load_addr (jmpbuf);
    ELSE
      CG.Load_addr_of (frame, M3RT.EF1_jmpbuf, 128);
    END;
    CG.Pop_param (CG.Type.Addr);
    CG.Call_direct (setjmp, Target.Integer.cg_type);
    CG.If_true (handler, CG.Never);
  END CaptureState;

(*------------------------------------------------------ misc. predicates ---*)

PROCEDURE ExitOK (): BOOLEAN =
  BEGIN
    FOR i := tos - 1 TO 0 BY  -1 DO
      WITH z = stack[i] DO
        IF (z.kind = Kind.zTRYELSE) THEN
          Error.Warn (1, "EXIT will be caught by TRY EXCEPT ELSE clause");
        END;
        IF (z.kind = Kind.zEXIT) THEN RETURN TRUE END;
        IF (z.kind = Kind.zPROC) THEN RETURN FALSE END;
      END;
    END;
    RETURN FALSE;
  END ExitOK;

PROCEDURE ReturnOK (): BOOLEAN =
  BEGIN
    FOR i := tos - 1 TO 0 BY  -1 DO
      WITH z = stack[i] DO
        IF (z.kind = Kind.zTRYELSE) THEN
          Error.Warn (1, "RETURN will be caught by TRY EXCEPT ELSE clause");
        END;
        IF (z.kind = Kind.zPROC) THEN RETURN TRUE END;
      END;
    END;
    RETURN FALSE;
  END ReturnOK;

PROCEDURE ReturnVar (VAR(*OUT*) t: Type.T;  VAR(*OUT*) v: Variable.T) =
  BEGIN
    FOR i := tos - 1 TO 0 BY  -1 DO
      WITH z = stack[i] DO
        IF (z.kind = Kind.zPROC) THEN
          t := z.type;
          v := z.variable;
          RETURN;
        END;
      END;
    END;
    <* ASSERT FALSE *>
  END ReturnVar;

(*------------------------------------------------------- code generation ---*)


PROCEDURE EmitExit () =
  VAR i: INTEGER;
  BEGIN
    (* mark every frame out to the loop boundary as 'exitSeen' *)
    i := tos - 1;
    WHILE (i >= 0) DO
      WITH z = stack[i] DO
        z.exitSeen := TRUE;
        IF (z.kind = Kind.zEXIT) OR (z.kind = Kind.zTRYELSE) THEN EXIT END;
      END;
      DEC (i);
    END;

    IF Target.Has_stack_walker
      THEN EmitExit1 ();
      ELSE EmitExit2 ();
    END;
  END EmitExit;

PROCEDURE EmitExit1 () =
  VAR i: INTEGER;
  BEGIN
    (* unwind as far as possible *)
    i := tos - 1;
    WHILE (i >= 0) DO
      WITH z = stack[i] DO
        CASE z.kind OF
        | Kind.zTRYELSE =>
            CG.Load_intt (Exit_exception);
            CG.Store_int (Target.Integer.cg_type, z.info);
            CG.Jump (z.stop);
            EXIT;
        | Kind.zFINALLY, Kind.zFINALLYPROC =>
            CG.Load_intt (Exit_exception);
            CG.Store_int (Target.Integer.cg_type, z.info);
            CG.Jump (z.stop);
            EXIT;
        | Kind.zLOCK =>
            SetLock (FALSE, z.info, 0);
        | Kind.zEXIT =>
            CG.Jump (z.stop);
            EXIT;
        | Kind.zTRY =>
            (* ignore *)
        | Kind.zRAISES, Kind.zPROC =>
            Error.Msg ("INTERNAL ERROR: EXIT not in loop");
            <* ASSERT FALSE *>
            (* EXIT; *)
        END;
      END;
      DEC (i);
    END;
  END EmitExit1;

PROCEDURE EmitExit2 () =
  VAR i: INTEGER;
  BEGIN
    (* unwind as far as possible *)
    i := tos - 1;
    WHILE (i >= 0) DO
      WITH z = stack[i] DO
        CASE z.kind OF
        | Kind.zTRYELSE, Kind.zFINALLY =>
            PopFrame (z.info);
            CG.Load_intt (Exit_exception);
            CG.Store_int (Target.Integer.cg_type,
                          z.info, M3RT.EF1_info + M3RT.EA_exception);
            CG.Jump (z.stop);
            EXIT;
        | Kind.zFINALLYPROC =>
            PopFrame (z.info);
            CallFinallyHandler (z.info, z.handler, z.h_level);
        | Kind.zLOCK =>
            PopFrame (z.info);
            SetLock (FALSE, z.info, M3RT.EF4_mutex);
        | Kind.zEXIT =>
            CG.Jump (z.stop);
            EXIT;
        | Kind.zTRY =>
            PopFrame (z.info);
        | Kind.zRAISES, Kind.zPROC =>
            Error.Msg ("INTERNAL ERROR: EXIT not in loop");
            <* ASSERT FALSE *>
            (* EXIT; *)
        END;
      END;
      DEC (i);
    END;
  END EmitExit2;
  
PROCEDURE AllocReturnTemp () =
  VAR
    ret_info: Type.Info;
  BEGIN
    (* Normally, to return a value from a procedure we assign to
       z.variable.  In the case of large-result procedures, z.variable
       is a hidden VAR parameter that points to a temporary allocated
       on the caller's stack.  This means that we can safely write
       to z.variable even if the eventual procedure outcome is an
       exception rather than a RETURN.

       However, if direct struct returns are enabled, the caller is
       allowed to pass the final destination address rather than a
       temporary, i.e. z.variable may refer to a real variable.  In
       this case it is not safe to write to z.variable unless the
       procedure outcome is a RETURN.  This causes problems for
       RETURN statements in the scope of a TRY-FINALLY, since we
       need to save the return result somewhere while we execute the
       FINALLY clause.  And we can't write to z.variable because the
       FINALLY clause could raise an exception.
       
       So, for large-result procedures, we always allocate a
       variable on the callee's stack to hold the return result.  If
       there are no RETURNs within TRY-FINALLY or LOCK statements,
       this storage is never used.  But it is no good to allocate
       the variable when we see the TRY-FINALLY, because the local
       variable we allocate must have a scope that extends over the
       entire procedure. *)
        
    IF Host.direct_struct_assign THEN
      WITH z = stack[tos-1] DO
        <* ASSERT z.kind = Kind.zPROC *>
        IF ProcType.LargeResult (z.type) THEN
          EVAL Type.CheckInfo (z.type, ret_info);
          (* Use Declare_local() rather than Declare_temp() because
             the life of this variable extends over the whole procedure *)
          z.tmp_result :=
              CG.Declare_local (M3ID.NoID, ret_info.size,
                                ret_info.alignment, CG.Type.Struct,
                                Type.GlobalUID(z.type), in_memory := TRUE,
                                up_level := FALSE, f := CG.Maybe);
        END;
      END;
    END;
  END AllocReturnTemp;

PROCEDURE EmitReturn (expr: Expr.T;  fromFinally: BOOLEAN) =
  VAR
    i: INTEGER;
    ret_info: Type.Info;
    simple: BOOLEAN;
    is_large: BOOLEAN;
  BEGIN
    (* mark every frame out to the procedure boundary as 'returnSeen' *)
    i := tos - 1;
    WHILE (i >= 0) DO
      WITH z = stack[i] DO
        z.returnSeen := TRUE;
        IF (z.kind = Kind.zPROC) OR (z.kind = Kind.zTRYELSE) THEN EXIT END;
      END;
      DEC (i);
    END;

    simple := TRUE;
    IF (expr # NIL) THEN
      (* check to see if the return value is absorbed by TRY-EXCEPT-ELSE
         or munged by a finally handler *)
      i := tos-1;
      WHILE (i >= 0) DO
        WITH z = stack[i] DO
          CASE z.kind OF
          | Kind.zTRYELSE =>
              Expr.Prep (expr);
              Expr.Compile (expr);
              CG.Discard (Type.CGType (Expr.TypeOf (expr)));
              expr := NIL;
              EXIT;
          | Kind.zFINALLY, Kind.zFINALLYPROC, Kind.zLOCK =>
              simple := FALSE;
          | Kind.zPROC =>
              EXIT;
          ELSE (* ignore *)
          END; (*CASE*)
        END; (*WITH*)
        DEC (i);
      END;

      IF (expr # NIL) THEN
        WITH z = stack[i] DO
          (* stuff the pending return value *)
          is_large := ProcType.LargeResult (z.type);

          IF Host.direct_struct_assign AND is_large AND NOT simple THEN
            <* ASSERT z.tmp_result # NIL *>
            EVAL Type.CheckInfo (z.type, ret_info);
            AssignStmt.PrepForEmit (z.type, expr, initializing := TRUE);
            CG.Load_addr_of (z.tmp_result, 0, ret_info.alignment);
            AssignStmt.DoEmit (z.type, expr);
          ELSIF is_large OR NOT simple THEN
            AssignStmt.PrepForEmit (z.type, expr, initializing := FALSE);
            Variable.LoadLValue (z.variable);
            AssignStmt.DoEmit (z.type, expr);
          ELSE
            Expr.Prep (expr);
          END;
        END;
      END;
    END;

    IF Target.Has_stack_walker
      THEN i := EmitReturn1 ();
      ELSE i := EmitReturn2 ();
    END;

    IF i >= 0 THEN
      WITH z = stack[i] DO
        IF Host.direct_struct_assign
          AND (fromFinally OR NOT simple)
          AND ProcType.LargeResult (z.type)
        THEN
          (* Copy the compiler temp to the return result z.variable *)
          <* ASSERT z.tmp_result # NIL *>
          EVAL Type.CheckInfo (z.type, ret_info);
          Variable.LoadLValue (z.variable);
          CG.Load_addr_of_temp (z.tmp_result, 0, ret_info.alignment);
          CG.Copy (ret_info.size, overlap := FALSE);
        END;
        IF (z.type = NIL) THEN
          (* there's no return value *)
          CG.Exit_proc (CG.Type.Void);
        ELSIF fromFinally THEN
          (* the return value is stuffed in 'z.variable',
             and 'expr' is 'NIL' on this call...  *)
          IF NOT ProcType.LargeResult (z.type) THEN
            Variable.Load (z.variable);
            CG.Exit_proc (Type.CGType (z.type));
          ELSIF (z.callConv.standard_structs) THEN
            CG.Exit_proc (CG.Type.Void);
          ELSE
            Variable.LoadLValue (z.variable);
            CG.Exit_proc (CG.Type.Struct);
          END;
        ELSIF is_large THEN
          IF (z.callConv.standard_structs) THEN
            CG.Exit_proc (CG.Type.Void);
          ELSE
            Variable.LoadLValue (z.variable);
            CG.Exit_proc (CG.Type.Struct);
          END;
        ELSIF simple THEN
          AssignStmt.DoEmitCheck (z.type, expr);
          CG.Exit_proc (Type.CGType (z.type));
        ELSE (* small scalar return value *)
          Variable.Load (z.variable);
          CG.Exit_proc (Type.CGType (z.type));
        END;
      END;
    END;
  END EmitReturn;

PROCEDURE EmitReturn1 (): INTEGER =
  VAR i: INTEGER;
  BEGIN
    (* now, unwind as far as possible *)
    i := tos - 1;
    WHILE (i >= 0) DO
      WITH z = stack[i] DO
        CASE z.kind OF
        | Kind.zTRYELSE =>
            CG.Load_intt (Return_exception);
            CG.Store_int (Target.Integer.cg_type, z.info);
            CG.Jump (z.stop);
            EXIT;
        | Kind.zFINALLY, Kind.zFINALLYPROC =>
            CG.Load_intt (Return_exception);
            CG.Store_int (Target.Integer.cg_type, z.info);
            CG.Jump (z.stop);
            EXIT;
        | Kind.zLOCK =>
            SetLock (FALSE, z.info, 0);
        | Kind.zEXIT =>
            (* ignore *)
        | Kind.zTRY  =>
            (* ignore *)
        | Kind.zRAISES =>
            (* ignore *)
        | Kind.zPROC =>
            RETURN i;
        END;
      END;
      DEC (i);
    END;
    RETURN -1;
  END EmitReturn1;

PROCEDURE EmitReturn2 (): INTEGER =
  VAR i: INTEGER;
  BEGIN
    (* now, unwind as far as possible *)
    i := tos - 1;
    WHILE (i >= 0) DO
      WITH z = stack[i] DO
        CASE z.kind OF
        | Kind.zTRYELSE =>
            PopFrame (z.info);
            CG.Load_nil ();  (* the current "RETURN" exception is lost *)
            CG.Store_addr (z.info, M3RT.EF1_info + M3RT.EA_exception);
            CG.Jump (z.stop);
            EXIT;
        | Kind.zFINALLY =>
            PopFrame (z.info);
            CG.Load_intt (Return_exception);
            CG.Store_int (Target.Integer.cg_type,
                          z.info, M3RT.EF1_info + M3RT.EA_exception);
            CG.Jump (z.stop);
            EXIT;
        | Kind.zFINALLYPROC =>
            PopFrame (z.info);
            CallFinallyHandler (z.info, z.handler, z.h_level);
        | Kind.zLOCK =>
            PopFrame (z.info);
            SetLock (FALSE, z.info, M3RT.EF4_mutex);
        | Kind.zEXIT =>
            (* ignore *)
        | Kind.zTRY  =>
            PopFrame (z.info);
        | Kind.zRAISES =>
            PopFrame (z.info);
        | Kind.zPROC =>
            RETURN i;
        END;
      END;
      DEC (i);
    END;
    RETURN -1;
  END EmitReturn2;

PROCEDURE EmitScopeTable (): INTEGER =
  VAR
    Align := MAX (Target.Address.align, Target.Integer.align);
    f: FramePtr := all_frames;
    base, x, size: INTEGER;
    e_base: CG.Var;
    e_offset: INTEGER;
  BEGIN
    IF (f = NIL) OR (NOT Target.Has_stack_walker) THEN RETURN -1; END;

    (* make sure that all the exception lists were declared *)
    WHILE (f # NIL) DO
      IF (f.e_set # NIL) THEN ESet.Declare (f.e_set) END;
      f := f.next;
    END;

    (* declare space for the table *)
    size := n_frames * M3RT.EX_SIZE;
    base := Module.Allocate (size, Align, TRUE, "*exception scopes*");
    CG.Comment (base, TRUE, "exception scopes");

    (* fill in the table *)
    f := all_frames;
    x := base;
    WHILE (f # NIL) DO
      CG.Init_intt  (x + M3RT.EX_class, Target.Char.size, RT_Kind [f.kind], TRUE);
      IF (f.outermost) THEN
        CG.Init_intt  (x + M3RT.EX_outermost, Target.Char.size, ORD(TRUE), TRUE);
      END;
      IF (f.next = NIL) THEN
        CG.Init_intt  (x + M3RT.EX_end_of_list, Target.Char.size, ORD(TRUE), TRUE);
      END;
      CG.Init_label (x + M3RT.EX_start, f.start, TRUE);
      CG.Init_label (x + M3RT.EX_stop, f.stop, TRUE);
      IF (f.info # NIL) THEN CG.Init_offset (x + M3RT.EX_offset, f.info, TRUE) END;
      IF (f.e_set # NIL) THEN
        ESet.GetAddress (f.e_set, e_base, e_offset);
        IF (e_base # NIL) OR (e_offset # 0) THEN
          CG.Init_var (x + M3RT.EX_excepts, e_base, e_offset, TRUE);
        END;
      END;
      INC (x, M3RT.EX_SIZE);
      f := f.next;
    END;

    RETURN base;
  END EmitScopeTable;

PROCEDURE EmitExceptionTest (signature: Type.T;  need_value: BOOLEAN): CG.Val =
  VAR
    i: INTEGER;
    value  : CG.Val := NIL;
    result := ProcType.CGResult (signature);
    (** ex := ProcType.Raises (signature); **)
  BEGIN
    IF need_value THEN value := CaptureResult (result); END;

    IF NOT Target.Has_stack_walker THEN  RETURN value;  END;

    (** nope -- any procedure could raise an <*IMPLICIT*> exception 
    IF ESet.RaisesNone (ex) THEN  RETURN value;  END;
    ***)

    (* scan the frame stack looking for the first active handler *)
    i := tos - 1;
    LOOP
      IF (i < 0) THEN  RETURN value;  END;
      WITH z = stack[i] DO
        CASE z.kind OF
        | Kind.zTRYELSE     => EXIT;
        | Kind.zFINALLYPROC => (* ignore the runtime does it *)
        | Kind.zFINALLY     => EXIT;
        | Kind.zLOCK        => (* ignore  (the runtime does the unlocks) *)
        | Kind.zEXIT        => (* ignore *)
        | Kind.zTRY         => EXIT;
        | Kind.zRAISES      => (* ignore *)
        | Kind.zPROC        => RETURN value;  (* no relevent handlers *)
        END;
      END;
      DEC (i);
    END;

    IF NOT need_value THEN
      value := CaptureResult (result);
    END;

    (* generate the conditional branch to the handler *)
    CG.Load_addr (stack[i].info, M3RT.EA_exception);
    CG.Load_nil ();
    CG.If_compare (CG.Type.Addr, CG.Cmp.NE, stack[i].stop, CG.Never);

    IF NOT need_value AND (value # NIL) THEN
      CG.Push (value);
      CG.Free (value);
      value := NIL;
    END;

    RETURN value;
  END EmitExceptionTest;

PROCEDURE CaptureResult (result: CG.Type): CG.Val =
  BEGIN
    IF (result = CG.Type.Void) THEN
      RETURN NIL;
    ELSIF (result # CG.Type.Struct) THEN
      RETURN CG.Pop ();
    ELSE
      CG.Discard (result);
      RETURN NIL;
    END;
  END CaptureResult;

PROCEDURE NextHandler (VAR(*OUT*) handler: CG.Label;
                       VAR(*OUT*) info: CG.Var): BOOLEAN =
  VAR i: INTEGER;
  BEGIN
    IF NOT Target.Has_stack_walker THEN RETURN FALSE END;

    (* scan the frame stack looking for the first active handler *)
    i := tos - 1;
    LOOP
      IF (i < 0) THEN RETURN FALSE END;
      WITH z = stack[i] DO
        CASE z.kind OF
        | Kind.zTRYELSE     => EXIT;
        | Kind.zFINALLYPROC => (* ignore the runtime does it *)
        | Kind.zFINALLY     => EXIT;
        | Kind.zLOCK        => (* ignore  (the runtime does the unlocks) *)
        | Kind.zEXIT        => (* ignore *)
        | Kind.zTRY         => EXIT;
        | Kind.zRAISES      => (* ignore *)
        | Kind.zPROC        => RETURN FALSE;  (* didn't find any handlers *)
        END;
      END;
      DEC (i);
    END;

    handler := stack[i].stop;
    info    := stack[i].info;
    RETURN TRUE;
  END NextHandler;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE Reset () =
  BEGIN
    all_frames   := NIL;
    n_frames     := 0;
    save_depth   := 0;
    tos          := 0;
  END Reset;

BEGIN
END Marker.
