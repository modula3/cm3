(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Exceptionz.m3                                         *)
(* Last Modified On Tue May 23 15:38:44 PDT 1995 by kalsow     *)
(*      Modified On Thu Dec  5 17:20:35 PST 1991 by muller     *)

MODULE Exceptionz;

IMPORT M3, M3ID, M3FP, CG, Value, ValueRep, Type, Scope, Formal, M3RT;
IMPORT Error, Expr, Token, Decl, Text, Procedure, AssignStmt, Addr, Scanner;
IMPORT Target, RefType, ProcBody, Module, RunTyme, Marker, M3Buf, Int;
FROM Scanner IMPORT GetToken, Match, MatchID, cur;

TYPE
  T = Value.T BRANDED "Exceptionz.T" OBJECT
        tipe    : Type.T;
        refTipe : Type.T;
        offset  : INTEGER;
        coffset : INTEGER;
        uid     : INTEGER;
        implicit: BOOLEAN;
        next    : T;
      OVERRIDES
        typeCheck   := Check;
        set_globals := SetGlobals;
        load        := Load;
        declare     := Declarer;
        const_init  := ValueRep.NoInit;
        need_init   := ValueRep.Never;
        lang_init   := ValueRep.NoInit;
        user_init   := ValueRep.NoInit;
        toExpr      := ValueRep.NoExpr;
        toType      := ValueRep.NoType;
        typeOf      := ValueRep.TypeVoid;
        repTypeOf   := ValueRep.TypeVoid;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := FPType;
      END;

TYPE
  Raiser = ProcBody.T OBJECT
    self   : T;
    arg    : CG.Var;
    module : CG.Var;
    line   : CG.Var;
  OVERRIDES
    gen_decl := EmitDecl;
    gen_body := EmitBody;
  END;

(* EXPORTED: *)
PROCEDURE ParseDecl (READONLY att: Decl.Attributes) =
  TYPE TK = Token.T;
  VAR t: T; id: M3ID.T;
  BEGIN
    Match (TK.tEXCEPTION);
    WHILE (cur.token = TK.tIDENT) DO
      id := MatchID ();
      t := NEW (T);
      ValueRep.Init (t, id, Value.Class.Exception);
      t.readonly := TRUE;
      t.unused   := att.isUnused;
      t.obsolete := att.isObsolete;
      t.tipe     := NIL;
      t.refTipe  := NIL;
      t.offset   := 0;
      t.coffset  := 0;
      t.uid      := 0;
      t.implicit := att.isImplicit;
      IF (cur.token = TK.tLPAREN) THEN
        GetToken (); (* ( *)
        t.tipe := Type.Parse ();
        Match (TK.tRPAREN);
      END;
      Scope.Insert (t);
      Match (TK.tSEMI);
    END;
  END ParseDecl;

(* EXPORTED: *)
PROCEDURE EmitRaise (v: Value.T;  arg: Expr.T) =
  VAR
    t: T := Value.Base (v);
    expr_type: Type.T;
    arg_type: CG.Type;
    info: Type.Info;
    p := RunTyme.LookUpProc (RunTyme.Hook.RaiseEx);
    tmp: CG.Val;
  BEGIN
    Value.Declare (t);
    <*ASSERT t.offset # 0*>

    IF (arg = NIL) THEN
      Procedure.StartCall (p);
      IF Target.DefaultCall.args_left_to_right THEN
        LoadSelf (t);
        CG.Pop_param (CG.Type.Addr);
        CG.Load_nil ();
        CG.Pop_param (CG.Type.Addr);
        PassLocation ();
      ELSE
        PassLocation ();
        CG.Load_nil ();
        CG.Pop_param (CG.Type.Addr);
        LoadSelf (t);
        CG.Pop_param (CG.Type.Addr);
      END;
      Procedure.EmitCall (p);
    ELSIF (t.refTipe = NIL) THEN
      expr_type := Expr.TypeOf (arg);
      expr_type := Type.CheckInfo (expr_type, info);
      arg_type := info.stk_type;
      Expr.Prep (arg);
      AssignStmt.EmitRTCheck (t.tipe, arg);
      tmp := CG.Pop ();
      Procedure.StartCall (p);
      IF Target.DefaultCall.args_left_to_right THEN
        LoadSelf (t);
        CG.Pop_param (CG.Type.Addr);
        CG.Push (tmp);
        IF (arg_type # CG.Type.Addr) THEN
          CG.Loophole (arg_type, CG.Type.Addr);
        END;
        CG.Pop_param (CG.Type.Addr);
        PassLocation ();
      ELSE
        PassLocation ();
        CG.Push (tmp);
        IF (arg_type # CG.Type.Addr) THEN
          CG.Loophole (arg_type, CG.Type.Addr);
        END;
        CG.Pop_param (CG.Type.Addr);
        LoadSelf (t);
        CG.Pop_param (CG.Type.Addr);
      END;
      Procedure.EmitCall (p);
      CG.Free (tmp);
    ELSE (* large argument => call the raise procedure *)
      expr_type := Expr.TypeOf (arg);
      expr_type := Type.CheckInfo (expr_type, info);
      arg_type := info.stk_type;
      Expr.Prep (arg);
      AssignStmt.EmitRTCheck (t.tipe, arg);
      tmp := CG.Pop ();
      CG.Start_call_indirect (CG.Type.Void, Target.DefaultCall);
      IF Target.DefaultCall.args_left_to_right THEN
        CG.Push (tmp);
        IF (arg_type # CG.Type.Addr) THEN Formal.GenCopy (expr_type) END;
        CG.Pop_param (CG.Type.Addr);
        PassLocation ();
      ELSE
        PassLocation ();
        CG.Push (tmp);
        IF (arg_type # CG.Type.Addr) THEN Formal.GenCopy (expr_type) END;
        CG.Pop_param (CG.Type.Addr);
      END;
      LoadSelf (t);
      CG.Add_offset (M3RT.ED_SIZE);
      CG.Boost_addr_alignment (Target.Address.align);
      CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size, CG.ProcAlign ());
      CG.Gen_Call_indirect (CG.Type.Void, Target.DefaultCall);
      EVAL Marker.EmitExceptionTest (Procedure.Signature (p), need_value := FALSE);
      CG.Free (tmp);
    END;
  END EmitRaise;

PROCEDURE LoadSelf (t: T) =
  BEGIN
    IF (t.imported) THEN
      Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
      CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);
      CG.Boost_addr_alignment (Target.Address.align);
    ELSE
      Module.LoadGlobalAddr (Scope.ToUnit (t), t.coffset, is_const := TRUE);
    END;
  END LoadSelf;

PROCEDURE PassLocation () =
  (* push the "module, linenumber" parameters *)
  VAR
    globals   := Module.GlobalData (is_const := FALSE);
    this_file : TEXT;
    this_line : INTEGER;
  BEGIN
    Scanner.Here (this_file, this_line);
    IF Target.DefaultCall.args_left_to_right THEN
      CG.Load_addr_of (globals, 0, CG.Max_alignment);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_intt (this_line);
      CG.Pop_param (Target.Integer.cg_type);
    ELSE
      CG.Load_intt (this_line);
      CG.Pop_param (Target.Integer.cg_type);
      CG.Load_addr_of (globals, 0, CG.Max_alignment);
      CG.Pop_param (CG.Type.Addr);
    END;
  END PassLocation;

(* EXPORTED: *)
PROCEDURE ArgByReference (type: Type.T): BOOLEAN =
  VAR info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (type, info);
    RETURN (info.size > Target.Address.size) OR Type.IsStructured (type);
  END ArgByReference;

(* Externally dispatched-to: *)
PROCEDURE Check (t: T;  <*UNUSED*> VAR cs: Value.CheckState) =
  VAR info: Type.Info;
  BEGIN
    IF (t.tipe # NIL) THEN
      t.tipe := Type.CheckInfo (t.tipe, info);
      IF (info.size < 0) THEN
        Error.ID (t.name, "argument type must have fixed length");
      END;
      IF ArgByReference (t.tipe) THEN
        t.refTipe := RefType.New (Type.StripPacked (t.tipe), TRUE, NIL);
(* FIXME: Most uses of t.tipe need to be StripPacked. *)
        t.refTipe := Type.Check (t.refTipe);
      END;
    END;
  END Check;

(* EXPORTED: *)
PROCEDURE ArgType (v: Value.T): Type.T =
  BEGIN
    TYPECASE Value.Base (v) OF
    | NULL => RETURN NIL;
    | T(t) => RETURN t.tipe;
    ELSE      RETURN NIL;
    END;
  END ArgType;

(* EXPORTED: *)
PROCEDURE UID (v: Value.T): INTEGER =
  BEGIN
    TYPECASE Value.Base (v) OF
    | NULL => RETURN 0;
    | T(t) => <*ASSERT t.uid # 0*> RETURN t.uid;
    ELSE      RETURN 0;
    END;
  END UID;

(* EXPORTED: *)
PROCEDURE IsImplicit (v: Value.T): BOOLEAN =
  BEGIN
    TYPECASE Value.Base (v) OF
    | NULL => RETURN FALSE;
    | T(t) => RETURN t.implicit;
    ELSE      RETURN FALSE;
    END;
  END IsImplicit;

(* Externally dispatched-to: *)
PROCEDURE Load (t: T) =
  BEGIN
    Value.Declare (t);
    LoadSelf (t);
  END Load;

(* Externally dispatched-to: *)
PROCEDURE SetGlobals (t: T) =
  VAR name: TEXT;  size: INTEGER;
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    (* Type.SetGlobals (t.refTipe); *)
    IF (t.offset # 0) OR (t.external) THEN RETURN END;

    name := Value.GlobalName (t, dots := TRUE, with_module := TRUE);
    size := M3RT.ED_SIZE;
    IF (t.refTipe # NIL) THEN INC (size, Target.Address.size) END;
    INC (size, (Text.Length (name) + 1) * Target.Char.size);
    t.offset := Module.Allocate (Target.Address.size, Target.Address.align,
                                 FALSE, name);
    t.coffset := Module.Allocate (size, Target.Address.align, TRUE, name);
    t.uid := M3FP.ToInt (M3FP.FromText (name));
  END SetGlobals;

(* Externally dispatched-to: *)
PROCEDURE Declarer (t: T): BOOLEAN =
  VAR
    name: TEXT;
    globals, consts: CG.Var;
    arg_type: CG.TypeUID := 0;
    size: INTEGER;
    proc: CG.Proc;
  BEGIN
    Type.Compile (t.tipe);
    Type.Compile (t.refTipe);
    IF (t.tipe # NIL) THEN arg_type := Type.GlobalUID (t.tipe) END;

    IF t.external OR t.imported THEN
      (* don't bother to force the import *)
    ELSE (* locally declared *)
      SetGlobals (t);
      name := Value.GlobalName (t, dots := TRUE, with_module := TRUE);
      globals := Module.GlobalData (is_const := FALSE);
      consts  := Module.GlobalData (is_const := TRUE);
      size := M3RT.ED_SIZE;
      IF (t.refTipe # NIL) THEN
        INC (size, Target.Address.pack);
        proc := DeclareRaiseProc (t);
      END;
      CG.Declare_exception (t.name, arg_type, (t.refTipe#NIL), consts, t.coffset);
      CG.Declare_global_field (t.name, t.offset, Target.Address.size, 0, FALSE);
      CG.Declare_global_field (t.name, t.coffset, size, 0, TRUE);
      CG.Init_var  (t.offset, consts, t.coffset, FALSE);
      CG.Init_intt (t.coffset + M3RT.ED_uid, Target.Integer.size, t.uid, TRUE);
      CG.Init_var  (t.coffset + M3RT.ED_name, consts, t.coffset + size, TRUE);
      CG.Init_intt (t.coffset + M3RT.ED_implicit, Target.Integer.size,
                                                       ORD (t.implicit), TRUE);
      IF (t.refTipe # NIL) THEN
        CG.Init_proc (t.coffset + M3RT.ED_SIZE, proc, TRUE);
      END;
      CG.Init_chars (t.coffset + size, name, TRUE);
    END;
    RETURN TRUE;
  END Declarer;

PROCEDURE DeclareRaiseProc (t: T): CG.Proc =
  VAR r := NEW (Raiser, self := t);
  BEGIN
    r.name    := Value.GlobalName (t, dots := FALSE, with_module := TRUE);
    r.name    := r.name & "_RAISE";
    r.cg_proc := CG.Declare_procedure (M3ID.Add (r.name), 3, CG.Type.Void,
                      lev := 0, cc := Target.DefaultCall, exported := FALSE,
                      parent := NIL);
    r.arg     := CG.Declare_param (M3ID.NoID, Target.Address.size,
                      Target.Address.align, CG.Type.Addr,
                      CG.Declare_indirect (Type.GlobalUID (t.tipe)),
                      in_memory := FALSE, up_level := FALSE, f := CG.Always);
    r.module  := CG.Declare_param (M3ID.NoID, Target.Address.size,
                      Target.Address.align, CG.Type.Addr,
                      Type.GlobalUID (Addr.T), in_memory := FALSE,
                      up_level := FALSE, f := CG.Always);
    r.line    := CG.Declare_param (M3ID.NoID, Target.Integer.size,
                      Target.Integer.align, Target.Integer.cg_type,
                      Type.GlobalUID (Int.T), in_memory := FALSE,
                      up_level := FALSE, f := CG.Always);
    ProcBody.Schedule (r);
    RETURN r.cg_proc;
  END DeclareRaiseProc;

(* Externally dispatched-to: *)
PROCEDURE EmitDecl (<*UNUSED*> x: Raiser) =
  BEGIN
  END EmitDecl;

(* Externally dispatched-to: *)
PROCEDURE EmitBody (x: Raiser) =
  VAR
    t := x.self;
    ptr: CG.Val;
    align: INTEGER;
    proc: Procedure.T;
    info: Type.Info;
  BEGIN
    CG.Gen_location (t.origin);
    CG.Begin_procedure (x.cg_proc);
    EVAL Type.CheckInfo (t.tipe, info);
    align := info.alignment;

    (* ptr := NEW (REF t.type) *)
    proc := RunTyme.LookUpProc (RunTyme.Hook.NewTracedRef);
    Procedure.StartCall (proc);
    Type.LoadInfo (t.refTipe, -1);
    CG.Pop_param (CG.Type.Addr);
    ptr := Procedure.EmitValueCall (proc);

    (* ptr^ := arg^ *)
    CG.Push (ptr);
    CG.Boost_addr_alignment (align);
    CG.Load_addr (x.arg, 0, align);
    CG.Copy (info.size, overlap := FALSE);

    (* RAISE (e, ptr) *)
    proc := RunTyme.LookUpProc (RunTyme.Hook.RaiseEx);
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      LoadSelf (t);
      CG.Pop_param (CG.Type.Addr);
      CG.Push (ptr);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_addr (x.module, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_int (Target.Integer.cg_type, x.line);
      CG.Pop_param (Target.Integer.cg_type);
    ELSE
      CG.Load_int (Target.Integer.cg_type, x.line);
      CG.Pop_param (Target.Integer.cg_type);
      CG.Load_addr (x.module, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
      CG.Push (ptr);
      CG.Pop_param (CG.Type.Addr);
      LoadSelf (t);
      CG.Pop_param (CG.Type.Addr);
    END;
    Procedure.EmitCall (proc);

    CG.Free (ptr);
    CG.End_procedure (x.cg_proc);
  END EmitBody;

(* EXPORTED: *)
PROCEDURE AddFPSetTag (tt: Value.T;  VAR x: M3.FPInfo): CARDINAL =
  (* called for RAISES sets, doesn't include the interface record offset *)
  VAR t: T := Value.Base (tt);
  BEGIN
    M3Buf.PutChar (x.buf, '<');
    ValueRep.FPStart (t, x, "EXCEPT ", 0, global := TRUE);
    M3Buf.PutChar (x.buf, '>');
    RETURN ORD (t.tipe # NIL);
  END AddFPSetTag;

(* Externally dispatched-to: *)
PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  VAR offset := t.offset DIV Target.Address.align;
  BEGIN
    ValueRep.FPStart (t, x, "EXCEPT ", offset, global := TRUE);
    RETURN ORD (t.tipe # NIL);
  END AddFPTag;

PROCEDURE FPType (t: T): Type.T =
  BEGIN
    RETURN t.tipe;
  END FPType;

BEGIN
END Exceptionz.
