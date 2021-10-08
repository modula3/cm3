(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Procedure.m3                                          *)
(* Last Modified On Tue Jun 20 09:58:41 PDT 1995 by kalsow     *)
(*      Modified On Tue Jun 13 20:00:51 PDT 1995 by ericv      *)
(*      Modified On Thu Dec  5 17:21:10 PST 1991 by muller     *)

MODULE Procedure;

IMPORT M3, M3ID, CG, Value, ValueRep, Type, Scope, Error, Host;
IMPORT ProcType, Stmt, BlockStmt, Marker, Coverage, M3RT;
IMPORT CallExpr, Token, Variable, ProcExpr, Tracer, RTIO, RTParams;
IMPORT Scanner, Decl, ESet, ProcBody, Target, Expr, Formal, Jmpbufs;
IMPORT Module;
FROM Scanner IMPORT GetToken, Match, MatchID, cur;

VAR debug := FALSE;

REVEAL
  T = Value.T BRANDED OBJECT
        intf_peer    : T;
        impl_peer    : T;
        next_defn    : T;
        signature    : Type.T;
        syms         : Scope.T;
        block        : Stmt.T;
        body         : Body;
        result       : Variable.T;
        builtin      : BOOLEAN;
        (* ^A global, predefined, pseudo-procedure, e.g. NEW. *) 
        predefined   : BOOLEAN;
        (* ^Either 'builtin' or a procedure in an interface known to the 
           compiler, e.g. Word.Plus. *) 
        needs_raises : BOOLEAN;
        direct       : BOOLEAN;
        fails        : ESet.T;
        assignable   : BOOLEAN;
        (* ^Can be assigned to a variable and passed as a parameter. *) 
        cg_proc      : CG.Proc;
        next_cg_proc : T;
        end_origin   : INTEGER;
        jmpbufs      : Jmpbufs.Proc;
      OVERRIDES
        typeCheck   := Check;
        set_globals := ValueRep.NoInit;
        load        := Load;
        declare     := Declarer;
        const_init  := ValueRep.NoInit;
        need_init   := NeedInit;
        lang_init   := LangInit;
        user_init   := ValueRep.NoInit;
        toExpr      := ToExpr;
        toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        repTypeOf   := TypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := FPType;
      END;

TYPE
  Body = ProcBody.T OBJECT
    self: T;
  OVERRIDES
    gen_decl := EmitDecl;
    gen_body := EmitBody;
  END;

VAR (*CONST*)
  resultName := M3ID.NoID;
  returnName := M3ID.NoID;

VAR
  defined_procs: T := NIL;
  (* interface procedures that are implemented in the current compilation unit *)

VAR
  cg_procs: T := NIL;
  (* procedures that have M3CG attachments *)

PROCEDURE Reset () =
  VAR t, u: T;
  BEGIN
    (* disconnect the interface->implementation mappings *)
    t := defined_procs;
    WHILE (t # NIL) DO
      u := t;  t := t.next_defn;
      u.impl_peer := NIL;
      u.next_defn := NIL;
    END;
    defined_procs := NIL;

    (* release any M3CG objects that were created. *)
    t := cg_procs;
    WHILE (t # NIL) DO
      u := t;  t := t.next_cg_proc;
      u.cg_proc      := NIL;
      u.next_cg_proc := NIL;
      u.body         := NIL;
    END;
    cg_procs := NIL;
  END Reset;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes;
                     headerOnly: BOOLEAN := FALSE) =
  TYPE TK = Token.T;
  VAR t: T;  id, final_id: M3ID.T;  cc := att.callingConv;
  BEGIN
    Match (TK.tPROCEDURE);
    id := MatchID ();
    t := Create (id);
    t.name := id;
    t.unused    := att.isUnused;
    t.obsolete  := att.isObsolete;
    IF (att.isExternal) THEN
      t.external := TRUE;
      IF (att.alias = M3ID.NoID)
        THEN t.extName := t.name;
        ELSE t.extName := att.alias;
      END;
    END;
    IF (cc = NIL) THEN cc := Target.DefaultCall; END;
    t.signature := ProcType.ParseSignature (id, cc);

    IF debug THEN
      RTIO.PutText ("ParseDecl id:");
      RTIO.PutText (M3ID.ToText (id));
      RTIO.PutText (" t:");
      RTIO.PutRef (t);
      RTIO.PutText (" signature:");
      RTIO.PutRef (t.signature);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;

    Scope.Insert (t);
    IF (cur.token = TK.tEQUAL) THEN
      t.body := NEW (Body, self := t);
      ProcBody.Push (t.body);
      GetToken (); (* = *)
      IF (headerOnly) THEN
        Error.ID (id, "procedure declaration cannot include a body");
      END;
      IF (att.isExternal) THEN
        Error.WarnID (2, id, "external procedure cannot include a body");
        t.external := FALSE;
        t.extName  := M3ID.NoID;
      END;
      t.syms  := Scope.PushNew (TRUE, id);
      t.block := BlockStmt.Parse (FALSE);
      t.fails := BlockStmt.ExtractFails (t.block);
      t.end_origin := Scanner.offset;
      final_id := MatchID ();
      IF (final_id # id) THEN
        Error.ID (id, "Initial name doesn\'t match final name");
      END;
      Scope.PopNew ();
      ProcBody.Pop ();
    ELSIF (headerOnly) OR (att.isExternal) THEN
      (* ok *)
    ELSIF (cur.token = TK.tSEMI) THEN
      t.body := NEW (Body, self := t);
      ProcBody.Push (t.body);
      (* It's a syntax error, but try recovering from it by assuming 
         it's Modula-2 syntax *)
      Error.ID (id, "expecting \'=\' before procedure body");
      GetToken (); (* ; *)
      t.syms  := Scope.PushNew (TRUE, id);
      t.block := BlockStmt.Parse (FALSE);
      t.fails := BlockStmt.ExtractFails (t.block);
      t.end_origin := Scanner.offset;
      final_id := MatchID ();
      IF (final_id # id) THEN
        Error.ID (id, "Initial name doesn\'t match final name");
      END;
      Scope.PopNew ();
      ProcBody.Pop ();
    ELSE
      Error.ID (id, "procedure declaration must include a body");
    END;
    Match (TK.tSEMI);
  END ParseDecl;

PROCEDURE IsEqual (a, b: Value.T): BOOLEAN =
  VAR ta, tb: T;
  BEGIN
    a := Value.Base (a);
    b := Value.Base (b);
    IF (a = b) THEN RETURN TRUE END;
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | T(t) => ta := t;
    ELSE      RETURN FALSE;
    END;
    TYPECASE b OF
    | NULL => RETURN FALSE;
    | T(t) => tb := t;
    ELSE      RETURN FALSE;
    END;
    RETURN (ta.intf_peer = tb) OR (tb.intf_peer = ta)
        OR (ta.impl_peer = tb) OR (tb.impl_peer = ta);
  END IsEqual;

PROCEDURE Create (name: M3ID.T): T =
  VAR t: T;
  BEGIN
    t := NEW (T);
    ValueRep.Init (t, name, Value.Class.Procedure);
    t.readonly     := TRUE;
    t.intf_peer    := NIL;
    t.impl_peer    := NIL;
    t.next_defn    := NIL;
    t.signature    := NIL;
    t.syms         := NIL;
    t.body         := NIL;
    t.block        := NIL;
    t.external     := FALSE;
    t.builtin      := FALSE;
    t.assignable   := TRUE;
    t.predefined   := FALSE;
    t.result       := NIL;
    t.extName      := M3ID.NoID;
    t.needs_raises := TRUE;
    t.fails        := NIL;
    t.cg_proc      := NIL;
    t.next_cg_proc := NIL;
    t.end_origin   := t.origin;
    RETURN t;
  END Create;

PROCEDURE DefinePredefined
                 (name      : TEXT;
                  methods   : CallExpr.MethodList;
                  reserved  : BOOLEAN;
                  signature : Type.T := NIL;
                  assignable: BOOLEAN := FALSE) =
  VAR t: T;  s: M3ID.T;  sig: Type.T;
  BEGIN
    IF (signature = NIL)
      THEN sig := ProcType.New (NIL);
      ELSE sig := signature;
    END;
    ProcType.SetMethods (sig, methods);
    s := M3ID.Add (name);
    t := Create (s);
    t.signature  := sig;
    t.builtin    := (signature = NIL);
    t.assignable := assignable; 
    t.predefined := TRUE;

    IF debug THEN
      RTIO.PutText ("DefinePredefined name:");
      RTIO.PutText (name);
      RTIO.PutText (" t:");
      RTIO.PutRef (t);
      RTIO.PutText (" signature:");
      RTIO.PutRef (t.signature);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;

    Scope.Insert (t);
    IF (reserved) THEN Scanner.NoteReserved (s, t) END;
  END DefinePredefined;

PROCEDURE NoteExport (implv, intfv: Value.T) =
  VAR impl: T := Value.Base (implv);  intf: T := Value.Base (intfv);
  BEGIN
    IF (impl.intf_peer # NIL) OR (intf.impl_peer # NIL) THEN
      Redefined (impl, NIL(*intf*));
    ELSE
      IF NOT Type.IsAssignable (intf.signature, impl.signature) THEN
        Redefined (impl, NIL(*intf*));
      END;
      intf.next_defn := defined_procs;  defined_procs := intf;
      intf.impl_peer := impl;
      impl.intf_peer := intf;
      impl.scope     := intf.scope;  (* retain the exported module name *)
      impl.used      := TRUE;
    END;
  END NoteExport;

PROCEDURE TypeOf (p: T): Type.T =
  BEGIN
    RETURN p.signature;
  END TypeOf;

PROCEDURE Check (p: T;  VAR cs: Value.CheckState) =
  BEGIN
    EVAL Type.Check (p.signature);
    (* NOTE: don't save the signature returned by Type.Check cause if
       you do, the formals will be reused by procedures with the
       same signature. *)

    IF Module.IsInterface () THEN
      Formal.NameDefaultConstructors (p.signature, p.name, cs);
    END;

    Value.TypeCheck (p.intf_peer, cs);

    (* defer the rest to CheckBody *)
  END Check;

PROCEDURE CheckBody (p: T;  VAR cs: Value.CheckState) =
  VAR
    v         : Variable.T;
    formal    : Value.T;
    n_formals : INTEGER;
    result    : Type.T;
    zz        : Scope.T;
    raises    : ESet.T;
    save      : Value.CheckState;
    f_info    : Formal.Info;
    cconv     : CG.CallingConvention;
  BEGIN
    IF (p.body = NIL) THEN RETURN END;

    Coverage.NoteProcedure (p);
    zz := Scope.Push (p.syms);
    p.body.name := Value.GlobalName (p, with_module := FALSE);
    result := ProcType.Result (p.signature);
    cconv := ProcType.CallConv (p.signature);

      (* create a variable for the return result *)
      IF cconv.results_on_left THEN CheckResult (p, result); END;

      (* create local variables for each of the formals *)
      formal := ProcType.Formals (p.signature);  n_formals := 0;
      WHILE (formal # NIL) DO
        v := Variable.NewFormal (formal, formal.name);
        Scope.Insert (v);
        Formal.Split (formal, f_info);
        Variable.BindTrace (v, f_info.trace);
        formal.scope := v.scope;
          (* identify the full names of the formal & its local variable *)
        formal := formal.next;  INC (n_formals);
      END;

      (* create a variable for the return result *)
      IF NOT cconv.results_on_left THEN CheckResult (p, result); END;

      raises := ProcType.Raises (p.signature);
      save := cs;
      cs.raises_others := FALSE;
      ESet.TypeCheck (p.fails);
      ESet.Push (cs, raises, p.fails, stop := TRUE);

      p.checked := TRUE;
      INC (Type.recursionDepth);
        Scope.TypeCheck (p.syms, cs);
        Marker.PushProcedure (result, p.result, cconv);
        p.jmpbufs := Jmpbufs.CheckProcPush (cs.jmpbufs, p.name);
          Stmt.TypeCheck (p.block, cs);
        Jmpbufs.CheckProcPop (cs.jmpbufs, p.jmpbufs);
        Marker.Pop ();
        Scope.WarnUnused (p.syms);
      DEC (Type.recursionDepth);

      p.needs_raises := cs.raises_others;
      cs.raises_others := save.raises_others;
      ESet.Pop (cs, raises, p.fails, stop := TRUE);

    Scope.Pop (zz);
  END CheckBody;

PROCEDURE CheckResult (p: T;  result: Type.T) =
  VAR v: Variable.T;  formal: Value.T;
  BEGIN
    IF (result = NIL) THEN RETURN; END;
    IF (resultName = M3ID.NoID) THEN resultName := M3ID.Add ("_result"); END;
    IF (returnName = M3ID.NoID) THEN returnName := M3ID.Add ("_return"); END;

    IF ProcType.LargeResult (result) THEN
      formal := Formal.New (Formal.Info {returnName, Formal.Mode.mVAR,
                                         -1, result, NIL, FALSE, NIL});
      formal.imported := FALSE;
      formal.origin := p.origin;
      formal.used := TRUE;
      Scope.Insert (formal);
      v := Variable.NewFormal (formal, resultName);
    ELSE
      v := Variable.New (resultName, TRUE);
      Variable.BindType (v, result, indirect := FALSE,
                         readonly := FALSE, needs_init := FALSE,
                         open_array_ok := FALSE);
    END;
    v.imported := FALSE;
    v.origin := p.origin;
    v.used := TRUE;
    Scope.Insert (v);
    p.result := v;
  END CheckResult;

PROCEDURE Load (t: T) =
  BEGIN
    IF (t.builtin) THEN
      Error.ID (t.name, "builtin operation is not a procedure");
    END;
    IF (t.impl_peer # NIL) THEN t := t.impl_peer; END;
    t.used := TRUE;
    Value.Declare (t);
    IF t.assignable THEN
      CG.Load_procedure (t.cg_proc);
    ELSE
      Error.ID 
        (t.name, "this predefined procedure cannot be passed or assigned");
      CG.Load_nil (); 
    END; 
  END Load;

PROCEDURE LoadStaticLink (t: T) =
 BEGIN
    IF (t.builtin) THEN
      Error.ID (t.name, "builtin operation is not a procedure");
    END;
    IF (t.impl_peer # NIL) THEN t := t.impl_peer; END;
    t.used := TRUE;
    Value.Declare (t);
    IF IsNested (t)
      THEN CG.Load_static_link (t.cg_proc);
      ELSE CG.Load_nil ();
    END;
 END LoadStaticLink;

PROCEDURE ImportProc (p: T;  name: TEXT;  n_formals: INTEGER;
                      cg_result: CG.Type; return_typeid: CG.TypeUID;
                      return_typename: M3ID.T;
                      cc: CG.CallingConvention) =
  VAR zz: Scope.T;  new: BOOLEAN; id := M3ID.Add (name);
  BEGIN

    IF debug THEN
      RTIO.PutText ("ImportProc:");
      RTIO.PutRef (p);
      RTIO.PutText (" name:");
      RTIO.PutText (name);
      RTIO.PutText (":");
      RTIO.PutInt (id);
      RTIO.PutText (" return_typename:");
      RTIO.PutInt (return_typename);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;

    <*ASSERT p.cg_proc = NIL*>
    p.next_cg_proc := cg_procs;  cg_procs := p;
    p.cg_proc := CG.Import_procedure (id, n_formals,
                                      cg_result, cc, new, return_typeid, return_typename);
    IF (new) THEN
      (* declare the formals *)
      IF (p.syms # NIL) THEN
        zz := Scope.Push (p.syms);
        Scope.Enter (p.syms);
        Scope.Pop (zz);
      ELSE
        DeclareFormals (p, cc);
      END;
    END;
  END ImportProc;

PROCEDURE DeclareFormals (p: T;  cc: CG.CallingConvention) =
  VAR
    v       : Value.T;
    formals := ProcType.Formals (p.signature);
    result  := ProcType.Result (p.signature);
  BEGIN
    (* declare types for each of the formals *)
    v := formals;
    WHILE (v # NIL) DO
      Formal.EmitDeclaration (v, TRUE, TRUE);
      v := v.next;
    END;

    (* declare parameters for each of the formals *)
    IF cc.results_on_left THEN DeclareResult (result); END;
    v := formals;
    WHILE (v # NIL) DO
      Formal.EmitDeclaration (v, FALSE, TRUE);
      v := v.next;
    END;
    IF NOT cc.results_on_left THEN DeclareResult (result); END;
  END DeclareFormals;

PROCEDURE DeclareResult (result: Type.T) =
  VAR result_id : INTEGER;
  BEGIN
    IF (result # NIL) AND ProcType.LargeResult (result) THEN
      IF (returnName = M3ID.NoID) THEN returnName := M3ID.Add ("_return"); END;
      result_id := CG.Declare_indirect (Type.GlobalUID (result));
      EVAL CG.Declare_param (returnName, Target.Address.size,
                Target.Address.align, CG.Type.Addr, result_id,
                in_memory := FALSE, up_level := FALSE, f := CG.Maybe);
    END;
  END DeclareResult;

PROCEDURE Declarer (p: T): BOOLEAN =
  VAR
    zz: Scope.T;
    parent: CG.Proc := NIL;
    cg_result: CG.Type;
    result: Type.T := NIL;
    name := Value.GlobalName (p, dots := FALSE);
    n_formals: INTEGER;
    cconv: CG.CallingConvention;
    result_typeid := 0;
  BEGIN

    IF p.predefined AND p.body = NIL AND NOT p.assignable THEN
      (* Don't bother importing procedures that can't be assigned
         or passed, or declaring their signatures, but do generate 
         a version stamp dependency *)
      RETURN TRUE;
    END;

    IF (p.intf_peer # NIL) THEN
      Type.Compile (p.intf_peer.signature);
    END;

    Type.Compile (p.signature);
    result := ProcType.Result (p.signature);
    EVAL Type.GlobalUID (p.signature);
    (* try to compile the imported type first... *)

    cg_result := ProcType.CGResult (p.signature);
    IF cg_result # CG.Type.Void THEN
      result_typeid := Type.GlobalUID (result);
    END;
    n_formals := ProcType.NFormals (p.signature);
    cconv     := ProcType.CallConv (p.signature);


    IF debug THEN
      RTIO.PutText ("Procedure.Declarer p:");
      RTIO.PutRef (p);
      RTIO.PutText (" name:");
      RTIO.PutText (name);
      RTIO.PutText (" p.signature:");
      RTIO.PutRef (p.signature);
      RTIO.PutText (" result:");
      RTIO.PutRef (result);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;

    IF (p.body = NIL) THEN
      (* it's not a local procedure *)
      IF p.impl_peer # NIL THEN
        (* it's an interface procedure that's implemented in this module *)
        Value.Declare (p.impl_peer);
        RETURN FALSE;
      ELSE
        (* it's an imported procedure *)
        ImportProc (p, name, n_formals, cg_result,
                    result_typeid,
                    ProcType.ResultTypename (p.signature),
                    cconv);
        RETURN TRUE;
      END;
    END;

    IF (p.body.parent # NIL) THEN parent := p.body.parent.cg_proc END;

    p.next_cg_proc := cg_procs;  cg_procs := p;
    p.cg_proc := CG.Declare_procedure (M3ID.Add (name),
                    n_formals, cg_result, p.body.level,  cconv,
                    exported := (p.exported OR p.imported),
                    parent := parent,
                    return_typeid := result_typeid,
                    return_typename := ProcType.ResultTypename (p.signature));
    p.body.cg_proc := p.cg_proc;
    Scanner.offset := p.origin;
    IF (p.syms # NIL) THEN
      zz := Scope.Push (p.syms);
      Scope.Enter (p.syms);
      Scope.Pop (zz);
    END;
    RETURN TRUE;
  END Declarer;

PROCEDURE NeedInit (t: T): BOOLEAN =
  BEGIN
    RETURN (t.body # NIL) AND (t.body.level > 0);
  END NeedInit;

PROCEDURE LangInit (t: T) =
  VAR x: Body := t.body;
  BEGIN
    IF (x = NIL) OR (x.level <= 0) THEN
      (* outer-level proc => don't do anything *)
    ELSIF (Host.inline_nested_procs) THEN
      GenBody (t);
    ELSE
      CG.Note_procedure_origin (t.cg_proc);
    END;
  END LangInit;

PROCEDURE ToExpr (t: T): Expr.T =
  BEGIN
    RETURN ProcExpr.New (t);
  END ToExpr;

PROCEDURE IsNested (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.body # NIL) AND (t.body.level # 0);
  END IsNested;

PROCEDURE StaticLevel (t: T): INTEGER =
  BEGIN
    IF (t = NIL) OR (t.body = NIL)
      THEN RETURN 0;
      ELSE RETURN t.body.level;
    END;
  END StaticLevel;

PROCEDURE CGName (t: T): CG.Proc =
  BEGIN
    IF (t.impl_peer # NIL) THEN t := t.impl_peer; END;
    t.used := TRUE;
    Value.Declare (t);
    RETURN t.cg_proc;
  END CGName;

PROCEDURE EmitDecl (x: Body) =
  BEGIN
    Value.Declare (x.self);
  END EmitDecl;

PROCEDURE EmitBody (x: Body) =
  VAR p: T := x.self;
  BEGIN
    IF (x.level <= 0) OR (NOT Host.inline_nested_procs) THEN
      GenBody (p);
    END;
  END EmitBody;

PROCEDURE GenBody (p: T) =
  VAR
    tresult  : Type.T;
    oc       : Stmt.Outcomes;
    zz       : Scope.T;
    fallThru : BOOLEAN;
    l        : CG.Label;
    frame    : CG.Var;
    cconv    : CG.CallingConvention;
  BEGIN
    IF (Host.inline_nested_procs)
      AND (p.body # NIL) AND (p.body.level > 0) THEN
      (* make sure outer-level variable initializations are traced
         in the outer procedure, before we enter the nested one.*)
      Tracer.EmitPending ();
    END;

    Scanner.offset := p.origin;
    zz := Scope.Push (p.syms);
    tresult := ProcType.Result (p.signature);
    cconv   := ProcType.CallConv (p.signature);

    CG.Gen_location (p.origin);
    CG.Begin_procedure (p.cg_proc);
    Scope.Enter (p.syms);

    Marker.PushProcedure (tresult, p.result, cconv);
    Marker.AllocReturnTemp ();
      StartRaises (p, l, frame);
        Scope.InitValues (p.syms);
        Scanner.offset := BlockStmt.BodyOffset (p.block);
        Coverage.CountProcedure (p);
        Jmpbufs.CompileProcAllocateJmpbufs (p.jmpbufs);
        oc := Stmt.Compile (p.block);
        fallThru := (Stmt.Outcome.FallThrough IN oc);
      EndRaises (p, l, frame, fallThru);
      IF (fallThru) THEN
        CG.Gen_location (p.end_origin);
        IF (tresult = NIL) THEN
          Marker.EmitReturn (NIL, fromFinally := FALSE);
        ELSE
          Error.WarnID (1, p.name, "function may not return a value");
          IF Host.doReturnChk THEN
            CG.Abort (CG.RuntimeError.NoReturnValue);
            oc := oc - Stmt.Outcomes {Stmt.Outcome.FallThrough};
          END;
        END;
      END;
    Marker.Pop ();
    Scope.Exit (p.syms);

    CG.End_procedure (p.cg_proc);

    Scope.Pop (zz);
  END GenBody;

PROCEDURE StartRaises (t: T;  VAR(*OUT*)l: CG.Label; VAR(*OUT*)frame: CG.Var) =
  VAR raises: ESet.T;  eset: CG.Var;  eoffset: INTEGER;  frame_size : INTEGER;
  BEGIN
    IF (NOT Host.doRaisesChk) OR (NOT t.needs_raises) THEN RETURN END;
    raises := ProcType.Raises (t.signature);
    IF ESet.RaisesAny (raises) THEN RETURN END;

    IF Target.Has_stack_walker THEN
      frame_size := 0;
    ELSIF ESet.RaisesNone (raises) THEN
      frame_size := M3RT.EF_SIZE;
    ELSE
      frame_size := M3RT.EF3_SIZE;
    END;
    frame := NIL;
    IF frame_size # 0 THEN
      frame := CG.Declare_local (M3ID.NoID, frame_size, Target.Address.align,
                                 CG.Type.Struct, 0, in_memory := TRUE,
                                 up_level := FALSE, f := CG.Never);
    END;

    l := CG.Next_label (2);
    CG.Set_label (l, barrier := TRUE);
    Marker.PushRaises (l, l+1, raises, frame);

    IF Target.Has_stack_walker THEN
      Marker.SaveFrame ();
    ELSIF ESet.RaisesNone (raises) THEN
      Marker.PushFrame (frame, M3RT.HandlerClass.RaisesNone);
    ELSE
      ESet.GetAddress (raises, eset, eoffset);
      CG.Load_addr_of (eset, eoffset, Target.Address.align);
      CG.Store_addr (frame, M3RT.EF3_raises);
      Marker.PushFrame (frame, M3RT.HandlerClass.Raises);
    END;
  END StartRaises;

PROCEDURE EndRaises (t: T;  l: CG.Label;  frame: CG.Var;  fallThru: BOOLEAN) =
  VAR raises: ESet.T;
  BEGIN
    IF (NOT Host.doRaisesChk) OR (NOT t.needs_raises) THEN RETURN END;
    raises := ProcType.Raises (t.signature);
    IF ESet.RaisesAny (raises) THEN RETURN END;
    CG.Set_label (l+1, barrier := TRUE);
    Marker.Pop ();
    IF Target.Has_stack_walker THEN
      (* nothing to do on exit *)
    ELSIF fallThru THEN
      Marker.PopFrame (frame);
    END;
  END EndRaises;

PROCEDURE StartCall (t: T) =
  VAR result := ProcType.CGResult (t.signature);
  BEGIN
    IF (t.impl_peer # NIL) THEN t := t.impl_peer; END;
    t.used := TRUE;
    Value.Declare (t);
    CG.Start_call_direct (t.cg_proc, StaticLevel (t), result);
  END StartCall;

PROCEDURE EmitValueCall (t: T): CG.Val =
  VAR result := ProcType.CGResult (t.signature);
  BEGIN
    IF (t.impl_peer # NIL) THEN t := t.impl_peer; END;
    CG.Call_direct (t.cg_proc, result);
    RETURN Marker.EmitExceptionTest (t.signature, need_value := TRUE);
  END EmitValueCall;

PROCEDURE EmitCall (t: T) =
  VAR result := ProcType.CGResult (t.signature);
  BEGIN
    IF (t.impl_peer # NIL) THEN t := t.impl_peer; END;
    CG.Call_direct (t.cg_proc, result);
    EVAL Marker.EmitExceptionTest (t.signature, need_value := FALSE);
  END EmitCall;

PROCEDURE Redefined (t: T;  other: Value.T;) =
  VAR save: INTEGER;
  BEGIN
    save := Scanner.offset;
    IF (other = NIL)
      THEN Scanner.offset := t.origin;
      ELSE Scanner.offset := MIN (t.origin, other.origin);
    END;
    Error.ID (t.name, "procedure redefined");
    Scanner.offset := save;
  END Redefined;

PROCEDURE Signature (t: T): Type.T =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    RETURN t.signature;
  END Signature;

PROCEDURE HasBody (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.body # NIL);
  END HasBody;

PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  BEGIN
    ValueRep.FPStart (t, x, "PROCEDURE ", 0, global := TRUE);
    RETURN 1;
  END AddFPTag;

PROCEDURE FPType (t: T): Type.T =
  BEGIN
    IF (t.intf_peer = NIL)
      THEN RETURN t.signature;
      ELSE RETURN t.intf_peer.signature;
    END;
  END FPType;

BEGIN
  debug := RTParams.IsPresent ("m3front-debug-procedure");
END Procedure.
