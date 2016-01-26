(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Variable.m3                                           *)
(* Last Modified On Tue Jun 20 09:58:08 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 12:45:02 PDT 1995 By ericv      *)
(*      Modified On Thu Dec  5 17:21:40 PST 1991 By muller     *)

MODULE Variable;

IMPORT M3, M3ID, CG, Value, ValueRep, Type, Expr, Error, RunTyme;
IMPORT Scope, AssignStmt, Formal, M3RT, IntegerExpr, TipeMap, M3String;
IMPORT OpenArrayType, Target, TInt, Token, Ident, Module, CallExpr;
IMPORT Decl, Null, Int, LInt, Fmt, Procedure, Tracer, TextExpr, NamedExpr;
IMPORT PackedType, ErrType;
FROM Scanner IMPORT GetToken, Match, cur;

CONST
  Big_Local = 8192; (* x Target.Char.size *)
  Big_Param = 8;    (* x Target.Integer.size *)
  Max_zero_global = 64; (* x Target.Integer.size *)

REVEAL
  T = Value.T BRANDED "Variable.T" OBJECT
        tipe        : Type.T;
        init        : Expr.T;
        sibling     : T;
        formal      : Value.T;
        alias       : T;
        trace       : Tracer.T;
        bounds      : BoundPair;
        cg_var      : CG.Var;
        bss_var     : CG.Var;
        next_cg_var : T;
        init_var    : INTEGER;
        offset      : INTEGER;
        size        : INTEGER;
        align       : AlignVal;
        cg_align    : AlignVal;
        mem_type    : BITS 4 FOR CG.Type;
        stk_type    : BITS 4 FOR CG.Type;
        indirect    : M3.Flag;
        open_ok     : M3.Flag;
        need_addr   : M3.Flag;
        no_type     : M3.Flag;
        global      : M3.Flag;
        initDone    : M3.Flag;
        initZero    : M3.Flag;
        initPending : M3.Flag;
        initStatic  : M3.Flag;
      OVERRIDES
        typeCheck   := Check;
        set_globals := SetGlobals;
        load        := Load;
        declare     := Declare;
        const_init  := ConstInit;
        need_init   := NeedInit;
        lang_init   := LangInit;
        user_init   := UserInit;
        toExpr      := ValueRep.NoExpr;
        toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := TypeOf;
      END;

TYPE
  AlignVal = [0..255];

TYPE
  BoundPair = REF RECORD
    min : Target.Int;
    max : Target.Int;
  END;

VAR
  all_cg_vars: T := NIL;
  (* variables with attached M3CG values *)

PROCEDURE Reset () =
  VAR t, u: T;
  BEGIN
    (* release any M3CG nodes that we've created *)
    t := all_cg_vars;
    WHILE (t # NIL) DO
      u := t;  t := t.next_cg_var;
      u.cg_var      := NIL;
      u.bss_var     := NIL;
      u.next_cg_var := NIL;
    END;
    all_cg_vars := NIL;
  END Reset;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes) =
  TYPE TK = Token.T;
  VAR
    t     : T;
    type  : Type.T;
    expr  : Expr.T;
    j, n  : INTEGER;
    trace : Tracer.T;
    alias : M3ID.T;
  BEGIN
    Match (TK.tVAR);
    WHILE (cur.token = TK.tIDENT) DO
      n := Ident.ParseList ();
      type := NIL;
      expr := NIL;
      IF (cur.token = TK.tCOLON) THEN
        GetToken (); (* : *)
        type := Type.Parse ();
      END;
      IF (cur.token = TK.tEQUAL) THEN
        Error.Msg ("variable initialization must begin with ':='");
        cur.token := TK.tASSIGN;
      END;
      IF (cur.token = TK.tASSIGN) THEN
        GetToken (); (* := *)
        expr := Expr.Parse ();
      END;
      trace := ParseTrace ();
      IF (expr = NIL) AND (type = NIL) THEN
        Error.Msg("variable declaration must include a type or initial value");
      END;
      IF att.isExternal AND att.alias # M3ID.NoID AND n > 1 THEN
        Error.WarnID (2, att.alias,
                       "EXTERNAL alias applies to first variable");
      END;
      alias := att.alias;
      j := Ident.top - n;
      FOR i := 0 TO n - 1 DO
        t := New (Ident.stack[j + i], FALSE);
        t.origin   := Ident.offset[j + i];
        t.external := att.isExternal;
        t.unused   := att.isUnused;
        t.obsolete := att.isObsolete;
        t.tipe     := type;
        t.init     := expr;
        t.no_type  := (type = NIL);
        IF (att.isExternal) THEN
          IF (alias # M3ID.NoID)
            THEN t.extName := alias;  alias := M3ID.NoID;
            ELSE t.extName := t.name;
          END;
        END;
        Scope.Insert (t);
        BindTrace (t, trace);
      END;
      DEC (Ident.top, n);
      Match (TK.tSEMI);
    END;
  END ParseDecl;

PROCEDURE New (name: M3ID.T;  used: BOOLEAN): T =
  VAR t: T;
  BEGIN
    t := NEW (T);
    ValueRep.Init (t, name, Value.Class.Var);
    t.used        := used;
    t.tipe        := NIL;
    t.init        := NIL;
    t.readonly    := FALSE;
    t.indirect    := FALSE;
    t.global      := FALSE;
    t.formal      := NIL;
    t.alias       := NIL;
    t.extName     := M3ID.NoID;
    t.open_ok     := FALSE;
    t.need_addr   := FALSE;
    t.no_type     := FALSE;
    t.initDone    := FALSE;
    t.initZero    := FALSE;
    t.initPending := FALSE;
    t.initStatic  := FALSE;
    t.bounds      := NIL;
    t.cg_align    := 0;
    t.cg_var      := NIL;
    t.bss_var     := NIL;
    t.init_var    := 0;
    t.offset      := 0;
    t.size        := 0;
    t.align       := 0;
    t.mem_type    := CG.Type.Void;
    t.stk_type    := CG.Type.Void;
    t.trace       := NIL;
    RETURN t;
  END New;

PROCEDURE NewFormal (formal: Value.T;  name: M3ID.T): T =
  VAR t: T;  f_info: Formal.Info;
  BEGIN
    t := New (name, FALSE);
    Formal.Split (formal, f_info);
    t.formal   := formal;
    t.tipe     := f_info.type;
    t.origin   := formal.origin;
    t.indirect := (f_info.mode # Formal.Mode.mVALUE);
    t.readonly := (f_info.mode = Formal.Mode.mCONST);
    t.unused   := f_info.unused;
    t.initDone := TRUE;
    t.imported := FALSE; (* in spite of Module.depth *)
    IF (NOT t.indirect) AND (OpenArrayType.Is (t.tipe)) THEN
      t.indirect := TRUE;
    END;
    t.trace := NIL;  (* the caller must call BindTrace after the variable
                        is inserted into a scope *)
    RETURN t;
  END NewFormal;

PROCEDURE Split (t: T;  VAR type: Type.T;
                 VAR global, indirect, traced: BOOLEAN) =
  BEGIN
    <* ASSERT t.checked *>
    type     := t.tipe;
    global   := t.global;
    indirect := t.indirect;
    traced   := t.traced;
  END Split;

PROCEDURE BindType (t: T; type: Type.T; 
                    indirect, readonly, open_array_ok, needs_init: BOOLEAN) =
  BEGIN
    <* ASSERT t.tipe = NIL *>
    t.tipe     := type;
    t.readonly := readonly;
    t.indirect := indirect;
    t.open_ok  := open_array_ok;
    IF NOT needs_init THEN t.initDone := TRUE END;
  END BindType;

PROCEDURE NeedsAddress (t: T) =
  BEGIN
    IF (t = NIL) THEN RETURN END;
    t.need_addr := TRUE;
  END NeedsAddress;

PROCEDURE IsFormal (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.formal # NIL);
  END IsFormal;

PROCEDURE HasClosure (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.formal # NIL) AND Formal.HasClosure (t.formal);
  END HasClosure;

PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    IF (t.tipe = NIL) THEN
      IF    (t.init # NIL)   THEN t.tipe := Expr.TypeOf (t.init)
      ELSIF (t.formal # NIL) THEN t.tipe := Value.TypeOf (t.formal) END;
      IF (t.tipe = NIL)
        THEN Error.ID (t.name, "variable has no type");  t.tipe := ErrType.T;
      END;
    END;
    RETURN t.tipe;
  END TypeOf;

PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
  VAR dfault: Expr.T;  min, max: Target.Int;  info: Type.Info;  ref: Type.T;
  BEGIN
    t.tipe     := Type.CheckInfo (TypeOf (t), info);
    IF (info.class = Type.Class.Packed)
      AND (t.formal # NIL)
      AND (NOT t.indirect) THEN
      EVAL Type.CheckInfo (PackedType.Base (t.tipe), info);
    END;
    t.size     := info.size;
    t.align    := info.alignment;
    t.mem_type := info.mem_type;
    t.stk_type := info.stk_type;
    IF (info.class = Type.Class.OpenArray)
      AND (t.formal = NIL) AND (NOT t.open_ok) THEN
      Error.ID (t.name, "variable cannot be an open array");
    END;
    IF (info.isEmpty) THEN
      Error.ID (t.name, "variable has empty type");
    END;
    IF (t.no_type) AND (t.tipe # ErrType.T)
      AND Type.IsEqual (t.tipe, Null.T, NIL) THEN
      Error.WarnID (1, t.name, "variable has type NULL"); 
    END;

    t.global := Scope.OuterMost (t.scope);
    t.checked := TRUE; (* allow recursions through the init expr *)

    IF (NOT t.indirect) AND (NOT t.global) THEN
      IF (t.formal # NIL) AND (info.size > Big_Param * Target.Integer.size) THEN
        Error.WarnID (1, t.name, "large parameter passed by value ("
                       & Fmt.Int (info.size DIV Target.Char.size) & " bytes)");
      ELSIF (info.size > Big_Local * Target.Char.size) THEN
        Error.WarnID (1, t.name, "large local variable ("
                       & Fmt.Int (info.size DIV Target.Char.size) & " bytes)");
      END;
    ELSIF (t.formal # NIL) AND (info.class = Type.Class.OpenArray)
      AND Formal.RefOpenArray (t.formal, ref) THEN
      Error.WarnID (1, t.name, "open array passed by value");
    END;

    IF Type.IsStructured (t.tipe) THEN
      t.need_addr := TRUE; (* every load requires an address *)
    END;

    Value.TypeCheck (t.formal, cs);
    IF (t.external) THEN
      IF (t.init # NIL) THEN
        Error.Msg ("<*EXTERNAL*> variables cannot be initialized");
        Expr.TypeCheck (t.init, cs);
        AssignStmt.Check (t.tipe, t.init, cs);
      END;
    ELSIF (t.init # NIL) THEN
      Expr.TypeCheck (t.init, cs);
      AssignStmt.Check (t.tipe, t.init, cs);
      dfault := Expr.ConstValue (t.init);
      IF (dfault = NIL) THEN
        IF Module.IsInterface () THEN
          Error.ID (t.name, "initial value is not a constant");
        END;
        IF (t.global) AND (info.size > Max_zero_global * Target.Integer.size) THEN
          <*ASSERT NOT t.indirect*>
          t.indirect := TRUE;
        END;
      ELSE (* initialize the variable to an explicit constant *)
        IF NOT t.indirect THEN
          t.initZero := Expr.IsZeroes (dfault);
          IF (t.global) THEN
            IF (t.initZero) THEN
              t.initDone := TRUE;
              IF (info.size > Max_zero_global * Target.Integer.size) THEN
                <*ASSERT NOT t.indirect*>
                t.indirect := TRUE;
              END;
            END;
          ELSIF (NOT t.initZero) AND Type.IsStructured (t.tipe) THEN
            t.initStatic := TRUE;
          END;
          t.init := dfault;
        END;
      END;
    ELSIF (t.global) THEN
      (* no explict initialization is given, but the var is global *)
      IF Type.InitCost (t.tipe, TRUE) <= 0 THEN
        IF (info.size > Max_zero_global * Target.Integer.size) THEN
          <*ASSERT NOT t.indirect*>
          t.indirect := TRUE;
        END;
        t.initDone := TRUE;
      ELSIF Type.GetBounds (t.tipe, min, max) THEN
        (* synthesize an initialization expression *)
        IF Type.IsSubtype (t.tipe, LInt.T)
          THEN t.init := IntegerExpr.New (LInt.T, min);
          ELSE t.init := IntegerExpr.New (Int.T, min);
        END;
      END;
    END;

    CheckTrace (t.trace, cs);
  END Check;

PROCEDURE Load (t: T) =
  BEGIN
    t.used := TRUE;
    Value.Declare (t);
    IF (t.initPending) THEN ForceInit (t); END;
    IF Type.IsStructured (t.tipe) THEN
      (* the RunTyme representation is an address *)
      IF (t.bss_var # NIL) THEN
        CG.Load_addr_of (t.bss_var, 0, t.cg_align);
      ELSIF (t.cg_var = NIL) THEN (* => global *)
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
        CG.Boost_alignment (t.align);
      ELSIF (t.indirect) THEN
        CG.Load_addr (t.cg_var, t.offset);
        CG.Boost_alignment (t.align);
      ELSE
        CG.Load_addr_of (t.cg_var, t.offset, t.cg_align);
      END;
    ELSE (* simple scalar *)
      IF (t.bss_var # NIL) THEN
        CG.Load (t.bss_var, 0, t.size, t.cg_align, t.stk_type);
      ELSIF (t.cg_var = NIL) THEN (* => global *)
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
        IF (t.indirect) THEN
          CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);
        END;
        CG.Boost_alignment (t.align);
        CG.Load_indirect (t.stk_type, 0, t.size);
      ELSIF (t.indirect) THEN
        CG.Load_addr (t.cg_var, t.offset);
        CG.Boost_alignment (t.align);
        CG.Load_indirect (t.stk_type, 0, t.size);
      ELSE
        CG.Load (t.cg_var, t.offset, t.size, t.cg_align, t.stk_type);
      END;
    END;
  END Load;

PROCEDURE LoadLValue (t: T) =
  BEGIN
    t.used := TRUE;
    Value.Declare (t);
    IF (t.initPending) THEN ForceInit (t); END;
    IF (t.bss_var # NIL) THEN
      CG.Load_addr_of (t.bss_var, 0, t.cg_align);
    ELSIF (t.cg_var = NIL) THEN (* => global variable *)
      Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
      IF (t.indirect) THEN
        CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);
      END;
    ELSIF (t.indirect) THEN
      CG.Load_addr (t.cg_var, t.offset);
    ELSE
      CG.Load_addr_of (t.cg_var, t.offset, t.cg_align);
    END;
    CG.Boost_alignment (t.align);
  END LoadLValue;

PROCEDURE SetLValue (t: T) =
  VAR v: CG.Var;  align: INTEGER;
  BEGIN
    t.used := TRUE;
    Value.Declare (t);
    IF (t.initPending) THEN t.initPending := FALSE; END;
    v := t.cg_var;
    align := t.cg_align;
    IF (v = NIL) THEN
      v := Module.GlobalData (is_const := FALSE);
      align := CG.Max_alignment;
    END;
    <*ASSERT t.indirect *>
    CG.Boost_alignment (align);
    CG.Store_addr (v, t.offset);
  END SetLValue;

PROCEDURE LocalCGName (t: T;  VAR unit: CG.Var;  VAR offset: INTEGER) =
  BEGIN
    t.used := TRUE;
    Value.Declare (t);
    IF (t.initPending) THEN ForceInit (t); END;
    <*ASSERT NOT t.imported*>
    IF (t.cg_var = NIL)
      THEN unit := Module.GlobalData (FALSE);  offset := t.offset;
      ELSE unit := t.cg_var;                   offset := 0;
    END;
  END LocalCGName;

PROCEDURE SetBounds (t: T;  READONLY min, max: Target.Int) =
  BEGIN
    IF (t.bounds = NIL) THEN t.bounds := NEW (BoundPair) END;
    t.bounds.min := min;
    t.bounds.max := max;
  END SetBounds;

PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int) =
  VAR xx := t.bounds;
  BEGIN
    EVAL Type.GetBounds (t.tipe, min, max);
    IF (xx = NIL) THEN RETURN; END;
    IF TInt.LT (min, xx.min) THEN min := xx.min; END;
    IF TInt.LT (xx.max, max) THEN max := xx.max; END;
  END GetBounds;

PROCEDURE SetGlobals (t: T) =
  VAR size, align: INTEGER;
  VAR initType: M3.Type := NIL; 
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    (* IF (t.init # NIL) THEN Type.SetGlobals (Expr.TypeOf (t.init)) END; *)
    IF (t.offset # 0) OR (NOT t.global) OR (t.external) THEN RETURN END;
    EVAL Type.Check (t.tipe);

    IF t.init # NIL THEN initType := Expr.TypeOf (t.init) END; 

    IF (t.indirect) THEN
      size  := Target.Address.size;
      align := Target.Address.align;
    ELSIF FALSE AND OpenArrayType.Is (initType) THEN
(* FIXME, maybe.  Can the variable be a formal with open array type, in 
   addition to the initializer's having an open array type?  If so, we want 
   to come here. *) 
      align := MAX (Target.Address.align, Target.Integer.align);
      size  := Target.Address.pack
               + OpenArrayType.OpenDepth(initType) * Target.Integer.pack;
    ELSE
      size  := t.size;
      align := t.align;
    END;

    (* declare the actual variable *)
    t.offset := Module.Allocate (size, align, FALSE, id := t.name);
  END SetGlobals;

PROCEDURE Declare (t: T): BOOLEAN =
  VAR
    size      := t.size;
    align     := t.align;
    type      := Type.GlobalUID (t.tipe);
    mtype     := Type.CGType (t.tipe, in_memory := TRUE);
    is_struct := Type.IsStructured (t.tipe);
    name        : TEXT;
    extern_name : M3ID.T;
  BEGIN
    Type.Compile (t.tipe);

    t.cg_var  := NIL;
    t.bss_var := NIL;

    IF (is_struct) THEN mtype := CG.Type.Struct; END;

    IF (t.indirect) THEN
      type  := CG.Declare_indirect (type);
      size  := Target.Address.size;
      align := Target.Address.align;
      mtype := CG.Type.Addr;
    END;

    (* declare the actual variable *)
    IF (t.external) THEN
      name := Value.GlobalName (t, dots := FALSE, with_module := FALSE);
      extern_name := M3ID.Add (name);
      t.next_cg_var := all_cg_vars;  all_cg_vars := t;
      t.cg_var := CG.Import_global (extern_name, size, align,
                                    mtype, 0(*no mangling*));
      t.cg_align := align;

    ELSIF (t.imported) THEN
      <*ASSERT t.offset # 0*>

    ELSIF (t.global) THEN
      <*ASSERT t.offset # 0*>
      CG.Declare_global_field (t.name, t.offset, size, type, FALSE);
      IF (t.initZero) THEN t.initDone := TRUE END;
      t.cg_align := align;
      IF (t.indirect) THEN
        t.cg_align := t.align;
        t.next_cg_var := all_cg_vars;  all_cg_vars := t;
        t.bss_var := CG.Declare_global (M3ID.NoID, t.size, t.cg_align,
                              CG.Type.Struct, Type.GlobalUID (t.tipe),
                              exported := FALSE, init := FALSE);
        CG.Init_var (t.offset, t.bss_var, 0, FALSE);
      END;

    ELSIF (t.formal = NIL) THEN
      (* simple local variable *)
      IF (size < 0) THEN
        (* it's an open array local introduced by a WITH statement *)
        align := MAX (Target.Address.align, Target.Integer.align);
        size  := Target.Address.pack
                  + OpenArrayType.OpenDepth(t.tipe) * Target.Integer.pack;
      END;
      (** align := FindAlignment (align, size); **)
      t.cg_align := align;
      t.next_cg_var := all_cg_vars;  all_cg_vars := t;
      t.cg_var := CG.Declare_local (t.name, size, align, mtype, type,
                                    t.need_addr, t.up_level, CG.Maybe);

    ELSIF (t.indirect) THEN
      (* formal passed by reference => param is an address *)
      t.cg_align := align;
      t.next_cg_var := all_cg_vars;  all_cg_vars := t;
      t.cg_var := CG.Declare_param (t.name, size, align, mtype, type,
                                    t.need_addr, t.up_level, CG.Maybe);

    ELSE
      (* simple parameter *)
      (** align := FindAlignment (align, size); **)
      t.cg_align := align;
      t.next_cg_var := all_cg_vars;  all_cg_vars := t;
      t.cg_var := CG.Declare_param (t.name, size, align, mtype, type,
                                    t.need_addr, t.up_level, CG.Maybe);
    END;

    RETURN TRUE;
  END Declare;

(** -- this doesn't work with the current gcc-based backend.  It
       chokes on  VAR v: BITS 32 FOR CHAR := 'X' -- 10/9/96 WKK
PROCEDURE FindAlignment (align: AlignVal;  size: INTEGER): AlignVal =
  (* Fix the alignment of small local variables and parameters
     with BITS FOR types *)
  BEGIN
    IF    size < 0                  THEN (*don't mess with open array alignments*)
    ELSIF size >= Target.Int_D.size THEN align := MAX (align, Target.Int_D.align);
    ELSIF size <= Target.Int_A.size THEN align := MAX (align, Target.Int_A.align);
    ELSIF size <= Target.Int_B.size THEN align := MAX (align, Target.Int_B.align);
    ELSIF size <= Target.Int_C.size THEN align := MAX (align, Target.Int_C.align);
    ELSE                                 align := MAX (align, Target.Int_D.align);
    END;
    RETURN align;
  END FindAlignment;
**)

PROCEDURE ConstInit (t: T) =
  VAR
    size      := t.size;
    align     := t.align;
    type      : INTEGER;
    init_expr : Expr.T;
    name      : TEXT;
    init_name : M3ID.T;
  BEGIN
    IF t.external OR t.imported THEN RETURN END;
    IF (NOT t.initStatic) AND (NOT t.global) THEN RETURN END;

    type := Type.GlobalUID (t.tipe);
    IF (t.indirect) THEN
      type  := CG.Declare_indirect (type);
      size  := Target.Address.size;
      align := Target.Address.align;
    END;

    IF (t.initStatic) THEN
      (* declare the holder for the initial value *)
      name := "_INIT_" & M3ID.ToText (t.name);
      init_name := M3ID.Add (name);
      t.init_var := Module.Allocate (size, align, TRUE,"initial value for ",t.name);
      CG.Declare_global_field (init_name, t.init_var, size, type, TRUE);
      CG.Comment (t.init_var, TRUE, "init expr for ",Value.GlobalName(t,TRUE,TRUE));
      init_expr := Expr.ConstValue (t.init);
      Expr.PrepLiteral (init_expr, t.tipe, TRUE);
      Expr.GenLiteral (init_expr, t.init_var, t.tipe, TRUE);
    END;

    IF (t.global) THEN
      (* try to statically initialize the variable *)
      <*ASSERT t.offset # 0*>
      init_expr := NIL;
      IF (t.init # NIL) AND (NOT t.initDone) AND (NOT t.initStatic) THEN
        init_expr := Expr.ConstValue (t.init);
      END;
      IF (init_expr # NIL) THEN
        Expr.PrepLiteral (init_expr, t.tipe, FALSE);
        Expr.GenLiteral (init_expr, t.offset, t.tipe, FALSE);
        t.initDone := TRUE;
      END;
    END;
  END ConstInit;

PROCEDURE NeedInit (t: T): BOOLEAN =
  VAR ref: Type.T;
  BEGIN
    IF (t.imported) OR (t.external) OR (t.initDone) THEN
      RETURN FALSE;
    ELSIF (t.formal # NIL) THEN
      RETURN (t.indirect) AND Formal.RefOpenArray (t.formal, ref);
    ELSIF (t.indirect) AND (NOT t.global) THEN
      RETURN FALSE;
    ELSIF (t.global) AND (t.init # NIL) AND (NOT t.initStatic)
      AND (Expr.ConstValue (t.init) # NIL) THEN
      RETURN FALSE;
    ELSIF (t.init # NIL) THEN
      RETURN TRUE;
    ELSE
      RETURN Type.InitCost (t.tipe, FALSE) > 0;
    END;
  END NeedInit;
  
PROCEDURE LangInit (t: T) =
  VAR ref: Type.T;
  BEGIN
    IF (t.imported) OR (t.external) THEN
      t.initDone := TRUE;
    ELSIF (t.formal # NIL) THEN
      IF (t.indirect) AND Formal.RefOpenArray (t.formal, ref) THEN
        (* a by-value open array! *)
        CG.Gen_location (t.origin);
        CopyOpenArray (t, ref);
      END;
      (* formal parameters don't need any further initialization *)
      Tracer.Schedule (t.trace);
      t.initDone := TRUE;
    ELSIF (t.indirect) AND (NOT t.global) THEN
      (* is a WITH variable bound to a designator *)
      Tracer.Schedule (t.trace);
      t.initDone := TRUE;
    END;

    IF (t.initDone) THEN RETURN END;

    (* initialize the value *)
    IF (t.init # NIL) AND (NOT t.up_level) AND (NOT t.imported) THEN
      (* variable has a user specified init value and isn't referenced
         by any nested procedures => try to avoid the language defined
         init and wait until we get to the user defined initialization. *)
      t.initPending := TRUE;
    ELSE
      IF Type.InitCost (t.tipe, FALSE) > 0 THEN
        CG.Gen_location (t.origin);
        LoadLValue (t);
        Type.InitValue (t.tipe, FALSE);
      END;
      IF (t.trace # NIL) AND (NOT t.imported) THEN
        IF (t.init = NIL) OR (t.initDone) THEN
          (* there's no explicit user init => might as well trace it now *)
          CG.Gen_location (t.origin);
          Tracer.Schedule (t.trace);
        END;
      END;
    END;
  END LangInit;

PROCEDURE ForceInit (t: T) =
  BEGIN
    t.initPending := FALSE;
    CG.Gen_location (t.origin);
    LoadLValue (t);
    Type.InitValue (t.tipe, FALSE);
  END ForceInit;

PROCEDURE CopyOpenArray (t: T;  ref: Type.T) =
  VAR
    ptr   : CG.Val;
    depth := OpenArrayType.OpenDepth (t.tipe);
    align := OpenArrayType.EltAlign (t.tipe);
    pack  := OpenArrayType.EltPack (t.tipe);
    sizes := CG.Declare_temp (Target.Address.pack + Target.Integer.pack,
                              Target.Address.align, CG.Type.Struct,
                              in_memory := TRUE);
    proc  : Procedure.T;
  BEGIN
    (* build the dope vector that describes the array *)
    Load (t);
    CG.Add_offset (M3RT.OA_sizes);
    (*** CG.Check_byte_aligned (); ****)
    CG.Store_addr (sizes, M3RT.OA_elt_ptr);
    CG.Load_intt (depth);
    CG.Store_int (Target.Integer.cg_type, sizes, M3RT.OA_size_0);
        
    (* allocate the storage *)
    proc := RunTyme.LookUpProc (RunTyme.Hook.NewTracedArray);
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      Type.LoadInfo (ref, -1);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_addr_of (sizes, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
    ELSE
      CG.Load_addr_of (sizes, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
      Type.LoadInfo (ref, -1);
      CG.Pop_param (CG.Type.Addr);
    END;
    ptr := Procedure.EmitValueCall (proc);

    (* load the destination and source addresses *)
    CG.Push (ptr);
    CG.Boost_alignment (t.align);
    CG.Open_elt_ptr (align);
    CG.Force ();
    Load (t);
    CG.Open_elt_ptr (align);
    CG.Force ();

    (* compute the number of elements *)
    FOR i := 0 TO depth - 1 DO
      Load (t); (* CG.Load_addr (sizes, M3RT.OA_elt_ptr); *)
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (Target.Word.cg_type) END;
    END;

    (* copy the actual argument into the new storage *)
    CG.Copy_n (pack, overlap := FALSE);

    (* set the formal parameter to refer to the new storage *)
    CG.Push (ptr);
    CG.Boost_alignment (t.align);
    CG.Store_addr (t.cg_var);

    (* free our temps *)
    CG.Free_temp (sizes);
    CG.Free (ptr);
  END CopyOpenArray;

PROCEDURE UserInit (t: T) =
  BEGIN
    IF (t.init # NIL) AND (NOT t.initDone) AND (NOT t.imported) THEN
      CG.Gen_location (t.origin);
      IF (t.initZero) THEN
        t.initPending := FALSE;
        LoadLValue (t);
        Type.Zero (t.tipe);
      ELSIF (t.init_var # 0) THEN
        t.initPending := FALSE;
        LoadLValue (t);
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.init_var, is_const := TRUE);
        CG.Copy (t.size, overlap := FALSE);
      ELSE
        t.initPending := FALSE;
        AssignStmt.PrepForEmit (t.tipe, t.init, initializing := TRUE);
        LoadLValue (t);
        AssignStmt.DoEmit (t.tipe, t.init);
      END;
      t.initDone := TRUE;
      Tracer.Schedule (t.trace);
    END;
  END UserInit;

PROCEDURE GenGlobalMap (s: Scope.T): INTEGER =
  (* generate the garbage collector's map-proc for the variables of s *)
  VAR started := FALSE;  info: Type.Info;  v := Scope.ToList (s);
  BEGIN
    WHILE (v # NIL) DO
      TYPECASE Value.Base (v) OF
      | NULL =>  (* do nothing *)
      | T(t) =>  IF (NOT t.imported)
                   AND (NOT t.external) THEN
                   EVAL Type.CheckInfo (t.tipe, info);
                   IF (info.isTraced) THEN
                     IF (NOT started) THEN
                       TipeMap.Start ();
                       started := TRUE;
                     END;
                     t.used := TRUE;
                     Value.Declare (t);
                     IF (t.indirect) THEN
                       TipeMap.Add (t.offset, TipeMap.Op.PushPtr, 0);
                       Type.GenMap (t.tipe, 0, -1, refs_only := TRUE);
                       TipeMap.Add (t.size, TipeMap.Op.Return, 0);
                       TipeMap.SetCursor (t.offset + Target.Address.size);
                     ELSE
                       Type.GenMap (t.tipe, t.offset, -1, refs_only := TRUE);
                     END;
                   END;
                 END;
      ELSE (* do nothing *)
      END;
      v := v.next;
    END;
    IF (started)
      THEN RETURN TipeMap.Finish ("global type map");
      ELSE RETURN -1;
    END;
  END GenGlobalMap;

PROCEDURE NeedGlobalInit (t: T): BOOLEAN =
  BEGIN
    RETURN (NOT t.initDone) AND (NOT t.external);
  END NeedGlobalInit;

PROCEDURE InitGlobal (t: T) =
  BEGIN
    IF (NOT t.initDone) AND (NOT t.external) THEN
      LoadLValue (t);
      Type.InitValue (t.tipe, TRUE);
    END;
  END InitGlobal;

PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  BEGIN
    ValueRep.FPStart (t, x, "VAR ", t.offset, global := TRUE);
    RETURN 1;
  END AddFPTag;

(*--------------------------------------------------------- trace support ---*)

TYPE TraceNode = Tracer.T OBJECT
                   handler : Expr.T := NIL;
                   call    : Expr.T := NIL;
                 OVERRIDES
                   apply := DoTrace;
                 END;

PROCEDURE ParseTrace (): Tracer.T =
  TYPE TK = Token.T;
  VAR e: Expr.T;
  BEGIN
    IF (cur.token # TK.tTRACE) THEN RETURN NIL END;
    Match (TK.tTRACE);
    e := Expr.Parse ();
    Match (TK.tENDPRAGMA);
    IF (e = NIL) THEN RETURN NIL END;
    RETURN NEW (TraceNode, handler := e);
  END ParseTrace;

PROCEDURE BindTrace (t: T;  xx: Tracer.T) =
  VAR x: TraceNode := xx;  p: Scope.IDStack;  z: M3String.T;  args: Expr.List;
  BEGIN
    IF (xx = NIL) THEN RETURN END;

    IF (x.call # NIL) THEN
      x := NEW (TraceNode, handler := x.handler);
    END;

    (* get the variable's full name *)
    p.top := 0;
    Scope.NameToPrefix (t, p, dots := TRUE, with_module := TRUE);
    z := M3String.Add (Scope.StackToText (p));

    (* build the trace procedure call *)
    args := NEW (Expr.List, 2);
    args[0] := TextExpr.New8 (z);
    args[1] := NamedExpr.FromValue (t);
    x.call  := CallExpr.New (x.handler, args);

    <*ASSERT t.trace = NIL*>
    t.trace := x;
  END BindTrace;

PROCEDURE DoTrace (x: TraceNode) =
  BEGIN
    Expr.Prep (x.call);
    Expr.Compile (x.call);
  END DoTrace;

PROCEDURE CheckTrace (tt: Tracer.T;  VAR cs: Value.CheckState) =
  VAR x: TraceNode := tt;
  BEGIN
    IF (x # NIL) THEN
      Expr.TypeCheck (x.handler, cs);
      Expr.TypeCheck (x.call, cs);
    END;
  END CheckTrace;

PROCEDURE ScheduleTrace (t: T) =
  BEGIN
    Tracer.Schedule (t.trace);
  END ScheduleTrace;

BEGIN
END Variable.
