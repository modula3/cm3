(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Variable.m3                                           *)
(* Last Modified On Tue Jun 20 09:58:08 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 12:45:02 PDT 1995 By ericv      *)
(*      Modified On Thu Dec  5 17:21:40 PST 1991 By muller     *)

MODULE Variable;
(* Including formal parameters. *)

IMPORT M3, M3ID, CG, Value, ValueRep, Error, RunTyme;
IMPORT Scope, AssignStmt, Formal, M3RT, M3String;
IMPORT Target, TInt, Token, Ident, Module, CallExpr;
IMPORT Decl, Null, Int, LInt, Fmt, Procedure, Tracer;
IMPORT Expr, IntegerExpr, ArrayExpr, TextExpr, NamedExpr;
IMPORT Type, PackedType, OpenArrayType, ErrType, TipeMap;
FROM Scanner IMPORT GetToken, Match, cur;

CONST
  Big_Local = 8192; (* x Target.Char.size *)
  Big_Param = 8;    (* x Target.Integer.size *)
  Max_zero_global = 64; (* x Target.Integer.size *)

REVEAL
  T = Value.T BRANDED "Variable.T" OBJECT
        tipe        : Type.T;
        initExpr    : Expr.T;
        qualName    : TEXT;
        sibling     : T;
        formal      : Value.T;
        (* ^This Variable.T represents a formal parameter, but that's a
            Formal.T, a different and hidden proper subtype of Value.T.
            Field formal is actually the Formal.T object.
            A great example of TMIH (Too Much Information Hiding). *)
        alias       : T;
        trace       : Tracer.T;
        bounds      : BoundPair;
        cg_var      : CG.Var; (* Used if it's a local, formal, or external. *)
        bss_var     : CG.Var; (* Used if it's a global. *)
        nextTWACGVar : T; (* Link field for list of Variable..Ts that have a
                             non-NIL bss_var or cg_var. *)
        initValOffset : INTEGER := 0;
        offset      : INTEGER := 0;
        size        : INTEGER;
        align       : AlignVal;
        cg_align    : AlignVal;
        mem_type    : BITS 4 FOR CG.Type;
        stk_type    : BITS 4 FOR CG.Type;
        indirect    : M3.Flag;
        open_ok     : M3.Flag;
        need_addr   : M3.Flag;
        no_type     : M3.Flag; (* Type not explicitly coded. *)
        global      : M3.Flag; (* Declared in outermost scope. *)
        initDone    : M3.Flag;
        initZero    : M3.Flag; (* Initial value is all binary zeros. *)
        initPending : M3.Flag; (* Initialization is postponed. *)
        initStatic  : M3.Flag; (* Needs RT initialization with a value from
                                  the static constant area. *)
        allocated     : M3.Flag;
          (* ^Has allocated space in the global variable area. *)
        initAllocated : M3.Flag;
          (* ^Static initial value has allocated space in the global constant area. *)
      OVERRIDES
        typeCheck   := Check;
        set_globals := AllocGlobalVarSpace;
        load        := Load;
        declare     := Declare;
        const_init  := ConstInit;
        need_init   := NeedInit;
        lang_init   := LangInit;
        user_init   := UserInit;
        toExpr      := ValueRep.NoExpr;
        toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        repTypeOf   := TypeOf;
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
  TsWCGVars: T := NIL;
  (* Linked list of Variable.Ts that have a non-NIL bss_var or cg_var. *)

(* EXPORTED *)
PROCEDURE Reset () =
(* Toss as garbage, any CG.Var nodes that we've created *)
  VAR t, u: T;
  BEGIN
    t := TsWCGVars;
    WHILE (t # NIL) DO
      u := t;  t := t.nextTWACGVar;
      u.cg_var      := NIL;
      u.bss_var     := NIL;
      u.nextTWACGVar := NIL;
    END;
    TsWCGVars := NIL;
  END Reset;

(* EXPORTED *)
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
        Error.Msg ("Variable initialization must begin with ':='.");
        cur.token := TK.tASSIGN;
      END;
      IF (cur.token = TK.tASSIGN) THEN
        GetToken (); (* := *)
        expr := Expr.Parse ();
      END;
      trace := ParseTrace ();
      IF (expr = NIL) AND (type = NIL) THEN
        Error.Msg("Variable declaration must include a type or initial value.");
      END;
      IF att.isExternal AND att.alias # M3ID.NoID AND n > 1 THEN
        Error.WarnID (2, att.alias,
                       "EXTERNAL alias applies only to the first variable.");
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
        t.initExpr := expr;
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

(* EXPORTED *)
PROCEDURE New (name: M3ID.T;  used: BOOLEAN): T =
  VAR t: T;
  BEGIN
    t := NEW (T);
    ValueRep.Init (t, name, Value.Class.Var);
    t.used        := used;
    t.tipe        := NIL;
    t.initExpr    := NIL;
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
    t.allocated   := FALSE;
    t.initAllocated := FALSE;
    t.bounds      := NIL;
    t.cg_align    := 0;
    t.cg_var      := NIL;
    t.bss_var     := NIL;
    t.size        := 0;
    t.align       := 0;
    t.mem_type    := CG.Type.Void;
    t.stk_type    := CG.Type.Void;
    t.trace       := NIL;
    RETURN t;
  END New;

(* EXPORTED *)
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
(* REVIEW^ can this be right? *) 
    t.imported := FALSE; (* in spite of Module.depth *)
    IF (NOT t.indirect) AND (OpenArrayType.Is (t.tipe)) THEN
      t.indirect := TRUE;
    END;
    t.trace := NIL;  (* the caller must call BindTrace after the variable
                        is inserted into a scope *)
    RETURN t;
  END NewFormal;

(* EXPORTED *)
PROCEDURE Split (t: T;  VAR type: Type.T;
                 VAR global, indirect, traced: BOOLEAN) =
  BEGIN
    <* ASSERT t.checked *>
    type     := t.tipe;
    global   := t.global;
    indirect := t.indirect;
    traced   := t.traced;
  END Split;

(* EXPORTED *)
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

(* EXPORTED *)
PROCEDURE NeedsAddress (t: T) =
  BEGIN
    IF (t = NIL) THEN RETURN END;
    t.need_addr := TRUE;
  END NeedsAddress;

(* EXPORTED *)
PROCEDURE IsFormal (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.formal # NIL);
  END IsFormal;

(* EXPORTED *)
PROCEDURE HasClosure (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.formal # NIL) AND Formal.HasClosure (t.formal);
  END HasClosure;

(* Externally dispatched-to *)
PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    IF (t.tipe = NIL) THEN
      IF t.initExpr # NIL THEN t.tipe := Expr.TypeOf (t.initExpr)
      ELSIF  t.formal # NIL THEN t.tipe := Value.TypeOf (t.formal)
      END;
      IF (t.tipe = NIL)
        THEN Error.ID (t.name, "Variable has no type.");  t.tipe := ErrType.T;
      END;
    END;
    RETURN t.tipe;
  END TypeOf;

(* Externally dispatched-to *)
PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
  VAR dfault: Expr.T;  min, max: Target.Int;  info: Type.Info;  refType: Type.T;
  BEGIN
    t.tipe     := Type.CheckInfo (TypeOf (t), info);
    IF (info.class = Type.Class.Packed)
       AND (t.formal # NIL)
       AND (NOT t.indirect) THEN (* VALUE formal of type BITS FOR. *)
      EVAL Type.CheckInfo (PackedType.Base (t.tipe), info);
    END;
    t.size     := info.size;
    t.align    := info.alignment;
    t.mem_type := info.mem_type;
    t.stk_type := info.stk_type;
    IF (info.class = Type.Class.OpenArray)
      AND (t.formal = NIL) AND (NOT t.open_ok) THEN
      Error.ID (t.name, "Variable cannot be an open array.");
    END;
    IF (info.isEmpty) THEN
      Error.ID (t.name, "Variable has empty type.");
    END;
    IF t.tipe = Null.T THEN
      Error.WarnID (1, t.name, "Variable has type NULL."); 
    END;

    t.global := Scope.OuterMost (t.scope);
    t.checked := TRUE; (* Allow recursions through initExpr. *)

    IF (NOT t.indirect) AND (NOT t.global) THEN
      IF (t.formal # NIL) AND (info.size > Big_Param * Target.Integer.size) THEN
        Error.WarnID (1, t.name, "Large parameter passed by value ("
                       & Fmt.Int (info.size DIV Target.Char.size) & " bytes).");
      ELSIF (info.size > Big_Local * Target.Char.size) THEN
        Error.WarnID (1, t.name, "Large local variable ("
                       & Fmt.Int (info.size DIV Target.Char.size) & " bytes).");
      END;
    ELSIF (t.formal # NIL) AND (info.class = Type.Class.OpenArray)
      AND Formal.OpenArrayByVALUE (t.formal, (*VAR*) refType) THEN
      Error.WarnID (1, t.name, "Open array passed by value.");
    END;

    IF Type.IsStructured (t.tipe) THEN
      t.need_addr := TRUE; (* every load requires an address *)
    END;

    Value.TypeCheck (t.formal, cs);
    IF (t.external) THEN
      IF (t.initExpr # NIL) THEN
        Error.Msg ("<*EXTERNAL*> variables cannot be initialized.");
        Expr.TypeCheck (t.initExpr, cs);
        AssignStmt.Check (t.tipe, t.initExpr, cs);
      END;
    ELSIF (t.initExpr # NIL) THEN
      Expr.TypeCheck (t.initExpr, cs);
      AssignStmt.Check (t.tipe, t.initExpr, cs);
(* TODO: What if initExpr contains RT errors? *)
      dfault := Expr.ConstValue (t.initExpr);
      IF (dfault = NIL) THEN
        IF Module.IsInterface () THEN
          Error.ID (t.name, "Initial value in an interface must be constant.");
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
          t.initExpr := dfault;
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
        THEN t.initExpr := IntegerExpr.New (LInt.T, min);
        ELSE t.initExpr := IntegerExpr.New (Int.T, min);
        END;
      END;
    END;

    t.qualName := Value.GlobalName(t,TRUE,TRUE);
    CheckTrace (t.trace, cs);
  END Check;

(* EXPORTED *)
(* Externally dispatched-to *)
PROCEDURE Load (t: T) =
  VAR type_info: Type.Info;
  BEGIN
    t.used := TRUE;
    Value.Declare (t);
    IF (t.initPending) THEN ForceInit (t); END;
    IF Type.IsStructured (t.tipe) THEN
      (* The runtime representation is an address *)
      IF (t.bss_var # NIL) THEN
        CG.Load_addr_of (t.bss_var, 0, t.cg_align);
      ELSIF (t.cg_var = NIL) THEN (* => global *)
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
        CG.Boost_addr_alignment (t.cg_align);
      ELSIF (t.indirect) THEN
        CG.Load_addr (t.cg_var, t.offset, t.cg_align);
      ELSE
        CG.Load_addr_of (t.cg_var, t.offset, CG.GCD(t.cg_align, t.offset));
      END;
    ELSE (* simple scalar *)
      EVAL Type.CheckInfo (t.tipe, type_info);
      IF (t.bss_var # NIL) THEN
        CG.Load
          (t.bss_var, 0, t.size, t.cg_align, type_info.addr_align, t.stk_type);
      ELSIF (t.cg_var = NIL) THEN (* => global *)
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
        IF (t.indirect) THEN
          CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);
        END;
        CG.Boost_addr_alignment (t.cg_align);
        CG.Load_indirect (t.stk_type, 0, t.size, type_info.addr_align);
      ELSIF (t.indirect) THEN
        CG.Load_addr (t.cg_var, t.offset, t.cg_align);
        CG.Load_indirect (t.stk_type, 0, t.size, type_info.addr_align);
      ELSE
        CG.Load
          (t.cg_var, t.offset, t.size, CG.GCD (t.cg_align, t.offset),
           type_info.addr_align, t.stk_type);
      END;
    END;
  END Load;

(* EXPORTED *)
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
      CG.Load_addr (t.cg_var, t.offset, t.cg_align);
    ELSE
      CG.Load_addr_of (t.cg_var, t.offset, CG.GCD (t.cg_align, t.offset));
    END;
    CG.Boost_addr_alignment (t.cg_align);
  END LoadLValue;

(* EXPORTED *)
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
    CG.Boost_addr_alignment (t.cg_align);
    CG.Store_addr (v, t.offset);
  END SetLValue;

(* EXPORTED *)
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

(* EXPORTED *)
PROCEDURE SetBounds (t: T;  READONLY min, max: Target.Int) =
  BEGIN
    IF (t.bounds = NIL) THEN t.bounds := NEW (BoundPair) END;
    t.bounds.min := min;
    t.bounds.max := max;
  END SetBounds;

(* EXPORTED *)
PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int) =
  VAR xx := t.bounds;
  BEGIN
    EVAL Type.GetBounds (t.tipe, min, max);
    IF (xx = NIL) THEN RETURN; END;
    IF TInt.LT (min, xx.min) THEN min := xx.min; END;
    IF TInt.LT (xx.max, max) THEN max := xx.max; END;
  END GetBounds;

(* Externally dispatched-to *)
PROCEDURE AllocGlobalVarSpace (t: T) =
(* Allocate space for a non-external global. *)
  VAR size, align: INTEGER;
  VAR constInitExpr: Expr.T;
  VAR initRepType: Type.T := NIL;
  VAR varID: M3ID.T;
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    (* IF (t.initExpr # NIL) THEN Type.SetGlobals (Expr.TypeOf (t.initExpr)) END; *)
    IF t.allocated (* Already done.*) OR NOT t.global OR t.external THEN RETURN END;
    EVAL Type.Check (t.tipe);

    IF t.initExpr # NIL THEN
      constInitExpr := Expr.ConstValue (t.initExpr);
      ArrayExpr.NoteTargetType (constInitExpr, t.tipe);
      initRepType := Expr.RepTypeOf (t.initExpr)
    END;

    IF (t.indirect) THEN
      size  := Target.Address.size;
      align := Target.Address.align;
      varID := M3ID.Add (t.qualName & "_INDIRECT_");
    ELSIF OpenArrayType.Is (initRepType) THEN
      size  := Target.Address.size
               + OpenArrayType.OpenDepth(initRepType) * Target.Integer.size;
      align := MAX (Target.Address.align, Target.Integer.align);
      varID := M3ID.Add (t.qualName & "_DOPE_");
    ELSE
      size  := t.size;
      align := t.align;
      varID := M3ID.Add (t.qualName);
    END;

    (* declare the actual variable *)
    t.offset := Module.Allocate (size, align, FALSE, id := varID);
    t.allocated := TRUE;
  END AllocGlobalVarSpace;

(* Externally dispatched-to *)
PROCEDURE Declare (t: T): BOOLEAN =
  VAR
    size       := t.size;
    align      := t.align;
    typeUID    := Type.GlobalUID (t.tipe);
    mtype      := Type.CGType (t.tipe, in_memory := TRUE);
    is_struct  := Type.IsStructured (t.tipe);
    externName : TEXT;
    externM3ID : M3ID.T;
  BEGIN
    Type.Compile (t.tipe);

    t.cg_var  := NIL;
    t.bss_var := NIL;

    IF (is_struct) THEN mtype := CG.Type.Struct; END;

    IF (t.indirect) THEN
      typeUID := CG.Declare_indirect (typeUID);
      size := Target.Address.size;
      align := Target.Address.align;
      mtype := CG.Type.Addr;
    END;

    (* declare the actual variable *)
    IF (t.external) THEN
      externName := Value.GlobalName (t, dots := FALSE, with_module := FALSE);
      externM3ID := M3ID.Add (externName);
      t.nextTWACGVar := TsWCGVars;  TsWCGVars := t;
      t.cg_var
        := CG.Import_global (externM3ID, size, align, mtype, 0(*no mangling*));
      t.cg_align := align;

    ELSIF (t.imported) THEN
      <*ASSERT t.allocated*>

    ELSIF (t.global) THEN
      <*ASSERT t.allocated*>
      CG.Declare_global_field (t.name, t.offset, size, typeUID, FALSE);
      IF (t.initZero) THEN t.initDone := TRUE END;
      t.cg_align := align;
      IF (t.indirect) THEN
        t.cg_align := t.align;
        t.nextTWACGVar := TsWCGVars;  TsWCGVars := t;
        t.bss_var := CG.Declare_global (t.name, t.size, t.cg_align,
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
      t.nextTWACGVar := TsWCGVars;  TsWCGVars := t;
      t.cg_var := CG.Declare_local (t.name, size, align, mtype, typeUID,
                                    t.need_addr, t.up_level, CG.Maybe);

    ELSIF (t.indirect) THEN
      (* formal passed by reference => param is an address *)
      t.cg_align := align;
      t.nextTWACGVar := TsWCGVars;  TsWCGVars := t;
      t.cg_var := CG.Declare_param (t.name, size, align, mtype, typeUID,
                                    t.need_addr, t.up_level, CG.Maybe);

    ELSE
      (* simple parameter *)
      (** align := FindAlignment (align, size); **)
      t.cg_align := align;
      t.nextTWACGVar := TsWCGVars;  TsWCGVars := t;
      t.cg_var := CG.Declare_param (t.name, size, align, mtype, typeUID,
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

(* Externally dispatched-to *)
PROCEDURE ConstInit (t: T) =
  VAR
    initSize : INTEGER;
    initAlign : AlignVal;
    initRepType : Type.T;
    initDepth : INTEGER;
    typeUID       : INTEGER;
    constInitExpr : Expr.T;
    initName      : TEXT;
    initM3ID      : M3ID.T;
    initInfo : Type.Info;
  BEGIN
    IF t.external OR t.imported THEN RETURN END;
    IF NOT t.initStatic AND NOT t.global THEN RETURN END;

    IF t.initStatic AND NOT t.initAllocated THEN
      (* Allocate space in the global constant area for the initial value. *)
      typeUID := Type.GlobalUID (t.tipe);
      constInitExpr := Expr.ConstValue (t.initExpr);
      <* ASSERT constInitExpr # NIL *>
      IF (t.indirect) THEN
        typeUID  := CG.Declare_indirect (typeUID);
        initSize  := Target.Address.size;
        initAlign := Target.Address.align;
        initName := t.qualName & "_INIT_INDIRECT_";
      ELSE
        initRepType := Expr.RepTypeOf (constInitExpr);
        EVAL Type.CheckInfo (initRepType, initInfo);
        initDepth := OpenArrayType.OpenDepth (initRepType);

        IF initDepth > 0 THEN (* initial value is an open array *)
          (* Allocate space for the dope only. *)
          (* See ArrayExpr.GenLiteral, where element space will be allocated. *)
          initSize := Target.Address.pack + initDepth * Target.Integer.pack;
          initAlign := MAX (Target.Address.align, Target.Integer.align);
          initName := t.qualName & "_INIT_DOPE_";
        ELSE
          initSize  := initInfo.size;
          initAlign := initInfo.alignment;
          initName := t.qualName & "_INIT_";
        END;
      END;
      initM3ID := M3ID.Add (initName);
(* TODO: Eliminate duplicate copies of same value, including reused, named constant. *) 
      t.initValOffset
        := Module.Allocate
             (initSize, initAlign, TRUE, "init value for ", initM3ID);
      t.initAllocated := TRUE;
      CG.Declare_global_field (t.name, t.initValOffset, initSize, typeUID, TRUE);
      CG.Comment
        (t.initValOffset, TRUE, "init value for ", initName);
      Expr.PrepLiteral (constInitExpr, initRepType, TRUE);
      Expr.GenLiteral (constInitExpr, t.initValOffset, initRepType, TRUE);
    END;

    IF (t.global) THEN
      (* Try to statically initialize directly in the global variable area. *)
      <*ASSERT t.allocated*>
      constInitExpr := NIL;
      IF (t.initExpr # NIL) AND (NOT t.initDone) AND (NOT t.initStatic) THEN
        constInitExpr := Expr.ConstValue (t.initExpr);
      END;
      IF (constInitExpr # NIL) THEN
        IF NOT Expr.Use (t.initExpr) THEN
         (* NOTE: Modula3 defines this as a checked runtime error, but in a
            global variable, execution of the assignment is inevitable.  Also,
            portions of the runtime system are executed before their module's
            initialization (the only place the compiler could put a runtime
            abort) and depend instead on variables statically initialized.
            So we make this a compile time error.
          *)
          Error.Msg
            ("Variable's initial value contains runtime assignability failure(s).");
        END;
        Expr.PrepLiteral (constInitExpr, t.tipe, FALSE);
        Expr.GenLiteral (constInitExpr, t.offset, t.tipe, FALSE);
        t.initDone := TRUE;
      END;
    END;
  END ConstInit;

(* Externally dispatched-to *)
PROCEDURE NeedInit (t: T): BOOLEAN =
  VAR refType: Type.T;
  BEGIN
    IF (t.imported) OR (t.external) OR (t.initDone) THEN
      RETURN FALSE;
    ELSIF (t.formal # NIL) THEN
      RETURN (t.indirect) AND Formal.OpenArrayByVALUE (t.formal, (*VAR*) refType);
    ELSIF (t.indirect) AND (NOT t.global) THEN
      RETURN FALSE;
    ELSIF (t.global) AND (t.initExpr # NIL) AND (NOT t.initStatic)
      AND (Expr.ConstValue (t.initExpr) # NIL) THEN
      RETURN FALSE;
    ELSIF (t.initExpr # NIL) THEN
      RETURN TRUE;
    ELSE
      RETURN Type.InitCost (t.tipe, FALSE) > 0;
    END;
  END NeedInit;
  
(* Externally dispatched-to *)
PROCEDURE LangInit (t: T) =
  VAR refType: Type.T;
  BEGIN
    IF (t.imported) OR (t.external) THEN
      t.initDone := TRUE;
    ELSIF (t.formal # NIL) THEN
      IF (t.indirect) AND Formal.OpenArrayByVALUE (t.formal, (*VAR*) refType) THEN
        (* a by-value open array! *)
        CG.Gen_location (t.origin);
        Load(t);
        CopyOpenArray (t.tipe, refType);
        (* change the formal parameter to refer to the new storage *)
        CG.Store_addr (t.cg_var);
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
    IF (t.initExpr # NIL) AND (NOT t.up_level) AND (NOT t.imported) THEN
      (* variable has a user specified initExpr value and isn't referenced
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
        IF (t.initExpr = NIL) OR (t.initDone) THEN
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

(* EXPORTED *)
PROCEDURE CopyOpenArray (arrayType: Type.T;  refType: Type.T) =
(* PRE: Pointer to array dope is on TOS. *)
(* Generate code to heap-allocate the copy. *)
(* POST: TOS replaced by pointer to dope of copy. *) 
  VAR
    oldDopePtr, newDopePtr : CG.Val;
    depth := OpenArrayType.OpenDepth (arrayType);
    align := MAX (OpenArrayType.EltAlign (arrayType), Target.Word8.align);
    pack  := OpenArrayType.EltPack (arrayType);
    sizes := CG.Declare_temp (Target.Address.pack + Target.Integer.pack,
                              Target.Address.align, CG.Type.Struct,
                              in_memory := TRUE);
    proc  : Procedure.T;
  BEGIN
    oldDopePtr := CG.Pop (); 
    (* This is confusing.  Build a new 1-D dope vector that treats the shape
       portion of the to-be-copied dope vector as an open array. *) 
    CG.Push(oldDopePtr);
    CG.Add_offset (M3RT.OA_sizes);
    CG.Store_addr (sizes, M3RT.OA_elt_ptr);
    CG.Load_intt (depth);
    CG.Store_int (Target.Integer.cg_type, sizes, M3RT.OA_size_0);
        
    (* allocate the storage *)
    proc := RunTyme.LookUpProc (RunTyme.Hook.NewTracedArray);
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      Type.LoadInfo (refType, -1);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_addr_of (sizes, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
    ELSE
      CG.Load_addr_of (sizes, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
      Type.LoadInfo (refType, -1);
      CG.Pop_param (CG.Type.Addr);
    END;
    newDopePtr := Procedure.EmitValueCall (proc);

    (* load the destination and source elements' addresses *)
    CG.Push (newDopePtr);
    CG.Boost_addr_alignment (Target.Address.align);
    CG.Open_elt_ptr (align); (* Addr of the new elements. *)
    CG.ForceStacked ();
    CG.Push(oldDopePtr);
    CG.Open_elt_ptr (align); (* Addr of the old elements. *)
    CG.ForceStacked ();

    (* compute the number of elements *)
    FOR i := 0 TO depth - 1 DO
      CG.Push(oldDopePtr); 
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (Target.Word.cg_type) END;
    END;

    (* copy the elements into the new storage *)
    CG.Copy_n (pack, overlap := FALSE);

    (* Push new dope pointer for the caller. *) 
    CG.Push (newDopePtr);
    CG.Boost_addr_alignment (Target.Address.align);

    (* free our temps *)
    CG.Free_temp (sizes);
    CG.Free (oldDopePtr);
    CG.Free (newDopePtr); (* It's now safely on the stack, so this is OK. *) 
  END CopyOpenArray;

(* Externally dispatched-to *)
PROCEDURE UserInit (t: T) =
  VAR constInitExpr: Expr.T;
  VAR initRepType: Type.T;
  VAR openEltAlign: INTEGER;
  BEGIN
    IF (t.initExpr # NIL) AND (NOT t.initDone) AND (NOT t.imported) THEN
      CG.Gen_location (t.origin);
      IF (t.initZero) THEN
        t.initPending := FALSE;
        LoadLValue (t);
        Type.Zero (t.tipe);
      ELSIF t.initAllocated THEN
        t.initPending := FALSE;
        IF Expr.Use (t.initExpr) THEN
          LoadLValue (t);
          Module.LoadGlobalAddr
            (Scope.ToUnit (t), t.initValOffset, is_const := TRUE);
          constInitExpr := Expr.ConstValue (t.initExpr);
          <* ASSERT constInitExpr # NIL *>
          initRepType := Expr.RepTypeOf (constInitExpr);
          IF OpenArrayType.Is (initRepType) THEN
            openEltAlign
              := MAX (OpenArrayType.EltAlign(initRepType), Target.Word8.align);
            CG.Open_elt_ptr (openEltAlign);
          END;
          CG.Copy (t.size, overlap := FALSE);
        ELSE (* Expr.Use will have generated an unconditional RT error. *)
        END;
      ELSE
        t.initPending := FALSE;
        ArrayExpr.NoteUseTargetVar (t.initExpr);
        AssignStmt.PrepForEmit (t.tipe, t.initExpr, initializing := TRUE);
        LoadLValue (t);
        AssignStmt.DoEmit (t.tipe, t.initExpr, t.cg_align);
      END;
      t.initDone := TRUE;
      Tracer.Schedule (t.trace);
    END;
  END UserInit;

(* EXPORTED *)
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

(* EXPORTED *)
PROCEDURE NeedGlobalInit (t: T): BOOLEAN =
  BEGIN
    RETURN (NOT t.initDone) AND (NOT t.external);
  END NeedGlobalInit;

(* EXPORTED *)
PROCEDURE InitGlobal (t: T) =
  BEGIN
    IF (NOT t.initDone) AND (NOT t.external) THEN
      LoadLValue (t);
      Type.InitValue (t.tipe, TRUE);
    END;
  END InitGlobal;

(* Externally dispatched-to *)
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

(* EXPORTED *)
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

(* EXPORTED *)
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

(* EXPORTED *)
PROCEDURE CheckTrace (tt: Tracer.T;  VAR cs: Value.CheckState) =
  VAR x: TraceNode := tt;
  BEGIN
    IF (x # NIL) THEN
      Expr.TypeCheck (x.handler, cs);
      Expr.TypeCheck (x.call, cs);
    END;
  END CheckTrace;

(* EXPORTED *)
PROCEDURE ScheduleTrace (t: T) =
  BEGIN
    Tracer.Schedule (t.trace);
  END ScheduleTrace;

BEGIN
END Variable.
