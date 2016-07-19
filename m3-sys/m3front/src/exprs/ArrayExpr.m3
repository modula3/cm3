(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ArrayExpr.m3                                          *)
(* Last modified on Tue Jun 20 15:46:08 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 12:45:06 PDT 1995 by ericv      *)
(*      modified on Tue Mar 12 00:29:44 1991 by muller         *)

MODULE ArrayExpr;

IMPORT M3, M3ID, CG, Expr, ExprRep, Error, Type, ArrayType;
IMPORT KeywordExpr, RangeExpr, Int, OpenArrayType, Module;
IMPORT IntegerExpr, EnumExpr, SubrangeType, Target, TInt, M3Buf;
IMPORT AssignStmt, RefType, M3RT, Procedure, RunTyme, ErrType;

TYPE
  Kind = {
    Fixed,     (* ARRAY I OF T { ... } *)
    EmptyOpen, (* ARRAY OF T { } *)
    FixedOpen, (* ARRAY OF T { ... with fixed shaped element ... } *)
    Open       (* ARRAY OF T { ... no compile-time fixed elements ... } *)
  };

TYPE
  P = Expr.T BRANDED "ArrayExpr.P" OBJECT
        tipe      : Type.T;
(* CLEANUP ^ tipe always duplicates field "type", inherited from Expr.T *) 
        args      : Expr.List;
        dots      : BOOLEAN;
        folded    : BOOLEAN;
        is_const  : BOOLEAN;
        kind      : Kind;
        index     : Type.T;
        refType   : Type.T; (* Kind.Open *)
        solidType : Type.T; (* Kind.FixedOpen *)
        offset    : INTEGER;
        tmp       : CG.Val;
        tmp_cnt   : INTEGER;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := IsZeroes;
        genFPLiteral := GenFPLiteral;
        prepLiteral  := PrepLiteral;
        genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (type: Type.T;  args: Expr.List;  dots: BOOLEAN): Expr.T =
  VAR p := NEW (P);  index, element: Type.T;
  BEGIN
    ExprRep.Init (p);
    IF  NOT ArrayType.Split (type, index, element) THEN
      Error.Msg ("expecting array type on array constructor");
      index := NIL;
    END;
    p.type      := type;
    p.tipe      := type;
    p.index     := index;
    p.args      := args;
    p.dots      := dots;
    p.folded    := FALSE;
    p.is_const  := FALSE;
    p.refType   := NIL;
    p.solidType := NIL;
    p.offset    := 0;
    p.tmp       := NIL;
    p.tmp_cnt   := 0;
    p.direct_ok := TRUE;
    RETURN p;
  END New;

PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    RETURN (TYPECODE (e) = TYPECODE (P));
  END Is;

PROCEDURE Subscript (array, index: Expr.T;  VAR e: Expr.T): BOOLEAN =
  VAR p: P;  i, n: INTEGER;  int, min, max, offs: Target.Int;  t: Type.T;
      b: BOOLEAN;
  BEGIN
    TYPECASE array OF
    | NULL => RETURN FALSE;
    | P(x) => p := x;
    ELSE      RETURN FALSE;
    END;
    index := Expr.ConstValue (index);
    IF (NOT IntegerExpr.Split (index, int, t))
      AND (NOT EnumExpr.Split (index, int, t)) THEN
      RETURN FALSE;
    END;
    IF p.index = NIL THEN
      min := TInt.Zero (* FIRST (p.args^) *);
      b := TInt.FromInt (LAST (p.args^), max);  <* ASSERT b *>
    ELSE
      EVAL Type.GetBounds (p.index, min, max);
    END;
    
    (* correct for the base index of the array *)
    IF NOT TInt.Subtract (int, min, offs) THEN RETURN FALSE END;
    IF TInt.LT (offs, TInt.Zero) THEN RETURN FALSE END;
    b := TInt.ToInt (offs, i); <* ASSERT b *>

    n := LAST (p.args^);
    IF (i <= n) THEN e := p.args[i]; RETURN TRUE END;
    IF (p.dots) THEN e := p.args[n]; RETURN TRUE END;
    RETURN FALSE;
  END Subscript;

PROCEDURE GetBounds (array: Expr.T;  VAR min, max: Target.Int): BOOLEAN =
  VAR b: BOOLEAN;
  BEGIN
    TYPECASE array OF 
    | NULL => RETURN FALSE;
    | P(p) =>
      IF p.index = NIL THEN
        (* open array type *)
        min := TInt.Zero (* FIRST (p.args^) *);
        b := TInt.FromInt (LAST (p.args^), max);  <*ASSERT b*>
        RETURN TRUE;
      ELSE
        RETURN Type.GetBounds (p.index, min, max);
      END;
    ELSE
      RETURN FALSE;
    END;
  END GetBounds;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR
    nn: Target.Int;
    n: INTEGER;
    e, value, minE, maxE: Expr.T;
    index, element, solidElt, elt: Type.T;
    key: M3ID.T;
    elt_info: Type.Info;
  BEGIN
    IF (p.index # NIL) THEN
      p.index := Type.Check (p.index);
    END;

    p.tipe := Type.Check (p.tipe);
    p.type := p.tipe;
    WITH b = ArrayType.Split (p.tipe, index, element) DO <* ASSERT b *> END;
    element := Type.CheckInfo (element, elt_info);

    IF (index # ErrType.T) THEN
      nn := Type.Number (index);
      IF NOT TInt.ToInt (nn, n) THEN
        Error.Msg 
          ("Compiler limit: array constructor type has too many elements.");
      END;
      IF (index # NIL) THEN
        IF n < NUMBER (p.args^) THEN
          Error.Msg ("too many values specified in fixed array constructor.");
        ELSIF n > NUMBER (p.args^) AND NOT p.dots THEN
          Error.Msg ("not enough values specified in fixed array constructor.");
        END;
      ELSIF (p.dots) THEN
        Error.Warn (1, "\"..\" ignored in open array constructor");
(* FIXME ^ The language seems to say this is illegal, not a warning. *) 
      END;
    END;

    FOR i := 0 TO LAST (p.args^) DO
      e := p.args[i];
      Expr.TypeCheck (e, cs);
      IF KeywordExpr.Split (e, key, value) THEN
        Error.Msg ("keyword values not allowed in array constructors");
        e := value;
      END;
      IF RangeExpr.Split (e, minE, maxE) THEN
        Error.Msg ("range values not allowed in array constructors");
        e := value;
      END;

      IF NOT Type.IsAssignable (element, Expr.TypeOf (e)) THEN
        Error.Msg ("expression is not assignable to array element");
      ELSE
        AssignStmt.Check (element, e, cs);
      END;
    END;


    IF (index # NIL) OR (index = ErrType.T) THEN
      p.kind := Kind.Fixed;
    ELSIF (NUMBER (p.args^) = 0) THEN
      p.kind := Kind.EmptyOpen;
    ELSE
      (* we're producing an open array => try to find a fixed shape element *)
      solidElt := NIL;
      IF elt_info.size > 0 THEN
        solidElt := element;
      ELSE
        FOR i := 0 TO LAST (p.args^) DO
          elt := Expr.TypeOf (p.args[i]);
          elt := Type.CheckInfo (elt, elt_info);
          IF (elt_info.size > 0) OR FixedArray (p.args[i], elt) THEN
            (* we found one! *)
            solidElt := elt;
            EXIT;
          END;
        END;
      END;
 
      IF (solidElt # NIL)
        AND TInt.FromInt (LAST (p.args^), nn)
        AND NOT TInt.LT (nn, Target.Integer.min)
        AND NOT TInt.LT (Target.Integer.max, nn) THEN
        p.kind := Kind.FixedOpen;
        index := SubrangeType.New (TInt.Zero, nn, Int.T, FALSE);
        p.solidType := ArrayType.New (index, solidElt);
        p.solidType := Type.CheckInfo (p.solidType, elt_info);
        element := solidElt;
      ELSE
        (* we can't determine the shape until runtime *)
        p.kind := Kind.Open;
        p.refType := RefType.New (p.tipe, traced := TRUE, brand := NIL);
        p.refType := Type.CheckInfo (p.refType, elt_info);
      END;
    END;
  END Check;

PROCEDURE FixedArray (e: Expr.T;  VAR solidType: Type.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => solidType := p.solidType; RETURN (solidType # NIL);
    ELSE      RETURN FixedArray (Expr.ConstValue (e), solidType);
    END;
  END FixedArray;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  VAR b: P;
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => b := p;
    ELSE      RETURN FALSE;
    END;
    IF   (NOT Type.IsEqual (a.tipe, b.tipe, x))
      OR (a.dots # b.dots)
      OR ((a.args = NIL) # (b.args = NIL))
      OR ((a.args # NIL) AND (NUMBER (a.args^) # NUMBER (b.args^))) THEN
      RETURN FALSE;
    END;
    FOR i := 0 TO LAST (a.args^) DO
      IF NOT Expr.IsEqual (a.args[i], b.args[i], x) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END EqCheck;

PROCEDURE NeedsAddress (<*UNUSED*> p: P) =
  BEGIN
    (* ya, so... *)
  END NeedsAddress;

PROCEDURE Compile (p: P) =
  VAR size, align, depth, offset: INTEGER;  info: Type.Info;
  BEGIN
    (* The bulk of the work was done by Prep *)
    IF (p.is_const) THEN
      EVAL Type.CheckInfo (p.tipe, info);
      size  := info.size;
      align := info.alignment;
      depth := OpenArrayType.OpenDepth (p.tipe);

      IF (depth > 0) THEN
        (* p.tipe is an open array *)
        size := Target.Address.pack + depth * Target.Integer.pack;
        align := MAX (Target.Address.align, Target.Integer.align);
      END;

      offset := Module.Allocate (size, align, TRUE, "*array*");
      PrepLiteral (p, p.tipe, TRUE);
      GenLiteral (p, offset, p.tipe, TRUE);
      CG.Load_addr_of (Module.GlobalData (TRUE), offset, align);
    ELSE
      CG.Push (p.tmp);
      DEC (p.tmp_cnt);
      IF (p.tmp_cnt <= 0) THEN
        CG.Free (p.tmp);
        p.tmp := NIL;
      END;
    END;
  END Compile;

PROCEDURE Prep (p: P) =
  VAR index, element: Type.T;  elt_info: Type.Info;  elt_pack: INTEGER;
  BEGIN
    Type.Compile (p.tipe);
    Type.Compile (p.refType);
    Type.Compile (p.solidType);

    IF (Fold (p) # NIL) THEN RETURN END;

    INC (p.tmp_cnt);
    IF (p.tmp # NIL) AND (p.tmp_cnt > 1) THEN RETURN END;

    index := p.tipe;  IF (p.solidType # NIL) THEN index := p.solidType END;
    WITH b = ArrayType.Split (index, index, element) DO <* ASSERT b *> END;
    element := Type.CheckInfo (element, elt_info);
    elt_pack  := (elt_info.size + elt_info.alignment - 1)
                     DIV elt_info.alignment * elt_info.alignment;

    CASE p.kind OF
    | Kind.Fixed     => DoFixed (p, element, elt_pack);
    | Kind.EmptyOpen => DoEmpty (p);
    | Kind.FixedOpen => DoFixedOpen (p, elt_pack);
    | Kind.Open      => DoOpen (p, elt_pack, elt_info.alignment);
    END;
  END Prep;

PROCEDURE DoFixed (p: P;  element: Type.T;  elt_pack: INTEGER) =
  VAR
    t1: CG.Var;
    t2: CG.Val;
    top: CG.Label;
    nn_args : Target.Int;
    n_args := NUMBER (p.args^);
    nn_elts := Type.Number (p.index);
    n_elts : INTEGER;
    b: BOOLEAN;
    info: Type.Info;
    align: INTEGER;
  BEGIN
    EVAL Type.CheckInfo (p.tipe, info);
    align := info.alignment;
    b := TInt.ToInt (nn_elts, n_elts);    <*ASSERT b*>
    b := TInt.FromInt (n_args, nn_args);  <*ASSERT b*>

    (* If this is a direct structure assignment, the LHS has already
     * been prepped and compiled -- save it.
     *)
    IF p.do_direct THEN
      p.tmp := CG.Pop ();
    ELSE
      t1 := CG.Declare_temp (info.size, align, CG.Type.Struct,
                             in_memory:= TRUE);
    END;

    (* assign the given elements *)
    FOR i := 0 TO n_args-1 DO
      AssignStmt.PrepForEmit (element, p.args[i], initializing := TRUE);
      PushAddr (p, t1, i * elt_pack, align);
      AssignStmt.DoEmit (element, p.args[i]);
    END;

    (* fill in the '..' section *)
    IF (p.dots) AND (n_elts > n_args) THEN
      CG.Load_integer (Target.Integer.cg_type, nn_args);
      t2 := CG.Pop_temp ();
      top := CG.Next_label ();
      CG.Set_label (top);

      (* ARRAY[t2] := ARRAY[n_args-1] *)
      PushAddr (p, t1, 0, align);
      CG.Push (t2);
      ArrayType.GenIndex (p.tipe);
      PushAddr (p, t1, (n_args-1) * elt_pack, align);
      IF ArrayType.IsBitAddressed (p.tipe) THEN
        CG.Load_indirect (Target.Integer.cg_type, 0, elt_pack);
        CG.Store_indirect (Target.Integer.cg_type, 0, elt_pack);
      ELSE
        CG.Copy (elt_pack, overlap := FALSE);
      END;

      (* t2 := t2 + 1 *)
      CG.Push (t2);
      CG.Load_integer (Target.Integer.cg_type, TInt.One);
      CG.Add (Target.Integer.cg_type);
      CG.Store_temp (t2);

      (* IF (t2 < NUMBER(ARRAY) GOTO TOP-OF-LOOP *)
      CG.Push (t2);
      CG.Load_integer (Target.Integer.cg_type, nn_elts);
      CG.If_compare (Target.Integer.cg_type, CG.Cmp.LT, top, CG.Likely);

      CG.Free (t2);
    END;

    (* remember the result *)
    IF p.do_direct THEN
      (* result is already in p.tmp *)
    ELSE
      CG.Load_addr_of_temp (t1, 0, info.alignment);
      p.tmp := CG.Pop ();
    END;
  END DoFixed;
  
PROCEDURE PushAddr (p: P; tmp: CG.Var; offset: INTEGER; align: CG.Alignment) =
  BEGIN
    IF p.do_direct THEN
      CG.Push (p.tmp);
      IF offset # 0 THEN  CG.Add_offset (offset);  END;
    ELSE
      CG.Load_addr_of (tmp, offset, align);
    END;
  END PushAddr;

PROCEDURE DoEmpty (p: P) =
  VAR t1: CG.Var;
  BEGIN
    t1 := CG.Declare_temp (Target.Address.pack + Target.Integer.pack,
                           Target.Address.align,  CG.Type.Struct,
                           in_memory := TRUE);
    CG.Load_nil ();
    CG.Store_addr (t1, M3RT.OA_elt_ptr);
    CG.Load_integer (Target.Integer.cg_type, TInt.Zero);
    CG.Store_int (Target.Integer.cg_type, t1, M3RT.OA_size_0);

    (* remember the result *)
    CG.Load_addr_of_temp (t1, 0, Target.Address.align);
    p.tmp := CG.Pop ();
  END DoEmpty;

PROCEDURE DoFixedOpen (p: P;  elt_pack: INTEGER) =
  VAR
    t1         : CG.Var;
    index, element: Type.T; 
    offset     : INTEGER;
    n_args     := NUMBER (p.args^);
    openDepth  := OpenArrayType.OpenDepth (p.type);
    align      : INTEGER;
    info       : Type.Info;
    dope_size  : INTEGER;
    data_offs  : INTEGER;
    dope_align : INTEGER;
    total_size : INTEGER;
  BEGIN
    EVAL Type.CheckInfo (p.solidType, info);
    align := info.alignment;

    (* allocate space for the result *)
    dope_align := MAX (Target.Address.align, align);
    dope_size  := Target.Address.pack + openDepth * Target.Integer.pack;
    data_offs  := (dope_size + align - 1) DIV align * align;
    total_size := data_offs + info.size;
    t1 := CG.Declare_temp (total_size, dope_align, CG.Type.Struct,
                           in_memory := TRUE);

    (* initialize the element pointer *)
    CG.Load_addr_of (t1, data_offs, align);
    CG.Store_addr (t1, M3RT.OA_elt_ptr);

    (* initialize size[0] of the dope vector *)
    CG.Load_intt (n_args);
    CG.Store_int (Target.Integer.cg_type, t1, M3RT.OA_size_0);

    (* initialize the remaining sizes of the dope vector *)
    EVAL ArrayType.Split (p.solidType, index, element);
    offset := M3RT.OA_size_0 + Target.Integer.pack;
    FOR i := 1 TO openDepth-1 DO
      EVAL ArrayType.Split (element, index, element);
      <*ASSERT index # NIL*>
      CG.Load_integer (Target.Integer.cg_type, Type.Number (index));
      CG.Store_int (Target.Integer.cg_type, t1, offset);
      INC (offset, Target.Integer.pack);
    END;

    (* fill with the elements *)
    EVAL ArrayType.Split (p.solidType, index, element);
    FOR i := 0 TO n_args-1 DO
      AssignStmt.PrepForEmit (element, p.args[i], initializing := TRUE);
      CG.Load_addr_of (t1, data_offs + i * elt_pack, align);
      AssignStmt.DoEmit (element, p.args[i]);
    END;

    (* remember the result *)
    CG.Load_addr_of_temp (t1, 0, dope_align);
    p.tmp := CG.Pop ();
  END DoFixedOpen;

PROCEDURE DoOpen (p: P;  elt_pack, elt_align: INTEGER) =
  VAR
    t1: CG.Var;
    t2, t3, t4: CG.Val;
    index, element: Type.T; 
    offset    : INTEGER;
    n_args    := NUMBER (p.args^);
    openDepth := OpenArrayType.OpenDepth (p.type);
    size      : INTEGER;
    proc      := RunTyme.LookUpProc (RunTyme.Hook.NewTracedArray);
  BEGIN
    <*ASSERT elt_align MOD Target.Byte = 0*>

    (* evaluate the first argument to fix the runtime shape *)
    Expr.Prep (p.args[0]);
    Expr.Compile (p.args[0]);
    t2 := CG.Pop ();

    (* allocate the temporary "sizes" array  *)
    size := Target.Address.pack + (1 + openDepth) * Target.Integer.pack;
    t1 := CG.Declare_temp (size, Target.Address.align, CG.Type.Struct,
                           in_memory := TRUE);

    (* initialize the sizes array *)
    CG.Load_addr_of (t1, M3RT.OA_size_1, Target.Address.align);
    CG.Store_addr (t1, M3RT.OA_elt_ptr);

    CG.Load_intt (openDepth);
    CG.Store_int (Target.Integer.cg_type, t1, M3RT.OA_size_0);

    CG.Load_intt (n_args);
    CG.Store_int (Target.Integer.cg_type, t1, M3RT.OA_size_1);

    element := Expr.TypeOf (p.args[0]);
    offset := M3RT.OA_size_1 + Target.Integer.pack;
    FOR i := 1 TO openDepth-1 DO
      EVAL ArrayType.Split (element, index, element);
      IF (index = NIL) THEN
        CG.Push (t2);
        CG.Open_size (i - 1);
      ELSE
        CG.Load_integer (Target.Integer.cg_type, Type.Number (index));
      END;
      CG.Store_int (Target.Integer.cg_type, t1, offset);
      INC (offset, Target.Integer.pack);
    END;

    (* allocate space for the value *)
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      Type.LoadInfo (p.refType, -1);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_addr_of (t1, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
    ELSE
      CG.Load_addr_of (t1, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
      Type.LoadInfo (p.refType, -1);
      CG.Pop_param (CG.Type.Addr);
    END;
    t3 := Procedure.EmitValueCall (proc);

    (* repack the "sizes" array as a dope vector for each array element *)
    CG.Push (t3);
    CG.Boost_alignment (elt_align);
    CG.Open_elt_ptr (Target.Integer.align);
    CG.Store_addr (t1, M3RT.OA_elt_ptr);
    FOR i := 0 TO openDepth-2 DO
      CG.Load_int (Target.Integer.cg_type,
                   t1, M3RT.OA_sizes + (i + 2) * Target.Integer.pack);
      CG.Store_int (Target.Integer.cg_type,
                    t1, M3RT.OA_sizes + i * Target.Integer.pack);
    END;

    (* compute the size of each element in "elt_pack" units *)
    FOR i := 0 TO openDepth-2 DO
      CG.Load_int (Target.Integer.cg_type,
                   t1, M3RT.OA_sizes + i * Target.Integer.pack);
      IF (i # 0) THEN CG.Multiply (Target.Integer.cg_type) END;
    END;
    t4 := CG.Pop ();

    (* copy the first element into place *)
    CG.Load_addr (t1, M3RT.OA_elt_ptr);
    CG.Force ();
    CG.Push (t2);
    CG.Force ();
    CG.Push (t4);
    CG.Copy_n (elt_pack, overlap := FALSE);

    (* fill in the remaining elements *)
    EVAL ArrayType.Split (p.tipe, index, element);
    FOR i := 1 TO n_args-1 DO
      (* bump the pointer to the next element *)
      CG.Load_addr (t1, M3RT.OA_elt_ptr);
      CG.Push (t4); (* == element size *)
      CG.Index_bytes (elt_pack);
      CG.Store_addr (t1, M3RT.OA_elt_ptr);

      (* do the assignment *)
      AssignStmt.PrepForEmit (element, p.args[i], initializing := TRUE);
      CG.Load_addr (t1);
      AssignStmt.DoEmit (element, p.args[i]);
    END;

    (* remember the result and free the other temporaries *)
    p.tmp := t3;

    CG.Free_temp (t1);
    CG.Free (t2);
    CG.Free (t4);
  END DoOpen;

PROCEDURE Fold (p: P): Expr.T =
  VAR e: Expr.T;
  BEGIN
    IF (NOT p.folded) THEN
      p.folded   := TRUE;
      p.is_const := TRUE;
      FOR i := 0 TO LAST (p.args^) DO
        e := Expr.ConstValue (p.args[i]);
        IF (e = NIL) THEN p.is_const := FALSE; ELSE p.args[i] := e; END;
      END;
    END;
    IF p.is_const
      THEN RETURN p;
      ELSE RETURN NIL;
    END;
  END Fold;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    FOR i := 0 TO LAST (p.args^) DO
      IF NOT Expr.IsZeroes (p.args[i]) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  VAR n_elts, n_args: INTEGER;  b: BOOLEAN;  e: Expr.T;
  BEGIN
    n_args := NUMBER (p.args^);
    M3Buf.PutText (buf, "ARRAY<");
    FOR i := 0 TO n_args-1 DO
      IF (i > 0) THEN M3Buf.PutChar (buf, ',') END;
      Expr.GenFPLiteral (p.args[i], buf);
    END;
    IF (p.kind = Kind.Fixed) AND (p.dots) THEN
      b := TInt.ToInt (Type.Number (p.index), n_elts);    <*ASSERT b*>
      IF (n_elts > n_args) THEN
        e := p.args [n_args-1];
        e := Expr.ConstValue (e);
        FOR i := n_args TO n_elts DO
          M3Buf.PutChar (buf, ',');
          Expr.GenFPLiteral (e, buf);
        END;
      END;
    END;
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

PROCEDURE PrepLiteral (p: P; type: Type.T; is_const: BOOLEAN) =
  VAR info: Type.Info;
  BEGIN
    IF (p.kind = Kind.Fixed) OR (p.kind = Kind.EmptyOpen) THEN RETURN END;

    <*ASSERT (p.kind = Kind.FixedOpen) AND (p.solidType # NIL) *>
    (* else p.kind = Kind.Open => not a runtime constant! *)

    type := Type.CheckInfo(type,info);
    IF info.class = Type.Class.Array (*=>Fixed array*) THEN
      (* GenLiteral will treat this as fixed array type.  No prep needed. *) 
      RETURN
    END; 

    PrepElements (p, p.tipe, is_const);

    IF (p.offset = 0) THEN
      EVAL Type.CheckInfo (p.solidType, info);
      p.offset := Module.Allocate (info.size, info.alignment, is_const,
                                   "*open array literal*");
      CG.Declare_global_field (M3ID.Add ("_array"), p.offset, info.size,
                               Type.GlobalUID(p.solidType), is_const);
      EVAL GenOpenLiteral (p, p.offset,
                           OpenArrayType.OpenDepth (p.tipe),
                           OpenArrayType.OpenType (p.tipe),
                           OpenArrayType.EltPack (p.type),
                           is_const);
    END;
  END PrepLiteral;

PROCEDURE PrepElements (e: Expr.T;  type: Type.T;  is_const: BOOLEAN) =
  VAR index, element: Type.T;  b: BOOLEAN;
  BEGIN
    TYPECASE e OF
    | NULL => (* skip *)
    | P(p) => b := ArrayType.Split (type, index, element); <* ASSERT b *>
              FOR i := FIRST (p.args^) TO LAST (p.args^) DO
                PrepElements (p.args[i], element, is_const);
              END;
    ELSE      Expr.PrepLiteral (e, type, is_const);
    END;
  END PrepElements;

PROCEDURE GenOpenLiteral (e: Expr.T;  offset: INTEGER;  depth: INTEGER;
                          elt_type: Type.T;  elt_pack: INTEGER;
                          is_const: BOOLEAN): INTEGER =
  VAR size, align, start: INTEGER;  info: Type.Info;  t: Type.T;
  BEGIN
    IF (depth <= 0) THEN
      (* we're out of the open-array morass *)
      Expr.GenLiteral (e, offset, elt_type, is_const);
      RETURN elt_pack;
    END;

    TYPECASE e OF
    | NULL => RETURN 0;
    | P(p) => start := offset;
              align := ArrayType.EltAlign (p.tipe);
              FOR i := FIRST (p.args^) TO LAST (p.args^) DO
                size := GenOpenLiteral (p.args[i], offset, depth-1,
                                        elt_type, elt_pack, is_const);
                INC (offset, (size + align - 1) DIV align * align);
              END;
              RETURN offset - start;
    ELSE      Error.Msg ("*** INTERNAL ERROR: missing open array expr? ***");
              t := Type.CheckInfo (Expr.TypeOf (e), info);
              Expr.GenLiteral (e, offset, t, is_const);
              RETURN info.size;
    END;
  END GenOpenLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER; type: Type.T;
                      is_const: BOOLEAN) =
  VAR index, element: Type.T;  last, n_elts, elt_size: INTEGER;  
      info: Type.Info; 
      b: BOOLEAN;
      LKind: Kind; 
  BEGIN
    type := Type.CheckInfo(type,info);
    IF p.kind = Kind.FixedOpen AND info.class = Type.Class.Array THEN
      (* The array constructor has an open array type, but the variable to be 
         initialized has a fixed array type.  Don't generate any open array
         dope.  Instead, trick the code below into treating the array
         constructor as if had the variable's fixed array type. *) 
      LKind := Kind.Fixed; 
      <*ASSERT p.solidType # NIL *>
      p.solidType := Type.CheckInfo(p.solidType,info);
      b := ArrayType.Split (p.solidType, index, element); <* ASSERT b *>
    ELSE 
      LKind := p.kind; 
      b := ArrayType.Split (p.tipe, index, element); <* ASSERT b *>
    END; 

    (* BUG!!  if p.tipe # type then we're generating an open literal
           for a fixed size expr or vice versa.... *)

    CASE LKind OF
    | Kind.EmptyOpen =>
        GenOpenDim (p, OpenArrayType.OpenDepth (p.tipe),
                    offset + M3RT.OA_sizes, is_const);

    | Kind.FixedOpen =>
        CG.Init_var (offset + M3RT.OA_elt_ptr, Module.GlobalData (is_const),
                      p.offset, is_const);
        GenOpenDim (p, OpenArrayType.OpenDepth (p.tipe),
                    offset + M3RT.OA_sizes, is_const);

    | Kind.Open =>
        <* ASSERT FALSE *>  (* not a compile-time constant *)

    | Kind.Fixed =>
        (* find the last non-zero element *)
        last := LAST (p.args^);
        WHILE (last > 0) AND Expr.IsZeroes (p.args[last]) DO DEC (last) END;
 
        IF (NUMBER (p.args^) > 0) THEN
          elt_size := ArrayType.EltPack (p.tipe);
          FOR i := 0 TO last DO
            Expr.GenLiteral (p.args[i], offset, element, is_const);
            INC (offset, elt_size);
          END;
          IF (p.dots) AND (last = LAST (p.args^)) THEN
            b := TInt.ToInt (Type.Number (index), n_elts); <*ASSERT b*>
            FOR i := last+1 TO n_elts-1 DO
              Expr.GenLiteral (p.args[last], offset, element, is_const);
              INC (offset, elt_size);
            END;
          END;
        END;
    END;
  END GenLiteral;

PROCEDURE GenOpenDim (e: Expr.T;  depth: INTEGER;  offset: INTEGER;  is_const: BOOLEAN) =
  VAR n_elts: INTEGER;
  BEGIN
    WHILE (depth > 0) DO
      TYPECASE e OF
      | NULL => n_elts := 0;
      | P(p) => n_elts := NUMBER (p.args^);
                IF (n_elts # 0)
                  THEN e := p.args[0];
                  ELSE e := NIL;
                END;
(* Hmm. The above IF stmt appears to be unnecessary, as e is unused hereafter. *)  
      ELSE      n_elts := 0;
      END;
      CG.Init_intt (offset, Target.Integer.size, n_elts, is_const);
      INC (offset, Target.Integer.pack);
      DEC (depth);
    END;
  END GenOpenDim;

BEGIN
END ArrayExpr.
