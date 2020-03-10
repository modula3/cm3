(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CastExpr.m3                                           *)
(* Last Modified On Tue May 23 15:33:47 PDT 1995 By kalsow     *)
(*      Modified On Sun Dec 23 08:07:22 1990 By muller         *)

MODULE CastExpr;

IMPORT M3Buf, CG, Expr, ExprRep, Type, Error, OpenArrayType;
IMPORT M3, M3ID, M3RT, Target, TInt;
FROM Target IMPORT FloatType;

TYPE
  Kind = {
    Noop,    (* code generator cannot tell the difference *)
    D_to_A,  (* designator -> open array *)
    S_to_A,  (* structure -> open array *)
    V_to_A,  (* value -> open array *)
    D_to_S,  (* designator -> structure *)
    S_to_S,  (* structure -> structure *)
    V_to_S,  (* value -> structure *)
    D_to_V,  (* designator -> value *)
    S_to_V,  (* structure -> value *)
    V_to_V   (* value -> value *)
  };

TYPE
  P = Expr.T BRANDED "CastExpr" OBJECT
        kind    : Kind;
        expr    : Expr.T;
        tipe    : Type.T;
        tmp     : CG.Var;
        tmp_cnt : INTEGER;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := PrepLV;
        compileLV    := CompileLV;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := Bounder;
        isWritable   := IsWritable;
        isDesignator := IsDesignator;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := GenFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
        exprAlign    := CastExprAlign;
      END;

PROCEDURE New (a: Expr.T;  t: Type.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.origin    := a.origin;
    p.expr      := a;
    p.tipe      := t;
    p.type      := t;
    p.repType   := t;
    p.tmp       := NIL;
    p.tmp_cnt   := 0;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR
    src, dest, elt: Type.T;  sz0, sz1: INTEGER;
    array_out, desig_in, struct_in, struct_out: BOOLEAN;
    align_in, align_out: INTEGER;
    dest_info, src_info, elt_info: Type.Info;
  BEGIN
    Expr.TypeCheck (p.expr, cs);
    p.tipe := Type.CheckInfo (p.tipe, dest_info);

    src        := Type.CheckInfo (Expr.TypeOf (p.expr), src_info);
    dest       := p.tipe;
    desig_in   := Expr.IsDesignator (p.expr);
    struct_in  := Type.IsStructured (src);
    struct_out := Type.IsStructured (dest);
    array_out  := OpenArrayType.Split (dest, elt);
    align_in   := src_info.alignment;
    align_out  := dest_info.alignment;

    (* check to see that the value is legal *)
    IF (src_info.class = Type.Class.OpenArray) THEN
      Error.Msg ("LOOPHOLE: first argument cannot be an open array");
    END;
    sz0 := src_info.size;

    (* check to see that the destination type is legal *)
    IF array_out THEN
      (* open array type *)
      elt := Type.CheckInfo (elt, elt_info);
      IF (elt_info.class = Type.Class.OpenArray) THEN
        Error.Msg ("LOOPHOLE: multidimensional open arrays not supported");
      END;
      sz1 := elt_info.size;
      IF (sz1 <= 0) OR ((sz0 MOD sz1) # 0) THEN
        Error.Msg ("LOOPHOLE: expression's size incompatible with type's");
      END;
      align_out := elt_info.alignment;
    ELSE
      (* fixed size type *)
      sz1 := dest_info.size;
      IF (sz0 # sz1) THEN
        Error.Msg ("LOOPHOLE: expression's size differs from type's");
      END;
    END;

    (* check for alignment problems *)
    IF (align_in < align_out) OR (align_in MOD align_out # 0) THEN
      Error.Warn(1,"LOOPHOLE: expression's alignment may differ from type's");
    END;

    (* classify the type of LOOPHOLE operation *)

    IF array_out THEN
      IF desig_in THEN
        p.kind := Kind.D_to_A;
      ELSIF struct_in THEN
        p.kind := Kind.S_to_A;
      ELSE
        p.kind := Kind.V_to_A;
      END;
    ELSIF (src_info.stk_type = dest_info.stk_type) THEN
      p.kind := Kind.Noop;
    ELSIF struct_out THEN
      IF desig_in THEN
        p.kind := Kind.D_to_S;
      ELSIF struct_in THEN
        p.kind := Kind.S_to_S;
      ELSE
        p.kind := Kind.V_to_S;
      END;
    ELSIF struct_in THEN
      p.kind := Kind.S_to_V;
    ELSIF desig_in THEN
      p.kind := Kind.D_to_V;
    ELSE
      p.kind := Kind.V_to_V;
    END;

    IF (p.kind = Kind.D_to_A) OR (p.kind = Kind.D_to_S) THEN
      (* we're going to take the address of this value *)
      Expr.NeedsAddress (p.expr);
    END;
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN Type.IsEqual (a.tipe, b.tipe, x)
                 AND Expr.IsEqual (a.expr, b.expr, x);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  BEGIN
    CASE p.kind OF
    | Kind.Noop,
      Kind.D_to_S,
      Kind.S_to_S,
      Kind.D_to_V,
      Kind.S_to_V,
      Kind.V_to_V =>
        Expr.NeedsAddress (p.expr);

    | Kind.D_to_A,
      Kind.S_to_A,
      Kind.V_to_A,
      Kind.V_to_S =>
        (* ok, because we build a temporary *)
    END;
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  VAR
    e  := p.expr;
    u  := Expr.TypeOf (e);
    t  := p.tipe;
    t1 : CG.Var;
    sz, t_align, u_align, z_align: INTEGER;
    t_cg, u_cg: CG.Type;
    u_info, t_info: Type.Info;
  BEGIN
    IF (p.tmp_cnt > 0) THEN  INC (p.tmp_cnt);  RETURN;  END;
    u := Type.CheckInfo (u, u_info);
    t := Type.CheckInfo (t, t_info);
    t_cg := t_info.stk_type;  t_align := t_info.alignment;
    u_cg := u_info.stk_type;  u_align := u_info.alignment;
    sz := u_info.size;
    Type.Compile (t);
    Type.Compile (u);
    z_align := MAX (t_align, u_align);

    CASE p.kind OF
    | Kind.Noop =>
        Expr.Prep (e);
    | Kind.D_to_A =>
        INC (p.tmp_cnt);
        Expr.PrepLValue (e, traced := FALSE);
        Expr.CompileAddress (e, traced := FALSE);
        p.tmp := BuildArray (p, sz);
    | Kind.S_to_A =>
        INC (p.tmp_cnt);
        Expr.Prep (e);
        Expr.Compile (e);
        p.tmp := BuildArray (p, sz);
    | Kind.V_to_A =>
        (* copy the value to a temporary *)
        INC (p.tmp_cnt);
        Expr.Prep (e);
        t1 := CG.Declare_local (M3ID.NoID, sz, z_align, u_cg,
                                Type.GlobalUID (u),
                                in_memory := TRUE, up_level := FALSE,
                                f := CG.Never);
        Expr.Compile (e);
        CG.Store (t1, 0, sz, z_align, u_cg);
        CG.Load_addr_of (t1, 0, z_align);
        p.tmp := BuildArray (p, sz);
    | Kind.D_to_S =>
        Expr.PrepLValue (e, traced := FALSE);
    | Kind.S_to_S =>
        Expr.Prep (e);
    | Kind.V_to_S =>
        INC (p.tmp_cnt);
        Expr.Prep (e);
        p.tmp := CG.Declare_temp (sz, z_align, t_cg, in_memory := TRUE);
        Expr.Compile (e);
        CG.Store (p.tmp, 0, sz, z_align, u_cg);
    | Kind.D_to_V =>
        Expr.Prep (e);
    | Kind.S_to_V =>
        Expr.Prep (e);
    | Kind.V_to_V =>
        Expr.Prep (e);
    END;
  END Prep;

PROCEDURE CastExprAlign (p: P): Type.BitAlignT =
  VAR type: Type.T;
  VAR typeInfo: Type.Info; 
  BEGIN
    type := Type.StripPacked (p.tipe);
    type := Type.CheckInfo (type, typeInfo);
    RETURN typeInfo.alignment;
  END CastExprAlign;

PROCEDURE Compile (p: P) =
  VAR
    e  := p.expr;
    u  := Expr.TypeOf (e);
    t  := p.tipe;
    sz, t_align, u_align, z_align: INTEGER;
    t_cg, u_cg: CG.Type;
    u_info, t_info: Type.Info;
  BEGIN
    u := Type.CheckInfo (u, u_info);
    t := Type.CheckInfo (t, t_info);
    t_cg := t_info.stk_type;  t_align := t_info.alignment;
    u_cg := u_info.stk_type;  u_align := u_info.alignment;
    sz := u_info.size;
    Type.Compile (t);
    Type.Compile (u);
    z_align := MAX (t_align, u_align);

    CASE p.kind OF
    | Kind.Noop =>
        Expr.Compile (e);
        CG.Boost_addr_alignment (t_align);
    | Kind.D_to_A,
      Kind.S_to_A,
      Kind.V_to_A =>
        PushTmp (p, z_align);
    | Kind.D_to_S =>
        Expr.CompileAddress (e, traced := FALSE);
        CG.Boost_addr_alignment (t_align);
    | Kind.S_to_S =>
        Expr.Compile (e);
        CG.Boost_addr_alignment (t_align);
    | Kind.V_to_S =>
        PushTmp (p, z_align);
    | Kind.D_to_V =>
        Expr.Compile (e);
        CG.Loophole (u_cg, t_cg);
        (*** back-ends have problems with this because floating-point
           variables may be in floating-point registers...
        Expr.PrepLValue (e);
        Expr.CompileLValue (e);
        CG.Boost_addr_alignment (t_align);
        CG.Load_indirect (t_cg, 0, sz, t_align);
        ******)
    | Kind.S_to_V =>
        Expr.Compile (e);
        CG.Boost_addr_alignment (t_align);
        CG.Load_indirect (t_cg, 0, sz, t_align);
    | Kind.V_to_V =>
        Expr.Compile (e);
        CG.Loophole (u_cg, t_cg);
    END;
  END Compile;

PROCEDURE PushTmp (p: P;  align: INTEGER) =
  BEGIN
    DEC (p.tmp_cnt);
    IF (p.tmp_cnt <= 0) THEN
      CG.Load_addr_of_temp (p.tmp, 0, align);
      p.tmp := NIL;
    ELSE
      CG.Load_addr_of (p.tmp, 0, align);
    END;
  END PushTmp;

PROCEDURE BuildArray (p: P;  src_size: INTEGER): CG.Var =
  VAR
    array : CG.Var;
    elt   := OpenArrayType.NonopenEltType (p.tipe);
    elt_info: Type.Info;
  BEGIN
    elt := Type.CheckInfo (elt, elt_info);
    (** CG.Check_byte_aligned (); **)
    array := OpenArrayType.DeclareDopeTemp (p.tipe);
    CG.Store_addr (array, M3RT.OA_elt_ptr);
    CG.Load_intt (src_size DIV elt_info.size);
    CG.Store_int (Target.Integer.cg_type, array, M3RT.OA_size_0);
    RETURN array;
  END BuildArray;

PROCEDURE PrepLV (p: P; traced: BOOLEAN) =
  VAR
    e  := p.expr;
    u  := Expr.TypeOf (e);
    t  := p.tipe;
    sz, t_align, u_align, z_align: INTEGER;
    t_cg, u_cg: CG.Type;
    t1 : CG.Var;
    u_info, t_info: Type.Info;
  BEGIN
    IF (p.tmp_cnt > 0) THEN  INC (p.tmp_cnt);  RETURN;  END;
    u := Type.CheckInfo (u, u_info);
    t := Type.CheckInfo (t, t_info);
    t_align := t_info.alignment;
    u_align := u_info.alignment;
    z_align := MAX (t_align, u_align);
    t_cg := t_info.stk_type;
    u_cg := u_info.stk_type;
    sz := u_info.size;
    Type.Compile (t);
    Type.Compile (u);

    CASE p.kind OF
    | Kind.Noop,
      Kind.D_to_S,
      Kind.S_to_S,
      Kind.D_to_V,
      Kind.S_to_V,
      Kind.V_to_V =>
        Expr.PrepLValue (p.expr, traced);

    | Kind.D_to_A =>
        INC (p.tmp_cnt);
        Expr.PrepLValue (e, traced);
        Expr.CompileLValue (e, traced);
        p.tmp := BuildArray (p, sz);
    | Kind.S_to_A =>
        INC (p.tmp_cnt);
        Expr.Prep (e);
        Expr.Compile (e);
        p.tmp := BuildArray (p, sz);
    | Kind.V_to_A =>
        (* copy the value to a temporary *)
        INC (p.tmp_cnt);
        Expr.Prep (e);
        t1 := CG.Declare_local (M3ID.NoID, sz, z_align, u_cg,
                                Type.GlobalUID (u),
                                in_memory := TRUE, up_level := FALSE,
                                f := CG.Never);
        Expr.Compile (e);
        CG.Store (t1, 0, sz, z_align, u_cg);
        CG.Load_addr_of (t1, 0, z_align);
        p.tmp := BuildArray (p, sz);
    | Kind.V_to_S =>
        INC (p.tmp_cnt);
        Expr.Prep (e);
        p.tmp := CG.Declare_temp (sz, z_align, t_cg, in_memory := TRUE);
        Expr.Compile (e);
        CG.Store (p.tmp, 0, sz, z_align, u_cg);
    END;
  END PrepLV;

PROCEDURE CompileLV (p: P; traced: BOOLEAN) =
  VAR
    e  := p.expr;
    u  := Expr.TypeOf (e);
    t  := p.tipe;
    sz, t_align, u_align, z_align: INTEGER;
    t_cg, u_cg: CG.Type;
    u_info, t_info: Type.Info;
  BEGIN
    u := Type.CheckInfo (u, u_info);
    t := Type.CheckInfo (t, t_info);
    t_align := t_info.alignment;
    u_align := u_info.alignment;
    z_align := MAX (t_align, u_align);
    t_cg := t_info.stk_type;
    u_cg := u_info.stk_type;
    sz := u_info.size;
    Type.Compile (t);
    Type.Compile (u);

    CASE p.kind OF
    | Kind.Noop,
      Kind.S_to_S,
      Kind.D_to_V,
      Kind.S_to_V,
      Kind.V_to_V =>
        Expr.CompileLValue (p.expr, traced);
        CG.Boost_addr_alignment (t_align);

    | Kind.D_to_S =>
        Expr.CompileLValue (p.expr, traced);
        CG.Boost_addr_alignment (t_align);

        (* Inhibit some optimization that causes compilation to fail. e.g.:
         * m3core/src/float/IEEE/RealFloat.m3:
         * In function 'RealFloat__CopySign':
         * internal compiler error: in convert_move, at expr.c:371
         *
         * ?ForceStacked() inhibits optimizations done by m3front/src/misc/CG.m3?
         * ?The particular optimizations are removal of address taken and
         * pointer indirections? ?Leaving the address taken inhibits
         * gcc optimization? ?Given that this is LOOPHOLE and floating point,
         * inhibiting optimization is very ok?
         *)
        IF FloatType[t_cg] # FloatType[u_cg] THEN
          CG.ForceStacked ();
        END;

    | Kind.D_to_A,
      Kind.S_to_A,
      Kind.V_to_A =>
        PushTmp (p, z_align);
    | Kind.V_to_S =>
        PushTmp (p, z_align);
    END;
  END CompileLV;

PROCEDURE Fold (p: P): Expr.T =
  VAR e: Expr.T;
  BEGIN
    e := Expr.ConstValue (p.expr);
    IF (e = NIL) THEN RETURN NIL END;
    p.expr := e;
    RETURN p;
  END Fold;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  VAR min1, max1: Target.Int;
  BEGIN
    Expr.GetBounds (p.expr, min, max);
    EVAL Type.GetBounds (p.tipe, min1, max1);
    IF TInt.LT (min, min1) THEN min := min1 END;
    IF TInt.LT (max1, max) THEN max := max1 END;
  END Bounder;

PROCEDURE IsDesignator (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN Expr.IsDesignator (p.expr);
  END IsDesignator;

PROCEDURE IsWritable (p: P;  lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN Expr.IsWritable (p.expr, lhs);
  END IsWritable;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    Expr.GenFPLiteral (p.expr, buf);
  END GenFPLiteral;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Expr.NoteWrite (p.expr);
  END NoteWrites;

BEGIN
END CastExpr.
