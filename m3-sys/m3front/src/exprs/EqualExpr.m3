(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: EqualExpr.m3                                          *)
(* Last modified on Thu Jun 29 14:03:25 PDT 1995 by kalsow     *)
(*      modified on Thu Nov 29 03:33:19 1990 by muller         *)

MODULE EqualExpr;

IMPORT M3, M3ID, CG, Expr, ExprRep, Type, Procedure, TargetMap;
IMPORT Bool, Int, Reel, LReel, EReel, SetExpr, Variable;
IMPORT IntegerExpr, ReelExpr, EnumExpr, AddressExpr, UserProc;
IMPORT ProcExpr, ProcType, TextExpr, Error, M3WString;
IMPORT RecordType, ArrayType, Field, Value, M3String, Textt;
IMPORT NamedExpr, QualifyExpr, OpenArrayType, Target, TInt;
IMPORT MSIR, MSIRBuilder, MSIRType, M3RT;

CONST
  Max_unroll = 4; (* max # of iterations in an unrolled loop *)

TYPE
  Kind = {SimpleScalar, SimpleStruct, Complex};

CONST
  OpName = ARRAY Op OF TEXT { "\'=\'", "\'#\'" };
  CGOp = ARRAY Op OF CG.Cmp { CG.Cmp.EQ, CG.Cmp.NE };

TYPE
  P = ExprRep.Tabc BRANDED "EqualExpr.P" OBJECT
        op     : Op;
        kind   : Kind;
        tmp    : CG.Val;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValueBool;
        prepBR       := PrepBR;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
        exprAlign    := ExprRep.ExprBoolAlign;
        compileMSIR  := CompileMSIR;
      END;

PROCEDURE New (a, b: Expr.T;  op: Op): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a       := a;
    p.b       := b;
    p.op      := op;
    p.type    := Bool.T;
    p.repType := Bool.T;
    p.kind    := Kind.SimpleScalar;
    p.tmp     := NIL;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb: Type.T;  str: M3String.T;  wstr: M3WString.T;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));
    IF (ta = NIL) OR (tb = NIL)
      OR NOT (Type.IsAssignable (ta, tb) OR Type.IsAssignable (tb, ta)) THEN
      p.type := Expr.BadOperands (OpName[p.op], ta, tb);
    END;
    p.kind := Classify (ta, tb);
    IF (p.kind = Kind.SimpleScalar) AND ((ta = Textt.T) OR (tb = Textt.T)) THEN
      IF TextExpr.Split8 (p.a, str) OR TextExpr.Split32 (p.a, wstr)
      OR TextExpr.Split8 (p.b, str) OR TextExpr.Split32 (p.b, wstr) THEN
        Error.Warn (1,"comparing pointers, use Text.Equal to compare strings");
      END;
    END;
  END Check;

PROCEDURE Classify (ta, tb: Type.T): Kind =
  VAR ia, ib: Type.Info;
  BEGIN
    IF (ta = Int.T) OR (ta = Reel.T) OR (ta = LReel.T) OR (ta = EReel.T) THEN
      RETURN Kind.SimpleScalar;
    END;

    EVAL Type.CheckInfo (ta, ia);

    CASE ia.class OF
    | Type.Class.Error,
      Type.Class.Integer,
      Type.Class.Longint,
      Type.Class.Real,
      Type.Class.Longreal,
      Type.Class.Extended,
      Type.Class.Enum,
      Type.Class.Object,
      Type.Class.Opaque,
      Type.Class.Ref,
      Type.Class.Subrange =>
          RETURN Kind.SimpleScalar;
    | Type.Class.Set =>
          IF (ia.size <= Target.Integer.size) THEN
            RETURN Kind.SimpleScalar;
          END;
    ELSE (* skip *)
    END;
    
    EVAL Type.CheckInfo (tb, ib);

    IF SimpleStructType (ia) AND SimpleStructType (ib) THEN
      RETURN Kind.SimpleStruct;
    END;

    RETURN Kind.Complex;
  END Classify;

PROCEDURE SimpleStructType (READONLY info: Type.Info): BOOLEAN =
  BEGIN
    RETURN (info.isSolid)
       AND (info.class # Type.Class.Procedure)
       AND (info.class # Type.Class.OpenArray)
       AND (info.size >= 0)
       AND (info.size MOD info.alignment = 0)
       AND (info.size DIV info.alignment <= Max_unroll)
       AND (FindCompareType (info.size, info.alignment) # CG.Type.Void);
  END SimpleStructType;

PROCEDURE FindCompareType (size, align: INTEGER): CG.Type =
  CONST Z = ARRAY [0..3] OF CG.Type { CG.Type.Word64, CG.Type.Word32,
                                      CG.Type.Word16, CG.Type.Word8 };
  VAR t: CG.Type;
  BEGIN
    FOR i := FIRST (Z) TO LAST (Z) DO
      t := Z[i];
      IF (align = TargetMap.CG_Align [t])
        AND (size MOD TargetMap.CG_Size [t] = 0)
        AND (Target.Word.size >= TargetMap.CG_Size [t]) THEN
        RETURN t;
      END;
    END;
    RETURN CG.Type.Void;
  END FindCompareType;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.op = b.op)
                 AND Expr.IsEqual (a.a, b.a, x)
                 AND Expr.IsEqual (a.b, b.b, x);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Prep (p: P) =
  VAR false: CG.Label;
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
    IF (p.kind = Kind.Complex) THEN
      (* use the branching code to compute a value *)
      false := CG.Next_label (2);
      PrepBR (p, CG.No_label, false, CG.Maybe);
      Value.Load (Bool.True);
      p.tmp := CG.Pop_temp ();
      CG.Jump (false+1);
      CG.Set_label (false);
      Value.Load (Bool.False);
      CG.Store_temp (p.tmp);
      CG.Set_label (false+1);
    END;
  END Prep;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  VAR t := p.a.type;
  BEGIN
    <* ASSERT NOT StaticOnly *>
    IF (p.kind = Kind.SimpleScalar) THEN
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      CG.Compare (Type.CGType (t), CGOp[p.op]);
    ELSIF (p.kind = Kind.SimpleStruct) THEN
      CompileSolidUnrolled (p);
    ELSE
      (* used the branching code to compute a value *)
      CG.Push (p.tmp);
      CG.Free (p.tmp);
      p.tmp := NIL;
    END;
  END Compile;

PROCEDURE CompileSolidUnrolled (p: P) =
  VAR
    info       : Type.Info;
    xa, xb     : CG.Val;
    cmp_type   : CG.Type;
    chunk_size : INTEGER;
    n_chunks   : INTEGER;
  BEGIN
    Expr.Compile (p.a);  xa := CG.Pop ();
    Expr.Compile (p.b);  xb := CG.Pop ();
    EVAL Type.CheckInfo (p.a.type, info);

    cmp_type := FindCompareType (info.size, info.alignment);
    <*ASSERT cmp_type # CG.Type.Void*>
    chunk_size := TargetMap.CG_Size [cmp_type];
    n_chunks := info.size DIV chunk_size;

    FOR i := 0 TO n_chunks - 1 DO
      CG.Push (xa);
      CG.Load_indirect (cmp_type, i * chunk_size, chunk_size, info.alignment);
      CG.Push (xb);
      CG.Load_indirect (cmp_type, i * chunk_size, chunk_size, info.alignment);
      CG.Compare (Target.Word.cg_type, CGOp[p.op]);
      IF (i > 0) THEN
        IF (p.op = Op.EQ)
          THEN CG.And (Target.Word.cg_type);
          ELSE CG.Or  (Target.Word.cg_type);
        END;
      END;
    END;

    CG.Free (xa);
    CG.Free (xb);
  END CompileSolidUnrolled;

PROCEDURE PrepBR (p: P;  true, false: CG.Label;  freq: CG.Frequency) =
  VAR
    ta := Type.Base (p.a.type);
    tb := Type.Base (p.b.type);

    skip: CG.Label;
    xa, xb: CG.Val;
    info: Type.Info;
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
    ta := Type.CheckInfo (ta, info);
    IF (p.kind = Kind.SimpleScalar) THEN
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      CG.If_then (info.stk_type, CGOp[p.op], true, false, freq);
      RETURN;
 
    (************ better to generate "If_eq" than  "eq; if_true" 
    ELSIF (p.kind = Kind.SimpleStruct) THEN
      CompileSolidUnrolled (p);
      IF (true # CG.No_label)
        THEN CG.If_true (true, freq);
        ELSE CG.If_false (false, freq);
      END;
      RETURN;
    ***************************************************************)

    ELSIF (info.class = Type.Class.Set) THEN
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      CG.Set_compare (info.size, CGOp[p.op]);
      IF (true = CG.No_label)
        THEN CG.If_false (false, freq);
        ELSE CG.If_true (true, freq);
      END;
      RETURN;

    ELSIF (info.class = Type.Class.Procedure) OR ProcType.Is (tb) THEN
      IF (p.op = Op.EQ)
        THEN CompileProcs (p, true, false, freq);
        ELSE CompileProcs (p, false, true, freq);
      END;

    ELSIF (info.class = Type.Class.Record)
       OR (info.class = Type.Class.Array)
       OR (info.class = Type.Class.OpenArray) THEN
      Expr.Compile (p.a);  xa := CG.Pop ();
      Expr.Compile (p.b);  xb := CG.Pop ();
      IF (p.op = Op.NE) THEN  (* swap true and false labels *)
        skip := true;  true := false;  false := skip;
      END;
      IF (false = CG.No_label) THEN
        skip := CG.Next_label ();
        CompileTest (xa, xb, ta, tb, skip, CG.Always - freq);
        CG.Jump (true);
        CG.Set_label (skip);
      ELSE
        CompileTest (xa, xb, ta, tb, false, freq);
      END;
      CG.Free (xa);
      CG.Free (xb);

    ELSE 
      (* typechecking removed the other cases. *)
      EVAL Expr.BadOperands (OpName[p.op], ta, tb);
    END;
  END PrepBR;

PROCEDURE CompileProcs (p: P;  true, false: CG.Label;  freq: CG.Frequency) =
  VAR
    procA, procB : Value.T;
    classA, classB: [0..2];
    t1, t2 : CG.Val;
    skip, no_closure, nope: CG.Label;
  BEGIN
    (* first we classify the two arguments:
         class 0: NIL, global proc, or non-formal variable => no frame pointer
         class 1: nested proc => fixed frame pointer
         class 2: formal variable => may be closure => may have frame

       Note: procedures pointers are always aligned!
     *)
    classA := 0;
    IF UserProc.IsProcedureLiteral (p.a, procA)
       AND Procedure.IsNested (procA) THEN      classA := 1;
    ELSIF CanHaveFrame (p.a) THEN               classA := 2;
    END;
    classB := 0;
    IF UserProc.IsProcedureLiteral (p.b, procB)
       AND Procedure.IsNested (procB) THEN      classB := 1;
    ELSIF CanHaveFrame (p.b) THEN               classB := 2;
    END;

    (* normalize the pair so that classA <= classB *)
    IF (classB < classA) THEN
      VAR tmp := classA;  BEGIN classA := classB;  classB := tmp  END;
      VAR tmp := p.a;     BEGIN p.a := p.b;        p.b := tmp     END;
      VAR tmp := procA;   BEGIN procA := procB;    procB := tmp   END;
    END;

    (* finally, we generate the tests, based on the classes *)
    CASE classA * 3 + classB OF

    | 0,   (* 0, 0 *)
      4 => (* 1, 1 *)
           Expr.Compile (p.a);
           Expr.Compile (p.b);
           CG.If_then (CG.Type.Addr, CG.Cmp.EQ, true, false, freq);

    | 1 => (* 0, 1 => never equal *)
           (* constant FALSE *)
           IF (true = CG.No_label)
             THEN CG.Jump (false);
             ELSE (* fall through *)
           END;

    | 2 => (* 0, 2 *)
           skip := CG.Next_label ();
           nope := skip;  IF (true = CG.No_label) THEN nope := false; END;
           Expr.Compile (p.b);
           t1 := CG.Pop ();
           CG.If_closure (t1, nope, CG.No_label, CG.Always - freq);
           Expr.Compile (p.a);
           CG.Push (t1);
           CG.If_then (CG.Type.Addr, CG.Cmp.EQ, true, false, freq);
           CG.Set_label (skip);
           CG.Free (t1);

    | 5 => (* 1, 2 *)
           skip := CG.Next_label ();
           nope := skip;  IF (true = CG.No_label) THEN nope := false; END;
           Expr.Compile (p.b);
           t1 := CG.Pop ();
           CG.If_closure (t1, CG.No_label, nope, freq);
           Expr.Compile (p.a);
           CG.Push (t1);
           CG.Closure_proc ();
           CG.If_compare (CG.Type.Addr, CG.Cmp.NE, nope, freq);
           Procedure.LoadStaticLink (procA);
           CG.Push (t1);
           CG.Closure_frame ();
           CG.If_then (CG.Type.Addr, CG.Cmp.EQ, true, false, freq);
           CG.Set_label (skip);
           CG.Free (t1);

    | 8 => (* 2, 2 *)
           no_closure := CG.Next_label (2);
           skip := no_closure + 1;
           nope := skip;  IF (true = CG.No_label) THEN nope := false; END;

           Expr.Compile (p.a);
           t1 := CG.Pop ();
           Expr.Compile (p.b);
           t2 := CG.Pop ();

           CG.If_closure (t1, CG.No_label, no_closure, CG.Maybe);
           (* A is a closure... *)

           CG.If_closure (t2, nope, CG.No_label, CG.Always - freq);

           (* both A and B are closures *)
           CG.Push (t1);
           CG.Closure_proc ();
           CG.Push (t2);
           CG.Closure_proc ();
           CG.If_compare (CG.Type.Addr, CG.Cmp.NE, nope, CG.Always - freq);
           CG.Push (t1);
           CG.Closure_frame ();
           CG.Push (t2);
           CG.Closure_frame ();
           CG.If_compare (CG.Type.Addr, CG.Cmp.NE, nope, CG.Always - freq);

           (* A is not a closure *)
           CG.Set_label (no_closure);
           CG.If_closure (t2, nope, CG.No_label, CG.Always - freq);

           (* neither A nor B is a closure *)
           CG.Push (t1);
           CG.Push (t2);
           CG.If_then (CG.Type.Addr, CG.Cmp.EQ, true, false, freq);
           CG.Set_label (skip);
           CG.Free (t1);
           CG.Free (t2);

    ELSE <*ASSERT FALSE*>
    END;
  END CompileProcs;

PROCEDURE CanHaveFrame (e: Expr.T): BOOLEAN =
  VAR name: M3ID.T;  obj: Value.T;
  BEGIN
    IF NOT (NamedExpr.Split (e, name, obj) OR QualifyExpr.Split (e, obj)) THEN
      (* non-constant, non-variable => no frame *)
      RETURN FALSE;
    ELSIF (Value.ClassOf (obj) = Value.Class.Procedure) THEN
      (* constant: no frame *)
      RETURN FALSE;
    ELSIF (Value.ClassOf (obj) = Value.Class.Var) AND
          Variable.HasClosure (Value.Base (obj)) THEN
      RETURN TRUE;
    ELSE (* non-formal, non-const => frame = NIL *)
      RETURN FALSE;
    END;
  END CanHaveFrame;

PROCEDURE CompileTest (x1, x2 : CG.Val;
                       t1, t2 : Type.T;
                       false  : CG.Label;
                       freq   : CG.Frequency) =
  VAR
    u1_info, u2_info: Type.Info;
    u1 := Type.Base (t1);  (* strip the BITS FOR *)
    u2 := Type.Base (t2);
  BEGIN
    EVAL Type.CheckInfo (u1, u1_info);
    EVAL Type.CheckInfo (u2, u2_info);
    IF (u1_info.class = Type.Class.Record) THEN
      CompileRecord (x1, x2, u1, false, freq);

    ELSIF (u1_info.class = Type.Class.Array)
       OR (u1_info.class = Type.Class.OpenArray) THEN
      CompileArray (x1, x2, u1, u2, false, freq);

    ELSIF (u1_info.class = Type.Class.Set) THEN
      CG.Push (x1);
      IF (u1_info.size <= Target.Integer.size) THEN
        CG.Load_indirect
          (Target.Word.cg_type, 0, Target.Integer.size, u1_info.alignment);
      END;
      CG.Push (x2);
      IF (u1_info.size <= Target.Integer.size) THEN
        CG.Load_indirect
          (Target.Word.cg_type, 0, Target.Integer.size, u2_info.alignment);
      END;
      CG.Set_compare (u1_info.size, CG.Cmp.EQ);
      CG.If_false (false, freq);

    ELSIF (u1_info.class = Type.Class.Procedure)
       OR (u2_info.class = Type.Class.Procedure) THEN
      (* we're already inside some variable => no frame pointers *)
      CG.Push (x1);
      CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size, CG.ProcAlign ());
      CG.Push (x2);
      CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size, CG.ProcAlign ());
      CG.If_compare (CG.Type.Addr, CG.Cmp.NE, false, freq);

    ELSE (* simple scalars *)
      EVAL Type.CheckInfo (t1, u1_info);  (* can't ignore BITS FOR *)
      EVAL Type.CheckInfo (t2, u2_info);
      CG.Push (x1);
      CG.Boost_addr_alignment (Target.Address.align);
      CG.Load_indirect (u1_info.stk_type, 0, u1_info.size, u1_info.alignment);
      CG.Push (x2);
      CG.Boost_addr_alignment (Target.Address.align);
      CG.Load_indirect (u2_info.stk_type, 0, u2_info.size, u2_info.alignment);
      CG.If_compare (u1_info.stk_type, CG.Cmp.NE, false, freq);
    END;
  END CompileTest;

PROCEDURE CompileArray (p1, p2 : CG.Val;
                        t1, t2 : Type.T;
                        false  : CG.Label;
                        freq   : CG.Frequency) =
  VAR i1, i2, e1, e2: Type.T;
  BEGIN
    IF CompileSolid (p1, p2, t1, t2, false, freq) THEN RETURN END;
    EVAL ArrayType.Split (t1, i1, e1);
    EVAL ArrayType.Split (t2, i2, e2);
    GenShapeCheck (p1, p2, i1, e1, i2, e2, false, freq);
    GenValueCheck (t1, i1, e1, p1, t2, i2, e2, p2, false, freq);
  END CompileArray;

PROCEDURE GenShapeCheck (p1, p2 : CG.Val;
                         i1, e1 : Type.T;
                         i2, e2 : Type.T;
                         false  : CG.Label;
                         freq   : CG.Frequency) =
  VAR n := 0;
  BEGIN
    LOOP
      IF (i1 # NIL) AND (i2 # NIL) THEN RETURN END;

      IF (i1 = NIL) THEN
        CG.Push (p1);
        CG.Open_size (n);
      ELSE
        CG.Load_integer (Target.Integer.cg_type, Type.Number (i1));
      END;
      IF (i2 = NIL) THEN
        CG.Push (p2);
        CG.Open_size (n);
      ELSE
        CG.Load_integer (Target.Integer.cg_type, Type.Number (i2));
      END;
      CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, false, freq);

      IF NOT ArrayType.Split (e1, i1, e1) THEN RETURN END;
      IF NOT ArrayType.Split (e2, i2, e2) THEN RETURN END;
      n := n + 1;
    END;
  END GenShapeCheck;


PROCEDURE GenValueCheck (t1, i1, e1: Type.T;  p1: CG.Val; 
                         t2, i2, e2: Type.T;  p2: CG.Val;
                         false: CG.Label;
                         freq: CG.Frequency) =
  VAR
    d1 := OpenArrayType.OpenDepth (t1);
    d2 := OpenArrayType.OpenDepth (t2);
    x: CG.Val;
  BEGIN
    IF (d1 > 0) AND (d2 > 0) THEN
      IF (d1 <= d2)
        THEN GenOpenValueCheck (t1, p1, p2, false, freq);
        ELSE GenOpenValueCheck (t2, p1, p2, false, freq);
      END;
    ELSIF (d1 > 0) THEN
      CG.Push (p1);
      CG.Open_elt_ptr (OpenArrayType.EltAlign (t1));
      x := CG.Pop ();
      GenFixedValueCheck (t2, i2, e2, x, p2, false, freq);
      CG.Free (x);
    ELSIF (d2 > 0) THEN
      CG.Push (p2);
      CG.Open_elt_ptr (OpenArrayType.EltAlign (t2));
      x := CG.Pop ();
      GenFixedValueCheck (t1, i1, e1, p1, x, false, freq);
      CG.Free (x);
    ELSE (* d1 = 0 AND d2 = 0 *)
      GenFixedValueCheck (t1, i1, e1, p1, p2, false, freq);
    END;
  END GenValueCheck;

PROCEDURE GenOpenValueCheck (t1: Type.T;  p1, p2: CG.Val; 
                             false: CG.Label;  freq: CG.Frequency) =
  VAR
    d1  := OpenArrayType.OpenDepth (t1);
    elt := OpenArrayType.NonopenEltType (t1);
    cnt       : CG.Val;
    elt_align : INTEGER;
    elt_pack  : INTEGER;
    top       : CG.Label;
    o1, o2    : CG.Val;
    info      : Type.Info;
  BEGIN
    elt := Type.CheckInfo (elt, info);
    elt_align := info.alignment;
    elt_pack  := (info.size + elt_align - 1) DIV elt_align * elt_align;

    (* compute the total number of elements that need to be compared *)
    FOR i := 0 TO d1-1 DO
      CG.Push (p1);
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (Target.Integer.cg_type) END;
    END;
    CG.Load_intt (1);
    CG.Subtract (Target.Integer.cg_type);
    cnt := CG.Pop_temp ();

    top := CG.Next_label (2);
    CG.Jump (top+1); (* test for empty arrays *)

    CG.Set_label (top);

    (* compute the address of the elements *)
    CG.Push (p1);
    CG.Open_elt_ptr (elt_align);
    CG.Push (cnt);
    CG.Index_bytes (elt_pack);
    o1 := CG.Pop ();

    CG.Push (p2);
    CG.Open_elt_ptr (elt_align);
    CG.Push (cnt);
    CG.Index_bytes (elt_pack);
    o2 := CG.Pop ();

    CompileTest (o1, o2, elt, elt, false, freq);

    (* free the element pointers *)
    CG.Free (o1);
    CG.Free (o2);

    (* decrement the count *)
    CG.Push (cnt);
    CG.Load_integer (Target.Integer.cg_type, TInt.One);
    CG.Subtract (Target.Integer.cg_type);
    CG.Store_temp (cnt);

    (* test for completion *)
    CG.Set_label (top+1);
    CG.Push (cnt);
    CG.Load_integer (Target.Integer.cg_type, TInt.Zero);
    CG.If_compare (Target.Integer.cg_type, CG.Cmp.GE, top, CG.Likely);

    CG.Free (cnt);
  END GenOpenValueCheck;

PROCEDURE GenFixedValueCheck (t1, i1, e1: Type.T;  p1, p2: CG.Val; 
                              false: CG.Label;  freq: CG.Frequency) =
  VAR
    cnt       : CG.Val;
    n_elts    : INTEGER;
    b         : BOOLEAN;
    top       : CG.Label;
    o1, o2    : CG.Val;
  BEGIN
    (* compute the total number of elements that need to be compared *)
    b := TInt.ToInt (Type.Number (i1), n_elts); <* ASSERT b *>
    IF (n_elts <= 0) THEN RETURN END;
    CG.Load_intt (n_elts - 1);
    cnt := CG.Pop_temp ();

    top := CG.Next_label (2);
    CG.Set_label (top);

    (* compute the address of the elements *)
    CG.Push (p1);
    CG.Push (cnt);
    ArrayType.GenIndex (t1);
    o1 := CG.Pop ();

    CG.Push (p2);
    CG.Push (cnt);
    ArrayType.GenIndex (t1);
    o2 := CG.Pop ();

    CompileTest (o1, o2, e1, e1, false, freq);

    (* free the element pointers *)
    CG.Free (o1);
    CG.Free (o2);

    (* decrement the count *)
    CG.Push (cnt);
    CG.Load_integer (Target.Integer.cg_type, TInt.One);
    CG.Subtract (Target.Integer.cg_type);
    CG.Store_temp (cnt);

    (* test for completion *)
    CG.Set_label (top+1);
    CG.Push (cnt);
    CG.Load_integer (Target.Integer.cg_type, TInt.Zero);
    CG.If_compare (Target.Integer.cg_type, CG.Cmp.GE, top, CG.Likely);

    CG.Free (cnt);
  END GenFixedValueCheck;

PROCEDURE CompileRecord (p1, p2: CG.Val;  t: Type.T;
                         false: CG.Label;  freq: CG.Frequency) =
  VAR
    v      : Value.T;
    field  : Field.Info;
    o1, o2 : CG.Val;
  BEGIN
    IF CompileSolid (p1, p2, t, t, false, freq) THEN RETURN END;
    EVAL RecordType.Split (t, v);
    WHILE (v # NIL) DO
      Field.Split (v, field);
      CG.Push (p1);
      CG.Add_offset (field.offset);
      o1 := CG.Pop ();
      CG.Push (p2);
      CG.Add_offset (field.offset);
      o2 := CG.Pop ();
      CompileTest (o1, o2, field.type, field.type, false, freq);
      CG.Free (o1);
      CG.Free (o2);
      v := v.next;
    END;
  END CompileRecord;

PROCEDURE CompileSolid (p1, p2: CG.Val;  t1, t2: Type.T;
                         false: CG.Label;  freq: CG.Frequency): BOOLEAN =
  VAR
    info1, info2 : Type.Info;
    cmp_type     : CG.Type;
    chunk_align  : INTEGER;
    chunk_size   : INTEGER;
    n_chunks     : INTEGER;
    cnt          : CG.Val;
    top          : CG.Label;
  BEGIN
    EVAL Type.CheckInfo (t1, info1);
    EVAL Type.CheckInfo (t2, info2);
    IF (NOT info1.isSolid) OR (NOT info2.isSolid) THEN RETURN FALSE END;
    IF (info1.size < 0) OR (info1.size # info2.size) THEN RETURN FALSE END;

    chunk_align := MIN (info1.alignment, info2.alignment);
    cmp_type := FindCompareType (info1.size, chunk_align);
    IF (cmp_type = CG.Type.Void) THEN RETURN FALSE; END;
    chunk_size := TargetMap.CG_Size [cmp_type];
    n_chunks   := info1.size DIV chunk_size;

    IF (n_chunks <= Max_unroll) THEN

      (* unroll the loop of comparisons *)
      FOR i := 0 TO n_chunks - 1 DO
        CG.Push (p1);
        CG.Load_indirect (cmp_type, i * chunk_size, chunk_size, chunk_align);
        CG.Push (p2);
        CG.Load_indirect (cmp_type, i * chunk_size, chunk_size, chunk_align);
        CG.If_compare (Target.Word.cg_type, CG.Cmp.NE, false, freq);
      END;

    ELSE
      (* generate a loop of comparisons *)
      CG.Load_intt (n_chunks - 1);
      cnt := CG.Pop_temp ();

      top := CG.Next_label (2);
      CG.Set_label (top);

      (* compute the address of the elements and load them *)
      CG.Push (p1);
      CG.Push (cnt);
      CG.Index_bytes (chunk_size);
      CG.Load_indirect (cmp_type, 0, chunk_size, chunk_align);

      CG.Push (p2);
      CG.Push (cnt);
      CG.Index_bytes (chunk_size);
      CG.Load_indirect (cmp_type, 0, chunk_size, chunk_align);

      (* do the comparison *)
      CG.If_compare (Target.Word.cg_type, CG.Cmp.NE, false, freq);

      (* decrement the count *)
      CG.Push (cnt);
      CG.Load_integer (Target.Integer.cg_type, TInt.One);
      CG.Subtract (Target.Integer.cg_type);
      CG.Store_temp (cnt);

      (* test for completion *)
      CG.Set_label (top+1);
      CG.Push (cnt);
      CG.Load_integer (Target.Integer.cg_type, TInt.Zero);
      CG.If_compare (Target.Integer.cg_type, CG.Cmp.GE, top, CG.Likely);

      CG.Free (cnt);
    END;

    RETURN TRUE;
  END CompileSolid;

(*************************
PROCEDURE MinFieldAlignment (f: Scope.T): INTEGER =
    (* compute the minimum field alignment that's needed *)
  VAR
    n, min  : INTEGER;
    fields  : Scope.ValueList;
    index   : INTEGER;
    offset  : INTEGER;
    type    : Type.T;
  BEGIN
    Scope.ToList (f, fields, n);
    min := MAX (Target.Address.align, Target.Integer.align);
    FOR i := 0 TO n-1 DO
      Field.Split (fields[i], index, offset, type);
      min := GCD (min, offset);
    END;
    RETURN min;
  END MinFieldAlignment;

PROCEDURE GCD (x, y: INTEGER): INTEGER =
  BEGIN
    IF (x <= 0) OR (y <= 0) THEN RETURN 1 END;
    LOOP
      IF    (x = 0) THEN RETURN y;
      ELSIF (y = 0) THEN RETURN x;
      ELSIF (x < y) THEN y := y MOD x;
      ELSIF (y < x) THEN x := x MOD y;
      ELSE  RETURN x;
      END;
    END;
  END GCD;
********************************)

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2: Expr.T;  s: INTEGER;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    IF (e1 = NIL) THEN RETURN NIL END;
    e2 := Expr.ConstValue (p.b);
    IF (e2 = NIL) THEN RETURN NIL END;
    IF   IntegerExpr.Compare (e1, e2, s)
      OR EnumExpr.Compare (e1, e2, s)
      OR ReelExpr.Compare (e1, e2, s)
      OR AddressExpr.Compare (e1, e2, s)
      OR SetExpr.Compare (e1, e2, s)
      OR ProcExpr.Compare (e1, e2, s) THEN
      RETURN Bool.Map[(p.op = Op.EQ) = (s = 0)];
    END;
    RETURN NIL;
  END Fold;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  (* =/# yield a BOOLEAN — widen the i1 result to ZType (i64). *)
  VAR r := CompileMSIRRaw (p);
  BEGIN
    IF r # NIL AND MSIR.Kind (MSIR.ValueType (r)) = MSIR.TypeKind.I1 THEN
      r := MSIR.BuildZExt (MSIRBuilder.CurrentBlock (), "", r,
                           MSIR.TI (Target.Integer.size));
    END;
    RETURN r;
  END CompileMSIR;

PROCEDURE CompileMSIRRaw (p: P): MSIR.Value =
  VAR
    lv, rv:  MSIR.Value;
    pred:    MSIR.CmpPred;
    fpred:   MSIR.FCmpPred;
    blk:     MSIR.Block;
    lvKind:  MSIR.TypeKind;
    ta:      Type.T;
    taInfo:  Type.Info;
  BEGIN
    IF p.kind = Kind.Complex THEN
      ta := Type.Base (Expr.TypeOf (p.a));
      EVAL Type.CheckInfo (ta, taInfo);
      (* CompileProcs (called during CG Prep/PrepBR) may have swapped p.a and p.b
         to normalize classA <= classB.  If p.a is now NIL/Null (Ref class) and
         p.b carries the Procedure type, re-derive ta from p.b instead. *)
      IF taInfo.class # Type.Class.Procedure THEN
        VAR tb := Type.Base (Expr.TypeOf (p.b)); tbInfo: Type.Info; BEGIN
          EVAL Type.CheckInfo (tb, tbInfo);
          IF tbInfo.class = Type.Class.Procedure THEN
            ta := tb;  taInfo := tbInfo;
          END;
        END;
      END;
      (* Multi-word SET (IWide): large sets are Complex but still comparable
         as wide integers via icmp eq/ne. *)
      IF taInfo.class = Type.Class.Set THEN
        lv := Expr.CompileMSIR (p.a);  IF lv = NIL THEN RETURN NIL END;
        rv := Expr.CompileMSIR (p.b);  IF rv = NIL THEN RETURN NIL END;
        blk := MSIRBuilder.CurrentBlock ();
        CASE p.op OF
        | Op.EQ => pred := MSIR.CmpPred.Eq;
        | Op.NE => pred := MSIR.CmpPred.Ne;
        END;
        RETURN MSIR.BuildICmp (blk, "", pred, lv, rv);
      END;
      (* Open-array equality: shape check then element-by-element loop. *)
      IF taInfo.class = Type.Class.OpenArray THEN
        VAR
          openRank   := OpenArrayType.OpenDepth (ta);
          eltT       := OpenArrayType.NonopenEltType (ta);
          eltMsir    : MSIR.T;
          eltInfo    : Type.Info;
          elemBytes  : INTEGER;
          shapeOk    : MSIR.Value;
          total      : MSIR.Value;
          zeros      : REF ARRAY OF MSIR.Value;
          zero       : MSIR.Value;
          pA, pB     : MSIR.Value;
          idxSlot    : MSIR.Value;
          resSlot    : MSIR.Value;
          eqConst    : MSIR.Value;
          neqConst   : MSIR.Value;
          checkBlk   : MSIR.Block;
          loopHdrBlk : MSIR.Block;
          loopBodBlk : MSIR.Block;
          incrBlk    : MSIR.Block;
          failBlk    : MSIR.Block;
          mergeBlk   : MSIR.Block;
          isEmpty    : MSIR.Value;
          idx0, hdrCond : MSIR.Value;
          idx1, ebV, byteOff, eA, eB, bodCond : MSIR.Value;
          idx2, nxt  : MSIR.Value;
        BEGIN
          eltMsir := MSIRType.Translate (eltT);
          IF eltMsir = NIL THEN
            MSIRBuilder.Abandon ("open-array equality: cannot translate element type");
            RETURN NIL;
          END;
          EVAL Type.CheckInfo (eltT, eltInfo);
          IF eltInfo.size <= 0 OR eltInfo.size MOD Target.Byte # 0 THEN
            MSIRBuilder.Abandon ("open-array equality: non-byte-sized element");
            RETURN NIL;
          END;
          elemBytes := eltInfo.size DIV Target.Byte;

          lv := Expr.CompileMSIR (p.a);  IF lv = NIL THEN RETURN NIL END;
          rv := Expr.CompileMSIR (p.b);  IF rv = NIL THEN RETURN NIL END;
          blk := MSIRBuilder.CurrentBlock ();

          (* One side may be a fixed array (e.g. ARRAY [0..7] OF Byte) even when
             the comparison type is OpenArray (e.g. comparing REF ARRAY OF Byte^
             to a fixed array).  For fixed-array values the MSIR representation
             is a pointer to storage (FixedArray kind), not a fat pointer.  Detect
             that case and use the statically-known element count instead of
             BuildOpenArraySize/BuildOpenArrayElemAddr, which assert on non-OA values.
             For multi-depth open arrays (openRank > MSIR OA rank), use the heap
             pointer to load extra dimension sizes directly from the dope. *)
          VAR
            lvIsOA := (MSIR.Kind (MSIR.ValueType (lv)) = MSIR.TypeKind.OpenArray);
            rvIsOA := (MSIR.Kind (MSIR.ValueType (rv)) = MSIR.TypeKind.OpenArray);
            tbType : Type.T;  tbInfo : Type.Info;  tbElts : INTEGER;
            (* Per-dim static extents for fixed side *)
            tbDimExtents : REF ARRAY OF INTEGER := NIL;
            (* Heap pointer for each OA side (for extra dims beyond MSIR rank) *)
            heapLV : MSIR.Value := NIL;
            heapRV : MSIR.Value := NIL;
            lvMsirRank : INTEGER := 0;
            rvMsirRank : INTEGER := 0;
            intT  := MSIR.TI (Target.Integer.size);
            ptrT  := MSIR.TPtr (MSIR.TVoid ());
            apB   := Target.Address.size DIV Target.Byte;
            intB  := Target.Integer.size DIV Target.Byte;
          BEGIN
            IF lvIsOA THEN
              lvMsirRank := MSIR.OpenArrayRank (MSIR.ValueType (lv));
            END;
            IF rvIsOA THEN
              rvMsirRank := MSIR.OpenArrayRank (MSIR.ValueType (rv));
            END;

            (* For OA sides with extra dims beyond MSIR rank, get the heap pointer
               so we can load dims directly from the dope. *)
            IF lvIsOA AND lvMsirRank < openRank THEN
              heapLV := Expr.LValueMSIR (p.a);
              blk := MSIRBuilder.CurrentBlock ();  (* refresh after LValueMSIR *)
            END;
            IF rvIsOA AND rvMsirRank < openRank THEN
              heapRV := Expr.LValueMSIR (p.b);
              blk := MSIRBuilder.CurrentBlock ();  (* refresh after LValueMSIR *)
            END;

            IF NOT lvIsOA THEN
              (* Derive static per-dim extents from the fixed array type. *)
              tbType := Type.Base (Expr.TypeOf (p.a));
              EVAL Type.CheckInfo (tbType, tbInfo);
              tbElts := tbInfo.size DIV (elemBytes * Target.Byte);
              tbDimExtents := NEW (REF ARRAY OF INTEGER, openRank);
              VAR curr := tbType;
              BEGIN
                FOR d := 0 TO openRank - 1 DO
                  VAR idxT, eltT2: Type.T; nTi: Target.Int; ni: INTEGER;
                  BEGIN
                    IF ArrayType.Split (curr, idxT, eltT2) THEN
                      nTi := Type.Number (idxT);
                      IF TInt.ToInt (nTi, ni)
                        THEN tbDimExtents[d] := ni;
                        ELSE tbDimExtents[d] := 1;
                      END;
                      curr := eltT2;
                    ELSE
                      tbDimExtents[d] := 1;
                    END;
                  END;
                END;
              END;
            END;
            IF NOT rvIsOA THEN
              tbType := Type.Base (Expr.TypeOf (p.b));
              EVAL Type.CheckInfo (tbType, tbInfo);
              tbElts := tbInfo.size DIV (elemBytes * Target.Byte);
              IF tbDimExtents = NIL THEN
                tbDimExtents := NEW (REF ARRAY OF INTEGER, openRank);
                VAR curr := tbType;
                BEGIN
                  FOR d := 0 TO openRank - 1 DO
                    VAR idxT, eltT2: Type.T; nTi: Target.Int; ni: INTEGER;
                    BEGIN
                      IF ArrayType.Split (curr, idxT, eltT2) THEN
                        nTi := Type.Number (idxT);
                        IF TInt.ToInt (nTi, ni)
                          THEN tbDimExtents[d] := ni;
                          ELSE tbDimExtents[d] := 1;
                        END;
                        curr := eltT2;
                      ELSE
                        tbDimExtents[d] := 1;
                      END;
                    END;
                  END;
                END;
              END;
            END;

            (* Helper: get dim 'd' size from an OA value. When d < msirRank,
               use BuildOpenArraySize; otherwise load directly from the dope. *)

            (* Shape check: AND of (sizeA[dim] = sizeB[dim]) for each dim *)
            shapeOk := NIL;
            FOR dim := 0 TO openRank - 1 DO
              VAR sa, sb : MSIR.Value;
              BEGIN
                IF lvIsOA THEN
                  IF dim < lvMsirRank
                    THEN sa := MSIR.BuildOpenArraySize (blk, "", lv, dim);
                    ELSE sa := MSIR.BuildLoad (blk, "", intT,
                                  MSIRBuilder.BuildPtrByteOff (blk, "", heapLV,
                                                                apB + dim * intB));
                  END;
                ELSE
                  sa := MSIR.ConstInt (intT, tbDimExtents[dim]);
                END;
                IF rvIsOA THEN
                  IF dim < rvMsirRank
                    THEN sb := MSIR.BuildOpenArraySize (blk, "", rv, dim);
                    ELSE sb := MSIR.BuildLoad (blk, "", intT,
                                  MSIRBuilder.BuildPtrByteOff (blk, "", heapRV,
                                                                apB + dim * intB));
                  END;
                ELSE
                  sb := MSIR.ConstInt (intT, tbDimExtents[dim]);
                END;
                blk := MSIRBuilder.CurrentBlock ();
                VAR eq := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Eq, sa, sb);
                BEGIN
                  IF shapeOk = NIL
                    THEN shapeOk := eq;
                    ELSE shapeOk := MSIR.BuildIAnd (blk, "", shapeOk, eq);
                  END;
                END;
              END;
            END;

            (* Total element count = product of all dimension sizes *)
            IF lvIsOA THEN
              IF 0 < lvMsirRank
                THEN total := MSIR.BuildOpenArraySize (blk, "", lv, 0);
                ELSE total := MSIR.BuildLoad (blk, "", intT,
                                MSIRBuilder.BuildPtrByteOff (blk, "", heapLV, apB));
              END;
              FOR dim := 1 TO openRank - 1 DO
                VAR sd : MSIR.Value;
                BEGIN
                  IF dim < lvMsirRank
                    THEN sd := MSIR.BuildOpenArraySize (blk, "", lv, dim);
                    ELSE sd := MSIR.BuildLoad (blk, "", intT,
                                  MSIRBuilder.BuildPtrByteOff (blk, "", heapLV,
                                                                apB + dim * intB));
                  END;
                  blk := MSIRBuilder.CurrentBlock ();
                  total := MSIR.BuildIMul (blk, "", total, sd);
                END;
              END;
            ELSIF rvIsOA THEN
              IF 0 < rvMsirRank
                THEN total := MSIR.BuildOpenArraySize (blk, "", rv, 0);
                ELSE total := MSIR.BuildLoad (blk, "", intT,
                                MSIRBuilder.BuildPtrByteOff (blk, "", heapRV, apB));
              END;
              FOR dim := 1 TO openRank - 1 DO
                VAR sd : MSIR.Value;
                BEGIN
                  IF dim < rvMsirRank
                    THEN sd := MSIR.BuildOpenArraySize (blk, "", rv, dim);
                    ELSE sd := MSIR.BuildLoad (blk, "", intT,
                                  MSIRBuilder.BuildPtrByteOff (blk, "", heapRV,
                                                                apB + dim * intB));
                  END;
                  blk := MSIRBuilder.CurrentBlock ();
                  total := MSIR.BuildIMul (blk, "", total, sd);
                END;
              END;
            ELSE
              total := MSIR.ConstInt (intT, tbElts);
            END;
            blk := MSIRBuilder.CurrentBlock ();

            (* Data pointers: address of flat element[0].
               For OA: use element-address extractor with single zero (works for
               any depth since it always gives ptr+0 = data_ptr).
               For fixed: use the lvalue (slot address), retyped to ptr(elt). *)
            zero  := MSIR.ConstInt (intT, 0);
            zeros := NEW (REF ARRAY OF MSIR.Value, 1);
            zeros[0] := zero;
            IF lvIsOA THEN
              pA := MSIR.BuildOpenArrayElemAddr (blk, "", lv, zeros^);
            ELSE
              pA := Expr.LValueMSIR (p.a);
              IF pA = NIL THEN
                MSIRBuilder.Abandon ("open-array equality: fixed-array lhs has no lvalue");
                RETURN NIL;
              END;
              pA := MSIR.RetypeValue (pA, MSIR.TPtr (eltMsir));
            END;
            IF rvIsOA THEN
              pB := MSIR.BuildOpenArrayElemAddr (blk, "", rv, zeros^);
            ELSE
              pB := Expr.LValueMSIR (p.b);
              IF pB = NIL THEN
                MSIRBuilder.Abandon ("open-array equality: fixed-array rhs has no lvalue");
                RETURN NIL;
              END;
              pB := MSIR.RetypeValue (pB, MSIR.TPtr (eltMsir));
            END;
          END;

          idxSlot    := MSIR.BuildAlloca (blk, "", MSIR.TI (Target.Integer.size));
          resSlot    := MSIR.BuildAlloca (blk, "", MSIR.TI1 ());
          checkBlk   := MSIRBuilder.NewBlock ("oa.check");
          loopHdrBlk := MSIRBuilder.NewBlock ("oa.hdr");
          loopBodBlk := MSIRBuilder.NewBlock ("oa.body");
          incrBlk    := MSIRBuilder.NewBlock ("oa.incr");
          failBlk    := MSIRBuilder.NewBlock ("oa.fail");
          mergeBlk   := MSIRBuilder.NewBlock ("oa.merge");

          IF p.op = Op.EQ
            THEN eqConst  := MSIR.ConstInt (MSIR.TI1 (), 1);
                 neqConst := MSIR.ConstInt (MSIR.TI1 (), 0);
            ELSE eqConst  := MSIR.ConstInt (MSIR.TI1 (), 0);
                 neqConst := MSIR.ConstInt (MSIR.TI1 (), 1);
          END;
          MSIR.BuildStore (blk, MSIR.ConstInt (MSIR.TI (Target.Integer.size), 0), idxSlot);
          MSIR.BuildStore (blk, eqConst, resSlot);
          (* if shape ok → check empty; else → fail *)
          MSIR.BuildCondBr (blk, shapeOk,
                            checkBlk, ARRAY OF MSIR.Value{},
                            failBlk,  ARRAY OF MSIR.Value{});

          (* check empty: skip loop if total = 0 *)
          MSIRBuilder.SetCurrentBlock (checkBlk);
          isEmpty := MSIR.BuildICmp (checkBlk, "", MSIR.CmpPred.Eq,
                       total, MSIR.ConstInt (MSIR.TI (Target.Integer.size), 0));
          MSIR.BuildCondBr (checkBlk, isEmpty,
                            mergeBlk,   ARRAY OF MSIR.Value{},
                            loopHdrBlk, ARRAY OF MSIR.Value{});

          (* loop header: if idx < total → body; else → merge (all matched) *)
          MSIRBuilder.SetCurrentBlock (loopHdrBlk);
          idx0    := MSIR.BuildLoad (loopHdrBlk, "", MSIR.TI (Target.Integer.size), idxSlot);
          hdrCond := MSIR.BuildICmp (loopHdrBlk, "", MSIR.CmpPred.Slt, idx0, total);
          MSIR.BuildCondBr (loopHdrBlk, hdrCond,
                            loopBodBlk, ARRAY OF MSIR.Value{},
                            mergeBlk,   ARRAY OF MSIR.Value{});

          (* loop body: compare element at current index *)
          MSIRBuilder.SetCurrentBlock (loopBodBlk);
          idx1    := MSIR.BuildLoad (loopBodBlk, "", MSIR.TI (Target.Integer.size), idxSlot);
          ebV     := MSIR.ConstInt (MSIR.TI (Target.Integer.size), elemBytes);
          byteOff := MSIR.BuildIMul (loopBodBlk, "", idx1, ebV);
          eA      := MSIR.BuildLoad (loopBodBlk, "", eltMsir,
                       MSIR.BuildGepByte (loopBodBlk, "", pA, byteOff));
          eB      := MSIR.BuildLoad (loopBodBlk, "", eltMsir,
                       MSIR.BuildGepByte (loopBodBlk, "", pB, byteOff));
          bodCond := MSIR.BuildICmp (loopBodBlk, "", MSIR.CmpPred.Eq, eA, eB);
          MSIR.BuildCondBr (loopBodBlk, bodCond,
                            incrBlk,  ARRAY OF MSIR.Value{},
                            failBlk,  ARRAY OF MSIR.Value{});

          (* increment: advance index, loop back *)
          MSIRBuilder.SetCurrentBlock (incrBlk);
          idx2 := MSIR.BuildLoad (incrBlk, "", MSIR.TI (Target.Integer.size), idxSlot);
          nxt  := MSIR.BuildIAdd (incrBlk, "", idx2, MSIR.ConstInt (MSIR.TI (Target.Integer.size), 1));
          MSIR.BuildStore (incrBlk, nxt, idxSlot);
          MSIR.BuildBr (incrBlk, loopHdrBlk, ARRAY OF MSIR.Value{});

          (* fail: shape or element mismatch → neq result *)
          MSIRBuilder.SetCurrentBlock (failBlk);
          MSIR.BuildStore (failBlk, neqConst, resSlot);
          MSIR.BuildBr (failBlk, mergeBlk, ARRAY OF MSIR.Value{});

          (* merge: return the result *)
          MSIRBuilder.SetCurrentBlock (mergeBlk);
          RETURN MSIR.BuildLoad (mergeBlk, "", MSIR.TI1 (), resSlot);
        END;
      END;
      (* Record / fixed-array equality: byte-chunk loop over taInfo.size DIV 8 bytes. *)
      IF (taInfo.class = Type.Class.Record) OR (taInfo.class = Type.Class.Array) THEN
        VAR
          totalBytes  := taInfo.size DIV Target.Byte;
          iT          := MSIR.TI (Target.Byte);
          msirT       : MSIR.T;
          addrA, addrB: MSIR.Value;
          idxSlot     : MSIR.Value;
          resSlot     : MSIR.Value;
          loopHdrBlk  : MSIR.Block;
          loopBodBlk  : MSIR.Block;
          incrBlk     : MSIR.Block;
          failBlk     : MSIR.Block;
          mergeBlk    : MSIR.Block;
          eqConst     : MSIR.Value;
          neqConst    : MSIR.Value;
          idx0, hdrCond : MSIR.Value;
          idx1, byteOff, vA, vB, cmpVal, nxt, idx2 : MSIR.Value;
        BEGIN
          IF taInfo.size <= 0 OR taInfo.size MOD Target.Byte # 0 THEN
            MSIRBuilder.Abandon ("record equality: non-byte-sized record");
            RETURN NIL;
          END;
          addrA := Expr.LValueMSIR (p.a);
          IF addrA = NIL THEN
            VAR v := Expr.CompileMSIR (p.a); BEGIN
              IF v = NIL THEN RETURN NIL END;
              msirT := MSIRType.Translate (ta);
              blk := MSIRBuilder.CurrentBlock ();
              IF msirT = NIL THEN
                MSIRBuilder.Abandon ("record equality: lhs not translatable");
                RETURN NIL;
              END;
              addrA := MSIR.BuildAlloca (blk, "", msirT);
              MSIR.BuildStore (blk, v, addrA);
            END;
          END;
          addrB := Expr.LValueMSIR (p.b);
          IF addrB = NIL THEN
            VAR v := Expr.CompileMSIR (p.b); BEGIN
              IF v = NIL THEN RETURN NIL END;
              msirT := MSIRType.Translate (ta);
              blk := MSIRBuilder.CurrentBlock ();
              IF msirT = NIL THEN
                MSIRBuilder.Abandon ("record equality: rhs not translatable");
                RETURN NIL;
              END;
              addrB := MSIR.BuildAlloca (blk, "", msirT);
              MSIR.BuildStore (blk, v, addrB);
            END;
          END;
          blk := MSIRBuilder.CurrentBlock ();
          (* When one side is a dereferenced REF ARRAY (gc_ref<OpenArray>), the
             address points to the heap dope {data_ptr, size}, not the element data.
             Extract the data pointer by loading the first field of the dope (p034). *)
          (* Retype the gc_ref<OpenArray> to an opaque ptr before loading the
             dope's data-pointer field.  A load's result type must match the
             pointer's element type UNLESS that element is Void (the verifier's
             opaque-pointer escape).  A gc_ref whose element is the OpenArray
             dope is not Void, so a direct `load ptr void` through it fails
             verification; go through TPtr(TVoid()) to read field 0 (p034). *)
          IF MSIR.Kind (MSIR.ValueType (addrA)) = MSIR.TypeKind.GcRef AND
             MSIR.Kind (MSIR.EltType (MSIR.ValueType (addrA))) = MSIR.TypeKind.OpenArray THEN
            addrA := MSIR.BuildLoad (blk, "", MSIR.TPtr (MSIR.TVoid ()),
                       MSIR.RetypeValue (addrA, MSIR.TPtr (MSIR.TVoid ())));
          END;
          IF MSIR.Kind (MSIR.ValueType (addrB)) = MSIR.TypeKind.GcRef AND
             MSIR.Kind (MSIR.EltType (MSIR.ValueType (addrB))) = MSIR.TypeKind.OpenArray THEN
            addrB := MSIR.BuildLoad (blk, "", MSIR.TPtr (MSIR.TVoid ()),
                       MSIR.RetypeValue (addrB, MSIR.TPtr (MSIR.TVoid ())));
          END;
          blk := MSIRBuilder.CurrentBlock ();
          idxSlot    := MSIR.BuildAlloca (blk, "", MSIR.TI (Target.Integer.size));
          resSlot    := MSIR.BuildAlloca (blk, "", MSIR.TI1 ());
          loopHdrBlk := MSIRBuilder.NewBlock ("rec.eq.hdr");
          loopBodBlk := MSIRBuilder.NewBlock ("rec.eq.body");
          incrBlk    := MSIRBuilder.NewBlock ("rec.eq.incr");
          failBlk    := MSIRBuilder.NewBlock ("rec.eq.fail");
          mergeBlk   := MSIRBuilder.NewBlock ("rec.eq.merge");
          CASE p.op OF
          | Op.EQ => eqConst  := MSIR.ConstInt (MSIR.TI1 (), 1);
                     neqConst := MSIR.ConstInt (MSIR.TI1 (), 0);
          | Op.NE => eqConst  := MSIR.ConstInt (MSIR.TI1 (), 0);
                     neqConst := MSIR.ConstInt (MSIR.TI1 (), 1);
          END;
          MSIR.BuildStore (blk, MSIR.ConstInt (MSIR.TI (Target.Integer.size), 0), idxSlot);
          MSIR.BuildStore (blk, eqConst, resSlot);
          MSIR.BuildBr (blk, loopHdrBlk, ARRAY OF MSIR.Value{});
          MSIRBuilder.SetCurrentBlock (loopHdrBlk);
          idx0 := MSIR.BuildLoad (loopHdrBlk, "", MSIR.TI (Target.Integer.size), idxSlot);
          hdrCond := MSIR.BuildICmp (loopHdrBlk, "", MSIR.CmpPred.Slt, idx0,
                       MSIR.ConstInt (MSIR.TI (Target.Integer.size), totalBytes));
          MSIR.BuildCondBr (loopHdrBlk, hdrCond,
                            loopBodBlk, ARRAY OF MSIR.Value{},
                            mergeBlk,   ARRAY OF MSIR.Value{});
          MSIRBuilder.SetCurrentBlock (loopBodBlk);
          idx1    := MSIR.BuildLoad (loopBodBlk, "", MSIR.TI (Target.Integer.size), idxSlot);
          byteOff := idx1;
          vA      := MSIR.BuildLoad (loopBodBlk, "", iT,
                       MSIR.BuildGepByte (loopBodBlk, "", addrA, byteOff));
          vB      := MSIR.BuildLoad (loopBodBlk, "", iT,
                       MSIR.BuildGepByte (loopBodBlk, "", addrB, byteOff));
          cmpVal  := MSIR.BuildICmp (loopBodBlk, "", MSIR.CmpPred.Eq, vA, vB);
          MSIR.BuildCondBr (loopBodBlk, cmpVal,
                            incrBlk, ARRAY OF MSIR.Value{},
                            failBlk, ARRAY OF MSIR.Value{});
          MSIRBuilder.SetCurrentBlock (incrBlk);
          idx2 := MSIR.BuildLoad (incrBlk, "", MSIR.TI (Target.Integer.size), idxSlot);
          nxt  := MSIR.BuildIAdd (incrBlk, "", idx2,
                    MSIR.ConstInt (MSIR.TI (Target.Integer.size), 1));
          MSIR.BuildStore (incrBlk, nxt, idxSlot);
          MSIR.BuildBr (incrBlk, loopHdrBlk, ARRAY OF MSIR.Value{});
          MSIRBuilder.SetCurrentBlock (failBlk);
          MSIR.BuildStore (failBlk, neqConst, resSlot);
          MSIR.BuildBr (failBlk, mergeBlk, ARRAY OF MSIR.Value{});
          MSIRBuilder.SetCurrentBlock (mergeBlk);
          RETURN MSIR.BuildLoad (mergeBlk, "", MSIR.TI1 (), resSlot);
        END;
      END;
      (* Procedure equality: both sides are function pointer values. *)
      IF taInfo.class # Type.Class.Procedure THEN
        MSIRBuilder.Abandon ("non-scalar equality not supported in MSIR v0");
        RETURN NIL;
      END;
      lv := Expr.CompileMSIR (p.a);  IF lv = NIL THEN RETURN NIL END;
      rv := Expr.CompileMSIR (p.b);  IF rv = NIL THEN RETURN NIL END;
      blk := MSIRBuilder.CurrentBlock ();
      (* Coerce nil constant to match procedure pointer type (e.g. ptr void vs gc_ref void). *)
      IF MSIR.GetValueKind (lv) = MSIR.ValueKind.ConstNil AND
         NOT MSIR.Equal (MSIR.ValueType (lv), MSIR.ValueType (rv)) THEN
        lv := MSIR.RetypeValue (lv, MSIR.ValueType (rv));
      ELSIF MSIR.GetValueKind (rv) = MSIR.ValueKind.ConstNil AND
            NOT MSIR.Equal (MSIR.ValueType (rv), MSIR.ValueType (lv)) THEN
        rv := MSIR.RetypeValue (rv, MSIR.ValueType (lv));
      END;
      CASE p.op OF
      | Op.EQ => pred := MSIR.CmpPred.Eq;
      | Op.NE => pred := MSIR.CmpPred.Ne;
      END;
      (* For procedure equality involving nested procedure values: normalize both
         sides to their "shim pointer" representation.  Nested procs may appear
         either as direct ConstProc refs (shim pointer, for non/peer-capturing
         procs compiled without access to captures) or as runtime closure structs
         {CL_marker=-1, shim, env} (when captures ARE accessible).  The tag check
         (load word-0, compare with -1) detects closures and extracts their shim.
         We apply this normalization when:
           (a) at least one side is a ConstProc (direct shim ref), OR
           (b) both sides are non-NIL non-ConstProc values of procedure type —
               i.e., both may be runtime closure structs for the same proc.
         We skip normalization when either side is ConstNil (nil proc check). *)
      IF (MSIR.GetValueKind (lv) = MSIR.ValueKind.ConstProc OR
          MSIR.GetValueKind (rv) = MSIR.ValueKind.ConstProc) OR
         (MSIR.GetValueKind (lv) # MSIR.ValueKind.ConstNil AND
          MSIR.GetValueKind (rv) # MSIR.ValueKind.ConstNil AND
          MSIR.GetValueKind (lv) # MSIR.ValueKind.ConstProc AND
          MSIR.GetValueKind (rv) # MSIR.ValueKind.ConstProc) THEN
        VAR intT    := MSIR.TI (Target.Integer.size);
            ptrT    := MSIR.TPtr (MSIR.TVoid ());
            IPb     := Target.Integer.bytes;
            markerV := MSIR.ConstInt (intT, M3RT.CL_marker_value);
        BEGIN
          blk := MSIRBuilder.CurrentBlock ();
          VAR nilV := MSIR.ConstNil (ptrT);
              (* Safe dummy: a small alloca that will never contain CL_marker.
                 When lv/rv is nil, we use this as the safe load address so
                 the tag read won't SIGSEGV, and the tag value won't be -1
                 (the alloca is zero-initialized, so tag = 0 ≠ -1). *)
              dummyAlloca := MSIR.BuildAlloca (blk, "", intT);
          BEGIN
            MSIR.BuildStore (blk, MSIR.ConstInt (intT, 0), dummyAlloca);
            IF MSIR.GetValueKind (lv) # MSIR.ValueKind.ConstProc THEN
              VAR isNilL := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Eq, lv, nilV);
                  (* If lv is nil, use dummyAlloca as safe base; else use lv *)
                  safeL  := MSIR.BuildSelect (blk, "", isNilL, dummyAlloca, lv);
                  tagL   := MSIR.BuildLoad (blk, "", intT,
                              MSIR.BuildGepByte (blk, "", safeL, MSIR.ConstInt (intT, 0)));
                  (* Closure iff tag == -1 AND lv is non-nil *)
                  tagEqL := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Eq, tagL, markerV);
                  isClL  := MSIR.BuildIAnd (blk, "", tagEqL,
                              MSIR.BuildICmp (blk, "", MSIR.CmpPred.Ne, lv, nilV));
                  shimL  := MSIR.BuildLoad (blk, "", ptrT,
                              MSIR.BuildGepByte (blk, "", safeL, MSIR.ConstInt (intT, IPb)));
              BEGIN
                lv := MSIR.BuildSelect (blk, "", isClL, shimL, lv);
              END;
            END;
            blk := MSIRBuilder.CurrentBlock ();
            IF MSIR.GetValueKind (rv) # MSIR.ValueKind.ConstProc THEN
              VAR isNilR := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Eq, rv, nilV);
                  safeR  := MSIR.BuildSelect (blk, "", isNilR, dummyAlloca, rv);
                  tagR   := MSIR.BuildLoad (blk, "", intT,
                              MSIR.BuildGepByte (blk, "", safeR, MSIR.ConstInt (intT, 0)));
                  tagEqR := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Eq, tagR, markerV);
                  isClR  := MSIR.BuildIAnd (blk, "", tagEqR,
                              MSIR.BuildICmp (blk, "", MSIR.CmpPred.Ne, rv, nilV));
                  shimR  := MSIR.BuildLoad (blk, "", ptrT,
                              MSIR.BuildGepByte (blk, "", safeR, MSIR.ConstInt (intT, IPb)));
              BEGIN
                rv := MSIR.BuildSelect (blk, "", isClR, shimR, rv);
              END;
            END;
            blk := MSIRBuilder.CurrentBlock ();
            (* Ensure both sides have the same type for icmp.  A ConstProc may
               have a proc-specific function-pointer type while the normalized
               side is ptr void.  Retype both to ptr void before comparing. *)
            IF NOT MSIR.Equal (MSIR.ValueType (lv), MSIR.ValueType (rv)) THEN
              IF MSIR.Kind (MSIR.ValueType (lv)) # MSIR.TypeKind.Void THEN
                lv := MSIR.RetypeValue (lv, ptrT);
              END;
              IF MSIR.Kind (MSIR.ValueType (rv)) # MSIR.TypeKind.Void THEN
                rv := MSIR.RetypeValue (rv, ptrT);
              END;
            END;
            RETURN MSIR.BuildICmp (blk, "", pred, lv, rv);
          END;
        END;
      END;
      RETURN MSIR.BuildICmp (blk, "", pred, lv, rv);
    ELSIF p.kind # Kind.SimpleScalar THEN
      (* Multi-word SETs map to wide integers (iN) in MSIR — compare with icmp. *)
      ta := Type.Base (Expr.TypeOf (p.a));
      EVAL Type.CheckInfo (ta, taInfo);
      IF taInfo.class = Type.Class.Set THEN
        lv := Expr.CompileMSIR (p.a);  IF lv = NIL THEN RETURN NIL END;
        rv := Expr.CompileMSIR (p.b);  IF rv = NIL THEN RETURN NIL END;
        blk := MSIRBuilder.CurrentBlock ();
        CASE p.op OF
        | Op.EQ => pred := MSIR.CmpPred.Eq;
        | Op.NE => pred := MSIR.CmpPred.Ne;
        END;
        RETURN MSIR.BuildICmp (blk, "", pred, lv, rv);
      END;
      (* SimpleStruct: load each chunk as integer and compare chunk-by-chunk. *)
      VAR cmpT: CG.Type;  chunkBits, nChunks: INTEGER;
          iT: MSIR.T;  addrA, addrB, cmpVal, result: MSIR.Value;
          msirT: MSIR.T;
      BEGIN
        cmpT := FindCompareType (taInfo.size, taInfo.alignment);
        IF cmpT = CG.Type.Void THEN
          MSIRBuilder.Abandon ("non-scalar equality: no compare chunk type");
          RETURN NIL;
        END;
        chunkBits := TargetMap.CG_Size [cmpT];
        nChunks   := taInfo.size DIV chunkBits;
        iT  := MSIR.TI (chunkBits);
        blk := MSIRBuilder.CurrentBlock ();
        CASE p.op OF
        | Op.EQ => pred := MSIR.CmpPred.Eq;
        | Op.NE => pred := MSIR.CmpPred.Ne;
        END;
        addrA := Expr.LValueMSIR (p.a);
        IF addrA = NIL THEN
          VAR v := Expr.CompileMSIR (p.a); BEGIN
            IF v = NIL THEN RETURN NIL END;
            msirT := MSIRType.Translate (ta);
            blk := MSIRBuilder.CurrentBlock ();
            IF msirT = NIL THEN
              MSIRBuilder.Abandon ("non-scalar equality: lhs not translatable");
              RETURN NIL;
            END;
            addrA := MSIR.BuildAlloca (blk, "", msirT);
            MSIR.BuildStore (blk, v, addrA);
          END;
        END;
        addrB := Expr.LValueMSIR (p.b);
        IF addrB = NIL THEN
          VAR v := Expr.CompileMSIR (p.b); BEGIN
            IF v = NIL THEN RETURN NIL END;
            msirT := MSIRType.Translate (ta);
            blk := MSIRBuilder.CurrentBlock ();
            IF msirT = NIL THEN
              MSIRBuilder.Abandon ("non-scalar equality: rhs not translatable");
              RETURN NIL;
            END;
            addrB := MSIR.BuildAlloca (blk, "", msirT);
            MSIR.BuildStore (blk, v, addrB);
          END;
        END;
        blk := MSIRBuilder.CurrentBlock ();
        result := NIL;
        FOR i := 0 TO nChunks - 1 DO
          VAR byteOff := i * chunkBits DIV Target.Byte;
              pA := MSIRBuilder.BuildPtrByteOff (blk, "", addrA, byteOff);
              pB := MSIRBuilder.BuildPtrByteOff (blk, "", addrB, byteOff);
              vA := MSIR.BuildLoad   (blk, "", iT, pA);
              vB := MSIR.BuildLoad   (blk, "", iT, pB);
          BEGIN
            cmpVal := MSIR.BuildICmp (blk, "", pred, vA, vB);
            IF result = NIL THEN
              result := cmpVal;
            ELSIF p.op = Op.EQ THEN
              result := MSIR.BuildIAnd (blk, "", result, cmpVal);
            ELSE
              result := MSIR.BuildIOr  (blk, "", result, cmpVal);
            END;
          END;
        END;
        IF result = NIL THEN
          MSIRBuilder.Abandon ("non-scalar equality: zero chunks");
          RETURN NIL;
        END;
        RETURN result;
      END;
    END;
    lv := Expr.CompileMSIR (p.a);  IF lv = NIL THEN RETURN NIL END;
    rv := Expr.CompileMSIR (p.b);  IF rv = NIL THEN RETURN NIL END;
    blk := MSIRBuilder.CurrentBlock ();
    lvKind := MSIR.Kind (MSIR.ValueType (lv));
    IF (lvKind = MSIR.TypeKind.F32) OR (lvKind = MSIR.TypeKind.F64) THEN
      CASE p.op OF
      | Op.EQ => fpred := MSIR.FCmpPred.OEq;
      | Op.NE => fpred := MSIR.FCmpPred.ONe;
      END;
      RETURN MSIR.BuildFCmp (blk, "", fpred, lv, rv);
    ELSE
      (* Coerce nil constant to match the other operand's type (e.g. ptr void vs gc_ref void). *)
      IF MSIR.GetValueKind (lv) = MSIR.ValueKind.ConstNil AND
         NOT MSIR.Equal (MSIR.ValueType (lv), MSIR.ValueType (rv)) THEN
        lv := MSIR.RetypeValue (lv, MSIR.ValueType (rv));
      ELSIF MSIR.GetValueKind (rv) = MSIR.ValueKind.ConstNil AND
            NOT MSIR.Equal (MSIR.ValueType (rv), MSIR.ValueType (lv)) THEN
        rv := MSIR.RetypeValue (rv, MSIR.ValueType (lv));
      END;
      (* Operands are ZType (machine width) — comparison/IN/boolean results are
         now widened to i64 by their CompileMSIR wrappers, so a BOOLEAN operand
         is i64 like a BOOLEAN variable; no reconciliation needed (nil-constant
         retype above handles pointer/ref equality). *)
      CASE p.op OF
      | Op.EQ => pred := MSIR.CmpPred.Eq;
      | Op.NE => pred := MSIR.CmpPred.Ne;
      END;
      RETURN MSIR.BuildICmp (blk, "", pred, lv, rv);
    END;
  END CompileMSIRRaw;

BEGIN
END EqualExpr.
