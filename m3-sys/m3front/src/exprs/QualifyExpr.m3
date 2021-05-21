(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: QualifyExpr.m3                                        *)
(* Last modified on Fri Feb 24 16:45:51 PST 1995 by kalsow     *)
(*      modified on Tue Feb 19 01:32:23 1991 by muller         *)

MODULE QualifyExpr;

IMPORT M3, M3ID, CG, Expr, ExprRep, Value, Type, Module;
IMPORT RecordType, ObjectType, Variable, VarExpr, Scope;
IMPORT EnumType, RefType, DerefExpr, NamedExpr, Error, ProcType;
IMPORT ErrType, RecordExpr, TypeExpr, MethodExpr, ProcExpr;
IMPORT Method, Field, Target, M3RT, Host, RunTyme;

TYPE
  Class = { cMODULE, cENUM, cOBJTYPE, cFIELD, cOBJFIELD, cMETHOD, cUNKNOWN };

TYPE
  VC = Value.Class;

TYPE
  P = Expr.T BRANDED "QualifyExpr.T" OBJECT
        expr        : Expr.T;
        obj         : Value.T;
        holder      : Type.T; (* Visible supertype of the Q-expr. *) 
        objType     : Type.T;
        temp        : CG.Val;
        name        : M3ID.T;
        class       : Class;
        inFold      : BOOLEAN;
        inIsZeroes  : BOOLEAN;
        inGetBounds : BOOLEAN;
        inTypeOf    : BOOLEAN;
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := RepTypeOf;
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
        isZeroes     := IsZeroes;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
        exprAlign    := QualifyExprAlign;
      END;

PROCEDURE New (a: Expr.T;  id: M3ID.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.expr        := a;
    p.name        := id;
    p.obj         := NIL;
    p.class       := Class.cUNKNOWN;
    p.holder      := NIL;
    p.objType     := NIL;
    p.inFold      := FALSE;
    p.inIsZeroes  := FALSE;
    p.inGetBounds := FALSE;
    p.inTypeOf    := FALSE;
    RETURN p;
  END New;

PROCEDURE Split (e: Expr.T; VAR obj: Value.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => Resolve (p); obj := p.obj; RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE SplitQID (e: Expr.T;  VAR module, item: M3ID.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => IF NamedExpr.SplitName (p.expr, module)
                 THEN item := p.name; RETURN TRUE;
                 ELSE RETURN FALSE;
              END;
    ELSE      RETURN FALSE;
    END;
  END SplitQID;

PROCEDURE PassObject (e: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => (* nothing *)
    | P(p) => IF (p.class = Class.cMETHOD) THEN
                CG.Push (p.temp);
                CG.Pop_param (CG.Type.Addr);
                CG.Free (p.temp);
                p.temp := NIL;
		RETURN TRUE;
              END;
    ELSE      (* nothing *)
    END;
    RETURN FALSE;
  END PassObject;

PROCEDURE MethodType (e: Expr.T): Type.T =
  BEGIN
    TYPECASE e OF
    | NULL => (* nothing *)
    | P(p) => Resolve (p);
              IF (p.class = Class.cMETHOD) THEN RETURN Value.TypeOf(p.obj) END;
    ELSE      (* nothing *)
    END;
    RETURN NIL;
  END MethodType;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    Resolve (p);
    IF (p.inGetBounds) THEN Value.IllegalRecursion (p.obj) END;
    p.inGetBounds := TRUE;
    CASE Value.ClassOf (p.obj) OF
    | Value.Class.Expr => Expr.GetBounds (Value.ToExpr (p.obj), min, max);
    | Value.Class.Var  => Variable.GetBounds (p.obj, min, max);
    ELSE                  EVAL Type.GetBounds (p.type, min, max);
    END;
    p.inGetBounds := FALSE;
  END Bounder;

PROCEDURE MakeDummy (p: P) =
  BEGIN
    p.class := Class.cMODULE;
    p.obj   := VarExpr.Obj (VarExpr.New (ErrType.T, p.name));
  END MakeDummy;

PROCEDURE Resolve (p: P) =
  VAR
    t      : Type.T;
    base_t : Type.T;
    s      : Scope.T;
    obj    : Value.T;
    name   : M3ID.T;
  BEGIN
    IF (p.class # Class.cUNKNOWN) THEN RETURN END;

    t := Expr.TypeOf (p.expr);

    IF RefType.Is (t) THEN
      (* auto-magic dereference *)
      p.expr := DerefExpr.New (p.expr);
      p.expr.origin := p.origin;
      t := Expr.TypeOf (p.expr);
    END;

    p.holder := t;
    p.obj := NIL;
    base_t := Type.Base (t);

    IF (t = ErrType.T) THEN
      (* the lhs already contains an error => silently make it look like
         everything is ok. *)
      MakeDummy (p);

    ELSIF (t = NIL) THEN
      (* a module or type *)
      IF TypeExpr.Split (p.expr, t) THEN
        IF EnumType.LookUp (t, p.name, p.obj) THEN
          p.class := Class.cENUM;
        ELSIF ObjectType.LookUp (t, p.name, p.obj, p.holder) THEN
          p.objType := t;
          p.class := Class.cOBJTYPE;
        END;
      ELSIF NamedExpr.Split (p.expr, name, obj) THEN
        IF (Value.ClassOf (obj) = VC.Module) THEN
          p.class := Class.cMODULE;
          s := Module.ExportScope (Value.Base (obj));
          p.obj := Scope.LookUp (s, p.name, TRUE);
        END;
      END;

    ELSIF RecordType.LookUp (base_t, p.name, p.obj) THEN
      p.class := Class.cFIELD;

    ELSIF ObjectType.LookUp (base_t, p.name, p.obj, p.holder) THEN
      IF (Value.ClassOf (p.obj) = VC.Field)
        THEN p.class := Class.cOBJFIELD;
        ELSE p.class := Class.cMETHOD;
      END;
    END;
  END Resolve;

PROCEDURE ResolveTypes (p: P) =
  BEGIN
    Resolve (p);
    IF (p.inTypeOf) THEN
      Value.IllegalRecursion (p.obj);
      p.type := ErrType.T;
      p.repType := p.type;
    ELSE
      p.inTypeOf := TRUE;
      p.type := Value.TypeOf (p.obj);
      IF p.type = ErrType.T THEN p.repType := ErrType.T;
      ELSIF p.class = Class.cMETHOD THEN
        p.type := NIL;
        p.repType := NIL;
      ELSIF p.class = Class.cOBJTYPE THEN
        p.type := ProcType.MethodSigAsProcSig (p.type, p.objType);
        p.repType := p.type;
      ELSE p.repType := Value.RepTypeOf (p.obj);
      END;
    END;
    p.inTypeOf := FALSE;
  END ResolveTypes;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    ResolveTypes (p);
    RETURN p.type;
  END TypeOf;

PROCEDURE RepTypeOf (p: P): Type.T =
  BEGIN
    ResolveTypes (p);
    RETURN p.repType;
  END RepTypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR nErrs0, nErrs1, nWarns: INTEGER;  info: Type.Info;
  BEGIN
    Error.Count (nErrs0, nWarns);
      Expr.TypeCheck (p.expr, cs);
      Resolve (p);
      Expr.TypeCheck (p.expr, cs);
    Error.Count (nErrs1, nWarns);

    IF (p.obj = NIL) THEN
      IF (nErrs0 = nErrs1) THEN
        Error.ID (p.name, "unknown qualification \'.\'");
      END;
      MakeDummy (p);
    ELSIF (p.class = Class.cFIELD) THEN
      EVAL Type.CheckInfo (p.holder, info);
      DerefExpr.SetOffset (p.expr, info.size);
    ELSIF (p.class = Class.cOBJTYPE)
      AND (Value.ClassOf (p.obj) # VC.Method) THEN
      Error.ID (p.name, "doesn\'t name a method");
    END;

    Value.TypeCheck (p.obj, cs);
    EVAL TypeOf (p);
    IF (p.type # NIL) THEN
      p.type := Type.Check (p.type);
    END;
  END Check;

PROCEDURE QualifyExprAlign (p: P): Type.BitAlignT =
  VAR fieldInfo: Field.Info;
  VAR offset: INTEGER;
  VAR objType: Type.T;
  VAR objTypeInfo: Type.Info;
  BEGIN
    CASE p.class
    OF Class.cFIELD, Class.cOBJFIELD =>
        Field.Split (p.obj, fieldInfo);
        offset := fieldInfo.offset MOD Target.Word.size;
        RETURN CG.GCD (Expr.Alignment (p.expr), offset); 
    | Class.cMETHOD => RETURN Target.Address.align;
    ELSE
      objType := Value.TypeOf (p.obj);
      EVAL Type.CheckInfo (objType, objTypeInfo);
      RETURN objTypeInfo.alignment; 
    END (*CASE*)
  END QualifyExprAlign; 

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => Resolve (a);  Resolve (b);
              RETURN (a.obj = b.obj)
                 AND (a.class = b.class)
                 AND Expr.IsEqual (a.expr, b.expr, x);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  VAR c: Value.Class;
  BEGIN
    CASE p.class OF
    | Class.cMODULE =>
        c := Value.ClassOf (p.obj);
        IF (c = Value.Class.Var) THEN
          Variable.NeedsAddress (p.obj);
        ELSIF (c = Value.Class.Expr) THEN
          Expr.NeedsAddress (Value.ToExpr (p.obj));
        END;
    | Class.cFIELD =>
        Expr.NeedsAddress (p.expr);
    | Class.cOBJFIELD =>
        (* ok, all objects have addresses *)
    | Class.cENUM,
      Class.cOBJTYPE,
      Class.cMETHOD,
      Class.cUNKNOWN =>
        <* ASSERT FALSE *>
    END;
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  VAR
    field: Field.Info;
    info: Type.Info;
  BEGIN
    CASE p.class OF
    | Class.cMODULE =>
        IF Host.doIncGC AND Value.ClassOf (p.obj) = Value.Class.Var THEN
          EVAL Type.CheckInfo (p.type, info);
          IF info.isTraced THEN
            CASE info.class OF 
            | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
              Variable.Load (p.obj);
              RunTyme.EmitCheckLoadTracedRef ();
              p.temp := CG.Pop ();
            ELSE
              (* no check *)
            END
          END
        END
    | Class.cENUM =>
        (* skip *)
    | Class.cOBJTYPE =>
        (* skip *)
    | Class.cFIELD =>
        IF Expr.IsDesignator (p.expr)
        THEN Expr.PrepLValue (p.expr, traced := FALSE);
        ELSE
          EVAL Expr.CheckUseFailure (p.expr);
          Expr.Prep (p.expr);
        END;
        Field.Split (p.obj, field);
        EVAL Type.CheckInfo (field.type, info);
        IF Host.doIncGC AND info.isTraced THEN
          CASE info.class OF
          | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
            Compile (p);
            RunTyme.EmitCheckLoadTracedRef ();
            p.temp := CG.Pop ();
          ELSE
            (* no check *)
          END
        END
    | Class.cOBJFIELD =>
        Expr.Prep (p.expr);
        Field.Split (p.obj, field);
        EVAL Type.CheckInfo (field.type, info);
        IF Host.doIncGC AND info.isTraced THEN
          CASE info.class OF
          | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
            Compile (p);
            RunTyme.EmitCheckLoadTracedRef ();
            p.temp := CG.Pop ();
          ELSE
            (* no check *)
          END
        END
    | Class.cMETHOD =>
        Expr.Prep (p.expr);
        Expr.Compile (p.expr);
        p.temp := CG.Pop ();
    | Class.cUNKNOWN =>
        <* ASSERT FALSE *>
    END;
  END Prep;

PROCEDURE Compile (p: P) =
  VAR
    obj_offset, obj_align: INTEGER;
    field: Field.Info;
    method: Method.Info;
  BEGIN
    CASE p.class OF
    | Class.cMODULE =>
        IF p.temp # NIL THEN
          CG.Push (p.temp);
          CG.Free (p.temp);
          p.temp := NIL;
          RETURN;
        END;
        (* Do we need to Compile p.obj, if it is a constant? *)
        Value.Load (p.obj);
    | Class.cENUM =>
        Value.Load (p.obj);
    | Class.cOBJTYPE =>
        Type.Compile (p.holder);
        Type.Compile (p.objType);
        Method.SplitX (p.obj, method);
        Type.LoadInfo (p.objType, M3RT.OTC_defaultMethods, addr := TRUE);
        obj_offset := ObjectType.MethodOffset (p.holder);
        IF (obj_offset >= 0) THEN
          INC (method.offset, obj_offset);
        ELSE
          Type.LoadInfo (p.holder, M3RT.OTC_methodOffset);
          CG.Index_bytes (Target.Byte);
        END;
        CG.Boost_addr_alignment (Target.Address.align);
        CG.Load_indirect (CG.Type.Addr, method.offset, Target.Address.size);
        CG.Boost_addr_alignment (Target.Address.align);
    | Class.cFIELD =>
        IF p.temp # NIL THEN
          CG.Push (p.temp);
          CG.Free (p.temp);
          p.temp := NIL;
          RETURN;
        END;
        Field.Split (p.obj, field);
        IF Expr.IsDesignator (p.expr)
          THEN Expr.CompileLValue (p.expr, traced := FALSE);
          ELSE Expr.Compile (p.expr);
        END;
        CG.Add_offset (field.offset);
        Type.LoadScalar (field.type);
    | Class.cOBJFIELD =>
        IF p.temp # NIL THEN
          CG.Push (p.temp);
          CG.Free (p.temp);
          p.temp := NIL;
          RETURN;
        END;
        Field.Split (p.obj, field);
        Expr.Compile (p.expr);
        CG.Boost_addr_alignment (Target.Address.align);
        ObjectType.GetFieldOffset (p.holder, obj_offset, obj_align);
        IF (obj_offset >= 0) THEN
          INC (field.offset, obj_offset);
        ELSE
          CG.Check_nil (CG.RuntimeError.BadMemoryReference);
          Type.LoadInfo (p.holder, M3RT.OTC_dataOffset);
          CG.Index_bytes (Target.Byte);
        END;
        CG.Add_offset (field.offset);
        CG.Boost_addr_alignment (obj_align);
        Type.LoadScalar (field.type);
    | Class.cMETHOD =>
        Method.SplitX (p.obj, method);
        CG.Push (p.temp);
        CG.Boost_addr_alignment (Target.Address.align);
        CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);
        CG.Boost_addr_alignment (Target.Address.align);
        obj_offset := ObjectType.MethodOffset (p.holder);
        IF (obj_offset >= 0) THEN
          INC (method.offset, obj_offset);
        ELSE
          Type.LoadInfo (p.holder, M3RT.OTC_methodOffset);
          CG.Index_bytes (Target.Byte);
        END;
        CG.Boost_addr_alignment (Target.Address.align);
        CG.Load_indirect (CG.Type.Addr, method.offset, Target.Address.size);
        CG.Boost_addr_alignment (Target.Address.align);
    | Class.cUNKNOWN =>
        <* ASSERT FALSE *>
    END;
 END Compile;

PROCEDURE PrepLV (p: P; traced: BOOLEAN) =
  VAR info: Type.Info;
  BEGIN
    CASE p.class OF
    | Class.cMODULE, Class.cENUM =>
        (* skip *)
    | Class.cOBJTYPE =>
        (* skip *)
    | Class.cFIELD =>
        IF Expr.IsDesignator (p.expr)
        THEN Expr.PrepLValue (p.expr, traced);
        ELSE
          EVAL Expr.CheckUseFailure (p.expr);
          Expr.Prep (p.expr);
        END;
    | Class.cOBJFIELD =>
        Expr.Prep (p.expr);
        IF traced AND Host.doGenGC THEN
          EVAL Type.CheckInfo (p.type, info);
          IF NOT info.isTraced THEN RETURN END;
          EVAL Type.CheckInfo (Expr.TypeOf (p.expr), info);
          IF NOT info.isTraced THEN RETURN END;
          Expr.Compile (p.expr);
          RunTyme.EmitCheckStoreTraced ();
          p.temp := CG.Pop ();
        END;
    | Class.cMETHOD =>
        Expr.Prep (p.expr);
        Expr.Compile (p.expr);
        p.temp := CG.Pop ();
    | Class.cUNKNOWN =>
        <* ASSERT FALSE *>
    END;
  END PrepLV;

PROCEDURE CompileLV (p: P;  traced: BOOLEAN) =
  VAR obj_offset, obj_align: INTEGER;  field: Field.Info;
  BEGIN
    CASE p.class OF
    | Class.cMODULE =>
        CASE Value.ClassOf (p.obj) OF
        | Value.Class.Expr => Value.Load (p.obj);
        | Value.Class.Var  => Variable.LoadLValue (p.obj);
        ELSE <*ASSERT FALSE*>
        END;
    | Class.cFIELD =>
        Field.Split (p.obj, field);
        Expr.CompileLValue (p.expr, traced);
        CG.Add_offset (field.offset);
    | Class.cOBJFIELD =>
        Field.Split (p.obj, field);
        IF p.temp # NIL THEN
          <*ASSERT traced*>
          CG.Push (p.temp);
          CG.Free (p.temp);
          p.temp := NIL;
        ELSE
          Expr.Compile (p.expr);
        END;
        ObjectType.GetFieldOffset (p.holder, obj_offset, obj_align);
        IF (obj_offset >= 0) THEN
          INC (field.offset, obj_offset);
        ELSE
          CG.Check_nil (CG.RuntimeError.BadMemoryReference);
          Type.LoadInfo (p.holder, M3RT.OTC_dataOffset);
          CG.Index_bytes (Target.Byte);
        END;
        CG.Add_offset (field.offset);
        CG.Boost_addr_alignment (obj_align);
    | Class.cENUM,
      Class.cOBJTYPE,
      Class.cMETHOD,
      Class.cUNKNOWN =>
        <* ASSERT FALSE *>
    END;
 END CompileLV;

TYPE
  Kind = {Value, Expr, Type, None};
  LHS = RECORD
          kind  : Kind;
          value : Value.T;
          expr  : Expr.T;
          type  : Type.T;
        END;

PROCEDURE Fold (p: P): Expr.T =
  VAR lhs: LHS;  e: Expr.T;
  BEGIN
    IF (p.inFold) THEN Value.IllegalRecursion (p.obj); RETURN NIL END;
    p.inFold := TRUE;

    (* evaluate the qualified expression *)
    lhs.kind := Kind.Expr;
    lhs.expr := p.expr;
    DoQualify (lhs, p.name);

    (* finally, simplify the result to an Expr.T if possible *)
    CASE lhs.kind OF
    | Kind.None =>
        e := NIL;
    | Kind.Expr =>
        e := Expr.ConstValue (lhs.expr);
    | Kind.Type =>
        e := TypeExpr.New (lhs.type);
    | Kind.Value =>
        CASE Value.ClassOf (lhs.value) OF
        | VC.Expr =>
            e := Expr.ConstValue (Value.ToExpr (lhs.value));
        | VC.Type =>
            e := NIL; (* TypeExpr.New (Value.ToType (lhs.value));*)
        | VC.Procedure =>
            e := ProcExpr.New (lhs.value);
            (* lhs.value is a procedure *)
        ELSE (* not possible to convert to an expression *)
            e := NIL;
        END;
    END;

    p.inFold := FALSE;
    RETURN e;
  END Fold;

PROCEDURE DoQualify (VAR lhs: LHS;  name: M3ID.T) =
  VAR
    e: Expr.T;
    v: Value.T;
    p: P;
    s: Scope.T;
    t, t1: Type.T;
    n: M3ID.T;
  BEGIN
    CASE lhs.kind OF
    | Kind.None =>
        (* don't even try *)
    | Kind.Expr =>
        IF lhs.expr = NIL THEN
          lhs.kind := Kind.None; (*FINAL*)
        ELSIF (TYPECODE (lhs.expr) = TYPECODE (P)) THEN
          p := lhs.expr;
          lhs.kind  := Kind.Expr;
          lhs.expr  := p.expr;
          DoQualify (lhs, p.name);
          DoQualify (lhs, name);
        ELSIF TypeExpr.Split (lhs.expr, t) THEN
          lhs.kind  := Kind.Type;
          lhs.type  := t;
          DoQualify (lhs, name);
        ELSIF NamedExpr.Split (lhs.expr, n, v) THEN
          lhs.kind  := Kind.Value;
          lhs.value := v;
          DoQualify (lhs, name);
        ELSIF RecordExpr.Qualify (lhs.expr, name, e) THEN
          lhs.kind  := Kind.Expr;  (*FINAL*)
          lhs.expr  := e;
        ELSE
          e := Expr.ConstValue (lhs.expr);
          IF (e # lhs.expr) THEN
            (* try qualifying the constant value *)
            lhs.kind  := Kind.Expr;
            lhs.expr  := Expr.ConstValue (lhs.expr);
            DoQualify (lhs, name);
          ELSE
            lhs.kind := Kind.None; (*FINAL*)
          END;
        END;
    | Kind.Type =>
        t := Type.Strip (lhs.type);
        IF EnumType.LookUp (t, name, v) THEN
          lhs.kind  := Kind.Expr;  (*FINAL*)
          lhs.expr  := Value.ToExpr (v);
        ELSIF ObjectType.LookUp (t, name, v, t1)
          AND (Value.ClassOf (v) = VC.Method) THEN
          lhs.kind  := Kind.Expr;  (*FINAL*)
          lhs.expr  := MethodExpr.New (t, name, v, t1);
        ELSE (* type that can't be qualified *)
          lhs.kind  := Kind.None;  (*FINAL*)
        END;
    | Kind.Value =>
        CASE Value.ClassOf (lhs.value) OF
        | VC.Expr =>
            lhs.kind  := Kind.Expr;
            lhs.expr  := Value.ToExpr (lhs.value);
            DoQualify (lhs, name);
        | VC.Type =>
            lhs.kind  := Kind.Type;
            lhs.type  := Value.ToType (lhs.value);
            DoQualify (lhs, name);
        | VC.Module =>
            s := Module.ExportScope (Value.Base (lhs.value));
            lhs.kind  := Kind.Value;   (*FINAL*)
            lhs.value := Scope.LookUp (s, name, TRUE);
        ELSE (* can't qualify this kind of value *)
            lhs.kind  := Kind.None;  (*FINAL*)
        END;
    END;
  END DoQualify;

PROCEDURE IsDesignator (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    CASE p.class OF
    | Class.cMODULE   => RETURN (Value.ClassOf (p.obj) = VC.Var);
    | Class.cENUM     => RETURN FALSE;
    | Class.cOBJTYPE  => RETURN FALSE;
    | Class.cFIELD    => RETURN Expr.IsDesignator (p.expr);
    | Class.cOBJFIELD => RETURN TRUE;
    | Class.cMETHOD   => RETURN FALSE;
    | Class.cUNKNOWN  => RETURN FALSE;
    END;
  END IsDesignator;

PROCEDURE IsWritable (p: P;  lhs: BOOLEAN): BOOLEAN =
  BEGIN
    CASE p.class OF
    | Class.cMODULE   => RETURN Value.IsWritable (p.obj, lhs);
    | Class.cENUM     => RETURN FALSE;
    | Class.cOBJTYPE  => RETURN FALSE;
    | Class.cFIELD    => RETURN Expr.IsWritable (p.expr, lhs);
    | Class.cOBJFIELD => RETURN TRUE;
    | Class.cMETHOD   => RETURN FALSE;
    | Class.cUNKNOWN  => RETURN FALSE;
    END;
  END IsWritable;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> l: BOOLEAN): BOOLEAN =
  VAR lhs: LHS;  b: BOOLEAN;
  BEGIN
    IF (p.inIsZeroes) THEN Value.IllegalRecursion (p.obj); RETURN FALSE END;
    p.inIsZeroes := TRUE;

    (* evaluate the qualified expression *)
    lhs.kind := Kind.Expr;
    lhs.expr := p.expr;
    DoQualify (lhs, p.name);

    (* finally, simplify the result to an Expr.T if possible *)
    CASE lhs.kind OF
    | Kind.None =>
        b := FALSE;
    | Kind.Expr =>
        b := Expr.IsZeroes (lhs.expr);
    | Kind.Type =>
        b := FALSE;
    | Kind.Value =>
        b := (Value.ClassOf (lhs.value) = VC.Expr)
              AND Expr.IsZeroes (Value.ToExpr (lhs.value));
    END;

    p.inIsZeroes := FALSE;
    RETURN b;
  END IsZeroes;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    CASE p.class OF
    | Class.cENUM     => (*skip*)
    | Class.cOBJTYPE  => (*skip*)
    | Class.cMETHOD   => (*skip*)
    | Class.cUNKNOWN  => (*skip*)
    | Class.cFIELD    => Expr.NoteWrite (p.expr);
    | Class.cOBJFIELD => Expr.NoteWrite (p.expr);
    | Class.cMODULE   => IF (Value.ClassOf (p.obj) = VC.Var) THEN
                           Variable.ScheduleTrace (Value.Base (p.obj));
                         END;
    END;
  END NoteWrites;

BEGIN
END QualifyExpr.
