(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: QualifyExpr.m3                                        *)
(* Last modified on Fri Feb 24 16:45:51 PST 1995 by kalsow     *)
(*      modified on Tue Feb 19 01:32:23 1991 by muller         *)

MODULE QualifyExpr;

IMPORT M3, M3ID, CG, Expr, ExprRep, Value, Type, Module;
IMPORT RecordType, ObjectType, OpaqueType, Variable, VarExpr, Scope;
IMPORT EnumType, RefType, DerefExpr, NamedExpr, Error, ProcType;
IMPORT ErrType, RecordExpr, TypeExpr, MethodExpr, ProcExpr;
IMPORT Method, Field, Target, M3RT, Host, RunTyme;
IMPORT MSIR, MSIRBuilder, MSIRType, CaptureAnalysis;

TYPE
  Class = { importDecl    (* <importedInterface>.<anyId> *),
            enumLit       (* <enumType>.<Id> *),
            objTypeMethod (* <objectType>.<methodId> *),
            recField      (* <recordExpr>.<fieldId> *),
            objField      (* <objectExpr>.<fieldId> *),
            objMethod     (* <objectExpr>.<methodId> *),
            unknown };

TYPE
  VC = Value.Class;

TYPE
  P = Expr.T BRANDED "QualifyExpr.T" OBJECT
        lhsExpr     : Expr.T;
        rhsValue    : Value.T;
        holder      : Type.T; (* Visible supertype of the Q-expr. *) 
        objType     : Type.T;
        temp        : CG.Val;
        name        : M3ID.T;
        class       : Class;
        addr_align  : INTEGER := Target.Word8.align;
        (* ^For lhsExpr with object type, alignment of the referent. *)
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
        note_write        := NoteWrites;
        exprAlign         := QualifyExprAlign;
        capture  := Capture;
        captureLV := CaptureLV;
        compileMSIR       := CompileMSIR;
        compileLValueMSIR := LValueMSIR;
      END;

PROCEDURE New (a: Expr.T;  id: M3ID.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.lhsExpr        := a;
    p.name        := id;
    p.rhsValue         := NIL;
    p.class       := Class.unknown;
    p.holder      := NIL;
    p.objType     := NIL;
    p.inFold      := FALSE;
    p.inIsZeroes  := FALSE;
    p.inGetBounds := FALSE;
    p.inTypeOf    := FALSE;
    RETURN p;
  END New;

PROCEDURE Split (e: Expr.T; VAR rhsValue: Value.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => Resolve (p); rhsValue := p.rhsValue; RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE SplitQID (e: Expr.T;  VAR module, item: M3ID.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => IF NamedExpr.SplitName (p.lhsExpr, module)
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
    | P(p) => IF (p.class = Class.objMethod) THEN
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
              IF (p.class = Class.objMethod) THEN RETURN Value.TypeOf(p.rhsValue) END;
    ELSE      (* nothing *)
    END;
    RETURN NIL;
  END MethodType;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    Resolve (p);
    IF (p.inGetBounds) THEN Value.IllegalRecursion (p.rhsValue) END;
    p.inGetBounds := TRUE;
    CASE Value.ClassOf (p.rhsValue) OF
    | Value.Class.Expr => Expr.GetBounds (Value.ToExpr (p.rhsValue), min, max);
    | Value.Class.Var  => Variable.GetBounds (p.rhsValue, min, max);
    ELSE                  EVAL Type.GetBounds (p.type, min, max);
    END;
    p.inGetBounds := FALSE;
  END Bounder;

PROCEDURE MakeDummy (p: P) =
  BEGIN
    p.class := Class.importDecl;
    p.rhsValue   := VarExpr.Obj (VarExpr.New (ErrType.T, p.name));
  END MakeDummy;

PROCEDURE Resolve (p: P) =
  VAR
    t            : Type.T;
    baseType     : Type.T;
    s            : Scope.T;
    rhsValue     : Value.T;
    name         : M3ID.T;
    baseTypeInfo : Type.Info;
  BEGIN
    IF (p.class # Class.unknown) THEN RETURN END;

    t := Expr.TypeOf (p.lhsExpr);

    IF RefType.Is (t) THEN
      (* auto-magic dereference *)
      p.lhsExpr := DerefExpr.New (p.lhsExpr);
      p.lhsExpr.origin := p.origin;
      t := Expr.TypeOf (p.lhsExpr);
    END;

    p.holder := t;
    p.rhsValue := NIL;
    baseType := Type.Base (t);

    IF (t = ErrType.T) THEN
      (* the lhs already contains an error => silently make it look like
         everything is ok. *)
      MakeDummy (p);

    ELSIF (t = NIL) THEN
      (* p.lhsExpr *has* no type, so it *is* either a module or type *)
      IF TypeExpr.Split (p.lhsExpr, t) THEN
        IF EnumType.LookUp (t, p.name, p.rhsValue) THEN
          p.class := Class.enumLit;
        ELSIF ObjectType.LookUp (t, p.name, p.rhsValue, p.holder) THEN
          p.objType := t (* Used? *);
          p.class := Class.objTypeMethod;
        END;
      ELSIF NamedExpr.Split (p.lhsExpr, name, rhsValue) THEN
        IF (Value.ClassOf (rhsValue) = VC.Module) THEN
          p.class := Class.importDecl;
          s := Module.ExportScope (Value.Base (rhsValue));
          p.rhsValue := Scope.LookUp (s, p.name, TRUE);
        END;
      END;

    ELSIF RecordType.LookUp (baseType, p.name, p.rhsValue) THEN
      p.class := Class.recField;

    ELSIF ObjectType.LookUp (baseType, p.name, p.rhsValue, p.holder) THEN
      EVAL Type.CheckInfo (baseType, baseTypeInfo);
      p.addr_align := baseTypeInfo.addr_align;
      IF (Value.ClassOf (p.rhsValue) = VC.Field)
      THEN p.class := Class.objField;
      ELSE p.class := Class.objMethod;
      END;
    END;
  END Resolve;

PROCEDURE ResolveTypes (p: P) =
  VAR objType: Type.T;
  BEGIN
    Resolve (p);
    IF (p.inTypeOf) THEN
      Value.IllegalRecursion (p.rhsValue);
      p.type := ErrType.T;
      p.repType := p.type;
    ELSE
      p.inTypeOf := TRUE;
      p.type := Value.TypeOf (p.rhsValue);
      IF p.type = ErrType.T THEN p.repType := ErrType.T;
      ELSIF p.class = Class.objMethod THEN
        p.type := NIL;
        p.repType := NIL;
      ELSIF p.class = Class.objTypeMethod THEN 
        WITH b = TypeExpr.Split (p.lhsExpr, objType) DO <*ASSERT b*> END;
        p.type := ProcType.MethodSigAsProcSig (p.type, objType);
        p.repType := p.type;
      ELSE p.repType := Value.RepTypeOf (p.rhsValue);
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
      Expr.TypeCheck (p.lhsExpr, cs);
      Resolve (p);
      Expr.TypeCheck (p.lhsExpr, cs);
    Error.Count (nErrs1, nWarns);

    IF (p.rhsValue = NIL) THEN
      IF (nErrs0 = nErrs1) THEN
        Error.ID (p.name, "unknown qualification \'.\'");
      END;
      MakeDummy (p);
    ELSIF (p.class = Class.recField) THEN
      EVAL Type.CheckInfo (p.holder, info);
      DerefExpr.SetOffset (p.lhsExpr, info.size);
    ELSIF (p.class = Class.objTypeMethod)
      AND (Value.ClassOf (p.rhsValue) # VC.Method) THEN
      Error.ID (p.name, "doesn\'t name a method");
    END;

    Value.TypeCheck (p.rhsValue, cs);
    EVAL TypeOf (p);
    IF (p.type # NIL) THEN
      p.type := Type.Check (p.type);
    END;
  END Check;

PROCEDURE QualifyExprAlign (p: P): Type.BitAlignT =
  VAR fieldInfo: Field.Info;
  VAR rhsRepType, lhsRepType: Type.T;
  VAR typeInfo: Type.Info;
  VAR fieldTypeAlign, fieldsAlign, result: Type.BitAlignT;
  BEGIN
    CASE p.class
    OF Class.objMethod => RETURN Target.Address.align;
    | Class.importDecl
    , Class.enumLit
    , Class.objTypeMethod
      => rhsRepType := Value.TypeOf (p.rhsValue);
         EVAL Type.CheckInfo (rhsRepType, typeInfo);
         RETURN typeInfo.alignment;
    ELSE
    END (*CASE*);

    (* It's a field.  get its alignment from its type, respecting packing. *)
    rhsRepType := Type.Strip (Value.TypeOf (p.rhsValue)); (* Remove named. *)
    rhsRepType := Type.CheckInfo (rhsRepType, (*OUT*) typeInfo);
    IF typeInfo.class = Type.Class.Packed
    THEN fieldTypeAlign := 1;
    ELSE fieldTypeAlign := typeInfo.alignment;
    END;
    Field.Split (p.rhsValue, fieldInfo);
    
    CASE p.class <*NOWARN*>
    OF Class.recField =>
        result := CG.GCD (Expr.Alignment (p.lhsExpr), fieldInfo.offset);
        <*ASSERT result MOD fieldTypeAlign = 0 *>
        RETURN result;
    | Class.objField =>
        fieldsAlign := Target.Address.align;
        (* ^Will be alignment of the whole block of fields. *)
        IF fieldsAlign < Target.MaxAlign
        THEN (* It's possible on a 32-bit target, that the heap referent's
                alignment is 64.  Does the object type indicate so? *)
          lhsRepType := Expr.RepTypeOf (p.lhsExpr);
          WHILE OpaqueType.Is (lhsRepType) DO
            (* This could have a revelation with MaxAlign, but we won't
               know that now.  So conservatively assume not. *)
            lhsRepType := OpaqueType.Super (lhsRepType)
          END; 
          fieldsAlign
            := MAX (fieldsAlign, ObjectType.FieldAlignment (lhsRepType) );
        END;
        result := CG.GCD (MAX (fieldsAlign, fieldTypeAlign), fieldInfo.offset);
        <*ASSERT result MOD fieldTypeAlign = 0 *>
        RETURN result;
    END (*CASE*)
  END QualifyExprAlign; 

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => Resolve (a);  Resolve (b);
              RETURN (a.rhsValue = b.rhsValue)
                 AND (a.class = b.class)
                 AND Expr.IsEqual (a.lhsExpr, b.lhsExpr, x);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  VAR c: Value.Class;
  BEGIN
    CASE p.class OF
    | Class.importDecl =>
        c := Value.ClassOf (p.rhsValue);
        IF (c = Value.Class.Var) THEN
          Variable.NeedsAddress (p.rhsValue);
        ELSIF (c = Value.Class.Expr) THEN
          Expr.NeedsAddress (Value.ToExpr (p.rhsValue));
        END;
    | Class.recField =>
        Expr.NeedsAddress (p.lhsExpr);
    | Class.objField =>
        (* ok, all objects have addresses *)
    | Class.enumLit,
      Class.objTypeMethod,
      Class.objMethod,
      Class.unknown =>
        <* ASSERT FALSE *>
    END;
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  VAR
    field: Field.Info;
    info: Type.Info;
  BEGIN
    CASE p.class OF
    | Class.importDecl =>
        IF Host.doIncGC AND Value.ClassOf (p.rhsValue) = Value.Class.Var THEN
          EVAL Type.CheckInfo (p.type, info);
          IF info.isTraced THEN
            CASE info.class OF 
            | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
              Variable.Load (p.rhsValue);
              RunTyme.EmitCheckLoadTracedRef ();
              p.temp := CG.Pop ();
            ELSE
              (* no check *)
            END
          END
        END
    | Class.enumLit =>
        (* skip *)
    | Class.objTypeMethod =>
        (* skip *)
    | Class.recField =>
        IF Expr.IsDesignator (p.lhsExpr)
        THEN Expr.PrepLValue (p.lhsExpr, traced := FALSE);
        ELSE
          EVAL Expr.CheckUseFailure (p.lhsExpr);
          Expr.Prep (p.lhsExpr);
        END;
        Field.Split (p.rhsValue, field);
        EVAL Type.CheckInfo (field.type, info);
        IF Host.doIncGC AND info.isTraced THEN
          CASE info.class OF
          | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
            Compile (p, StaticOnly := FALSE);
            RunTyme.EmitCheckLoadTracedRef ();
            p.temp := CG.Pop ();
          ELSE
            (* no check *)
          END
        END
    | Class.objField =>
        Expr.Prep (p.lhsExpr);
        Field.Split (p.rhsValue, field);
        EVAL Type.CheckInfo (field.type, info);
        IF Host.doIncGC AND info.isTraced THEN
          CASE info.class OF
          | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
            Compile (p, StaticOnly := FALSE);
            RunTyme.EmitCheckLoadTracedRef ();
            p.temp := CG.Pop ();
          ELSE
            (* no check *)
          END
        END
    | Class.objMethod =>
        Expr.Prep (p.lhsExpr);
        Expr.Compile (p.lhsExpr);
        p.temp := CG.Pop ();
    | Class.unknown =>
        <* ASSERT FALSE *>
    END;
  END Prep;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  VAR
    obj_offset, obj_align: INTEGER;
    fieldInfo: Field.Info;
    method: Method.Info;
  BEGIN
    IF StaticOnly THEN RETURN END;
    CASE p.class OF
    | Class.importDecl =>
        IF p.temp # NIL THEN
          CG.Push (p.temp);
          CG.Free (p.temp);
          p.temp := NIL;
          RETURN;
        END;
        (* Do we need to Compile p.rhsValue, if it is a constant? *)
        Value.Load (p.rhsValue);
    | Class.enumLit =>
        Value.Load (p.rhsValue);
    | Class.objTypeMethod =>
        Type.Compile (p.holder);
        Type.Compile (p.objType);
        Method.SplitX (p.rhsValue, method);
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
    | Class.recField =>
        IF p.temp # NIL THEN
          CG.Push (p.temp);
          CG.Free (p.temp);
          p.temp := NIL;
          RETURN;
        END;
        Field.Split (p.rhsValue, fieldInfo);
        IF Expr.IsDesignator (p.lhsExpr)
          THEN Expr.CompileLValue (p.lhsExpr, traced := FALSE);
          ELSE Expr.Compile (p.lhsExpr);
        END;
        CG.Add_offset (fieldInfo.offset);
        Type.LoadScalar (fieldInfo.type);
    | Class.objField =>
        IF p.temp # NIL THEN
          CG.Push (p.temp);
          CG.Free (p.temp);
          p.temp := NIL;
          RETURN;
        END;
        Field.Split (p.rhsValue, fieldInfo);
        Expr.Compile (p.lhsExpr);
        CG.Boost_addr_alignment (p.addr_align);
        ObjectType.GetFieldsOffsetAndAlign (p.holder, obj_offset, obj_align);
        IF (obj_offset >= 0) THEN
          INC (fieldInfo.offset, obj_offset);
        ELSE
          IF Host.doNilChk THEN
            CG.Check_nil (CG.RuntimeError.BadMemoryReference);
          END;
          Type.LoadInfo (p.holder, M3RT.OTC_dataOffset);
          CG.Index_bytes (Target.Byte);
        END;
        CG.Add_offset (fieldInfo.offset);
        CG.Boost_addr_alignment (obj_align);
        Type.LoadScalar (fieldInfo.type);
    | Class.objMethod =>
        Method.SplitX (p.rhsValue, method);
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
    | Class.unknown =>
        <* ASSERT FALSE *>
    END;
 END Compile;

PROCEDURE PrepLV (p: P; traced: BOOLEAN) =
  VAR info: Type.Info;
  BEGIN
    CASE p.class OF
    | Class.importDecl, Class.enumLit =>
        (* skip *)
    | Class.objTypeMethod =>
        (* skip *)
    | Class.recField =>
        IF Expr.IsDesignator (p.lhsExpr)
        THEN Expr.PrepLValue (p.lhsExpr, traced);
        ELSE
          EVAL Expr.CheckUseFailure (p.lhsExpr);
          Expr.Prep (p.lhsExpr);
        END;
    | Class.objField =>
        Expr.Prep (p.lhsExpr);
        IF traced AND Host.doGenGC THEN
          EVAL Type.CheckInfo (p.type, info);
          IF NOT info.isTraced THEN RETURN END;
          EVAL Type.CheckInfo (Expr.TypeOf (p.lhsExpr), info);
          IF NOT info.isTraced THEN RETURN END;
          Expr.Compile (p.lhsExpr);
          RunTyme.EmitCheckStoreTraced ();
          p.temp := CG.Pop ();
        END;
    | Class.objMethod =>
        Expr.Prep (p.lhsExpr);
        Expr.Compile (p.lhsExpr);
        p.temp := CG.Pop ();
    | Class.unknown =>
        <* ASSERT FALSE *>
    END;
  END PrepLV;

PROCEDURE CompileLV (p: P;  traced: BOOLEAN; StaticOnly: BOOLEAN) =
  VAR obj_offset, obj_align: INTEGER;  field: Field.Info;
  BEGIN
    <* ASSERT NOT StaticOnly *>
    CASE p.class OF
    | Class.importDecl =>
        CASE Value.ClassOf (p.rhsValue) OF
        | Value.Class.Expr => Value.Load (p.rhsValue);
        | Value.Class.Var  => Variable.LoadLValue (p.rhsValue);
        ELSE <*ASSERT FALSE*>
        END;
    | Class.recField =>
        Field.Split (p.rhsValue, field);
        Expr.CompileLValue (p.lhsExpr, traced);
        CG.Add_offset (field.offset);
    | Class.objField =>
        Field.Split (p.rhsValue, field);
        IF p.temp # NIL THEN
          <*ASSERT traced*>
          CG.Push (p.temp);
          CG.Free (p.temp);
          p.temp := NIL;
        ELSE
          Expr.Compile (p.lhsExpr);
        END;
        ObjectType.GetFieldsOffsetAndAlign (p.holder, obj_offset, obj_align);
        IF (obj_offset >= 0) THEN
          INC (field.offset, obj_offset);
        ELSE
          IF Host.doNilChk THEN
            CG.Check_nil (CG.RuntimeError.BadMemoryReference);
          END;
          Type.LoadInfo (p.holder, M3RT.OTC_dataOffset);
          CG.Index_bytes (Target.Byte);
        END;
        CG.Add_offset (field.offset);
        CG.Boost_addr_alignment (obj_align);
    | Class.enumLit,
      Class.objTypeMethod,
      Class.objMethod,
      Class.unknown =>
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
    IF (p.inFold) THEN Value.IllegalRecursion (p.rhsValue); RETURN NIL END;
    p.inFold := TRUE;

    (* evaluate the qualified expression *)
    lhs.kind := Kind.Expr;
    lhs.expr := p.lhsExpr;
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
          lhs.expr  := p.lhsExpr;
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
    | Class.importDecl   => RETURN (Value.ClassOf (p.rhsValue) = VC.Var);
    | Class.enumLit     => RETURN FALSE;
    | Class.objTypeMethod  => RETURN FALSE;
    | Class.recField    => RETURN Expr.IsDesignator (p.lhsExpr);
    | Class.objField => RETURN TRUE;
    | Class.objMethod   => RETURN FALSE;
    | Class.unknown  => RETURN FALSE;
    END;
  END IsDesignator;

PROCEDURE IsWritable (p: P;  lhs: BOOLEAN): BOOLEAN =
  BEGIN
    CASE p.class OF
    | Class.importDecl   => RETURN Value.IsWritable (p.rhsValue, lhs);
    | Class.enumLit     => RETURN FALSE;
    | Class.objTypeMethod  => RETURN FALSE;
    | Class.recField    => RETURN Expr.IsWritable (p.lhsExpr, lhs);
    | Class.objField => RETURN TRUE;
    | Class.objMethod   => RETURN FALSE;
    | Class.unknown  => RETURN FALSE;
    END;
  END IsWritable;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> l: BOOLEAN): BOOLEAN =
  VAR lhs: LHS;  b: BOOLEAN;
  BEGIN
    IF (p.inIsZeroes) THEN Value.IllegalRecursion (p.rhsValue); RETURN FALSE END;
    p.inIsZeroes := TRUE;

    (* evaluate the qualified expression *)
    lhs.kind := Kind.Expr;
    lhs.expr := p.lhsExpr;
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
    | Class.enumLit     => (*skip*)
    | Class.objTypeMethod  => (*skip*)
    | Class.objMethod   => (*skip*)
    | Class.unknown  => (*skip*)
    | Class.recField    => Expr.NoteWrite (p.lhsExpr);
    | Class.objField => Expr.NoteWrite (p.lhsExpr);
    | Class.importDecl   => IF (Value.ClassOf (p.rhsValue) = VC.Var) THEN
                           Variable.ScheduleTrace (Value.Base (p.rhsValue));
                         END;
    END;
  END NoteWrites;

PROCEDURE LValueMSIR (p: P): MSIR.Value =
  VAR
    fieldInfo: Field.Info;
    baseAddr:  MSIR.Value;
    objOff:    INTEGER;
    objAlign:  INTEGER;
    byteOff:   INTEGER;
  BEGIN
    Resolve (p);
    CASE p.class OF
    | Class.importDecl =>
        TYPECASE p.rhsValue OF
        | Variable.T (v) =>
            Variable.RegisterExternMSIR (v);
            RETURN MSIRBuilder.LookupVarAddr (v);
        ELSE
          (* Constant import: fold, then try LValueMSIR first — ArrayExpr and
             RecordExpr implement it and return their own alloca. Scalars return
             NIL (no abandon); fall back to compile+spill in that case. *)
          VAR folded := Fold (p); BEGIN
            IF folded = NIL THEN
              MSIRBuilder.Abandon ("importDecl lvalue: cannot fold constant");
              RETURN NIL;
            END;
            VAR lv := Expr.LValueMSIR (folded); BEGIN
              IF lv # NIL THEN RETURN lv END;
            END;
            VAR v := Expr.CompileMSIR (folded); BEGIN
              IF v = NIL THEN RETURN NIL END;
              VAR blk  := MSIRBuilder.CurrentBlock ();
                  slot := MSIR.BuildAlloca (blk, "", MSIR.ValueType (v));
              BEGIN
                MSIR.BuildStore (blk, v, slot);
                RETURN slot;
              END;
            END;
          END;
        END;
    | Class.recField =>
        baseAddr := Expr.LValueMSIR (p.lhsExpr);
        IF baseAddr = NIL THEN RETURN NIL END;
        Field.Split (p.rhsValue, fieldInfo);
        (* Sub-byte field: no lvalue — CompileMSIR handles via ExtractBitField. *)
        IF fieldInfo.offset MOD Target.Byte # 0 THEN
          RETURN NIL;
        END;
        byteOff := fieldInfo.offset DIV 8;
        VAR
          b      := MSIRBuilder.CurrentBlock ();
          ft     := MSIRType.Translate (fieldInfo.type);
          fti    : Type.Info;
          slot   : MSIR.Value;
        BEGIN
          EVAL Type.CheckInfo (fieldInfo.type, fti);
          (* Sub-byte field: no lvalue — CompileMSIR handles via ExtractBitField. *)
          IF fti.size MOD Target.Byte # 0 THEN
            RETURN NIL;
          END;
          (* When actual storage width differs from natural MSIR type (e.g.
             [0..255] stored as i8 but Translate gives i64), use TI(size) so
             the pointer element type matches the real field width. *)
          IF ft # NIL AND fti.size > 0 AND MSIR.BitWidth (ft) > 0
                      AND fti.size # MSIR.BitWidth (ft) THEN
            ft := MSIR.TI (fti.size);
          END;
          slot := MSIRBuilder.BuildPtrByteOff (b, "", baseAddr, byteOff);
          (* Heap record (GcRef base): set container and retype traced fields
             as GcSlot so AssignStmt.CompileMSIR fires the write barrier. *)
          IF MSIR.Kind (MSIR.ValueType (baseAddr)) = MSIR.TypeKind.GcRef THEN
            MSIRBuilder.SetPendingContainer (baseAddr);
            IF ft # NIL AND MSIR.Kind (ft) = MSIR.TypeKind.GcRef THEN
              slot := MSIR.RetypeValue (slot, MSIR.TGcSlot (MSIR.EltType (ft)));
            ELSIF ft # NIL THEN
              slot := MSIR.RetypeValue (slot, MSIR.TPtr (ft));
            END;
          ELSIF ft # NIL THEN
            slot := MSIR.RetypeValue (slot, MSIR.TPtr (ft));
          END;
          RETURN slot;
        END;
    | Class.objField =>
        (* Compute byte offset: fields start at obj_offset bits from the object
           pointer (typically 64 bits = 8 bytes for the vtable word), plus the
           field's own bit offset within the field region.  When obj_offset is
           not statically known (complex inheritance chain), load it at runtime
           from the type cell's OTC_dataOffset field. *)
        Field.Split (p.rhsValue, fieldInfo);
        ObjectType.GetFieldsOffsetAndAlign (p.holder, objOff, objAlign);
        IF fieldInfo.offset MOD Target.Byte # 0 THEN
          MSIRBuilder.Abandon ("sub-byte object field offset not supported in MSIR");
          RETURN NIL;
        END;
        baseAddr := Expr.CompileMSIR (p.lhsExpr);
        IF baseAddr = NIL THEN RETURN NIL END;
        (* Set container for GC write barrier in AssignStmt.CompileMSIR. *)
        MSIRBuilder.SetPendingContainer (baseAddr);
        VAR slotAddr: MSIR.Value;
        BEGIN
          IF objOff >= 0 THEN
            IF (objOff + fieldInfo.offset) MOD Target.Byte # 0 THEN
              MSIRBuilder.Abandon ("sub-byte object field offset not supported in MSIR");
              RETURN NIL;
            END;
            byteOff := (objOff + fieldInfo.offset) DIV 8;
            slotAddr := MSIRBuilder.BuildPtrByteOff(MSIRBuilder.CurrentBlock(), "", baseAddr, byteOff);
          ELSE
            (* Non-static: load dataOffset from the runtime type cell (OTC_dataOffset),
               then GepByte(objBase, dataOffset) + fieldByteOff. *)
            VAR b        := MSIRBuilder.CurrentBlock();
                tcPtr    := MSIRBuilder.TypeLinkValueForObject(p.holder);
                otcByte  := M3RT.OTC_dataOffset DIV Target.Byte;
                offSlot  := MSIRBuilder.BuildPtrByteOff(b, "", tcPtr, otcByte);
                i64T     := MSIR.TI(Target.Integer.size);
                dynOff   := MSIR.BuildLoad(b, "", i64T,
                              MSIR.RetypeValue(offSlot, MSIR.TPtr(i64T)));
                fieldByte := fieldInfo.offset DIV Target.Byte;
                dynBase  := MSIR.BuildGepByte(b, "", baseAddr, dynOff);
            BEGIN
              slotAddr := MSIRBuilder.BuildPtrByteOff(b, "", dynBase, fieldByte);
            END;
          END;
          (* Retype: GcRef fields → GcSlot (write barrier); others → TPtr(ft) for
             type-preserving access (e.g. array field subscript needs FixedArray type).
             Use storage type TI(size) when it differs from the natural MSIR type. *)
          VAR ft  := MSIRType.Translate(fieldInfo.type);
              fti : Type.Info;
          BEGIN
            EVAL Type.CheckInfo(fieldInfo.type, fti);
            (* Sub-byte object field: no lvalue — CompileMSIR handles via ExtractBitField. *)
            IF ft # NIL AND fti.size MOD Target.Byte # 0 THEN
              RETURN NIL;
            END;
            IF ft # NIL AND fti.size > 0 AND MSIR.BitWidth(ft) > 0
                        AND fti.size # MSIR.BitWidth(ft) THEN
              ft := MSIR.TI(fti.size);
            END;
            IF ft # NIL AND MSIR.Kind(ft) = MSIR.TypeKind.GcRef THEN
              slotAddr := MSIR.RetypeValue(slotAddr, MSIR.TGcSlot(MSIR.EltType(ft)));
            ELSIF ft # NIL THEN
              slotAddr := MSIR.RetypeValue(slotAddr, MSIR.TPtr(ft));
            END;
          END;
          RETURN slotAddr;
        END;
    ELSE
      (* enumLit / objTypeMethod / objMethod / unknown: these are values or
         method references, not writable designators — they have no lvalue.
         Return NIL (recoverable) rather than Abandon so a speculative caller
         (e.g. RecordType.InitFieldDefaultMSIR, which tries LValueMSIR for an
         aggregate default before falling back to CompileMSIR) is not poisoned.
         Abandoning here dropped the whole enclosing proc — e.g. NEW(Activation)
         where the `state` field default is an enum literal (Thread.Fork). *)
      RETURN NIL;
    END;
  END LValueMSIR;

(* Load a record/object field value from 'addr', casting narrow storage to the
   natural expression type.  Same narrowing/widening logic as SubscriptExpr:
   - narrow storage (e.g. i8 for [0..255]) → ZExt or SExt to natural type
   - wide storage (e.g. i8 for BOOLEAN) → Trunc to natural type (i1) *)
PROCEDURE SubByteStoreMSIR (e: Expr.T;  rhs: MSIR.Value): BOOLEAN =
  VAR
    p         : P;
    fieldInfo : Field.Info;
    fti       : Type.Info;
    bitOff    : INTEGER;
  BEGIN
    TYPECASE e OF
    | P (pp) => p := pp;
    ELSE        RETURN FALSE;
    END;
    Resolve (p);
    CASE p.class OF
    | Class.recField =>
        Field.Split (p.rhsValue, fieldInfo);
        EVAL Type.CheckInfo (fieldInfo.type, fti);
        IF fti.size MOD Target.Byte = 0 AND fieldInfo.offset MOD Target.Byte = 0 THEN
          RETURN FALSE;
        END;
        VAR baseAddr := Expr.LValueMSIR (p.lhsExpr);
        BEGIN
          IF baseAddr = NIL THEN RETURN FALSE END;
          MSIRBuilder.InsertBitField (baseAddr, fieldInfo.offset, fti.size, rhs);
          RETURN TRUE;
        END;
    | Class.objField =>
        Field.Split (p.rhsValue, fieldInfo);
        VAR objOff, objAlign : INTEGER;
        BEGIN
          ObjectType.GetFieldsOffsetAndAlign (p.holder, objOff, objAlign);
          IF objOff < 0 THEN RETURN FALSE END;
          bitOff := objOff + fieldInfo.offset;
          EVAL Type.CheckInfo (fieldInfo.type, fti);
          IF fti.size MOD Target.Byte = 0 AND bitOff MOD Target.Byte = 0 THEN
            RETURN FALSE;
          END;
          VAR baseAddr := Expr.CompileMSIR (p.lhsExpr);
          BEGIN
            IF baseAddr = NIL THEN RETURN FALSE END;
            MSIRBuilder.InsertBitField (baseAddr, bitOff, fti.size, rhs);
            RETURN TRUE;
          END;
        END;
    ELSE
      RETURN FALSE;
    END;
  END SubByteStoreMSIR;

(* If e is a sub-byte/bit-field record or object field designator, compute the
   containing storage's base pointer (evaluated ONCE) and return the field's bit
   offset, width, and raw type.  Used by WithStmt to bind a bit-field WITH alias
   by reference (base captured once; reads/writes route through Extract/Insert).
   Returns FALSE for byte-aligned fields (they have an ordinary lvalue) and for
   anything that is not a rec/obj field. *)
PROCEDURE BitFieldBaseMSIR (e: Expr.T;  VAR base: MSIR.Value;
                            VAR bitOff, width: INTEGER;
                            VAR ftype: Type.T): BOOLEAN =
  VAR
    p         : P;
    fieldInfo : Field.Info;
    fti       : Type.Info;
  BEGIN
    TYPECASE e OF
    | P (pp) => p := pp;
    ELSE        RETURN FALSE;
    END;
    Resolve (p);
    CASE p.class OF
    | Class.recField =>
        Field.Split (p.rhsValue, fieldInfo);
        EVAL Type.CheckInfo (fieldInfo.type, fti);
        IF fti.size MOD Target.Byte = 0 AND fieldInfo.offset MOD Target.Byte = 0 THEN
          RETURN FALSE;
        END;
        base := Expr.LValueMSIR (p.lhsExpr);
        IF base = NIL THEN RETURN FALSE END;
        bitOff := fieldInfo.offset;  width := fti.size;  ftype := fieldInfo.type;
        RETURN TRUE;
    | Class.objField =>
        Field.Split (p.rhsValue, fieldInfo);
        VAR objOff, objAlign : INTEGER;
        BEGIN
          ObjectType.GetFieldsOffsetAndAlign (p.holder, objOff, objAlign);
          IF objOff < 0 THEN RETURN FALSE END;
          bitOff := objOff + fieldInfo.offset;
          EVAL Type.CheckInfo (fieldInfo.type, fti);
          IF fti.size MOD Target.Byte = 0 AND bitOff MOD Target.Byte = 0 THEN
            RETURN FALSE;
          END;
          base := Expr.CompileMSIR (p.lhsExpr);
          IF base = NIL THEN RETURN FALSE END;
          width := fti.size;  ftype := fieldInfo.type;
          RETURN TRUE;
        END;
    ELSE
      RETURN FALSE;
    END;
  END BitFieldBaseMSIR;

PROCEDURE LoadFieldValue (addr: MSIR.Value;  naturalT: MSIR.T;
                          rawFieldType: Type.T): MSIR.Value =
  VAR blk      := MSIRBuilder.CurrentBlock ();
      addrEltT := MSIR.EltType (MSIR.ValueType (addr));
      (* Produce the field's ZType (machine width) so an ordinal field read is a
         uniform-width value (e.g. a BOOLEAN/CHAR/enum field yields i64, not i1/
         i8).  Load at the field's MType (storage width = addrEltT, matching the
         pointer element type) then CoerceToMSIR widens/truncates to the ZType,
         choosing SExt vs ZExt from the storage type's kind (TI vs TW). *)
      zt       := MSIRType.ComputeType (rawFieldType);
  BEGIN
    IF zt = NIL THEN zt := naturalT END;
    VAR loaded := MSIR.BuildLoad (blk, "", addrEltT, addr);
    BEGIN
      RETURN MSIRBuilder.CoerceToMSIR (blk, loaded, zt);
    END;
  END LoadFieldValue;

PROCEDURE LhsExpr (e: Expr.T): Expr.T =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN NIL;
    | P(p) => Resolve (p);
              IF p.class = Class.objMethod THEN RETURN p.lhsExpr END;
              RETURN NIL;
    ELSE RETURN NIL;
    END;
  END LhsExpr;

PROCEDURE MethodSlotBase (e: Expr.T): INTEGER =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN -1;
    | P(p) => Resolve (p);
              IF p.class = Class.objMethod THEN
                RETURN ObjectType.MethodOffset (p.holder);
              END;
              RETURN -1;
    ELSE RETURN -1;
    END;
  END MethodSlotBase;

PROCEDURE MethodHolder (e: Expr.T): Type.T =
  BEGIN
    TYPECASE e OF
    | P(p) => Resolve (p);
              IF p.class = Class.objMethod OR p.class = Class.objTypeMethod THEN
                RETURN p.holder;
              END;
    ELSE
    END;
    RETURN NIL;
  END MethodHolder;

PROCEDURE ObjTypeMethod (e: Expr.T;  VAR objType, holder: Type.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => Resolve (p);
              IF p.class = Class.objTypeMethod THEN
                objType := p.objType;  holder := p.holder;
                RETURN TRUE;
              END;
              RETURN FALSE;
    ELSE RETURN FALSE;
    END;
  END ObjTypeMethod;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  VAR fieldInfo: Field.Info;  fieldType: MSIR.T;  addr: MSIR.Value;
      folded: Expr.T;
  BEGIN
    Resolve (p);
    CASE p.class OF
    | Class.importDecl =>
        (* Module.X: look up the exported entity in its owning module.
           For Variable: register as extern global on demand, then look up. *)
        TYPECASE p.rhsValue OF
        | Variable.T (v) =>
            Variable.RegisterExternMSIR (v);
            VAR val := MSIRBuilder.LookupVar (v);
            BEGIN
              IF val = NIL THEN
                MSIRBuilder.Abandon ("importDecl: variable not in global map");
                RETURN NIL;
              END;
              RETURN val;
            END;
        ELSE
            folded := Fold (p);
            IF folded # NIL THEN RETURN Expr.CompileMSIR (folded) END;
            MSIRBuilder.Abandon ("importDecl: unsupported value class");
            RETURN NIL;
        END;
    | Class.recField =>
        (* Fold CONST record field access: e.g. EVAL OK.rank where OK is a CONST.
           StripNamedCons pierces the NamedExpr+ConsExpr wrapper to get the
           underlying RecordExpr.P; RecordExpr.Qualify extracts the field value.
           This avoids going through LValueMSIR, which requires an addressable base. *)
        folded := NIL;
        VAR stripped := Expr.StripNamedCons (p.lhsExpr);
        BEGIN
          IF stripped # NIL THEN
            (* Selecting a field of a record constructor with a statically
               out-of-range field is a checked RT error (CT-warned); the fold
               below bypasses CompileLValueMSIR, so emit the fault here (p268). *)
            RecordExpr.EmitUseFailureMSIR (stripped);
            EVAL RecordExpr.Qualify (stripped, p.name, folded);
          END;
        END;
        IF folded # NIL THEN RETURN Expr.CompileMSIR (folded) END;
        (* Sub-byte field: extract via shift+mask from the byte array. *)
        Field.Split (p.rhsValue, fieldInfo);
        VAR ftiRec: Type.Info;
        BEGIN
          EVAL Type.CheckInfo (fieldInfo.type, ftiRec);
          IF ftiRec.size MOD Target.Byte # 0 OR fieldInfo.offset MOD Target.Byte # 0 THEN
            VAR baseAddr := Expr.LValueMSIR (p.lhsExpr);
            BEGIN
              IF baseAddr = NIL THEN RETURN NIL END;
              RETURN MSIRBuilder.ExtractBitField (baseAddr, fieldInfo.offset, ftiRec.size,
                                      fieldInfo.type);
            END;
          END;
        END;
        addr := LValueMSIR (p);
        IF addr = NIL THEN RETURN NIL END;
        fieldType := MSIRType.Translate (fieldInfo.type);
        IF fieldType = NIL THEN
          MSIRBuilder.Abandon ("unsupported record field type");
          RETURN NIL;
        END;
        (* LValueMSIR retyped traced-ref fields to gc_slot for write-barrier;
           use BuildGcLoad (read barrier + correct result type) for those. *)
        IF MSIR.Kind (MSIR.ValueType (addr)) = MSIR.TypeKind.GcSlot THEN
          RETURN MSIR.BuildGcLoad (MSIRBuilder.CurrentBlock (), "", addr);
        END;
        RETURN LoadFieldValue (addr, fieldType, fieldInfo.type);
    | Class.objField =>
        (* Sub-byte object field: extract via shift+mask. *)
        Field.Split (p.rhsValue, fieldInfo);
        VAR objOff2, objAlign2 : INTEGER;
            ftiObj : Type.Info;
        BEGIN
          ObjectType.GetFieldsOffsetAndAlign (p.holder, objOff2, objAlign2);
          IF objOff2 >= 0 THEN
            EVAL Type.CheckInfo (fieldInfo.type, ftiObj);
            VAR totalBitOff := objOff2 + fieldInfo.offset;
            BEGIN
              IF ftiObj.size MOD Target.Byte # 0 OR totalBitOff MOD Target.Byte # 0 THEN
                VAR baseAddr := Expr.CompileMSIR (p.lhsExpr);
                BEGIN
                  IF baseAddr = NIL THEN RETURN NIL END;
                  RETURN MSIRBuilder.ExtractBitField (baseAddr, totalBitOff, ftiObj.size,
                                          fieldInfo.type);
                END;
              END;
            END;
          END;
        END;
        (* Byte-aligned object field: normal LValueMSIR→load path. *)
        addr := LValueMSIR (p);
        IF addr = NIL THEN RETURN NIL END;
        Field.Split (p.rhsValue, fieldInfo);
        fieldType := MSIRType.Translate (fieldInfo.type);
        IF fieldType = NIL THEN
          MSIRBuilder.Abandon ("unsupported object field type");
          RETURN NIL;
        END;
        (* LValueMSIR retyped traced-ref fields to gc_slot for write-barrier;
           use BuildGcLoad (read barrier + correct result type) for those. *)
        IF MSIR.Kind (MSIR.ValueType (addr)) = MSIR.TypeKind.GcSlot THEN
          RETURN MSIR.BuildGcLoad (MSIRBuilder.CurrentBlock (), "", addr);
        END;
        RETURN LoadFieldValue (addr, fieldType, fieldInfo.type);
    | Class.enumLit =>
        folded := Fold (p);
        IF folded = NIL THEN
          MSIRBuilder.Abandon ("enum literal fold failed");
          RETURN NIL;
        END;
        RETURN Expr.CompileMSIR (folded);
    ELSE
      MSIRBuilder.Abandon ("unsupported qualify expression");
      RETURN NIL;
    END;
  END CompileMSIR;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    Expr.Capture (p.lhsExpr, ca);
  END Capture;

PROCEDURE CaptureLV (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    CASE p.class OF
    | Class.recField =>
        (* Assigning through a record field modifies the record variable itself:
           propagate the lvalue context so the outer variable is marked written. *)
        Expr.CaptureLV (p.lhsExpr, ca);
    | Class.objField =>
        (* Assigning through an object field modifies heap data, not the pointer
           variable holding the object reference: the pointer is only read. *)
        Expr.Capture (p.lhsExpr, ca);
    ELSE
        (* importDecl, enumLit, objTypeMethod, objMethod, unknown:
           the lhsExpr is either a module/type expression or an object
           reference; in all cases we just read it. *)
        Expr.Capture (p.lhsExpr, ca);
    END;
  END CaptureLV;

BEGIN
END QualifyExpr.
