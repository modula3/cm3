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
        note_write   := NoteWrites;
        exprAlign    := QualifyExprAlign;
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
        result := CG.GCD (fieldsAlign, fieldInfo.offset);
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
            Compile (p);
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
            Compile (p);
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

PROCEDURE Compile (p: P) =
  VAR
    obj_offset, obj_align: INTEGER;
    fieldInfo: Field.Info;
    method: Method.Info;
  BEGIN
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

PROCEDURE CompileLV (p: P;  traced: BOOLEAN) =
  VAR obj_offset, obj_align: INTEGER;  field: Field.Info;
  BEGIN
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

BEGIN
END QualifyExpr.
