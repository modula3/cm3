(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MethodExpr.m3                                         *)
(* Last modified on Fri Feb 24 16:44:09 PST 1995 by kalsow     *)
(*      modified on Tue Feb 19 01:32:23 1991 by muller         *)

MODULE MethodExpr;

IMPORT M3, CG, Expr, ExprRep, Type, ObjectType, M3Buf, Target;
IMPORT Value, ProcType, Method, Error, M3ID, M3RT;
IMPORT MSIR, MSIRBuilder;

TYPE
  P = Expr.T BRANDED "MethodExpr.T" OBJECT
        object      : Type.T;
        method      : Value.T;
        holder      : Type.T;
        name        : M3ID.T;
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := TypeOf;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValueBool;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := ExprRep.Self;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := GenFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
        exprAlign    := ExprRep.ExprAddrAlign; 
        compileMSIR  := CompileMSIR;
      END;

PROCEDURE New (object: Type.T;  name: M3ID.T;
                  method: Value.T;  holder: Type.T): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.object  := object;
    p.name    := name;
    p.method  := method;
    p.holder  := holder;
    RETURN p;
  END New;

PROCEDURE TypeOf (p: P): Type.T =
  VAR m: Method.Info;
  BEGIN
    Method.SplitX (p.method, m);
    RETURN ProcType.MethodSigAsProcSig (m.signature, p.object);
  END TypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    p.object := Type.Check (p.object);
    p.holder := Type.Check (p.holder);
    Value.TypeCheck (p.method, cs);
    EVAL Type.Check (TypeOf (p));
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.name = b.name)
                 AND Type.IsEqual (a.object, b.object, x);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  VAR
    x := ObjectType.MethodOffset (p.holder);
    method: Method.Info;
  BEGIN
    <* ASSERT NOT StaticOnly *>
    Type.Compile (p.object);
    Method.SplitX (p.method, method);

    Type.LoadInfo (p.object, M3RT.OTC_defaultMethods, addr := TRUE);
    IF (x >= 0) THEN
      INC (method.offset, x);
    ELSE (* runtime offset to methods *)
      Type.LoadInfo (p.holder, M3RT.OTC_methodOffset);
      CG.Index_bytes (Target.Byte);
    END;
    CG.Boost_addr_alignment (Target.Address.align);
    CG.Load_indirect (CG.Type.Addr, method.offset, Target.Address.size);
    CG.Boost_addr_alignment (CG.ProcAlign ());
  END Compile;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  BEGIN
    RETURN LoadMSIR (p.object, p.method, p.holder);
  END CompileMSIR;

(* EXPORTED *)
PROCEDURE LoadMSIR (object: Type.T;  method: Value.T;  holder: Type.T): MSIR.Value =
  VAR
    m    : Method.Info;
    ptrT := MSIR.TPtr (MSIR.TVoid ());
    intT := MSIR.TI (Target.Integer.size);
    base := ObjectType.MethodOffset (holder);
    b    : MSIR.Block;
    tc, defaults, off, htc: MSIR.Value;
  BEGIN
    (* As QualifyExpr.Compile: the holder and the object type (compiling a
       subtype does not compile its supertypes). *)
    Type.Compile (holder);
    Type.Compile (object);
    IF NOT Method.Split (method, m) THEN
      MSIRBuilder.Abandon ("object type method: not a method");
      RETURN NIL;
    END;
    tc := MSIRBuilder.TypeLinkValueForObject (object);
    IF tc = NIL THEN
      MSIRBuilder.Abandon ("object type method: no TypeLink");
      RETURN NIL;
    END;
    b := MSIRBuilder.CurrentBlock ();
    defaults := MSIR.BuildLoad (b, "", ptrT,
                  MSIRBuilder.BuildPtrByteOff (b, "", tc,
                    M3RT.OTC_defaultMethods DIV Target.Char.size));
    IF base >= 0 THEN
      off := MSIR.ConstInt (intT, (base + m.offset) DIV Target.Char.size);
    ELSE
      (* opaque holder: its methods' offset is known only at run time *)
      htc := MSIRBuilder.TypeLinkValueForObject (holder);
      IF htc = NIL THEN
        MSIRBuilder.Abandon ("object type method: no TypeLink for opaque holder");
        RETURN NIL;
      END;
      off := MSIR.BuildIAdd (b, "",
               MSIR.BuildLoad (b, "", intT,
                 MSIRBuilder.BuildPtrByteOff (b, "", htc,
                   M3RT.OTC_methodOffset DIV Target.Char.size)),
               MSIR.ConstInt (intT, m.offset DIV Target.Char.size));
    END;
    RETURN MSIR.BuildLoad (b, "", ptrT, MSIR.BuildGepByte (b, "", defaults, off));
  END LoadMSIR;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "METHOD<");
    (*** M3Buf.PutText (buf, Type.Name (p.object)); ***)
    M3ID.Put   (buf, p.name);
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  type: Type.T;  is_const: BOOLEAN) =
  VAR m_name := Value.GlobalName (p.method, dots := FALSE);
  VAR t_name := Type.Name (p.object);
  VAR name   := t_name & "_" & m_name;
  VAR uid    := Type.GlobalUID (type);
  VAR var    := CG.Import_global (M3ID.Add (name), Target.Address.size,
                                  Target.Address.align, CG.Type.Addr, uid);
  BEGIN
    CG.Init_var (offset, var, 0, is_const);
    Type.Compile (type);
    Error.ID (p.name,
    "CM3 restriction: default method must be a compile-time constant");
  END GenLiteral;

BEGIN
END MethodExpr.
