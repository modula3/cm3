(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Constant.m3                                           *)
(* Last Modified On Tue Feb 28 16:58:43 PST 1995 By kalsow     *)

MODULE Constant;

IMPORT M3, M3ID, CG, Value, ValueRep, Type, Expr, Scope, Error;
IMPORT Token, AssignStmt, Scanner, UserProc, Target, M3Buf;
IMPORT Decl, ProcType, Procedure, OpenArrayType, Module, ErrType;
FROM Scanner IMPORT GetToken, Match, MatchID, cur;

REVEAL
  T = Value.T BRANDED "Constant.T" OBJECT
        tipe     : Type.T;
        value    : Expr.T;
        offset   : INTEGER;
        coffset  : INTEGER;
        calign   : INTEGER;
        explicit : BOOLEAN;
        gen_init : BOOLEAN;
      OVERRIDES
        typeCheck   := Check;
        set_globals := SetGlobals;
        load        := Load;
        declare     := Declarer;
        const_init  := ConstInit;
        need_init   := ValueRep.Never;
        lang_init   := ValueRep.NoInit;
        user_init   := ValueRep.NoInit;
        toExpr      := ToExpr;
        toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := TypeOf;
      END;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes) =
  TYPE TK = Token.T;
  VAR t: T; id: M3ID.T;
  BEGIN
    Match (TK.tCONST);
    WHILE (cur.token = TK.tIDENT) DO
      id := MatchID ();
      t := Create (id);
      t.unused := att.isUnused;
      t.obsolete := att.isObsolete;
      IF (cur.token = TK.tCOLON) THEN
        GetToken (); (* : *)
        t.tipe := Type.Parse ();
      END;
      Match (TK.tEQUAL);
      t.value := Expr.Parse ();
      Scope.Insert (t);
      Match (TK.tSEMI);
    END;
  END ParseDecl;

PROCEDURE Create (name: M3ID.T): T =
  VAR t: T;
  BEGIN
    t := NEW (T);
    ValueRep.Init (t, name, Value.Class.Expr);
    t.readonly := TRUE;
    t.tipe     := NIL;
    t.value    := NIL;
    t.offset   := 0;
    t.coffset  := 0;
    t.calign   := 0;
    t.explicit := FALSE;
    t.gen_init := FALSE;
    RETURN t;
  END Create;

PROCEDURE Declare (name: TEXT;  value: Expr.T;  reserved: BOOLEAN) =
  VAR t: T;
  BEGIN
    t := Create (M3ID.Add (name));
    t.tipe := Expr.TypeOf (value);
    t.value := value;
    Scope.Insert (t);
    IF (reserved) THEN Scanner.NoteReserved (t.name, t) END;
  END Declare;

PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    IF (t.tipe = NIL) THEN t.tipe := Expr.TypeOf (t.value) END;
    RETURN t.tipe;
  END TypeOf;

PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
  VAR e: Expr.T;  proc: Value.T;  n_errs0, n_errs1, n_warns: INTEGER;
  BEGIN
    Error.Count (n_errs0, n_warns);
    Expr.TypeCheck (t.value, cs);
    t.tipe := Type.Check (TypeOf (t));
    Error.Count (n_errs1, n_warns);

    IF ProcType.Is (t.tipe)
      AND UserProc.IsProcedureLiteral (t.value, proc)
      AND Procedure.IsNested (proc) THEN
      Error.Msg ("nested procedures are not constants");
    END;

    IF (t.tipe = ErrType.T) THEN
      (* there's no way that we can evaluate the constant *)
      t.explicit := FALSE;
      IF (n_errs1 <= n_errs0) THEN
        (* no error was generated, but we don't have a type! *)
        Error.Msg ("value is not a constant expression");
      END;
    ELSE
      AssignStmt.Check (t.tipe, t.value, cs);
      e := Expr.ConstValue (t.value);
      IF (t.value # NIL) AND (e = NIL)
        THEN Error.Msg ("value is not constant");
        ELSE t.value := e;
      END;
      t.explicit := Type.IsStructured (t.tipe);
    END;
  END Check;

PROCEDURE SetGlobals (t: T) =
  VAR size, align, depth: INTEGER;  info: Type.Info;
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    IF (t.offset # 0) OR (NOT t.explicit) THEN RETURN END;

    EVAL Type.CheckInfo (t.tipe, info);
    size  := info.size;
    align := info.alignment;
    depth := OpenArrayType.OpenDepth (t.tipe);

    IF (depth > 0) THEN
      (* t.tipe is an open array *)
      size := Target.Address.pack + depth * Target.Integer.pack;
      align := MAX (Target.Address.align, Target.Integer.align);
    END;

    t.calign  := align;
    t.coffset := Module.Allocate (size, align, TRUE, "constant ", id := t.name);
    t.offset  := Module.Allocate (Target.Address.size, Target.Address.align,
                                  FALSE, "constant", id := t.name);
  END SetGlobals;

PROCEDURE Load (t: T) =
(* Note: because a named constant may be the default value for
   a procedure parameter, it is possible for a structured constant
   to be used in a compilation unit without anywhere mentioning
   its name => its use will not be detected => it won't be
   imported => we force the import here by calling Scope.ToUnit. *)
  BEGIN
    IF (t.explicit) THEN
      SetGlobals (t);
      IF (t.imported) THEN
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
        CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);
        CG.Boost_addr_alignment (t.calign);
      ELSE
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.coffset, is_const := TRUE);
      END;
    ELSE
      Expr.Prep (t.value);
      Expr.Compile (t.value);
    END;
  END Load;

PROCEDURE Declarer (t: T): BOOLEAN =
  VAR type: CG.TypeUID;  size, depth: INTEGER;  info: Type.Info;
  BEGIN
    IF (t.exported) THEN Type.Compile (t.tipe) END;
    IF (NOT t.explicit) THEN RETURN TRUE END;

    EVAL Type.CheckInfo (t.tipe, info);
    Type.Compile (t.tipe);
    type  := Type.GlobalUID (t.tipe);
    size  := info.size;
    depth := OpenArrayType.OpenDepth (t.tipe);

    IF (depth > 0) THEN
      (* t.tipe is an open array *)
      size := Target.Address.pack + depth * Target.Integer.pack;
    END;

    IF (t.imported) THEN
      EVAL Scope.ToUnit (t); (* force the module to be imported *)
    ELSE
      SetGlobals (t);
      CG.Declare_global_field (t.name, t.offset, Target.Address.size,
                               CG.Declare_indirect (type), is_const := FALSE);
      CG.Declare_global_field (t.name, t.coffset, size,
                               type, is_const := TRUE);
      t.gen_init := TRUE;
    END;

    RETURN TRUE;
  END Declarer;

PROCEDURE ConstInit (t: T) =
  BEGIN
    IF t.gen_init THEN
      t.gen_init := FALSE;

      CG.Comment (t.offset, FALSE, "constant ", M3ID.ToText (t.name));
      CG.Init_var (t.offset, Module.GlobalData (TRUE), t.coffset, FALSE);

      CG.Comment (t.coffset, TRUE, "constant ", M3ID.ToText (t.name));
      Expr.PrepLiteral (t.value, t.tipe, TRUE);
      Expr.GenLiteral (t.value, t.coffset, t.tipe, TRUE);
    END;
  END ConstInit;

PROCEDURE ToExpr (t: T): Expr.T =
  BEGIN
    RETURN t.value;
  END ToExpr;

PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  BEGIN
    ValueRep.FPStart (t, x, "CONST ", t.offset, global := TRUE);
    M3Buf.PutText (x.buf, " = ");
    Expr.GenFPLiteral (t.value, x.buf);
    RETURN 1;
  END AddFPTag;

BEGIN
END Constant.
