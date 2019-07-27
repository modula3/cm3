(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Constant.m3                                           *)
(* Last Modified On Tue Feb 28 16:58:43 PST 1995 By kalsow     *)

MODULE Constant;

IMPORT M3, M3ID, CG, Value, ValueRep, Type, Expr, Scope, Error;
IMPORT Token, AssignStmt, Scanner, UserProc, Target, M3Buf;
IMPORT Decl, ProcType, Procedure, OpenArrayType, Module, ErrType;
IMPORT ArrayExpr, ExprRep;
FROM Scanner IMPORT GetToken, Match, MatchID, cur;

REVEAL
  T = Value.T BRANDED "Constant.T" OBJECT
        tipe       : Type.T;
        valExpr    : Expr.T;
        offset     : INTEGER;
        (* ^ Of a pointer, located within the static variable area, to the
             constant's value, located in the static constant area. *)
        coffset    : INTEGER;
        (* ^ Of a place in the static constant area for the constant's value.
             Dope only, if constant has open array representation. *)
        calign     : INTEGER;
        structured : BOOLEAN;
        gen_init   : BOOLEAN;
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

(* EXPORTED: *)
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
      t.valExpr := Expr.Parse ();
      Scope.Insert (t);
      Match (TK.tSEMI);
    END;
  END ParseDecl;

PROCEDURE Create (name: M3ID.T): T =
  VAR t: T;
  BEGIN
    t := NEW (T);
    ValueRep.Init (t, name, Value.Class.Expr);
    t.readonly   := TRUE;
    t.tipe       := NIL;
    t.valExpr    := NIL;
    t.offset     := 0;
    t.coffset    := 0;
    t.calign     := 0;
    t.structured := FALSE;
    t.gen_init   := FALSE;
    RETURN t;
  END Create;

(* EXPORTED: *)
PROCEDURE Declare (name: TEXT;  valExpr: Expr.T;  reserved: BOOLEAN) =
  VAR t: T;
  BEGIN
    t := Create (M3ID.Add (name));
    t.tipe := Expr.TypeOf (valExpr);
    t.valExpr := valExpr;
    Scope.Insert (t);
    IF (reserved) THEN Scanner.NoteReserved (t.name, t) END;
  END Declare;

(* Externally dispatched-to: *)
PROCEDURE TypeOf
 (t: T): Type.T =
  BEGIN
    IF (t.tipe = NIL) THEN t.tipe := Expr.TypeOf (t.valExpr) END;
    RETURN t.tipe;
  END TypeOf;

(* Externally dispatched-to: *)
PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
  VAR e: Expr.T;  proc: Value.T;  n_errs0, n_errs1, n_warns: INTEGER;
  BEGIN
    Error.Count (n_errs0, n_warns);
    Expr.TypeCheck (t.valExpr, cs);
    IF t.tipe = NIL THEN t.tipe := Expr.TypeOf (t.valExpr) END;
    t.tipe := Type.Check (t.tipe);
    Error.Count (n_errs1, n_warns);

    IF ProcType.Is (t.tipe)
      AND UserProc.IsProcedureLiteral (t.valExpr, proc)
      AND Procedure.IsNested (proc) THEN
      Error.Msg ("nested procedures are not constants");
    END;

    IF (t.tipe = ErrType.T) THEN
      (* there's no way that we can evaluate the constant *)
      t.structured := FALSE;
      IF (n_errs1 <= n_errs0) THEN
        (* no error was generated, but we don't have a type! *)
        Error.Msg ("value is not a constant expression");
      END;
    ELSE
      t.valExpr.isNamedConst := TRUE;
      AssignStmt.Check (t.tipe, t.valExpr, cs);
      e := Expr.ConstValue (t.valExpr);
      (* N.B. ^This will strip away both a NamedExpr.T and a ConsExpr.T from
               above a constant ArrayExpr.T. *)
      IF (t.valExpr # NIL) AND (e = NIL)
      THEN Error.Msg ("value is not constant");
      ELSE
        e.isNamedConst := TRUE;
        ArrayExpr.NoteTargetType (e, t.tipe);
        t.valExpr := e;
      END;
      t.structured := Type.IsStructured (t.tipe);
    END;
  END Check;

(* Externally dispatched-to: *)
PROCEDURE SetGlobals (t: T) =
  VAR size, align, depth: INTEGER;
  VAR repType: Type.T;
  VAR info: Type.Info;
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    IF (t.offset # 0) OR (NOT t.structured) THEN RETURN END;

    repType := Expr.RepTypeOf (t.valExpr);
    EVAL Type.CheckInfo (repType, info);
    size  := info.size;
    align := info.alignment;
    depth := OpenArrayType.OpenDepth (repType);

    IF (depth > 0) THEN (* t.tipe is an open array *)
      (* Allocate space for the dope only. *)
      (* See ArrayExpr.GenLiteral. *)
      size := Target.Address.pack + depth * Target.Integer.pack;
      align := MAX (Target.Address.align, Target.Integer.align);
    END;

    t.calign  := align;
(* TODO: Eliminate duplicate copies of same value. *) 
    t.coffset := Module.Allocate (size, align, TRUE, "constant ", id := t.name);
    t.offset  := Module.Allocate (Target.Address.size, Target.Address.align,
                                  FALSE, "constant", id := t.name);
  END SetGlobals;

(* Externally dispatched-to: *)
PROCEDURE Load (t: T) =
(* Note: because a named constant may be the default value for
   a procedure parameter, it is possible for a structured constant
   to be used in a compilation unit without anywhere mentioning
   its name => its use would not be detected => it wouldn't be
   imported.  To prevent this, we force the import here by calling
   Scope.ToUnit. *)
  BEGIN
    IF (t.structured) THEN
      SetGlobals (t);
      IF (t.imported) THEN
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
        CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);
        CG.Boost_addr_alignment (t.calign);
      ELSE
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.coffset, is_const := TRUE);
      END;
    ELSE
      Expr.Prep (t.valExpr);
      Expr.Compile (t.valExpr);
    END;
  END Load;

(* Externally dispatched-to: *)
PROCEDURE Declarer (t: T): BOOLEAN =
  VAR type: CG.TypeUID;  size, depth: INTEGER;  info: Type.Info;
  BEGIN
    IF (t.exported) THEN Type.Compile (t.tipe) END;
    IF (NOT t.structured) THEN RETURN TRUE END;

    EVAL Type.CheckInfo (t.tipe, info);
    Type.Compile (t.tipe);
    type  := Type.GlobalUID (t.tipe);
    size  := info.size;
    depth := OpenArrayType.OpenDepth (t.tipe);

    IF (depth > 0) THEN (* t.tipe is an open array *)
      (* Declare debug field for the dope only. *)
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

(* Externally dispatched-to: *)
PROCEDURE ConstInit (t: T) =
  BEGIN
    IF t.gen_init THEN
      t.gen_init := FALSE;

      CG.Comment
        (t.offset, FALSE, "Address of constant ", M3ID.ToText (t.name));
      CG.Init_var (t.offset, Module.GlobalData (TRUE), t.coffset, FALSE);

      CG.Comment
        (t.coffset, TRUE, "Contents of constant", M3ID.ToText (t.name));

      Expr.PrepLiteral (t.valExpr, t.tipe, TRUE);
      Expr.GenLiteral (t.valExpr, t.coffset, t.tipe, TRUE);
    END;
  END ConstInit;

(* Externally dispatched-to: *)
PROCEDURE ToExpr (t: T): Expr.T =
  BEGIN
    RETURN t.valExpr;
  END ToExpr;

(* Externally dispatched-to: *)
PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  BEGIN
    ValueRep.FPStart (t, x, "CONST ", t.offset, global := TRUE);
    M3Buf.PutText (x.buf, " = ");
    Expr.GenFPLiteral (t.valExpr, x.buf);
    RETURN 1;
  END AddFPTag;

BEGIN
END Constant.
