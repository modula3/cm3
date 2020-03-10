(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: New.m3                                                *)
(* Last Modified On Tue Jun 20 08:30:57 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 12:45:06 PDT 1995 By ericv      *)
(*      Modified On Fri Jan 25 08:10:52 1991 By muller         *)

MODULE New;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Error;
IMPORT RefType, ObjectType, OpaqueType, KeywordExpr, Value;
IMPORT Field, Method, Int, ProcType, AssignStmt, OpenArrayType;
IMPORT Scope, RecordType, TypeExpr, Null, Revelation, Target;
IMPORT ArrayExpr, M3ID, M3RT, RunTyme, ErrType;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    IF NOT TypeExpr.Split (ce.args[0], t) THEN  t := Null.T;
    ELSIF RefType.Is (t)    THEN (* ok *)
    ELSIF ObjectType.Is (t) THEN (* sleazy bug!!  ignore method overrides *)
    ELSIF OpaqueType.Is (t) THEN (* sleazy bug!!  ignore method overrides *)
    ELSE  t := Null.T;
    END;
    RETURN t;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t, r: Type.T;
  BEGIN
    IF KeywordExpr.Is (ce.args[0]) THEN
      Error.Msg ("NEW: keyword bindings not allowed for type");
    END;
    IF NOT TypeExpr.Split (ce.args[0], t) THEN
      Error.Msg ("NEW must be applied to a reference type");
      t := Null.T;
    ELSIF (RefType.Split (t, r)) THEN
      CheckRef (r, ce, cs);
    ELSIF (ObjectType.Is (t)) THEN
      r := CheckObject (t, ce, cs);
      IF (r # t) THEN
        ce.args[0] := TypeExpr.New (r);
        Expr.TypeCheck (ce.args[0], cs);
        t := r;
      END;
    ELSIF (OpaqueType.Is (t)) THEN
      r := CheckOpaque (t, ce, cs);
      IF (r # t) THEN
        ce.args[0] := TypeExpr.New (r);
        Expr.TypeCheck (ce.args[0], cs);
        t := r;
      END;
    ELSIF (t # ErrType.T) THEN
      Error.Msg ("NEW must be applied to a reference type");
    END;
    ce.type := t;
  END Check;

PROCEDURE CheckRef (r: Type.T;  ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR
    base: Type.T;
    fields: Value.T;
    info : Type.Info;
  BEGIN
    IF (r = NIL) THEN
     Error.Msg("cannot NEW a variable of type REFANY, ADDRESS, or NULL");
     RETURN;
    END;
    r := Type.Check (r);
    base := Type.Base (r);
    base := Type.CheckInfo (base, info);
    IF (info.isEmpty) THEN
      Error.Msg ("cannot allocate variables of empty types");
    ELSIF (info.class = Type.Class.OpenArray) THEN
      CheckOpenArray (base, ce);
    ELSIF (info.class = Type.Class.Record) THEN
      CheckRecord (base, ce, cs);
    ELSIF RecordType.Split (base, fields) THEN
      CheckRecord (base, ce, cs);
    ELSIF (NUMBER (ce.args^) > 1) THEN
      Error.Msg ("too many arguments to NEW");
    END;
  END CheckRef;

PROCEDURE CheckOpenArray (r: Type.T;  ce: CallExpr.T) =
  VAR x, elt: Type.T;
  BEGIN
    FOR i := 1 TO LAST (ce.args^) DO
      x := Type.Base (Expr.TypeOf (ce.args[i]));
      IF KeywordExpr.Is (ce.args[i]) THEN
        Error.Msg ("NEW: not a procedure; keyword bindings not allowed for array dimensions");
      END;
      IF  NOT Type.IsEqual (x, Int.T, NIL) THEN
        Error.Int (i, "argument must be an integer");
      ELSIF (NOT OpenArrayType.Split (r, elt)) THEN
        Error.Int (i, "too many dimensions specified");
      ELSE (* ok *)
        r := elt;
      END;
    END;
    IF OpenArrayType.Is (r) THEN
      Error.Msg ("not enough dimensions specified");
    END;
  END CheckOpenArray;

PROCEDURE CheckRecord (t: Type.T;  ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR
    x: Type.T;
    key: M3ID.T;
    value: Expr.T;
    field: Value.T;
  BEGIN
    FOR i := 1 TO LAST (ce.args^) DO
      x := Expr.TypeOf (ce.args[i]);
      IF  NOT KeywordExpr.Split (ce.args[i], key, value) THEN
        Error.Msg ("extra arguments to NEW must include keywords (2.6.9)");
      ELSIF NOT RecordType.LookUp (t, key, field) THEN
        Error.ID (key, "unknown record field");
      ELSIF NOT Field.Is (field) THEN
        Error.ID (key, "undefined field?");
      ELSIF NOT Type.IsAssignable (Value.TypeOf (field), x) THEN
        Error.ID (key, "value is not assignable to field");
      ELSE
        ArrayExpr.NoteUseTargetVar (value);
        AssignStmt.Check (Value.TypeOf (field), value, cs);
      END;
    END;
  END CheckRecord;

PROCEDURE CheckObject (t: Type.T;  ce: CallExpr.T;  VAR cs: Expr.CheckState): Type.T =
  VAR
    x: Type.T;
    key: M3ID.T;
    value: Expr.T;
    v: Value.T;
    visible: Type.T;
    newType: Type.T := NIL;
    fields: Scope.T;
    overrides: Scope.T;
    n_overrides: INTEGER := 0;
    zz: Scope.T;
    method: Method.Info;
    field: Field.Info;
    info: Type.Info;
  BEGIN
    t := Type.CheckInfo (t, info);

    (* first pass, remove the method overrides & build a new object type *)
    FOR i := 1 TO LAST (ce.args^) DO
      x := Expr.TypeOf (ce.args[i]);
      IF KeywordExpr.Split (ce.args[i], key, value)
        AND ObjectType.LookUp (t, key, v, visible)
        AND Method.Split (v, method) THEN
        IF (newType = NIL) THEN
          fields := Scope.PushNew (FALSE, M3ID.NoID); Scope.PopNew ();
          overrides := Scope.PushNew (FALSE, M3ID.NoID); Scope.PopNew ();
          newType := ObjectType.New (t, info.isTraced, NIL, fields, overrides);
        END;
        zz := Scope.Push (overrides);
          method.name      := key;
          method.offset    := n_overrides * Target.Address.size;
          method.parent    := newType;
          method.signature := NIL;
          method.dfault    := value;
          method.override  := TRUE;
          Method.NoteOverride (Method.New (method), v);
          INC (n_overrides);
        Scope.Pop (zz);
      END;
    END;

    IF (newType # NIL) THEN
      newType := Type.Check (newType);
      t := newType;
    END;

    (* second pass, do the checking *)
    FOR i := 1 TO LAST (ce.args^) DO
      x := Expr.TypeOf (ce.args[i]);
      IF NOT KeywordExpr.Split (ce.args[i], key, value) THEN
        Error.Msg ("extra arguments must include keywords");
      ELSIF NOT ObjectType.LookUp (t, key, v, visible) THEN
        Error.ID (key, "unknown object field or method");
      ELSIF Method.Split (v, method) THEN
        IF NOT ProcType.IsCompatible (x, t, method.signature) THEN
          Error.ID (key, "procedure is not compatible with method");
        END;
        ce.args[i] := NIL;
      ELSIF Field.Is (v) THEN
        Field.Split (v, field);
        IF NOT Type.IsAssignable (field.type, x)
        THEN Error.ID (key, "value is not assignable to field");
        ELSE
          ArrayExpr.NoteUseTargetVar (value);
          AssignStmt.Check (field.type, value, cs);
        END;
      ELSE
        Error.ID (key, "undefined?");
      END;
    END;

    RETURN t;
  END CheckObject;

PROCEDURE CheckOpaque (t: Type.T; ce: CallExpr.T;  VAR cs: Expr.CheckState): Type.T =
  (* we already know that t is not an object, so we only need to
     check for a full revelation that says it's a REF *)
  VAR x := Revelation.LookUp (t);  r: Type.T;
  BEGIN
    IF (x = NIL) THEN
      Error.Msg ("cannot apply NEW to non-object, opaque types");
    ELSIF RefType.Split (x, r) THEN
      (* full revelation => t is a REF *)
      CheckRef (r, ce, cs);
    ELSE
      Error.Msg ("cannot apply NEW to this type");
    END;
    RETURN t;
  END CheckOpaque;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Gen (ce);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    (* all the work was done by Prep *)
    CG.Push (ce.tmp);
    CG.Boost_addr_alignment (ce.align);
    CG.Free (ce.tmp);
    ce.tmp := NIL;
  END Compile;

PROCEDURE Gen (ce: CallExpr.T) =
  VAR t, r: Type.T;
  BEGIN
    VAR b := TypeExpr.Split (ce.args[0], t); BEGIN <* ASSERT b *> END;
    Type.Compile (t);
    t := Type.StripPacked (t); 
    IF (RefType.Split (t, r)) THEN GenRef (t, Type.StripPacked (r), ce);
    ELSIF (ObjectType.Is (t)) THEN GenObject (t, ce);
    ELSIF (OpaqueType.Is (t)) THEN GenOpaque (t, ce);
    ELSE Error.Msg ("NEW must name a reference type");
    END;
  END Gen;

PROCEDURE GenRef (t, r: Type.T;  ce: CallExpr.T) =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedRef,
                                                RunTyme.Hook.NewTracedRef };
  VAR
    base   := Type.Base (r);
    fields : Value.T;
    proc   : Procedure.T;
    t_info : Type.Info;
    r_info : Type.Info;
  BEGIN
    t := Type.CheckInfo (t, t_info);
    r := Type.Check (r);
    r := Type.StripPacked (r);
    r := Type.CheckInfo (r, r_info);

    IF (r_info.class = Type.Class.OpenArray) THEN
      GenOpenArray (t, t_info.isTraced, r_info, ce);

    ELSIF RecordType.Split (base, fields) THEN
      GenRecord (t, base, t_info.isTraced, r_info, ce);

    ELSE
      proc := RunTyme.LookUpProc (PHook [t_info.isTraced]);
      Procedure.StartCall (proc);
      Type.LoadInfo (t, -1);
      CG.Pop_param (CG.Type.Addr);
      ce.tmp := Procedure.EmitValueCall (proc);
      ce.align := r_info.alignment;
    END;
  END GenRef;

PROCEDURE GenOpenArray (t: Type.T;  traced: BOOLEAN;
                        READONLY r_info: Type.Info;  ce: CallExpr.T) =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedArray,
                                                RunTyme.Hook.NewTracedArray };
  VAR
    n := LAST (ce.args^); (* number of open dimensions *)
    sizes := CG.Declare_temp (Target.Address.pack + (n+1)*Target.Integer.pack,
                              Target.Address.align, CG.Type.Struct,
                              in_memory := TRUE);
    offset: INTEGER;
    proc := RunTyme.LookUpProc (PHook [traced]);
  BEGIN
    (* initialize the pointer to the array sizes *)
    CG.Load_addr_of (sizes, M3RT.OA_size_1, Target.Integer.align);
    CG.Store_addr (sizes, M3RT.OA_elt_ptr);

    (* initialize the count of array sizes *)
    CG.Load_intt (n);
    CG.Store_int (Target.Integer.cg_type, sizes, M3RT.OA_size_0);

    (* initialize each array size *)
    offset := M3RT.OA_size_1;
    FOR i := 1 TO n DO
      Expr.Prep (ce.args[i]);
      Expr.Compile (ce.args[i]);
      CG.Store_int (Target.Integer.cg_type, sizes, offset);
      INC (offset, Target.Integer.pack);
    END;

    (* allocate the storage *)
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      Type.LoadInfo (t, -1);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_addr_of (sizes, 0, Target.Integer.align);
      CG.Pop_param (CG.Type.Addr);
    ELSE
      CG.Load_addr_of (sizes, 0, Target.Integer.align);
      CG.Pop_param (CG.Type.Addr);
      Type.LoadInfo (t, -1);
      CG.Pop_param (CG.Type.Addr);
    END;
    ce.tmp := Procedure.EmitValueCall (proc);
    ce.align := r_info.alignment;

    CG.Free_temp (sizes);
  END GenOpenArray;

PROCEDURE GenRecord (t, r: Type.T;  traced: BOOLEAN;
                     READONLY r_info: Type.Info;  ce: CallExpr.T) =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedRef,
                                                RunTyme.Hook.NewTracedRef };
  VAR
    key: M3ID.T;
    value: Expr.T;
    field: Field.Info;
    v: Value.T;
    align := r_info.alignment;
    proc := RunTyme.LookUpProc (PHook [traced]);
    b: BOOLEAN;
  BEGIN
    (* allocate the record's storage *)
    Procedure.StartCall (proc);
    Type.LoadInfo (t, -1);
    CG.Pop_param (CG.Type.Addr);
    ce.tmp := Procedure.EmitValueCall (proc);
    ce.align := align;

    (* do the user specified initialization *)
    FOR i := 1 TO LAST (ce.args^) DO
      b := KeywordExpr.Split (ce.args[i], key, value); <*ASSERT b*>
      AssignStmt.PrepForEmit (field.type, value, initializing := TRUE);
      EVAL RecordType.LookUp (r, key, v);
      Field.Split (v, field);
      CG.Push (ce.tmp);
      CG.Boost_addr_alignment (align);
      CG.Add_offset (field.offset);
      AssignStmt.DoEmit (field.type, value);
    END;
  END GenRecord;

PROCEDURE GenObject (t: Type.T;  ce: CallExpr.T) =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedObj,
                                                RunTyme.Hook.NewTracedObj };
  VAR
    key: M3ID.T;
    value: Expr.T;
    field: Field.Info;
    v: Value.T;
    visible: Type.T;
    obj_offset: INTEGER;
    obj_align: INTEGER;
    proc: Procedure.T;
    info: Type.Info;
    b: BOOLEAN;
  BEGIN
    t := Type.CheckInfo (t, info);
    proc := RunTyme.LookUpProc (PHook [info.isTraced]);

    (* allocate the object's storage *)
    Procedure.StartCall (proc);
    Type.LoadInfo (t, -1);
    CG.Pop_param (CG.Type.Addr);
    ce.tmp := Procedure.EmitValueCall (proc);
    ce.align := info.alignment;

    (* do the user specified initialization *)
    FOR i := 1 TO LAST (ce.args^) DO
      IF (ce.args[i] # NIL) THEN
        b := KeywordExpr.Split (ce.args[i], key, value); <*ASSERT b*>
        AssignStmt.PrepForEmit (field.type, value, initializing := TRUE);
        b := ObjectType.LookUp (t, key, v, visible); <*ASSERT b*>
        Field.Split (v, field);
        CG.Push (ce.tmp);
        ObjectType.GetFieldOffset (visible, obj_offset, obj_align);
        IF (obj_offset >= 0) THEN
          INC (field.offset, obj_offset);
        ELSE
          Type.LoadInfo (visible, M3RT.OTC_dataOffset);
          CG.Index_bytes (Target.Byte);
        END;
        CG.Add_offset (field.offset);
        CG.Boost_addr_alignment (obj_align);
        AssignStmt.DoEmit (field.type, value);
      END;
    END;
  END GenObject;

PROCEDURE GenOpaque (t: Type.T;  ce: CallExpr.T) =
  VAR x := Revelation.LookUp (t);  r: Type.T;
  BEGIN
    IF (x = NIL) THEN
      <* ASSERT FALSE *>
    ELSIF RefType.Split (x, r) THEN
      (* full revelation => t is a REF *)
      GenRef (x, Type.StripPacked (r), ce);
    ELSE
      <* ASSERT FALSE *>
    END;
  END GenOpaque;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, LAST (INTEGER), TRUE, TRUE, TRUE, NIL,
                                 TypeOf,
                                 TypeOf,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("NEW", Z, TRUE);
  END Initialize;

BEGIN
END New.
