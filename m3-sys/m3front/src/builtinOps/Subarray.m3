(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Subarray.m3                                           *)
(* Last Modified On Wed Jun 29 17:03:23 PDT 1994 By kalsow     *)
(*      Modified On Thu Mar  7 20:18:53 1991 By muller         *)

MODULE Subarray;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Error, ArrayType, Card;
IMPORT OpenArrayType, CheckExpr, Host, Target, TInt, M3RT, IntegerExpr;
IMPORT ErrType; 

VAR Z: CallExpr.MethodList;

(* Called indirectly through MethodList. *)
PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  BEGIN
    RETURN ArrayType.OpenCousin (Type.Base (Expr.TypeOf (ce.args[0])));
  END TypeOf;

(* Called indirectly through MethodList. *)
PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t, u, v, index, elt: Type.T;
  BEGIN
    t := Type.Base (Expr.TypeOf (ce.args[0]));
    u := Expr.TypeOf (ce.args[1]);
    v := Expr.TypeOf (ce.args[2]);
    ce.type := ErrType.T; (* May change. *) 
    IF (NOT ArrayType.Split (t, index, elt)) THEN
      Error.Msg ("SUBARRAY: first argument must be an array (2.6.3).");
    ELSIF (NOT Type.IsAssignable (Card.T, u)) THEN
      Error.Msg ("SUBARRAY: second argument must be assignable to CARDINAL (2.6.3).");
    ELSIF (NOT Type.IsAssignable (Card.T, v)) THEN
      Error.Msg ("SUBARRAY: third argument must be assignable to CARDINAL (2.6.3).");
    ELSIF ArrayType.EltAlign (t) MOD Target.Byte # 0 THEN
      Error.Msg ("CM3 restriction: SUBARRAY elements must be byte-aligned (2.2.5).");
    ELSE
      ce.args[1] := RTCheckNonNeg (ce.args[1], cs);
      ce.args[2] := RTCheckNonNeg (ce.args[2], cs);
      Expr.NeedsAddress (ce.args[0]);
      t := ArrayType.OpenCousin (t);
      ce.type := Type.Check (t);
    END;
  END Check;

PROCEDURE RTCheckNonNeg (e: Expr.T;  VAR cs: Expr.CheckState): Expr.T =
  VAR min, max: Target.Int;
  BEGIN
    IF (e = NIL) THEN RETURN NIL; END;
(* TODO: Warn on statically-detectable negative value. *)
    Expr.GetBounds (e, min, max);
    IF TInt.LT (min, TInt.Zero) OR TInt.LT (max, min) THEN
      e := CheckExpr.NewLower (e, TInt.Zero, CG.RuntimeError.ValueOutOfRange);
      Expr.TypeCheck (e, cs);
    END;
    RETURN e;
  END RTCheckNonNeg;

(* Called indirectly through MethodList. *)
PROCEDURE NeedsAddress (<*UNUSED*> ce: CallExpr.T) =
  BEGIN
    (* yes, all subarrays get memory addresses *)
  END NeedsAddress;

(* Called indirectly through MethodList. *)
PROCEDURE SubarrayExprAlign (ce: CallExpr.T): Type.BitAlignT =
(* TODO: Take advantage of static values of ce.args[1] and ce.args[2]. *)
  VAR 
    arrayExpr := ce.args[0];
    arrayType := Type.Base (Expr.TypeOf (arrayExpr));
    eltPack, align, len : INTEGER; 
  BEGIN
    IF OpenArrayType.Is (arrayType) THEN
      align := OpenArrayType.EltAlign (arrayType);
      eltPack := OpenArrayType.EltPack (arrayType);
    ELSE (* Fixed array. *)
      align := Expr.Alignment(arrayExpr);
      IF IntegerExpr.ToInt (Expr.ConstValue(ce.args[2]), len) THEN
        IF len <= 1 THEN RETURN align END;
      END; 
      eltPack  := ArrayType.EltPack (arrayType);
    END;
    RETURN CG.GCD (align, eltPack);
  END SubarrayExprAlign;

(* Called indirectly through MethodList. *)
PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    PrepLV (ce, traced := FALSE);
  END Prep;

(* Called indirectly through MethodList. *)
PROCEDURE PrepLV (ce: CallExpr.T; traced: BOOLEAN) =
  VAR
    base      := ce.args[0];
    start     := ce.args[1];
    len       := ce.args[2];
    array     := Type.Base (Expr.TypeOf (base));
    open      := ArrayType.OpenCousin (array);
    src_depth := OpenArrayType.OpenDepth (array);
    elt_pack  := ArrayType.EltPack (array);

    index, element: Type.T;
    t_result: CG.Var;
    t_base, t_start: CG.Val;
    i_start, i_len : INTEGER;
    case: [0..7];
    n_elts, x_len, x_start, max: Target.Int;
  BEGIN
    Type.Compile (array);
    Type.Compile (open);
    EVAL ArrayType.Split (array, index, element);

    Expr.PrepLValue (base, traced);

    (* determine which case to use *)
    case := 0;
    IF (src_depth # 0)
      THEN (*Open array*) case := 2_100;
      ELSE n_elts := Type.Number (index);
    END;
    IF GetCard (start, i_start, x_start)
      THEN (*Constant stert*) INC (case, 2_010);
      ELSE Expr.Prep (start);
    END;
    IF GetCard (len,   i_len,   x_len)
      THEN (*Constant length*)INC (case, 2_001);
      ELSE Expr.Prep (len);
    END;

    (* declare space for the result *)
    t_result := OpenArrayType.DeclareDopeTemp (open);

    CASE case OF
    | 2_000 =>  (* fixed array, var start, var len ------------------------------*)

          (* initialize the new count *)
          Expr.Compile (len);
          CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);

          IF NOT Host.doRangeChk THEN
            Expr.CompileAddress (base, traced);
            Expr.Compile (start);
          ELSE
            Expr.Compile (start);         t_start := CG.Pop ();
            CG.Push (t_start);
            CG.Load_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
            CG.Add (Target.Integer.cg_type);
            CG.Check_hi (Target.Integer.cg_type, n_elts,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Discard (Target.Integer.cg_type);
            Expr.CompileAddress (base, traced);
            CG.Push (t_start);
            CG.Free (t_start);
          END;

          (* initialize the new data pointer *)
          CG.Index_bytes (elt_pack);
          CG.Store_addr (t_result, M3RT.OA_elt_ptr);

    | 2_001 =>  (* fixed array, var start, const len ----------------------------*)

          (* initialize the new count *)
          CG.Load_integer (Target.Integer.cg_type, x_len);
          CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);

          IF NOT Host.doRangeChk THEN
            Expr.CompileAddress (base, traced);
            Expr.Compile (start);
          ELSIF NOT TInt.Subtract (n_elts, x_len, max) THEN
            (* cannot compute n-len at compile time *)
           Expr.Compile (start);         t_start := CG.Pop ();
            CG.Push (t_start);
            CG.Load_integer (Target.Integer.cg_type, x_len);
            CG.Add (Target.Integer.cg_type);
            CG.Check_hi (Target.Integer.cg_type,
                         n_elts, CG.RuntimeError.ValueOutOfRange);
           CG.Discard (Target.Integer.cg_type);
            Expr.CompileAddress (base, traced);
            CG.Push (t_start);
            CG.Free (t_start);
          ELSIF TInt.LT (max, TInt.Zero) THEN
            Error.Warn (2, "SUBARRAY length out of range");
            Expr.CompileAddress (base, traced);
            Expr.Compile (start);
            CG.Check_hi (Target.Integer.cg_type,
                         max, CG.RuntimeError.ValueOutOfRange);
          ELSE
            Expr.CompileAddress (base, traced);
            Expr.Compile (start);
            CG.Check_hi (Target.Integer.cg_type, max,
                         CG.RuntimeError.ValueOutOfRange);
          END;

          (* initialize the new data pointer *)
          CG.Index_bytes (elt_pack);
          CG.Store_addr (t_result, M3RT.OA_elt_ptr);

    | 2_010 =>  (* fixed array, const start, var len ----------------------------*)

          IF NOT Host.doRangeChk THEN
            (* no check => initialize the new count *)
            Expr.Compile (len);
            CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
          ELSIF NOT TInt.Subtract (n_elts, x_start, max) THEN
            (* initialize the new count *)
            Expr.Compile (len);
            CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
            (* cannot compute n-start at compile time *)
            CG.Load_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
            CG.Load_integer (Target.Integer.cg_type, x_start);
            CG.Add (Target.Integer.cg_type);
            CG.Check_hi (Target.Integer.cg_type, n_elts,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Discard (Target.Integer.cg_type);
          ELSIF TInt.LT (max, TInt.Zero) THEN
            (* initialize and check the new count *)
            Error.Warn (2, "SUBARRAY initial index out of range");
            Expr.Compile (len);
            CG.Check_hi (Target.Integer.cg_type,
                         max, CG.RuntimeError.ValueOutOfRange);
            CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
          ELSE
            (* initialize and check the new count *)
            Expr.Compile (len);
            CG.Check_hi (Target.Integer.cg_type, max,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
          END;

          (* initialize the new data pointer *)
          Expr.CompileAddress (base, traced);
          CG.Add_offset (i_start * elt_pack);
          CG.Store_addr (t_result, M3RT.OA_elt_ptr);

    | 2_011 =>  (* fixed array, const start, const len --------------------------*)

          (* initialize the new count *)
          CG.Load_integer (Target.Integer.cg_type, x_len);
          CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);

          IF NOT Host.doRangeChk THEN
            (* no check *)
          ELSIF NOT TInt.Add (x_start, x_len, max) THEN
            (* cannot compute start+len at compile time *)
            CG.Load_integer (Target.Integer.cg_type, x_start);
            CG.Load_integer (Target.Integer.cg_type, x_len);
            CG.Add (Target.Integer.cg_type);
            CG.Check_hi (Target.Integer.cg_type, n_elts,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Discard (Target.Integer.cg_type);
          ELSIF TInt.LT (n_elts, max) THEN
            (* oops, they're too big *)
            Error.Warn (2, "SUBARRAY start+length out of range (2.6.3).");
            CG.Load_integer (Target.Integer.cg_type, max);
            CG.Check_hi (Target.Integer.cg_type, n_elts,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Discard (Target.Integer.cg_type);
          ELSE
            (* ok *)
          END;

          (* initialize the new data pointer *)
          Expr.CompileAddress (base, traced);
          CG.Add_offset (i_start * elt_pack);
          CG.Store_addr (t_result, M3RT.OA_elt_ptr);

    | 2_100 =>  (* open array, var start, var len -------------------------------*)

          Expr.CompileAddress (base, traced);   t_base  := CG.Pop ();
          Expr.Compile (start);                 t_start := CG.Pop ();

          (* initialize the new counts *)
          Expr.Compile (len);
          CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
          CopyDopeVector (t_base, t_result, src_depth);

          IF Host.doRangeChk THEN
            CG.Push (t_start);
            CG.Load_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
            CG.Add (Target.Integer.cg_type);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (Target.Integer.cg_type);
            CG.Check_hi (Target.Integer.cg_type, TInt.Zero,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Discard (Target.Integer.cg_type);
          END;

          (* initialize the new data pointer *)
          CG.Push (t_base);
          CG.Open_elt_ptr (ArrayType.EltAlign (array));
          CG.Push (t_start);
          ComputeOffset (t_result, src_depth, elt_pack);

          CG.Free (t_base);
          CG.Free (t_start);

    | 2_101 =>  (* open array, var start, const len -----------------------------*)

          Expr.CompileAddress (base, traced);   t_base  := CG.Pop ();
          Expr.Compile (start);                 t_start := CG.Pop ();

          (* initialize the new counts *)
          CG.Load_integer (Target.Integer.cg_type, x_len);
          CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
          CopyDopeVector (t_base, t_result, src_depth);

          IF Host.doRangeChk THEN
            CG.Push (t_start);
            CG.Load_integer (Target.Integer.cg_type, x_len);
            CG.Add (Target.Integer.cg_type);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (Target.Integer.cg_type);
            CG.Check_hi (Target.Integer.cg_type, TInt.Zero,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Discard (Target.Integer.cg_type);
          END;

          (* initialize the new data pointer *)
          CG.Push (t_base);
          CG.Open_elt_ptr (ArrayType.EltAlign (array));
          CG.Push (t_start);
          ComputeOffset (t_result, src_depth, elt_pack);

          CG.Free (t_base);
          CG.Free (t_start);

    | 2_110 =>  (* open array, const start, var len -----------------------------*)

          Expr.CompileAddress (base, traced);   t_base  := CG.Pop ();

          (* initialize the new counts *)
          Expr.Compile (len);
          CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
          CopyDopeVector (t_base, t_result, src_depth);

          IF Host.doRangeChk THEN
            CG.Load_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
            CG.Load_integer (Target.Integer.cg_type, x_start);
            CG.Add (Target.Integer.cg_type);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (Target.Integer.cg_type);
            CG.Check_hi (Target.Integer.cg_type, TInt.Zero,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Discard (Target.Integer.cg_type);
          END;

          (* initialize the new data pointer *)
          CG.Push (t_base);
          CG.Open_elt_ptr (ArrayType.EltAlign (array));
          IF (src_depth <= 1) THEN
            CG.Add_offset (i_start * elt_pack);
            CG.Store_addr (t_result, M3RT.OA_elt_ptr);
          ELSE
            CG.Load_integer (Target.Integer.cg_type, x_start);
            ComputeOffset (t_result, src_depth, elt_pack);
          END;

          CG.Free (t_base);

    | 2_111 =>  (* open array, const start, const len ---------------------------*)

          Expr.CompileAddress (base, traced);   t_base  := CG.Pop ();

          (* initialize the new counts *)
          CG.Load_integer (Target.Integer.cg_type, x_len);
          CG.Store_int (Target.Integer.cg_type, t_result, M3RT.OA_size_0);
          CopyDopeVector (t_base, t_result, src_depth);

          IF NOT Host.doRangeChk THEN
            (* no check *)
          ELSIF TInt.Add (x_start, x_len, max) THEN
            CG.Load_integer (Target.Integer.cg_type, max);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (Target.Integer.cg_type);
            CG.Check_hi (Target.Integer.cg_type, TInt.Zero,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Discard (Target.Integer.cg_type);
          ELSE
            CG.Load_integer (Target.Integer.cg_type, x_start);
            CG.Load_integer (Target.Integer.cg_type, x_len);
            CG.Add (Target.Integer.cg_type);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (Target.Integer.cg_type);
            CG.Check_hi (Target.Integer.cg_type, TInt.Zero,
                         CG.RuntimeError.ValueOutOfRange);
            CG.Discard (Target.Integer.cg_type);
          END;

          (* initialize the new data pointer *)
          CG.Push (t_base);
          CG.Open_elt_ptr (ArrayType.EltAlign (array));
          IF (src_depth <= 1) THEN
            CG.Add_offset (i_start * elt_pack);
            CG.Store_addr (t_result, M3RT.OA_elt_ptr);
          ELSE
            CG.Load_integer (Target.Integer.cg_type, x_start);
            ComputeOffset (t_result, src_depth, elt_pack);
          END;

          CG.Free (t_base);

    END; (* CASE *)

    (* leave the new subarray as the result. *)
    CG.Load_addr_of_temp (t_result, 0, Target.Address.align);
    ce.tmp := CG.Pop ();

  END PrepLV;

PROCEDURE GetCard (e: Expr.T;  VAR i: INTEGER;  VAR x: Target.Int): BOOLEAN =
  VAR t: Type.T;
  BEGIN
    e := Expr.ConstValue (e);
    IF (e = NIL) THEN RETURN FALSE END;
    RETURN IntegerExpr.Split (e, x, t)
       AND TInt.ToInt (x, i)
       AND (0 <= i);
  END GetCard;

PROCEDURE CopyDopeVector (src: CG.Val;  dest: CG.Var;  depth: INTEGER) =
  BEGIN
    FOR i := 1 TO depth - 1 DO
      CG.Push (src);
      CG.Open_size (i);
      CG.Store_int (Target.Integer.cg_type,
                    dest, M3RT.OA_sizes + i * Target.Integer.pack);
    END;
  END CopyDopeVector;

PROCEDURE ComputeOffset (array: CG.Var;  depth, elt_pack: INTEGER) =
  BEGIN
    FOR i := 1 TO depth - 1 DO
      CG.Load_int (Target.Integer.cg_type,
                   array, M3RT.OA_sizes + i * Target.Integer.pack);
      CG.Multiply (Target.Integer.cg_type);
    END;
    CG.Index_bytes (elt_pack);
    CG.Store_addr (array, M3RT.OA_elt_ptr);
  END ComputeOffset;

(* Called indirectly through MethodList. *)
PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    CompileLV (ce, traced := FALSE);
  END Compile;

(* Called indirectly through MethodList. *)
PROCEDURE CompileLV (ce: CallExpr.T; <*UNUSED*> traced: BOOLEAN) =
  BEGIN
    (* all the real work was done by PrepLV *)
    CG.Push (ce.tmp);
    CG.Boost_addr_alignment (Target.Address.align);
    CG.Free (ce.tmp);
    ce.tmp := NIL;
  END CompileLV;

(* Called indirectly through MethodList. *)
PROCEDURE IsWritable (ce: CallExpr.T;  lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN Expr.IsWritable (ce.args[0], lhs);
  END IsWritable;

(* Called indirectly through MethodList. *)
PROCEDURE IsDesignator (ce: CallExpr.T;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN Expr.IsDesignator (ce.args[0]);
  END IsDesignator;

(* Called indirectly through MethodList. *)
PROCEDURE NoteWrites (ce: CallExpr.T) =
  BEGIN
    Expr.NoteWrite (ce.args[0]);
  END NoteWrites;

(* EXPORTED:*)
PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (3, 3, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 TypeOf,
                                 NeedsAddress,
                                 Check,
                                 Prep,
                                 Compile,
                                 PrepLV,
                                 CompileLV,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue, (* fold *)
                                 CallExpr.NoBounds,
                                 IsWritable,
                                 IsDesignator,
                                 NoteWrites,
                                 SubarrayExprAlign);
    Procedure.DefinePredefined ("SUBARRAY", Z, TRUE);
  END Initialize;

BEGIN
END Subarray.
