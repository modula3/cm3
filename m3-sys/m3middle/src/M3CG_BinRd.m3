(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3CG_BinRd;

IMPORT Fmt, Rd, Stdio, Text, Thread, Word, Wr;
IMPORT M3ID, M3CG, M3CG_Ops, M3CG_Binary;
IMPORT Target, TargetMap, TInt, TFloat, TWord;
FROM M3CG IMPORT CompareOp, ConvertOp, AtomicOp, RuntimeError, MemoryOrder;

TYPE
  Bop = M3CG_Binary.Op;

CONST
  BufSize = 16_10000; (* 64K *)

TYPE
  InputBuffer = REF ARRAY [0..BufSize-1] OF CHAR;

TYPE
  State = RECORD
    rd     : Rd.T;
    cg     : M3CG.T;
    buf    : InputBuffer;
    buf_len: CARDINAL;
    buf_ptr: CARDINAL;
    hit_eof: BOOLEAN;
    vars   : REF ARRAY OF M3CG.Var;
    procs  : REF ARRAY OF M3CG.Proc;
    labels : REF ARRAY OF M3CG.Label;
  END;

TYPE
  Cmd = RECORD
    bop  : Bop;
    proc : PROCEDURE (VAR s: State);
  END;

CONST
  CmdMap = ARRAY [Bop.begin_unit..Bop.widechar_size] OF Cmd {
    Cmd {Bop.begin_unit, begin_unit},
    Cmd {Bop.end_unit, end_unit},
    Cmd {Bop.import_unit, import_unit},
    Cmd {Bop.export_unit, export_unit},
    Cmd {Bop.set_source_file, set_source_file},
    Cmd {Bop.set_source_line, set_source_line},
    Cmd {Bop.declare_typename, declare_typename},
    Cmd {Bop.declare_array, declare_array},
    Cmd {Bop.declare_open_array, declare_open_array},
    Cmd {Bop.declare_enum, declare_enum},
    Cmd {Bop.declare_enum_elt, declare_enum_elt},
    Cmd {Bop.declare_packed, declare_packed},
    Cmd {Bop.declare_record, declare_record},
    Cmd {Bop.declare_field, declare_field},
    Cmd {Bop.declare_set, declare_set},
    Cmd {Bop.declare_subrange, declare_subrange},
    Cmd {Bop.declare_pointer, declare_pointer},
    Cmd {Bop.declare_indirect, declare_indirect},
    Cmd {Bop.declare_proctype, declare_proctype},
    Cmd {Bop.declare_formal, declare_formal},
    Cmd {Bop.declare_raises, declare_raises},
    Cmd {Bop.declare_object, declare_object},
    Cmd {Bop.declare_method, declare_method},
    Cmd {Bop.declare_opaque, declare_opaque},
    Cmd {Bop.reveal_opaque, reveal_opaque},
    Cmd {Bop.declare_exception, declare_exception},
    Cmd {Bop.set_runtime_proc, set_runtime_proc},
    Cmd {Bop.import_global, import_global},
    Cmd {Bop.declare_segment, declare_segment},
    Cmd {Bop.bind_segment, bind_segment},
    Cmd {Bop.declare_global, declare_global},
    Cmd {Bop.declare_constant, declare_constant},
    Cmd {Bop.declare_local, declare_local},
    Cmd {Bop.declare_param, declare_param},
    Cmd {Bop.declare_temp, declare_temp},
    Cmd {Bop.free_temp, free_temp},
    Cmd {Bop.begin_init, begin_init},
    Cmd {Bop.end_init, end_init},
    Cmd {Bop.init_int, init_int},
    Cmd {Bop.init_proc, init_proc},
    Cmd {Bop.init_label, init_label},
    Cmd {Bop.init_var, init_var},
    Cmd {Bop.init_offset, init_offset},
    Cmd {Bop.init_chars, init_chars},
    Cmd {Bop.init_float, init_float},
    Cmd {Bop.import_procedure, import_procedure},
    Cmd {Bop.declare_procedure, declare_procedure},
    Cmd {Bop.begin_procedure, begin_procedure},
    Cmd {Bop.end_procedure, end_procedure},
    Cmd {Bop.begin_block, begin_block},
    Cmd {Bop.end_block, end_block},
    Cmd {Bop.note_procedure_origin, note_procedure_origin},
    Cmd {Bop.set_label, set_label},
    Cmd {Bop.jump, jump},
    Cmd {Bop.if_true, if_true},
    Cmd {Bop.if_false, if_false},
    Cmd {Bop.if_eq, if_eq},
    Cmd {Bop.if_ne, if_ne},
    Cmd {Bop.if_gt, if_gt},
    Cmd {Bop.if_ge, if_ge},
    Cmd {Bop.if_lt, if_lt},
    Cmd {Bop.if_le, if_le},
    Cmd {Bop.case_jump, case_jump},
    Cmd {Bop.exit_proc, exit_proc},
    Cmd {Bop.load, load},
    Cmd {Bop.load_address, load_address},
    Cmd {Bop.load_indirect, load_indirect},
    Cmd {Bop.store, store},
    Cmd {Bop.store_indirect, store_indirect},
    Cmd {Bop.load_nil, load_nil},
    Cmd {Bop.load_integer, load_integer},
    Cmd {Bop.load_float, load_float},
    Cmd {Bop.eq, eq},
    Cmd {Bop.ne, ne},
    Cmd {Bop.gt, gt},
    Cmd {Bop.ge, ge},
    Cmd {Bop.lt, lt},
    Cmd {Bop.le, le},
    Cmd {Bop.add, add},
    Cmd {Bop.subtract, subtract},
    Cmd {Bop.multiply, multiply},
    Cmd {Bop.divide, divide},
    Cmd {Bop.negate, negate},
    Cmd {Bop.abs, abs},
    Cmd {Bop.max, max},
    Cmd {Bop.min, min},
    Cmd {Bop.round, round},
    Cmd {Bop.trunc, trunc},
    Cmd {Bop.floor, floor},
    Cmd {Bop.ceiling, ceiling},
    Cmd {Bop.cvt_float, cvt_float},
    Cmd {Bop.div, div},
    Cmd {Bop.mod, mod},
    Cmd {Bop.set_union, set_union},
    Cmd {Bop.set_difference, set_difference},
    Cmd {Bop.set_intersection, set_intersection},
    Cmd {Bop.set_sym_difference, set_sym_difference},
    Cmd {Bop.set_member, set_member},
    Cmd {Bop.set_eq, set_eq},
    Cmd {Bop.set_ne, set_ne},
    Cmd {Bop.set_lt, set_lt},
    Cmd {Bop.set_le, set_le},
    Cmd {Bop.set_gt, set_gt},
    Cmd {Bop.set_ge, set_ge},
    Cmd {Bop.set_range, set_range},
    Cmd {Bop.set_singleton, set_singleton},
    Cmd {Bop.not, not},
    Cmd {Bop.and, and},
    Cmd {Bop.or, or},
    Cmd {Bop.xor, xor},
    Cmd {Bop.shift, shift},
    Cmd {Bop.shift_left, shift_left},
    Cmd {Bop.shift_right, shift_right},
    Cmd {Bop.rotate, rotate},
    Cmd {Bop.rotate_left, rotate_left},
    Cmd {Bop.rotate_right, rotate_right},
    Cmd {Bop.widen, widen},
    Cmd {Bop.chop, chop},
    Cmd {Bop.extract, extract},
    Cmd {Bop.extract_n, extract_n},
    Cmd {Bop.extract_mn, extract_mn},
    Cmd {Bop.insert, insert},
    Cmd {Bop.insert_n, insert_n},
    Cmd {Bop.insert_mn, insert_mn},
    Cmd {Bop.swap, swap},
    Cmd {Bop.pop, pop},
    Cmd {Bop.copy_n, copy_n},
    Cmd {Bop.copy, copy},
    Cmd {Bop.zero_n, zero_n},
    Cmd {Bop.zero, zero},
    Cmd {Bop.loophole, loophole},
    Cmd {Bop.abort, abort},
    Cmd {Bop.check_nil, check_nil},
    Cmd {Bop.check_lo, check_lo},
    Cmd {Bop.check_hi, check_hi},
    Cmd {Bop.check_range, check_range},
    Cmd {Bop.check_index, check_index},
    Cmd {Bop.check_eq, check_eq},
    Cmd {Bop.add_offset, add_offset},
    Cmd {Bop.index_address, index_address},
    Cmd {Bop.start_call_direct, start_call_direct},
    Cmd {Bop.call_direct, call_direct},
    Cmd {Bop.start_call_indirect, start_call_indirect},
    Cmd {Bop.call_indirect, call_indirect},
    Cmd {Bop.pop_param, pop_param},
    Cmd {Bop.pop_struct, pop_struct},
    Cmd {Bop.pop_static_link, pop_static_link},
    Cmd {Bop.load_procedure, load_procedure},
    Cmd {Bop.load_static_link, load_static_link},
    Cmd {Bop.comment, comment},
    Cmd {Bop.store_ordered, store_ordered},
    Cmd {Bop.load_ordered, load_ordered},
    Cmd {Bop.exchange, exchange},
    Cmd {Bop.compare_exchange, compare_exchange},
    Cmd {Bop.fence, fence},
    Cmd {Bop.fetch_and_add,  fetch_and_add},
    Cmd {Bop.fetch_and_sub,  fetch_and_sub},
    Cmd {Bop.fetch_and_or,   fetch_and_or},
    Cmd {Bop.fetch_and_and,  fetch_and_and},
    Cmd {Bop.fetch_and_xor,  fetch_and_xor},
    Cmd {Bop.widechar_size, widechar_size}
  };

PROCEDURE Inhale (rd: Rd.T;  cg: M3CG.T) =
  VAR s: State;  op: M3CG.Name;  cmd: INTEGER;
  BEGIN
    s.rd      := rd;
    s.cg      := cg;
    s.buf     := NEW (InputBuffer);
    s.buf_len := 0;
    s.buf_ptr := 0;
    s.hit_eof := FALSE;
    s.vars    := NEW (REF ARRAY OF M3CG.Var, 400);
    s.procs   := NEW (REF ARRAY OF M3CG.Proc, 50);
    s.labels  := NEW (REF ARRAY OF M3CG.Label, 400);

    FOR i := 0 TO LAST(s.labels^) DO s.labels[i] := M3CG.No_label END;

    cmd := Scan_int (s);
    IF (cmd # M3CG_Binary.Version) THEN
      Error (s, "** bad intermediate code version: expected ",
                Fmt.Int (M3CG_Binary.Version) & ", but found ",
                Fmt.Int (cmd));
    END;

    LOOP
      op := GetByte (s);
      IF (s.hit_eof) THEN
        EXIT;
      ELSIF (op < 0) OR (ORD (LAST (Bop)) < op) THEN
        Error (s, "** bad intermediate code operator: ", Fmt.Int (op));
      ELSE
        CmdMap [VAL (op, Bop)].proc (s);
      END;
    END;
  END Inhale;

PROCEDURE Error (<*UNUSED*> VAR s: State;  a, b, c: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR msg := Wr.EOL & "** ERROR in M3CG_BinRd.Inhale: ";
  BEGIN
    IF (a # NIL) THEN msg := msg & a END;
    IF (b # NIL) THEN msg := msg & b END;
    IF (c # NIL) THEN msg := msg & c END;
    msg := msg & " **" & Wr.EOL;
    Wr.PutText (Stdio.stdout, msg);
    Wr.Flush(Stdio.stdout);
  END Error;

(*--------------------------------------------------------------- parsing ---*)

PROCEDURE Scan_name (VAR s: State): M3CG.Name =
  VAR txt := Scan_text (s);
  BEGIN
    IF (txt = NIL)
      THEN RETURN M3ID.NoID;
      ELSE RETURN M3ID.Add (txt);
    END;
  END Scan_name;

PROCEDURE Scan_text (VAR s: State): TEXT =
  VAR
    len := Scan_int (s);
    txt : TEXT;
    buf : ARRAY [0..255] OF CHAR;
  BEGIN
    IF (len < 0) THEN  RETURN NIL; END;
    txt := "";
    WHILE (len > NUMBER (buf)) DO
      FOR i := 0 TO LAST(buf) DO buf[i] := VAL (GetByte (s), CHAR); END;
      txt := txt & Text.FromChars (buf);
      DEC (len, NUMBER (buf));
    END;
    IF (len > 0) THEN
      FOR i := 0 TO len-1 DO buf[i] := VAL (GetByte (s), CHAR); END;
      txt := txt & Text.FromChars (SUBARRAY (buf, 0, len));
    END;
    RETURN txt;
  END Scan_text;

PROCEDURE Scan_type (VAR s: State): M3CG.Type =
  VAR x := GetByte (s);
  BEGIN
    IF (ORD (FIRST (M3CG.Type)) <= x) AND (x <= ORD (LAST (M3CG.Type))) THEN
      RETURN VAL (x, M3CG.Type);
    ELSE
      Error (s, "illegal type: ", Fmt.Int (x));
      RETURN M3CG.Type.Int32;
    END;
  END Scan_type;

PROCEDURE Scan_bool (VAR s: State): BOOLEAN =
  VAR x := GetByte (s);
  BEGIN
    CASE x OF
    | ORD (FALSE) => RETURN FALSE;
    | ORD (TRUE)  => RETURN TRUE;
    ELSE Error (s, "illegal boolean: ", Fmt.Int (x));
    END;
    RETURN TRUE;
  END Scan_bool;

PROCEDURE Scan_label (VAR s: State): INTEGER =
  VAR val, x: INTEGER;
  BEGIN
    val := Scan_int (s);
    IF (val < 0) THEN RETURN M3CG.No_label; END;

    WHILE (val > LAST (s.labels^)) DO ExpandLabels (s) END;

    x := s.labels[val];
    IF (x = M3CG.No_label) THEN
      x := s.cg.next_label ();
      s.labels[val] := x;
    END;
    RETURN x;
  END Scan_label;

PROCEDURE ExpandLabels (VAR s: State) =
  VAR new := NEW (REF ARRAY OF M3CG.Label, 2 * NUMBER (s.labels^));
  BEGIN
    SUBARRAY (new^, 0, NUMBER (s.labels^)) := s.labels^;
    FOR i := NUMBER (s.labels^) TO LAST (new^) DO new[i] := M3CG.No_label END;
    s.labels := new;
  END ExpandLabels;

PROCEDURE Scan_tipe (VAR s: State): M3CG.TypeUID =
  BEGIN
    RETURN Scan_int (s);
  END Scan_tipe;

PROCEDURE Scan_var (VAR s: State): M3CG.Var =
  VAR id := Scan_int (s);
  BEGIN
    IF (id <= 0)
      THEN RETURN NIL;
      ELSE RETURN s.vars[id];
    END;
  END Scan_var;

PROCEDURE Scan_proc (VAR s: State): M3CG.Proc =
  VAR id := Scan_int (s);
  BEGIN
    IF (id <= 0)
      THEN RETURN NIL;
      ELSE RETURN s.procs[id];
    END;
  END Scan_proc;

PROCEDURE Scan_sign (VAR s: State): M3CG.Sign =
  VAR x := Scan_int (s);
  BEGIN
    CASE x OF
    | ORD (M3CG.Sign.Positive) => RETURN M3CG.Sign.Positive;
    | ORD (M3CG.Sign.Negative) => RETURN M3CG.Sign.Negative;
    | ORD (M3CG.Sign.Unknown)  => RETURN M3CG.Sign.Unknown;
    ELSE Error (s, "bad sign: ", Fmt.Int (x));
    END;
    RETURN M3CG.Sign.Unknown;
  END Scan_sign;

PROCEDURE Scan_callConv (VAR s: State): Target.CallingConvention =
  VAR
    id := GetByte (s);
    cc := Target.ConventionFromID (id);
  BEGIN
    IF (cc = NIL) THEN
      Error (s, "unknown calling convention: ", Fmt.Int (id));
    END;
    RETURN cc;
  END Scan_callConv;

PROCEDURE Scan_float (VAR s: State): Target.Float =
  VAR
    i       := GetByte (s);
    pre     := Target.Precision.Short;
    n_bytes : INTEGER;
    bytes   : ARRAY [0..BYTESIZE(EXTENDED)] OF TFloat.Byte;
    result  : Target.Float;
  BEGIN
    CASE i OF
    | ORD (Target.Precision.Short)    => pre := Target.Precision.Short;
    | ORD (Target.Precision.Long)     => pre := Target.Precision.Long;
    | ORD (Target.Precision.Extended) => pre := Target.Precision.Extended;
    ELSE Error (s, "bad floating-point precision: ", Fmt.Int (i));
    END;
    n_bytes := TargetMap.Float_types[pre].size DIV BITSIZE (TFloat.Byte);
    FOR x := 0 TO n_bytes-1 DO bytes[x] := GetByte (s); END;
    TFloat.FromBytes (SUBARRAY (bytes, 0, n_bytes), pre, result);
    RETURN result;
  END Scan_float;

PROCEDURE Scan_Tint (VAR s: State): Target.Int =
  VAR
    i : INTEGER := GetByte (s);
    n_bytes, sign, shift: INTEGER;
    val, byte: Target.Int;
    ok: BOOLEAN;
  BEGIN
    CASE i OF
    | M3CG_Binary.Int1  =>  n_bytes := 1;  sign := +1;
    | M3CG_Binary.NInt1 =>  n_bytes := 1;  sign := -1;
    | M3CG_Binary.Int2  =>  n_bytes := 2;  sign := +1;
    | M3CG_Binary.NInt2 =>  n_bytes := 2;  sign := -1;
    | M3CG_Binary.Int4  =>  n_bytes := 4;  sign := +1;
    | M3CG_Binary.NInt4 =>  n_bytes := 4;  sign := -1;
    | M3CG_Binary.Int8  =>  n_bytes := 8;  sign := +1;
    | M3CG_Binary.NInt8 =>  n_bytes := 8;  sign := -1;
    ELSE
      ok := TInt.FromInt (i, val);  <*ASSERT ok*>
      RETURN val;
    END;

    val := TInt.Zero;  shift := 0;
    FOR i := 0 TO n_bytes-1 DO
      ok := TInt.FromInt (GetByte (s), byte);  <*ASSERT ok*>
      TWord.Shift (byte, shift, byte);
      TWord.Or (val, byte, val);
      INC (shift, 8);
    END;
    IF (sign < 0) THEN TWord.Subtract (TInt.Zero, val, val); END;

    RETURN val;
  END Scan_Tint;

PROCEDURE Scan_int (VAR s: State): INTEGER =
  VAR i := GetByte (s);  n_bytes, sign, val, shift: INTEGER;
  BEGIN
    CASE i OF
    | M3CG_Binary.Int1  =>  RETURN GetByte (s);
    | M3CG_Binary.NInt1 =>  RETURN - GetByte (s);
    | M3CG_Binary.Int2  =>  n_bytes := 2;  sign := +1;
    | M3CG_Binary.NInt2 =>  n_bytes := 2;  sign := -1;
    | M3CG_Binary.Int4  =>  n_bytes := 4;  sign := +1;
    | M3CG_Binary.NInt4 =>  n_bytes := 4;  sign := -1;
    | M3CG_Binary.Int8  =>  n_bytes := 8;  sign := +1;
    | M3CG_Binary.NInt8 =>  n_bytes := 8;  sign := -1;
    ELSE                    RETURN i;
    END;
    val := 0;  shift := 0;
    FOR x := 0 TO n_bytes-1 DO
      val := Word.Or (val, Word.LeftShift (GetByte (s), shift));
      INC (shift, 8);
    END;
    RETURN sign * val;
  END Scan_int;

PROCEDURE Scan_error (VAR s: State): RuntimeError =
  VAR x := Scan_int (s);
  BEGIN
    IF (x < ORD (FIRST (RuntimeError))) OR (ORD (LAST (RuntimeError)) < x) THEN
      Error (s, "bad error code: ", Fmt.Int (x));
      x := ORD (FIRST (RuntimeError));
    END;
    RETURN VAL (x, RuntimeError);
  END Scan_error;

TYPE Byte = [0..255];

PROCEDURE GetByte (VAR s: State): Byte =
  VAR val: Byte;
  BEGIN
    IF (s.buf_ptr >= s.buf_len) THEN RefillBuffer (s) END;
    val := ORD (s.buf[s.buf_ptr]);  INC (s.buf_ptr);
    RETURN val;
  END GetByte;

PROCEDURE RefillBuffer (VAR s: State) =
  <*FATAL Rd.Failure, Thread.Alerted*>
  BEGIN
    s.buf_ptr := 0;
    s.buf_len := Rd.GetSub (s.rd, s.buf^);
    IF (s.buf_len <= 0) THEN
      s.hit_eof := TRUE;
      s.buf_len := 1;
      s.buf[0]  := '\000';
    END;
  END RefillBuffer;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (VAR s: State) =
  VAR optimize := Scan_int (s);
  BEGIN
    s.cg.begin_unit (optimize);
  END begin_unit;

PROCEDURE end_unit   (VAR s: State) =
  BEGIN
    s.cg.end_unit ();
  END end_unit;

PROCEDURE import_unit (VAR s: State) =
  VAR name := Scan_name (s);
  BEGIN
    s.cg.import_unit (name);
  END import_unit;

PROCEDURE export_unit (VAR s: State) =
  VAR name := Scan_name (s);
  BEGIN
    s.cg.export_unit (name);
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (VAR s: State) =
  VAR file := Scan_text (s);
  BEGIN
    s.cg.set_source_file (file);
  END set_source_file;

PROCEDURE set_source_line (VAR s: State) =
  VAR line := Scan_int (s);
  BEGIN
    s.cg.set_source_line (line);
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (VAR s: State) =
  VAR type := Scan_tipe (s);
      name := Scan_name (s);
  BEGIN
    s.cg.declare_typename (type, name);
  END declare_typename;

PROCEDURE declare_array (VAR s: State)=
  VAR type  := Scan_tipe (s);
      index := Scan_tipe (s);
      elt   := Scan_tipe (s);
      size  := Scan_int (s);
  BEGIN
    s.cg.declare_array (type, index, elt, size);
  END declare_array;

PROCEDURE declare_open_array (VAR s: State)=
  VAR type  := Scan_tipe (s);
      elt   := Scan_tipe (s);
      size  := Scan_int (s);
  BEGIN
    s.cg.declare_open_array (type, elt, size);
  END declare_open_array;

PROCEDURE declare_enum (VAR s: State) =
  VAR type   := Scan_tipe (s);
      n_elts := Scan_int (s);
      size   := Scan_int (s);
  BEGIN
    s.cg.declare_enum (type, n_elts, size);
  END declare_enum;

PROCEDURE declare_enum_elt (VAR s: State) =
  VAR name := Scan_name (s);
  BEGIN
    s.cg.declare_enum_elt (name);
  END declare_enum_elt;

PROCEDURE declare_packed  (VAR s: State) =
  VAR type := Scan_tipe (s);
      size := Scan_int (s);
      base := Scan_tipe (s);
  BEGIN
    s.cg.declare_packed (type, size, base);
  END declare_packed;

PROCEDURE declare_record (VAR s: State) =
  VAR type     := Scan_tipe (s);
      size     := Scan_int (s);
      n_fields := Scan_int (s);
  BEGIN
    s.cg.declare_record (type, size, n_fields);
  END declare_record;

PROCEDURE declare_field (VAR s: State) =
  VAR name   := Scan_name (s);
      offset := Scan_int (s);
      size   := Scan_int (s);
      type   := Scan_tipe (s);
  BEGIN
    s.cg.declare_field (name, offset, size, type);
  END declare_field;

PROCEDURE declare_set (VAR s: State) =
  VAR type   := Scan_tipe (s);
      domain := Scan_tipe (s);
      size   := Scan_int (s);
  BEGIN
    s.cg.declare_set (type, domain, size);
  END declare_set;

PROCEDURE declare_subrange (VAR s: State) =
  VAR type   := Scan_tipe (s);
      domain := Scan_tipe (s);
      min    := Scan_Tint (s);
      max    := Scan_Tint (s);
      size   := Scan_int (s);
  BEGIN
    s.cg.declare_subrange (type, domain, min, max, size);
  END declare_subrange;

PROCEDURE declare_pointer (VAR s: State) =
  VAR type   := Scan_tipe (s);
      target := Scan_tipe (s);
      brand  := Scan_text (s);
      traced := Scan_bool (s);
  BEGIN
    s.cg.declare_pointer (type, target, brand, traced);
  END declare_pointer;

PROCEDURE declare_indirect (VAR s: State) =
  VAR type   := Scan_tipe (s);
      target := Scan_tipe (s);
      (* TODO target_typename but there are no downstream users *)
  BEGIN
    s.cg.declare_indirect (type, target);
  END declare_indirect;

PROCEDURE declare_proctype (VAR s: State) =
  VAR type      := Scan_tipe (s);
      n_formals := Scan_int (s);
      result    := Scan_tipe (s);
      n_raises  := Scan_int (s);
      calling   := Scan_callConv (s);
      result_typename := M3ID.NoID; (* TODO typename but it is not used downstream and can be omitted indefinitely *)
  BEGIN
    s.cg.declare_proctype (type, n_formals, result, n_raises, calling, result_typename);
  END declare_proctype;

PROCEDURE declare_formal (VAR s: State) =
  VAR name := Scan_name (s);
      type := Scan_tipe (s);
      typename := M3ID.NoID; (* TODO typename but it is not used downstream and can be omitted indefinitely *)
  BEGIN
    s.cg.declare_formal (name, type, typename);
  END declare_formal;

PROCEDURE declare_raises (VAR s: State) =
  VAR name := Scan_name (s);
  BEGIN
    s.cg.declare_raises (name);
  END declare_raises;

PROCEDURE declare_object (VAR s: State) =
  VAR type       := Scan_tipe (s);
      super      := Scan_tipe (s);
      brand      := Scan_text (s);
      traced     := Scan_bool (s);
      n_fields   := Scan_int (s);
      n_methods  := Scan_int (s);
      field_size := Scan_int (s);
  BEGIN
    s.cg.declare_object (type, super, brand, traced,
                         n_fields, n_methods, field_size);
  END declare_object;

PROCEDURE declare_method (VAR s: State) =
  VAR name := Scan_name (s);
      type := Scan_tipe (s);
  BEGIN
    s.cg.declare_method (name, type);
  END declare_method;

PROCEDURE declare_opaque (VAR s: State) =
  VAR type    := Scan_tipe (s);
      super   := Scan_tipe (s);
  BEGIN
    s.cg.declare_opaque (type, super);
  END declare_opaque;

PROCEDURE reveal_opaque (VAR s: State) =
  VAR lhs     := Scan_tipe (s);
      rhs     := Scan_tipe (s);
  BEGIN
    s.cg.reveal_opaque (lhs, rhs);
  END reveal_opaque;

PROCEDURE declare_exception (VAR s: State) =
  VAR name       := Scan_name (s);
      arg_type   := Scan_tipe (s);
      raise_proc := Scan_bool (s);
      base       := Scan_var (s);
      offset     := Scan_int (s);
  BEGIN
    s.cg.declare_exception (name, arg_type, raise_proc, base, offset);
  END declare_exception;

PROCEDURE widechar_size (VAR s: State) =
  VAR size := Scan_int (s);
  BEGIN
    s.cg.widechar_size (size);
  END widechar_size;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (VAR s: State) =
  VAR name   := Scan_name (s);
      proc   := Scan_proc (s);
  BEGIN
    s.cg.set_runtime_proc (name, proc);
  END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE AddVar (VAR s: State;  id: INTEGER;  v: M3CG.Var) =
  BEGIN
    WHILE (id >= NUMBER (s.vars^)) DO ExpandVars (s) END;
    s.vars[id] := v;
  END AddVar;

PROCEDURE ExpandVars (VAR s: State) =
  VAR new := NEW (REF ARRAY OF M3CG.Var, 2 * NUMBER (s.vars^));
  BEGIN
    SUBARRAY (new^, 0, NUMBER (s.vars^)) := s.vars^;
    s.vars := new;
  END ExpandVars;

PROCEDURE import_global (VAR s: State) =
  VAR name  := Scan_name (s);
      size  := Scan_int (s);
      align := Scan_int (s);
      type  := Scan_type (s);
      m3t   := Scan_tipe (s);
      v     := Scan_int (s);
      (* TODO typename *)
  BEGIN
    AddVar (s, v, s.cg.import_global (name, size, align, type, m3t));
  END import_global;

PROCEDURE declare_segment (VAR s: State) =
  VAR name     := Scan_name (s);
      m3t      := Scan_tipe (s);
      is_const := Scan_bool (s);
      v        := Scan_int (s);
  BEGIN
    AddVar (s, v, s.cg.declare_segment (name, m3t, is_const));
  END declare_segment;

PROCEDURE bind_segment (VAR s: State) =
  VAR v      := Scan_var (s);
      size   := Scan_int (s);
      align  := Scan_int (s);
      type   := Scan_type (s);
      export := Scan_bool (s);
      init   := Scan_bool (s);
  BEGIN
    s.cg.bind_segment (v, size, align, type, export, init);
  END bind_segment;

PROCEDURE declare_global (VAR s: State) =
  VAR name   := Scan_name (s);
      size   := Scan_int (s);
      align  := Scan_int (s);
      type   := Scan_type (s);
      m3t    := Scan_tipe (s);
      export := Scan_bool (s);
      init   := Scan_bool (s);
      v      := Scan_int (s);
  BEGIN
    AddVar (s, v, s.cg.declare_global (name, size, align, type,
                                       m3t, export, init));
  END declare_global;

PROCEDURE declare_constant (VAR s: State) =
  VAR name   := Scan_name (s);
      size   := Scan_int (s);
      align  := Scan_int (s);
      type   := Scan_type (s);
      m3t    := Scan_tipe (s);
      export := Scan_bool (s);
      init   := Scan_bool (s);
      v      := Scan_int (s);
  BEGIN
    AddVar (s, v, s.cg.declare_constant (name, size, align, type,
                                         m3t, export,init));
  END declare_constant;

PROCEDURE declare_local (VAR s: State) =
  VAR name   := Scan_name (s);
      size   := Scan_int (s);
      align  := Scan_int (s);
      type   := Scan_type (s);
      m3t    := Scan_tipe (s);
      in_mem := Scan_bool (s);
      up_lev := Scan_bool (s);
      freq   := Scan_int (s);
      v      := Scan_int (s);
  BEGIN
    AddVar (s, v, s.cg.declare_local (name, size, align, type, m3t,
                                      in_mem, up_lev, freq));
  END declare_local;

PROCEDURE declare_param (VAR s: State) =
  VAR name   := Scan_name (s);
      size   := Scan_int (s);
      align  := Scan_int (s);
      type   := Scan_type (s);
      m3t    := Scan_tipe (s);
      in_mem := Scan_bool (s);
      up_lev := Scan_bool (s);
      freq   := Scan_int (s);
      v      := Scan_int (s);
      typename := M3ID.NoID; (* TODO typename but it is not used downstream and can be omitted indefinitely *)
  BEGIN
    AddVar (s, v, s.cg.declare_param (name, size, align, type, m3t,
                                      in_mem, up_lev, freq, typename));
  END declare_param;

PROCEDURE declare_temp (VAR s: State) =
  VAR size   := Scan_int (s);
      align  := Scan_int (s);
      type   := Scan_type (s);
      in_mem := Scan_bool (s);
      v      := Scan_int (s);
  BEGIN
    AddVar (s, v, s.cg.declare_temp (size, align, type, in_mem));
  END declare_temp;

PROCEDURE free_temp (VAR s: State) =
  VAR v := Scan_var (s);
  BEGIN
    s.cg.free_temp (v);
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (VAR s: State) =
  VAR v := Scan_var (s);
  BEGIN
    s.cg.begin_init (v);
  END begin_init;

PROCEDURE end_init (VAR s: State) =
  VAR v := Scan_var (s);
  BEGIN
    s.cg.end_init (v);
  END end_init;

PROCEDURE init_int (VAR s: State) =
  VAR offset := Scan_int (s);
      value  := Scan_Tint (s);
      type   := Scan_type (s);
  BEGIN
    s.cg.init_int (offset, value, type);
  END init_int;

PROCEDURE init_proc (VAR s: State) =
  VAR offset := Scan_int (s);
      value  := Scan_proc (s);
  BEGIN
    s.cg.init_proc (offset, value);
  END init_proc;

PROCEDURE init_label (VAR s: State) =
  VAR offset := Scan_int (s);
      value  := Scan_label (s);
  BEGIN
    s.cg.init_label (offset, value);
  END init_label;

PROCEDURE init_var (VAR s: State) =
  VAR offset := Scan_int (s);
      value  := Scan_var (s);
      bias   := Scan_int (s);
  BEGIN
    s.cg.init_var (offset, value, bias);
  END init_var;

PROCEDURE init_offset (VAR s: State) =
  VAR offset := Scan_int (s);
      value  := Scan_var (s);
  BEGIN
    s.cg.init_offset (offset, value);
  END init_offset;

PROCEDURE init_chars (VAR s: State) =
  VAR offset := Scan_int (s);
      value  := Scan_text (s);
  BEGIN
    s.cg.init_chars (offset, value);
  END init_chars;

PROCEDURE init_float (VAR s: State) =
  VAR offset := Scan_int (s);
      value  := Scan_float (s);
  BEGIN
    s.cg.init_float (offset, value);
  END init_float;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE AddProc (VAR s: State;  id: INTEGER;  p: M3CG.Proc) =
  BEGIN
    WHILE (id >= NUMBER (s.procs^)) DO ExpandProcs (s) END;
    s.procs[id] := p;
  END AddProc;

PROCEDURE ExpandProcs (VAR s: State) =
  VAR new := NEW (REF ARRAY OF M3CG.Proc, 2 * NUMBER (s.procs^));
  BEGIN
    SUBARRAY (new^, 0, NUMBER (s.procs^)) := s.procs^;
    s.procs := new;
  END ExpandProcs;

PROCEDURE import_procedure (VAR s: State) =
  VAR name     := Scan_name (s);
      n_params := Scan_int (s);
      ret_type := Scan_type (s);
      calling  := Scan_callConv (s);
      p        := Scan_int (s);
      (* TODO return_typeid, return_typename but nobody uses it *)
  BEGIN
    AddProc (s, p, s.cg.import_procedure (name, n_params, ret_type, calling));
  END import_procedure;

PROCEDURE declare_procedure (VAR s: State) =
  VAR name     := Scan_name (s);
      n_params := Scan_int (s);
      ret_type := Scan_type (s);
      level    := Scan_int (s);
      calling  := Scan_callConv (s);
      export   := Scan_bool (s);
      parent   := Scan_proc (s);
      p        := Scan_int (s);
      (* TODO return_typeid, return_typename but it is not used downstream and can be omitted indefinitely *)
  BEGIN
    AddProc (s, p, s.cg.declare_procedure (name, n_params, ret_type,
                                           level, calling, export, parent));
  END declare_procedure;

PROCEDURE begin_procedure (VAR s: State) =
  VAR p := Scan_proc (s);
  BEGIN
    s.cg.begin_procedure (p);
  END begin_procedure;

PROCEDURE end_procedure (VAR s: State) =
  VAR p := Scan_proc (s);
  BEGIN
    s.cg.end_procedure (p);
  END end_procedure;

PROCEDURE begin_block (VAR s: State) =
  BEGIN
    s.cg.begin_block ();
  END begin_block;

PROCEDURE end_block (VAR s: State) =
  BEGIN
    s.cg.end_block ();
  END end_block;

PROCEDURE note_procedure_origin (VAR s: State) =
  VAR p := Scan_proc (s);
  BEGIN
    s.cg.note_procedure_origin (p);
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (VAR s: State) =
  VAR label   := Scan_label (s);
      barrier := Scan_bool (s);
  BEGIN
    s.cg.set_label (label, barrier);
  END set_label;

PROCEDURE jump (VAR s: State) =
  VAR label := Scan_label (s);
  BEGIN
    s.cg.jump (label);
  END jump;

PROCEDURE if_true  (VAR s: State) =
  VAR type  := Scan_type (s);
      label := Scan_label (s);
      freq  := Scan_int (s);
  BEGIN
    s.cg.if_true (type, label, freq);
  END if_true;

PROCEDURE if_false (VAR s: State) =
  VAR type  := Scan_type (s);
      label := Scan_label (s);
      freq  := Scan_int (s);
  BEGIN
    s.cg.if_false (type, label, freq);
  END if_false;

PROCEDURE if_eq (VAR s: State) =
  BEGIN
    if_compare (s, CompareOp.EQ);
  END if_eq;

PROCEDURE if_ne (VAR s: State) =
  BEGIN
    if_compare (s, CompareOp.NE);
  END if_ne;

PROCEDURE if_gt (VAR s: State) =
  BEGIN
    if_compare (s, CompareOp.GT);
  END if_gt;

PROCEDURE if_ge (VAR s: State) =
  BEGIN
    if_compare (s, CompareOp.GE);
  END if_ge;

PROCEDURE if_lt (VAR s: State) =
  BEGIN
    if_compare (s, CompareOp.LT);
  END if_lt;

PROCEDURE if_le (VAR s: State) =
  BEGIN
    if_compare (s, CompareOp.LE);
  END if_le;

PROCEDURE if_compare (VAR s: State;  op: CompareOp) =
  VAR type  := Scan_type (s);
      label := Scan_label (s);
      freq  := Scan_int (s);
  BEGIN
    s.cg.if_compare (type, op, label, freq);
  END if_compare;

PROCEDURE case_jump (VAR s: State) =
  VAR type := Scan_type (s);
      n    := Scan_int (s);
      x    := NEW (REF ARRAY OF M3CG.Label, n);
  BEGIN
    FOR i := 0 TO n-1 DO x[i] := Scan_label (s) END;
    s.cg.case_jump (type, x^);
  END case_jump;

PROCEDURE exit_proc (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.exit_proc (type);
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load  (VAR s: State) =
  VAR v      := Scan_var (s);
      offset := Scan_int (s);
      src    := Scan_type (s);
      dest   := Scan_type (s);
  BEGIN
    s.cg.load (v, offset, src, dest);
  END load;

PROCEDURE store  (VAR s: State) =
  VAR v      := Scan_var (s);
      offset := Scan_int (s);
      src    := Scan_type (s);
      dest   := Scan_type (s);
  BEGIN
    s.cg.store (v, offset, src, dest);
  END store;

PROCEDURE load_address (VAR s: State) =
  VAR v      := Scan_var (s);
      offset := Scan_int (s);
  BEGIN
    s.cg.load_address (v, offset);
  END load_address;

PROCEDURE load_indirect (VAR s: State) =
  VAR offset := Scan_int (s);
      src    := Scan_type (s);
      dest   := Scan_type (s);
  BEGIN
    s.cg.load_indirect (offset, src, dest);
  END load_indirect;

PROCEDURE store_indirect (VAR s: State) =
  VAR offset := Scan_int (s);
      src    := Scan_type (s);
      dest   := Scan_type (s);
  BEGIN
    s.cg.store_indirect (offset, src, dest);
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (VAR s: State) =
  BEGIN
    s.cg.load_nil ();
  END load_nil;

PROCEDURE load_integer  (VAR s: State) =
  VAR type  := Scan_type (s);
      value := Scan_Tint (s);
  BEGIN
    s.cg.load_integer (type, value);
  END load_integer;

PROCEDURE load_float    (VAR s: State) =
  VAR type  := Scan_type (s);
      value := Scan_float (s);
  BEGIN
    s.cg.load_float (type, value);
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE eq (VAR s: State) =
  BEGIN
    compare (s, CompareOp.EQ);
  END eq;

PROCEDURE ne (VAR s: State) =
  BEGIN
    compare (s, CompareOp.NE);
  END ne;

PROCEDURE gt (VAR s: State) =
  BEGIN
    compare (s, CompareOp.GT);
  END gt;

PROCEDURE ge (VAR s: State) =
  BEGIN
    compare (s, CompareOp.GE);
  END ge;

PROCEDURE lt (VAR s: State) =
  BEGIN
    compare (s, CompareOp.LT);
  END lt;

PROCEDURE le (VAR s: State) =
  BEGIN
    compare (s, CompareOp.LE);
  END le;

PROCEDURE compare (VAR s: State;  op: CompareOp) =
  VAR src  := Scan_type (s);
      dest := Scan_type (s);
  BEGIN
    s.cg.compare (src, dest, op);
  END compare;

PROCEDURE add (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.add (type);
  END add;

PROCEDURE subtract (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.subtract (type);
  END subtract;

PROCEDURE multiply (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.multiply (type);
  END multiply;

PROCEDURE divide (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.divide (type);
  END divide;

PROCEDURE div (VAR s: State) =
  VAR type := Scan_type (s);
      a := Scan_sign (s);
      b := Scan_sign (s);
  BEGIN
    s.cg.div (type, a, b);
  END div;

PROCEDURE mod (VAR s: State) =
  VAR type := Scan_type (s);
      a := Scan_sign (s);
      b := Scan_sign (s);
  BEGIN
    s.cg.mod (type, a, b);
  END mod;

PROCEDURE negate (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.negate (type);
  END negate;

PROCEDURE abs (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.abs (type);
  END abs;

PROCEDURE max (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.max (type);
  END max;

PROCEDURE min (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.min (type);
  END min;

PROCEDURE round (VAR s: State) =
  BEGIN
    cvt_int (s, ConvertOp.Round);
  END round;

PROCEDURE trunc (VAR s: State) =
  BEGIN
    cvt_int (s, ConvertOp.Trunc);
  END trunc;

PROCEDURE floor (VAR s: State) =
  BEGIN
    cvt_int (s, ConvertOp.Floor);
  END floor;

PROCEDURE ceiling (VAR s: State) =
  BEGIN
    cvt_int (s, ConvertOp.Ceiling);
  END ceiling;

PROCEDURE cvt_int (VAR s: State;  op: ConvertOp) =
  VAR src  := Scan_type (s);
      dest := Scan_type (s);
  BEGIN
    s.cg.cvt_int (src, dest, op);
  END cvt_int;

PROCEDURE cvt_float    (VAR s: State) =
  VAR src  := Scan_type (s);
      dest := Scan_type (s);
  BEGIN
    s.cg.cvt_float (src, dest);
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (VAR s: State) =
  VAR size := Scan_int (s);
  BEGIN
    s.cg.set_union (size);
  END set_union;

PROCEDURE set_difference (VAR s: State) =
  VAR size := Scan_int (s);
  BEGIN
    s.cg.set_difference (size);
  END set_difference;

PROCEDURE set_intersection (VAR s: State) =
  VAR size := Scan_int (s);
  BEGIN
    s.cg.set_intersection (size);
  END set_intersection;

PROCEDURE set_sym_difference (VAR s: State) =
  VAR size := Scan_int (s);
  BEGIN
    s.cg.set_sym_difference (size);
  END set_sym_difference;

PROCEDURE set_member (VAR s: State) =
  VAR size := Scan_int (s);
      type := Scan_type (s);
  BEGIN
    s.cg.set_member (size, type);
  END set_member;

PROCEDURE set_eq (VAR s: State) =
  BEGIN
    set_compare (s, CompareOp.EQ);
  END set_eq;

PROCEDURE set_ne (VAR s: State) =
  BEGIN
    set_compare (s, CompareOp.NE);
  END set_ne;

PROCEDURE set_gt (VAR s: State) =
  BEGIN
    set_compare (s, CompareOp.GT);
  END set_gt;

PROCEDURE set_ge (VAR s: State) =
  BEGIN
    set_compare (s, CompareOp.GE);
  END set_ge;

PROCEDURE set_lt (VAR s: State) =
  BEGIN
    set_compare (s, CompareOp.LT);
  END set_lt;

PROCEDURE set_le (VAR s: State) =
  BEGIN
    set_compare (s, CompareOp.LE);
  END set_le;

PROCEDURE set_compare (VAR s: State;  op: CompareOp) =
  VAR size := Scan_int (s);
      type := Scan_type (s);
  BEGIN
    s.cg.set_compare (size, op, type);
  END set_compare;

PROCEDURE set_range (VAR s: State) =
  VAR size := Scan_int (s);
      type := Scan_type (s);
  BEGIN
    s.cg.set_range (size, type);
  END set_range;

PROCEDURE set_singleton (VAR s: State) =
  VAR size := Scan_int (s);
      type := Scan_type (s);
  BEGIN
    s.cg.set_singleton (size, type);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.not (type);
  END not;

PROCEDURE and (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.and (type);
  END and;

PROCEDURE or  (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.or (type);
  END or;

PROCEDURE xor (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.xor (type);
  END xor;

PROCEDURE shift (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.shift (type);
  END shift;

PROCEDURE shift_left (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.shift_left (type);
  END shift_left;

PROCEDURE shift_right (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.shift_right (type);
  END shift_right;

PROCEDURE rotate (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.rotate (type);
  END rotate;

PROCEDURE rotate_left  (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.rotate_left (type);
  END rotate_left;

PROCEDURE rotate_right (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.rotate_right (type);
  END rotate_right;

PROCEDURE widen (VAR s: State) =
  VAR sign_extend := Scan_bool (s);
  BEGIN
    s.cg.widen (sign_extend);
  END widen;

PROCEDURE chop (VAR s: State) =
  BEGIN
    s.cg.chop ();
  END chop;

PROCEDURE extract (VAR s: State) =
  VAR type        := Scan_type (s);
      sign_extend := Scan_bool (s);
  BEGIN
    s.cg.extract (type, sign_extend);
  END extract;

PROCEDURE extract_n (VAR s: State) =
  VAR type        := Scan_type (s);
      sign_extend := Scan_bool (s);
      width       := Scan_int (s);
  BEGIN
    s.cg.extract_n (type, sign_extend, width);
  END extract_n;

PROCEDURE extract_mn (VAR s: State) =
  VAR type        := Scan_type (s);
      sign_extend := Scan_bool (s);
      offset      := Scan_int (s);
      width       := Scan_int (s);
  BEGIN
    s.cg.extract_mn (type, sign_extend, offset, width);
  END extract_mn;

PROCEDURE insert  (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.insert (type);
  END insert;

PROCEDURE insert_n  (VAR s: State) =
  VAR type  := Scan_type (s);
      width := Scan_int (s);
  BEGIN
    s.cg.insert_n (type, width);
  END insert_n;

PROCEDURE insert_mn  (VAR s: State) =
  VAR type   := Scan_type (s);
      offset := Scan_int (s);
      width  := Scan_int (s);
  BEGIN
    s.cg.insert_mn (type, offset, width);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (VAR s: State) =
  VAR a := Scan_type (s);
      b := Scan_type (s);
  BEGIN
    s.cg.swap (a, b);
  END swap;

PROCEDURE pop  (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.pop (type);
  END pop;

PROCEDURE copy_n (VAR s: State) =
  VAR cnt_type := Scan_type (s);
      type     := Scan_type (s);
      overlap  := Scan_bool (s);
  BEGIN
    s.cg.copy_n (cnt_type, type, overlap);
  END copy_n;

PROCEDURE copy (VAR s: State) =
  VAR cnt  := Scan_int (s);
      type := Scan_type (s);
      overlap := Scan_bool (s);
  BEGIN
    s.cg.copy (cnt, type, overlap);
  END copy;

PROCEDURE zero_n (VAR s: State) =
  VAR cnt_type := Scan_type (s);
      type     := Scan_type (s);
  BEGIN
    s.cg.zero_n (cnt_type, type);
  END zero_n;

PROCEDURE zero (VAR s: State) =
  VAR cnt  := Scan_int (s);
      type := Scan_type (s);
  BEGIN
    s.cg.zero (cnt, type);
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (VAR s: State) =
  VAR from := Scan_type (s);
      two  := Scan_type (s);
  BEGIN
    s.cg.loophole (from, two);
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (VAR s: State) =
  VAR code := Scan_error (s);
  BEGIN
    s.cg.abort (code);
  END abort;

PROCEDURE check_nil (VAR s: State) =
  VAR code := Scan_error (s);
  BEGIN
    s.cg.check_nil (code);
  END check_nil;

PROCEDURE check_lo (VAR s: State) =
  VAR type := Scan_type (s);
      i    := Scan_Tint (s);
      code := Scan_error (s);
  BEGIN
    s.cg.check_lo (type, i, code);
  END check_lo;

PROCEDURE check_hi (VAR s: State) =
  VAR type := Scan_type (s);
      i    := Scan_Tint (s);
      code := Scan_error (s);
  BEGIN
    s.cg.check_hi (type, i, code);
  END check_hi;

PROCEDURE check_range (VAR s: State) =
  VAR type := Scan_type (s);
      a    := Scan_Tint (s);
      b    := Scan_Tint (s);
      code := Scan_error (s);
  BEGIN
    s.cg.check_range (type, a, b, code);
  END check_range;

PROCEDURE check_index (VAR s: State) =
  VAR type := Scan_type (s);
      code := Scan_error (s);
  BEGIN
    s.cg.check_index (type, code);
  END check_index;

PROCEDURE check_eq (VAR s: State) =
  VAR type := Scan_type (s);
      code := Scan_error (s);
  BEGIN
    s.cg.check_eq (type, code);
  END check_eq;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (VAR s: State) =
  VAR i := Scan_int (s);
  BEGIN
    s.cg.add_offset (i);
  END add_offset;

PROCEDURE index_address (VAR s: State) =
  VAR type := Scan_type (s);
      size := Scan_int (s);
  BEGIN
    s.cg.index_address (type, size);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE start_call_direct (VAR s: State) =
  VAR p     := Scan_proc (s);
      level := Scan_int (s);
      type  := Scan_type (s);
  BEGIN
    s.cg.start_call_direct (p, level, type);
  END start_call_direct;

PROCEDURE start_call_indirect (VAR s: State) =
  VAR type    := Scan_type (s);
      calling := Scan_callConv (s);
  BEGIN
    s.cg.start_call_indirect (type, calling);
  END start_call_indirect;

PROCEDURE pop_param (VAR s: State) =
  VAR type := Scan_type (s);
  BEGIN
    s.cg.pop_param (type);
  END pop_param;

PROCEDURE pop_struct (VAR s: State) =
  VAR type  := Scan_tipe (s);
      size  := Scan_int (s);
      align := Scan_int (s);
  BEGIN
    s.cg.pop_struct (type, size, align);
  END pop_struct;

PROCEDURE pop_static_link (VAR s: State) =
  BEGIN
    s.cg.pop_static_link ();
  END pop_static_link;

PROCEDURE call_direct (VAR s: State) =
  VAR p    := Scan_proc (s);
      type := Scan_type (s);
  BEGIN
    s.cg.call_direct (p, type);
  END call_direct;

PROCEDURE call_indirect (VAR s: State) =
  VAR type    := Scan_type (s);
      calling := Scan_callConv (s);
  BEGIN
    s.cg.call_indirect (type, calling);
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (VAR s: State) =
  VAR p := Scan_proc (s);
  BEGIN
    s.cg.load_procedure (p);
  END load_procedure;

PROCEDURE load_static_link (VAR s: State) =
  VAR p := Scan_proc (s);
  BEGIN
    s.cg.load_static_link (p);
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (VAR s: State) =
  VAR x := Scan_text (s);
  BEGIN
    s.cg.comment (x);
  END comment;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered (VAR s: State) =
  VAR src    := Scan_type (s);
      dest   := Scan_type (s);
      order  := Scan_int (s);
  BEGIN
    s.cg.store_ordered (src, dest, VAL(order, MemoryOrder));
  END store_ordered;

PROCEDURE load_ordered (VAR s: State) =
  VAR src    := Scan_type (s);
      dest   := Scan_type (s);
      order  := Scan_int (s);
  BEGIN
    s.cg.load_ordered (src, dest, VAL(order, MemoryOrder));
  END load_ordered;

PROCEDURE exchange (VAR s: State) =
  VAR src    := Scan_type (s);
      dest   := Scan_type (s);
      order  := Scan_int (s);
  BEGIN
    s.cg.exchange (src, dest, VAL(order, MemoryOrder));
  END exchange;

PROCEDURE compare_exchange (VAR s: State) =
  VAR src     := Scan_type (s);
      dest    := Scan_type (s);
      res     := Scan_type (s);
      success := Scan_int (s);
      failure := Scan_int (s);
  BEGIN
    s.cg.compare_exchange (src, dest, res,
                           VAL(success, MemoryOrder),
                           VAL(failure, MemoryOrder));
  END compare_exchange;

PROCEDURE fence (VAR s: State) =
  VAR order  := Scan_int (s);
  BEGIN
    s.cg.fence (VAL(order, MemoryOrder));
  END fence;

PROCEDURE fetch_and_op (VAR s: State;  op: AtomicOp) =
  VAR src    := Scan_type (s);
      dest   := Scan_type (s);
      order  := Scan_int (s);
  BEGIN
    s.cg.fetch_and_op (op, src, dest, VAL(order, MemoryOrder));
  END fetch_and_op;

PROCEDURE fetch_and_add (VAR s: State) =
  BEGIN
    fetch_and_op (s, AtomicOp.Add);
  END fetch_and_add;

PROCEDURE fetch_and_sub (VAR s: State) =
  BEGIN
    fetch_and_op (s, AtomicOp.Sub);
  END fetch_and_sub;

PROCEDURE fetch_and_or (VAR s: State) =
  BEGIN
    fetch_and_op (s, AtomicOp.Or);
  END fetch_and_or;

PROCEDURE fetch_and_and (VAR s: State) =
  BEGIN
    fetch_and_op (s, AtomicOp.And);
  END fetch_and_and;

PROCEDURE fetch_and_xor (VAR s: State) =
  BEGIN
    fetch_and_op (s, AtomicOp.Xor);
  END fetch_and_xor;

BEGIN
  FOR op := FIRST (CmdMap) TO LAST (CmdMap) DO
    <*ASSERT CmdMap[op].bop = op *>
  END;
END M3CG_BinRd.
