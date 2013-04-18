(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: CG.m3                                                 *)
(* Last modified on Tue Jun 20 15:58:21 PDT 1995 by kalsow     *)
(*      modified on Tue May 25 11:19:53 PDT 1993 by muller     *)

MODULE CG;

IMPORT Text, IntIntTbl, IntRefTbl, Fmt, Word;
IMPORT Scanner, Error, Module, RunTyme, WebInfo;
IMPORT M3, M3CG, M3CG_Ops, M3CG_Check;
IMPORT Host, Target, TInt, TFloat, TWord, TargetMap, M3RT (**, RTObject **);

CONST
  Max_init_chars = 256; (* max size of a single init_chars string *)

REVEAL
  Val = BRANDED "CG.Val" REF ValRec;

TYPE
  VKind = {      (* TYPE   VALUE                 *)
    Integer,     (* Int    int                   *)
    Float,       (* Float  float                 *)
    Stacked,     (* any    S0.type               *)
    Direct,      (* any    MEM(ADR(base) + OFFS) *)
    Absolute,    (* Addr   ADR(base) + OFFS      *)
    Indirect,    (* Addr   MEM(base) + OFFS      *)
    Pointer      (* Addr   S0.A + OFFS           *)
  }; (* where OFFS == offset + MEM(bits)         *)

TYPE
  ValRec = RECORD
    kind      : VKind;        (* type of descriptor *)
    type      : Type;         (* type of the value *)
    temp_base : BOOLEAN;      (* TRUE => base is a temp. *)
    temp_bits : BOOLEAN;      (* TRUE => bits is a temp. *)
    align     : Alignment;    (* assumed alignment of base address *)
    base      : Var;          (* base address *)
    bits      : Var;          (* non-constant bit offset *)
    offset    : INTEGER;      (* constant bit offset *)
    next      : Val;          (* link for lists *)
    int       : Target.Int;   (* literal integer value *)
    float     : Target.Float; (* literal floating point value *)
  END;

TYPE
  TempWrapper = REF RECORD
    next   : TempWrapper;
    temp   : Var;
    size   : Size;
    align  : Alignment;
    type   : Type;
    in_mem : BOOLEAN;
    block  : INTEGER;
  END;

TYPE
  Node = OBJECT
    next : Node;
    (** file : String.T;**)
    (** line : INTEGER; **)
    o    : Offset;
  METHODS
    dump();
  END;

TYPE
  FloatNode   = Node OBJECT f: Target.Float OVERRIDES dump := DumpFloat END;
  CharsNode   = Node OBJECT t: TEXT  OVERRIDES dump := DumpChars END;
  ProcNode    = Node OBJECT v: Proc OVERRIDES dump := DumpProc END;
  LabelNode   = Node OBJECT v: Label OVERRIDES dump := DumpLabel END;
  VarNode     = Node OBJECT v: Var;  b: Offset OVERRIDES dump := DumpVar END;
  OffsetNode  = Node OBJECT v: Var;  OVERRIDES dump := DumpOffset END;
  CommentNode = Node OBJECT a, b, c, d: TEXT OVERRIDES dump := DumpComment END;
  IntNode     = Node OBJECT s: Size; v: Target.Int OVERRIDES dump := DumpInt END;
  FieldNode   = Node OBJECT n: Name; s: Size; t: TypeUID OVERRIDES dump := DumpField END;

VAR
  cg_wr       : M3CG.T      := NIL;
  cg_check    : M3CG.T      := NIL;
  cg          : M3CG.T      := NIL;
  last_offset : INTEGER     := -2;
  last_file   : TEXT        := NIL;
  last_line   : INTEGER     := -2;
  pending     : ARRAY BOOLEAN OF Node;
  fields      : ARRAY BOOLEAN OF Node;
  in_init     : BOOLEAN     := FALSE;
  init_pc     : INTEGER     := 0;
  init_bits   : Target.Int  := TInt.Zero;
  free_temps  : TempWrapper := NIL;
  busy_temps  : TempWrapper := NIL;
  free_values : Val         := NIL;
  busy_values : Val         := NIL;
  indirects   : IntIntTbl.T := NIL;
  variables   : IntRefTbl.T := NIL;
  procedures  : IntRefTbl.T := NIL;
  block_cnt   : INTEGER     := 0;
  tos         : CARDINAL    := 0;  (* top-of-stack *)
  stack       : ARRAY [0..99] OF ValRec;

VAR (*CONST*)
  StackType   : ARRAY Type OF Type;

(*---------------------------------------------------------------------------*)

PROCEDURE Init () =
  BEGIN
    Max_alignment := Target.Alignments [LAST (Target.Alignments)];

    FOR t := FIRST (Type) TO LAST (Type) DO StackType[t] := t; END;
    FOR t := Type.Word8 TO Type.Int64 DO
      IF TargetMap.CG_Size[t] <= Target.Integer.size
        THEN StackType[t] := Target.Integer.cg_type;
        ELSE StackType[t] := Target.Longint.cg_type;
      END;
    END;

    cg_wr := Host.env.init_code_generator ();
    IF (cg_wr = NIL) THEN
      Error.Msg ("unable to create a code generator");
      RETURN;
    END;
    (** RTObject.PatchMethods (cg_wr); **)

    cg_check := M3CG_Check.New (cg_wr,
                                clean_jumps  := Host.clean_jumps,
                                clean_stores := Host.clean_stores,
                                nested_calls := Host.nested_calls,
                                nested_procs := Host.inline_nested_procs);
    (** RTObject.PatchMethods (cg_check); **)
    cg := cg_check;

    cg.set_error_handler (Error.Msg);

    last_offset    := -2;
    last_file      := NIL;
    last_line      := -2;
    pending[FALSE] := NIL;
    pending[TRUE]  := NIL;
    fields[FALSE]  := NIL;
    fields[TRUE]   := NIL;
    in_init        := FALSE;
    init_pc        := 0;
    init_bits      := TInt.Zero;
    free_temps     := NIL;
    busy_temps     := NIL;
    free_values    := NIL;
    busy_values    := NIL;
    indirects      := NIL;
    variables      := NIL;
    procedures     := NIL;
    block_cnt      := 0;
    tos            := 0;
  END Init;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE Next_label (n_labels := 1): Label =
  BEGIN
    RETURN cg.next_label (n_labels);
  END Next_label;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE Begin_unit (optimize: INTEGER := 0) =
  BEGIN
    cg.begin_unit (optimize);
  END Begin_unit;

PROCEDURE End_unit () =
  BEGIN
    Free_all_values ();
    Free_all_temps ();
    cg.end_unit ();
  END End_unit;

PROCEDURE Import_unit (n: Name) =
  BEGIN
    cg.import_unit (n);
    WebInfo.Import_unit (n);
  END Import_unit;

PROCEDURE Export_unit (n: Name) =
  BEGIN
    cg.export_unit (n);
    WebInfo.Export_unit (n);
  END Export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE Gen_location (here: INTEGER) =
  VAR file: TEXT;  save, line: INTEGER;
  BEGIN
    IF (here = last_offset) THEN RETURN END;

    save := Scanner.offset;
    Scanner.offset := here;
    Scanner.LocalHere (file, line);

    IF (last_file = NIL) OR NOT Text.Equal (last_file, file) THEN
      cg.set_source_file (file);
      last_file := file;
    END;

    IF (last_line # line) THEN
      cg.set_source_line (line);
      last_line := line;
    END;

    Scanner.offset := save;
    last_offset := here;
  END Gen_location;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE Declare_typename (t: TypeUID;  n: Name) =
  BEGIN
    cg.declare_typename (t, n);
  END Declare_typename;

PROCEDURE Declare_array (t: TypeUID;  index, elt: TypeUID;  s: Size) =
  BEGIN
    cg.declare_array (t, index, elt, s);
    WebInfo.Declare_array (t, index, elt, s);
  END Declare_array;

PROCEDURE Declare_open_array (t: TypeUID;  elt: TypeUID;  s: Size) =
  BEGIN
    cg.declare_open_array (t, elt, s);
    WebInfo.Declare_open_array (t, elt, s);
  END Declare_open_array;

PROCEDURE Declare_enum (t: TypeUID;  n_elts: INTEGER;  s: Size) =
  BEGIN
    cg.declare_enum (t, n_elts, s);
    WebInfo.Declare_enum (t, n_elts, s);
  END Declare_enum;

PROCEDURE Declare_enum_elt (n: Name) =
  BEGIN
    cg.declare_enum_elt (n);
    WebInfo.Declare_enum_elt (n);
  END Declare_enum_elt;

PROCEDURE Declare_packed (t: TypeUID;  s: Size;  base: TypeUID) =
  BEGIN
    cg.declare_packed (t, s, base);
    WebInfo.Declare_packed (t, s, base);
  END Declare_packed;

PROCEDURE Declare_record (t: TypeUID;  s: Size;  n_fields: INTEGER) =
  BEGIN
    cg.declare_record (t, s, n_fields);
    WebInfo.Declare_record (t, s, n_fields);
  END Declare_record;

PROCEDURE Declare_field (n: Name;  o: Offset;  s: Size;  t: TypeUID) =
  BEGIN
    cg.declare_field (n, o, s, t);
    WebInfo.Declare_field (n, o, s, t);
  END Declare_field;

PROCEDURE Declare_set (t, domain: TypeUID;  s: Size) =
  BEGIN
    cg.declare_set (t, domain, s);
    WebInfo.Declare_set (t, domain, s);
  END Declare_set;

PROCEDURE Declare_subrange (t, domain: TypeUID;  READONLY min, max: Target.Int;
                                                 s: Size) =
  BEGIN
    cg.declare_subrange (t, domain, min, max, s);
    WebInfo.Declare_subrange (t, domain, min, max, s);
  END Declare_subrange;

PROCEDURE Declare_pointer (t, target: TypeUID;  brand: TEXT;  traced: BOOLEAN)=
  BEGIN
    cg.declare_pointer (t, target, brand, traced);
    WebInfo.Declare_pointer (t, target, brand, traced);
  END Declare_pointer;

PROCEDURE Declare_indirect (target: TypeUID): TypeUID =
  VAR x: INTEGER;
  BEGIN
    IF (indirects = NIL) THEN indirects := NewIntTbl () END;
    IF NOT indirects.get (target, x) THEN
      x := Word.Not (target);  (* !! fingerprint HACK !! *)
      cg.declare_indirect (x, target);
      WebInfo.Declare_indirect (x, target);
      EVAL indirects.put (target, x);
    END;
    RETURN x;
  END Declare_indirect;

PROCEDURE Declare_proctype (t: TypeUID;  n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention) =
  BEGIN
    cg.declare_proctype (t, n_formals, result, n_raises, cc);
    WebInfo.Declare_proctype (t, n_formals, result, n_raises);
  END Declare_proctype;

PROCEDURE Declare_formal (n: Name;  t: TypeUID) =
  BEGIN
    cg.declare_formal (n, t);
    WebInfo.Declare_formal (n, t);
  END Declare_formal;

PROCEDURE Declare_raises (n: Name) =
  BEGIN
    cg.declare_raises (n);
    WebInfo.Declare_raises (n);
  END Declare_raises;

PROCEDURE Declare_object (t, super: TypeUID;  brand: TEXT;  traced: BOOLEAN;
                           n_fields, n_methods, n_overrides: INTEGER;
                           field_size: Size) =
  BEGIN
    cg.declare_object (t, super, brand, traced,
                       n_fields, n_methods, field_size);
    WebInfo.Declare_object (t, super, brand, traced,
                            n_fields, n_methods, n_overrides, field_size);
  END Declare_object;

PROCEDURE Declare_method (n: Name;  signature: TypeUID;  dfault: M3.Expr) =
  BEGIN
    cg.declare_method (n, signature);
    WebInfo.Declare_method (n, signature, dfault);
  END Declare_method;

PROCEDURE Declare_override (n: Name;  dfault: M3.Expr) =
  BEGIN
    WebInfo.Declare_override (n, dfault);
  END Declare_override;

PROCEDURE Declare_opaque (t, super: TypeUID) =
  BEGIN
    cg.declare_opaque (t, super);
    WebInfo.Declare_opaque (t, super);
  END Declare_opaque;

PROCEDURE Reveal_opaque (lhs, rhs: TypeUID) =
  BEGIN
    cg.reveal_opaque (lhs, rhs);
    WebInfo.Reveal_opaque (lhs, rhs);
  END Reveal_opaque;

PROCEDURE Declare_global_field (n: Name;  o: Offset;  s: Size;  t: TypeUID;
                                is_const: BOOLEAN) =
  BEGIN
    fields[is_const] := NEW (FieldNode, next := fields[is_const],
                             n := n, o := o, s := s, t := t);
  END Declare_global_field;

PROCEDURE DumpField (x: FieldNode) =
  BEGIN
    (* DumpNode (x);  -- no file & line number info *)
    cg.declare_field (x.n, x.o, x.s, x.t);
  END DumpField;

PROCEDURE Emit_global_record (s: Size;  is_const: BOOLEAN) =
  VAR n := fields[is_const];  cnt := 0;  xx: REF ARRAY OF Node;
  BEGIN
    (* build a sorted array of fields *)
    WHILE (n # NIL) DO INC (cnt);  n := n.next END;
    xx := NEW (REF ARRAY OF Node, cnt);
    n := fields[is_const];  cnt := 0;
    WHILE (n # NIL) DO xx[cnt] := n;  INC (cnt);  n := n.next;  END;
    SortNodes (xx^);

    (* finally, declare the record *)
    cg.declare_record (-1, s, NUMBER (xx^));
    FOR i := 0 TO LAST (xx^) DO  xx[i].dump () END;
    fields[is_const] := NIL;
  END Emit_global_record;

PROCEDURE Declare_exception (n: Name;  arg_type: TypeUID;
                           raise_proc: BOOLEAN;  base: Var;  offset: INTEGER) =
  BEGIN
    cg.declare_exception (n, arg_type, raise_proc, base, ToBytes (offset));
  END Declare_exception;

(*--------------------------------------------------------- RunTyme hooks ---*)

PROCEDURE Set_runtime_proc (n: Name;  p: Proc) =
  BEGIN
    cg.set_runtime_proc (n, p);
  END Set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE Import_global (n: Name;  s: Size;  a: Alignment;  t: Type;
                         m3t: TypeUID): Var =
  VAR ref: REFANY;  v: Var;
  BEGIN
    IF (variables = NIL) THEN variables := NewNameTbl () END;
    IF variables.get (n, ref) THEN RETURN ref END;
    v := cg.import_global (n, ToVarSize (s, a), FixAlign (a), t, m3t);
    EVAL variables.put (n, v);
    RETURN v;
  END Import_global;

PROCEDURE Declare_segment (n: Name;  m3t: TypeUID;  is_const: BOOLEAN): Var =
  BEGIN
    RETURN cg.declare_segment (n, m3t, is_const);
  END Declare_segment;

PROCEDURE Bind_segment (seg: Var;  s: Size;  a: Alignment;  t: Type;
                        exported, init, is_const: BOOLEAN) =
  BEGIN
    cg.bind_segment (seg, ToVarSize (s, a), FixAlign (a), t, exported, init);
    IF (init) THEN
      Begin_init (seg);
      DumpPendingNodes (is_const);
      End_init (seg);
    END;
  END Bind_segment;

PROCEDURE Declare_global (n: Name;  s: Size;  a: Alignment;  t: Type;
                          m3t: TypeUID;  exported, init: BOOLEAN): Var =
  BEGIN
    RETURN cg.declare_global (n, ToVarSize (s, a), FixAlign (a),
                              t, m3t, exported, init);
  END Declare_global;

PROCEDURE Declare_constant (n: Name;  s: Size;  a: Alignment;  t: Type;
                            m3t: TypeUID;  exported, init: BOOLEAN): Var =
  BEGIN
    RETURN cg.declare_constant (n, ToVarSize (s, a), FixAlign (a),
                                t, m3t, exported, init);
  END Declare_constant;

PROCEDURE Declare_local (n: Name;  s: Size;  a: Alignment;  t: Type;
                         m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  BEGIN
    RETURN cg.declare_local (n, ToVarSize (s, a), FixAlign (a),
                             t, m3t, in_memory, up_level, f);
  END Declare_local;

PROCEDURE Declare_param (n: Name;  s: Size;  a: Alignment;  t: Type;
                         m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  BEGIN
    RETURN cg.declare_param (n, ToVarSize (s, a), FixAlign (a),
                             t, m3t, in_memory, up_level, f);
  END Declare_param;

(*----------------------------------------------------------- temporaries ---*)


PROCEDURE Declare_temp (s: Size;  a: Alignment;  t: Type;
                          in_memory: BOOLEAN): Var =
  VAR w := free_temps;  last_w: TempWrapper := NIL;  tmp: Var;
  BEGIN
    LOOP
      IF (w = NIL) THEN
        (* we need to allocate a fresh one *)
        tmp := cg.declare_temp (ToVarSize (s, a), FixAlign (a), t, in_memory);
        busy_temps := NEW (TempWrapper, size := s, align := a, type := t,
                           in_mem := in_memory, temp := tmp,
                           block := block_cnt, next := busy_temps);
        RETURN tmp;
      ELSIF (w.size = s) AND (w.align = a) AND (w.type = t) AND
        (w.in_mem = in_memory) THEN
        (* we found a match *)
        IF (last_w = NIL)
          THEN free_temps := w.next;
          ELSE last_w.next := w.next;
        END;
        w.next := busy_temps;  busy_temps := w;
        RETURN w.temp;
      ELSE
        (* try the next one *)
        last_w := w;
        w := w.next;
      END;
    END;
  END Declare_temp;

PROCEDURE Free_temp (<*UNUSED*> v: Var) =
  BEGIN
  END Free_temp;

PROCEDURE Free_temps () =
  VAR w := busy_temps;
  BEGIN
    SEmpty ("Free_temps");
    IF (w # NIL) THEN
      WHILE (w.next # NIL) DO  w := w.next;  END;
      w.next := free_temps;
      free_temps := busy_temps;
      busy_temps := NIL;
    END;
  END Free_temps;

(******
PROCEDURE Free_one_temp (v: Var) =
  VAR w := busy_temps;  last_w : TempWrapper := NIL;
  BEGIN
    LOOP
      IF (w = NIL) THEN Error.Msg ("");
        (* missing wrapper! *)
        Err ("missing temp wrapper");
        cg.free_temp (v);
        RETURN;
      ELSIF (w.temp = v) THEN
        (* we found the match *)
        IF (last_w = NIL)
          THEN busy_temps := w.next;
          ELSE last_w.next := w.next;
        END;
        w.next := free_temps;  free_temps := w;
        RETURN;
      ELSE
        (* try the next one *)
        last_w := w;
        w := w.next;
      END;
    END;
  END Free_one_temp;
*********)

PROCEDURE Free_all_temps () =
  VAR w: TempWrapper;
  BEGIN
    Free_temps ();
    <*ASSERT busy_temps = NIL*>
    w := free_temps;
    WHILE (w # NIL) DO
      cg.free_temp (w.temp);
      w := w.next;
    END;
    free_temps := NIL;
  END Free_all_temps;

PROCEDURE Free_block_temps (block: INTEGER) =
  VAR w, prev_w: TempWrapper;
  BEGIN
    Free_temps ();
    <*ASSERT busy_temps = NIL*>
    w := free_temps;  prev_w := NIL;
    WHILE (w # NIL) DO
      IF (w.block = block) THEN
        cg.free_temp (w.temp);
        IF (prev_w # NIL)
          THEN  prev_w.next := w.next;
          ELSE  free_temps := w.next;
        END;
      END;
      w := w.next;
    END;
  END Free_block_temps;

(*--------------------------------------------- direct stack manipulation ---*)

PROCEDURE Pop (): Val =
  VAR z: Var;  v: Val;
  BEGIN
    (* get a free value *)
    v := free_values;
    IF (v = NIL)
      THEN v := NEW (Val);
      ELSE free_values := v.next;
    END;

    (* fill it in *)
    WITH x = stack [SCheck (1, "Pop")] DO
      v^ := x;
    END;
    SPop (1, "Pop");

    (* mark it as busy *)
    v.next := busy_values;
    busy_values := v;

    (* make sure it's not bound to the M3CG stack *)
    IF (v.kind = VKind.Stacked) THEN
      z := Declare_temp (TargetMap.CG_Size [v.type], TargetMap.CG_Align [v.type],
                         v.type, in_memory := FALSE);
      cg.store (z, 0, StackType[v.type], v.type);
      v.kind      := VKind.Direct;
      v.temp_base := TRUE;
      v.temp_bits := FALSE;
      v.align     := TargetMap.CG_Align [v.type];
      v.base      := z;
      v.bits      := NIL;
      v.offset    := 0;

    ELSIF (v.kind = VKind.Pointer) THEN
      z := Declare_temp (Target.Address.size, Target.Address.align,
                         Type.Addr, in_memory := FALSE);
      cg.store (z, 0, Type.Addr, Type.Addr);

      v.kind      := VKind.Indirect;
      v.type      := Type.Addr;
      v.temp_base := TRUE;
      v.temp_bits := FALSE;
      v.base      := z;
      v.bits      := NIL;
    END;

    RETURN v;
  END Pop;

PROCEDURE Pop_temp (): Val =
  BEGIN
    Force ();
    RETURN Pop ();
  END Pop_temp;

PROCEDURE Push (v: Val) =
  BEGIN
    WITH x = stack [SCheck (0, "Push")] DO
      x := v^;
      x.temp_base := FALSE;
      x.temp_bits := FALSE;
      x.next      := NIL;
    END;
    INC (tos);
  END Push;

PROCEDURE Store_temp (v: Val) =
  BEGIN
    <*ASSERT v.kind = VKind.Direct  AND  v.offset = 0 *>
    Store (v.base, 0, TargetMap.CG_Size[v.type], TargetMap.CG_Align[v.type], v.type);
  END Store_temp;

PROCEDURE Free (v: Val) =
  VAR x := busy_values;  last_x: Val := NIL;
  BEGIN
    (* remove 'v' from the busy list *)
    LOOP
      IF (x = NIL) THEN
        Err ("non-busy value freed");
        EXIT;
      ELSIF (x = v) THEN
        (* we found the match *)
        IF (last_x = NIL)
          THEN busy_values := v.next;
          ELSE last_x.next := v.next;
        END;
        v.next := free_values;  free_values := v;
        EXIT;
      ELSE
        last_x := x;
        x := x.next;
      END;
    END;

    (* finally, free the temps *)
    Release_temps (v^);
  END Free;

PROCEDURE Free_all_values () =
  BEGIN
    WHILE (busy_values # NIL) DO  Free (busy_values); END;
  END Free_all_values;

PROCEDURE XForce () =
  (* force the value enough so that we can do a simple indirect load/store *)
  VAR offs: INTEGER;
  BEGIN
    WITH x = stack [SCheck (1, "XForce")] DO
      IF (x.kind = VKind.Direct) THEN
        Force ();
      ELSIF (x.kind = VKind.Indirect) THEN
        offs := x.offset;  x.offset := 0;
        Force ();
        x.offset := offs;
      END;
    END;
  END XForce;

PROCEDURE Force () =
  BEGIN
    WITH x = stack [SCheck (1, "Force")] DO

      (* force the value on the stack *)
      CASE (x.kind) OF

      | VKind.Integer =>
          IF x.type = Target.Word.cg_type THEN
            x.type := Target.Integer.cg_type;
          ELSIF x.type = Target.Long.cg_type THEN
            x.type := Target.Longint.cg_type;
          ELSIF x.type = Target.Integer.cg_type THEN
            (* ok *)
          ELSIF x.type = Target.Longint.cg_type THEN
            (* ok *)
          ELSE
            <*ASSERT FALSE*>
          END;
          cg.load_integer (x.type, x.int);

      | VKind.Float =>
          x.type := TargetMap.Float_types [TFloat.Prec (x.float)].cg_type;
          cg.load_float (x.type, x.float);

      | VKind.Stacked =>
          (* value is already on the stack *)

      | VKind.Direct =>
          Force_align (x);
          cg.load (x.base, AsBytes (x.offset), x.type, StackType[x.type]);
          IF (x.bits # NIL) THEN
            Err ("attempt to force a direct bit-level address...");
          END;

      | VKind.Absolute =>
          Force_align (x);
          cg.load_address (x.base, AsBytes (x.offset));
          Force_LValue (x);

      | VKind.Indirect =>
          Force_align (x);
          cg.load  (x.base, 0, Type.Addr, Type.Addr);
          IF (x.offset # 0) THEN cg.add_offset (AsBytes (x.offset)) END;
          Force_LValue (x);

      | VKind.Pointer =>
          Force_align (x);
          IF (x.offset # 0) THEN cg.add_offset (AsBytes (x.offset)) END;
          Force_LValue (x);

      END;

      (* free any temps that we used *)
      Release_temps (x);

      (* finish the descriptor *)
      x.kind      := VKind.Stacked;
      x.type      := StackType[x.type];
      x.offset    := 0;
      x.next      := NIL;
      (** x.align     := TargetMap.CG_Align [x.type];
        --- we're not changing the alignment of this value **)
    END;
  END Force;

PROCEDURE Force_align (VAR x: ValRec) =
  BEGIN
    x.align := LV_align (x);
    IF (x.align MOD Target.Byte) # 0 THEN
      Err ("address is not byte-aligned");
    END;
  END Force_align;

PROCEDURE Force_LValue (VAR x: ValRec) =
  BEGIN
    x.type := Type.Addr;
    IF (x.bits # NIL) THEN
      Err ("attempt to force a bit-level L-value...");
    END;
  END Force_LValue;

PROCEDURE Release_temps (VAR x: ValRec) =
  BEGIN
    IF (x.temp_base) THEN Free_temp (x.base); END;
    IF (x.temp_bits) THEN Free_temp (x.bits); END;
    x.temp_base := FALSE;
    x.temp_bits := FALSE;
    x.base      := NIL;
    x.bits      := NIL;
  END Release_temps;

PROCEDURE Force1 (tag: TEXT) =
  BEGIN
    Force ();
    SPop (1, tag);
  END Force1;

PROCEDURE Force2 (tag: TEXT;  commute: BOOLEAN): BOOLEAN =
  VAR swapped := Force_pair (commute);
  BEGIN
    SPop (2, tag);
    RETURN swapped;
  END Force2;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE Begin_init (v: Var) =
  BEGIN
    cg.begin_init (v);
    in_init := TRUE;
    init_pc := 0;
    init_bits := TInt.Zero;
  END Begin_init;

PROCEDURE End_init (v: Var) =
  BEGIN
    AdvanceInit (init_pc + Target.Byte - 1); (* flush any pending bits *)
    cg.end_init (v);
    in_init := FALSE;
  END End_init;

PROCEDURE DumpPendingNodes (is_const: BOOLEAN) =
  VAR n := pending[is_const];  cnt := 0;  xx: REF ARRAY OF Node;
  BEGIN
    WHILE (n # NIL) DO INC (cnt);  n := n.next END;
    xx := NEW (REF ARRAY OF Node, cnt);
    n := pending[is_const];  cnt := 0;
    WHILE (n # NIL) DO xx[cnt] := n;  INC (cnt);  n := n.next;  END;
    SortNodes (xx^);
    FOR i := 0 TO LAST (xx^) DO  xx[i].dump () END;
    pending[is_const] := NIL;
  END DumpPendingNodes;

PROCEDURE SortNodes (VAR x: ARRAY OF Node) =
  BEGIN
    QuickSort (x, 0, NUMBER (x));
    InsertionSort (x, 0, NUMBER (x));
  END SortNodes;

PROCEDURE QuickSort (VAR a: ARRAY OF Node;  lo, hi: INTEGER) =
  CONST CutOff = 9;
  VAR i, j: INTEGER;  key, tmp: Node;
  BEGIN
    WHILE (hi - lo > CutOff) DO (* sort a[lo..hi) *)

      (* use median-of-3 to select a key *)
      i := (hi + lo) DIV 2;
      IF (a[lo].o < a[i].o) THEN
        IF (a[i].o < a[hi-1].o) THEN
          key := a[i];
        ELSIF (a[lo].o < a[hi-1].o) THEN
          key := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        ELSE
          key := a[lo];  a[lo] := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        END;
      ELSE
        IF (a[hi-1].o < a[i].o) THEN
          key := a[i];  tmp := a[hi-1];  a[hi-1] := a[lo];  a[lo] := tmp;
        ELSIF (a[lo].o < a[hi-1].o) THEN
          key := a[lo];  a[lo] := a[i];  a[i] := key;
        ELSE
          key := a[hi-1];  a[hi-1] := a[lo];  a[lo] := a[i];  a[i] := key;
        END;
      END;

      (* partition the array *)
      i := lo+1;  j := hi-2;

      (* find the first hole *)
      WHILE (a[j].o > key.o) DO DEC (j) END;
      tmp := a[j];
      DEC (j);

      LOOP
        IF (i > j) THEN EXIT END;

        WHILE (a[i].o < key.o) DO INC (i) END;
        IF (i > j) THEN EXIT END;
        a[j+1] := a[i];
        INC (i);

        WHILE (a[j].o > key.o) DO DEC (j) END;
        IF (i > j) THEN  IF (j = i-1) THEN  DEC (j)  END;  EXIT  END;
        a[i-1] := a[j];
        DEC (j);
      END;

      (* fill in the last hole *)
      a[j+1] := tmp;
      i := j+2;

      (* then, recursively sort the smaller subfile *)
      IF (i - lo < hi - i)
        THEN  QuickSort (a, lo, i-1);   lo := i;
        ELSE  QuickSort (a, i, hi);     hi := i-1;
      END;

    END; (* WHILE (hi-lo > CutOff) *)
  END QuickSort;

PROCEDURE InsertionSort (VAR a: ARRAY OF Node;  lo, hi: INTEGER) =
  VAR j: INTEGER;  key: Node;
  BEGIN
    FOR i := lo+1 TO hi-1 DO
      key := a[i];
      j := i-1;
      WHILE (j >= lo) AND (key.o < a[j].o) DO
        a[j+1] := a[j];
        DEC (j);
      END;
      a[j+1] := key;
    END;
  END InsertionSort;

PROCEDURE PushPending (n: Node;  is_const: BOOLEAN) =
  BEGIN
    (** n.file := last_file; **)
    (** n.line := last_line; **)
    n.next := pending[is_const];
    pending[is_const] := n;
  END PushPending;

PROCEDURE DumpNode (<*UNUSED*> n: Node) =
  BEGIN
    (******
    IF (last_file # n.file) THEN
      cg.set_source_file (n.file);
      last_file := n.file;
    END;
    IF (last_line # n.line) THEN
      cg.set_source_line (n.line);
      last_line := n.line;
    END;
    *******)
  END DumpNode;

PROCEDURE AdvanceInit (o: Offset) =
  VAR
    n_bytes := (o - init_pc) DIV Target.Byte;
    tmp, new_bits: Target.Int;
    size, excess: CARDINAL;
    t: Type;
  BEGIN
    <*ASSERT n_bytes >= 0*>
    <*ASSERT in_init*>
    WHILE (n_bytes > 0) DO
      IF TInt.EQ (init_bits, TInt.Zero) THEN
        (* no more bits to flush *)
        n_bytes := 0;
        init_pc := (o DIV Target.Byte) * Target.Byte;
      ELSE
        (* send out some number of bytes *)
        EVAL FindInitType (n_bytes, init_pc, t);
        size := TargetMap.CG_Size[t];
        excess := TWord.Size - size;
        IF (excess = 0) THEN
          cg.init_int (init_pc DIV Target.Byte, init_bits, t);
          init_bits := TInt.Zero;
        ELSIF Target.Little_endian
          AND TWord.Extract (init_bits, 0, size, tmp)
          AND TWord.Extract (init_bits, size, excess, new_bits) THEN
          cg.init_int (init_pc DIV Target.Byte, tmp, t);
          init_bits := new_bits;
        ELSIF (NOT Target.Little_endian)
          AND TWord.Extract (init_bits, excess, size, tmp)
          AND TWord.Extract (init_bits, 0, excess, new_bits) THEN
          cg.init_int (init_pc DIV Target.Byte, tmp, t);
          TWord.LeftShift (new_bits, size, init_bits);
        ELSE
          Err ("unable to convert or initialize bit field value??  n_bytes="
                & Fmt.Int(n_bytes) & "  size=" & Fmt.Int (size));
          (** <*ASSERT FALSE*> **)
        END;
        DEC (n_bytes, TargetMap.CG_Bytes[t]);
        INC (init_pc, TargetMap.CG_Size[t]);
      END;
    END;
  END AdvanceInit;

PROCEDURE FindInitType (n_bytes, offset: INTEGER;  VAR t: Type): BOOLEAN =
  BEGIN
    FOR i := LAST (TargetMap.Integer_types)
          TO FIRST (TargetMap.Integer_types) BY -1 DO
      WITH z = TargetMap.Integer_types[i] DO
        IF (z.bytes <= n_bytes)
          AND (offset MOD z.align = 0) THEN
          t := z.cg_type;
          RETURN TRUE;
        END;
      END;
    END;
    ErrI (n_bytes, "cg: unable to find suitable target machine type");
    t := Type.Void;
    RETURN FALSE;
  END FindInitType;

PROCEDURE Init_int (o: Offset;  s: Size;  READONLY value: Target.Int;
                    is_const: BOOLEAN) =
  VAR bit_offset: CARDINAL;  itype: Type;  tmp: Target.Int;
  BEGIN
    IF (NOT in_init) THEN
      PushPending (NEW (IntNode, o := o, s := s, v := value), is_const);
      RETURN;
    END;

    AdvanceInit (o);
    IF Target.Little_endian
      THEN bit_offset := o - init_pc;
      ELSE bit_offset := TWord.Size - (o - init_pc) - s;
    END;

    IF (o = init_pc)
      AND (s >= Target.Byte) 
      AND (FindInitType (s DIV Target.Byte, init_pc, itype))
      AND (TargetMap.CG_Size[itype] = s) THEN
      (* simple, aligned integer initialization *)
      cg.init_int (o DIV Target.Byte, value, itype);
    ELSIF TWord.Insert (init_bits, value, bit_offset, s, tmp) THEN
      init_bits := tmp;
    ELSE
      Err ("unable to stuff bit field value??");
      <*ASSERT FALSE*>
    END;
  END Init_int;

PROCEDURE Init_intt (o: Offset;  s: Size;  value: INTEGER;  is_const: BOOLEAN) =
  VAR val: Target.Int;  b := TInt.FromInt (value, val);
  BEGIN
    IF NOT b
      OR TInt.LT (val, Target.Integer.min)
      OR TInt.LT (Target.Integer.max, val)
    THEN ErrI (value, "integer const not representable") END;
    Init_int (o, s, val, is_const);
  END Init_intt;

PROCEDURE DumpInt (x: IntNode) =
  BEGIN
    DumpNode (x);
    Init_int (x.o, x.s, x.v, FALSE);
  END DumpInt;

PROCEDURE Init_proc (o: Offset;  value: Proc;  is_const: BOOLEAN) =
  BEGIN
    <*ASSERT o MOD Target.Address.align = 0 *>
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
      cg.init_proc (AsBytes (o), value);
    ELSE
      PushPending (NEW (ProcNode, o := o, v := value), is_const);
    END;
  END Init_proc;

PROCEDURE DumpProc (x: ProcNode) =
  BEGIN
    DumpNode (x);
    Init_proc (x.o, x.v, FALSE);
  END DumpProc;

PROCEDURE Init_label (o: Offset;  value: Label;  is_const: BOOLEAN) =
  BEGIN
    <*ASSERT o MOD Target.Address.align = 0 *>
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
      cg.init_label (AsBytes (o), value);
    ELSE
      PushPending (NEW (LabelNode, o := o, v := value), is_const);
    END;
  END Init_label;

PROCEDURE DumpLabel (x: LabelNode) =
  BEGIN
    DumpNode (x);
    Init_label (x.o, x.v, FALSE);
  END DumpLabel;

PROCEDURE Init_var (o: Offset;  value: Var;  bias: Offset;  is_const: BOOLEAN) =
  BEGIN
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
      <*ASSERT o MOD Target.Address.align = 0 *>
      <*ASSERT bias MOD Target.Byte = 0*>
      cg.init_var (AsBytes (o), value, AsBytes (bias));
    ELSE
      PushPending (NEW (VarNode, o := o, v := value, b := bias), is_const);
    END;
  END Init_var;

PROCEDURE DumpVar (x: VarNode) =
  BEGIN
    DumpNode (x);
    Init_var (x.o, x.v, x.b, FALSE);
  END DumpVar;

PROCEDURE Init_offset (o: Offset;  value: Var;  is_const: BOOLEAN) =
  BEGIN
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
      <*ASSERT o MOD Target.Integer.align = 0 *>
      cg.init_offset (AsBytes (o), value);
    ELSE
      PushPending (NEW (OffsetNode, o := o, v := value), is_const);
    END;
  END Init_offset;

PROCEDURE DumpOffset (x: OffsetNode) =
  BEGIN
    DumpNode (x);
    Init_offset (x.o, x.v, FALSE);
  END DumpOffset;

PROCEDURE Init_chars (o: Offset;  value: TEXT;  is_const: BOOLEAN) =
  VAR len, start: INTEGER;
  BEGIN
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
      <*ASSERT o MOD Target.Char.align = 0 *>
      start := 0;
      len := Text.Length (value);
      WHILE (len - start > Max_init_chars) DO
        cg.init_chars (AsBytes (o), Text.Sub (value, start, Max_init_chars));
        INC (o, Max_init_chars * Target.Char.size);
        INC (start, Max_init_chars);
      END;
      IF (start < len) THEN
        cg.init_chars (AsBytes (o), Text.Sub (value, start));
      END;
    ELSE
      PushPending (NEW (CharsNode, o := o, t := value), is_const);
    END;
  END Init_chars;

PROCEDURE DumpChars (x: CharsNode) =
  BEGIN
    DumpNode (x);
    Init_chars (x.o, x.t, FALSE);
  END DumpChars;

PROCEDURE Init_float (o: Offset;  READONLY f: Target.Float;  is_const: BOOLEAN) =
  BEGIN
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
      <*ASSERT o MOD Target.Real.align = 0 *>
      cg.init_float (AsBytes (o), f);
    ELSE
      PushPending (NEW (FloatNode, o := o, f := f), is_const);
    END;
  END Init_float;

PROCEDURE DumpFloat (x: FloatNode) =
  BEGIN
    DumpNode (x);
    Init_float (x.o, x.f, FALSE);
  END DumpFloat;

PROCEDURE EmitText (t: TEXT;  is_const: BOOLEAN): INTEGER =
  VAR  len, size, align, offset: INTEGER;
  BEGIN
    IF (t = NIL) THEN t := "" END;
    len    := Text.Length (t) + 1;
    size   := len * Target.Char.size;
    (** align  := MAX (Target.Char.align, Target.Integer.align); **)
    align  := Target.Char.align;
    offset := Module.Allocate (size, align, is_const, "*string*");
    PushPending (NEW (CharsNode, o := offset, t := t), is_const);
    RETURN offset;
  END EmitText;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE Import_procedure (n: Name;  n_params: INTEGER;  ret_type: Type;
                            cc: CallingConvention;
                            VAR(*OUT*) new: BOOLEAN): Proc =
  VAR ref: REFANY;  p: Proc;
  BEGIN
    IF (procedures = NIL) THEN procedures := NewNameTbl() END;
    IF procedures.get (n, ref) THEN new := FALSE;  RETURN ref END;
    p := cg.import_procedure (n, n_params, ret_type, cc);
    EVAL procedures.put (n, p);
    new := TRUE;
    RETURN p;
  END Import_procedure;

PROCEDURE Declare_procedure (n: Name;  n_params: INTEGER;  ret_type: Type;
                             lev: INTEGER;  cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc): Proc =
  VAR p: Proc;
  BEGIN
    IF (procedures = NIL) THEN procedures := NewNameTbl() END;
    p := cg.declare_procedure (n, n_params, ret_type,
                                 lev, cc, exported, parent);
    EVAL procedures.put (n, p);
    RETURN p;
  END Declare_procedure;

PROCEDURE Begin_procedure (p: Proc) =
  BEGIN
    cg.begin_procedure (p);
  END Begin_procedure;

PROCEDURE End_procedure (p: Proc) =
  BEGIN
    Free_all_values ();
    Free_all_temps ();
    cg.end_procedure (p);
  END End_procedure;

PROCEDURE Begin_block () =
  BEGIN
    cg.begin_block ();
    INC (block_cnt);
  END Begin_block;

PROCEDURE End_block () =
  BEGIN
    Free_block_temps (block_cnt);
    DEC (block_cnt);
    cg.end_block ();
  END End_block;

PROCEDURE Note_procedure_origin (p: Proc) =
  BEGIN
    cg.note_procedure_origin (p);
  END Note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE Set_label (l: Label;  barrier: BOOLEAN := FALSE) =
  BEGIN
    cg.set_label (l, barrier);
  END Set_label;

PROCEDURE Jump (l: Label) =
  BEGIN
    cg.jump (l);
  END Jump;

PROCEDURE If_true (l: Label;  f: Frequency) =
  BEGIN
    Force1 ("If_true");
    cg.if_true (Target.Integer.cg_type, l, f);
  END If_true;

PROCEDURE If_false (l: Label;  f: Frequency) =
  BEGIN
    Force1 ("If_false");
    cg.if_false (Target.Integer.cg_type, l, f);
  END If_false;

PROCEDURE If_compare (t: ZType;  op: Cmp;  l: Label;  f: Frequency) =
  BEGIN
    IF Force2 ("If_compare", commute := TRUE) THEN
      op := M3CG.SwappedCompare [op];
    END;
    cg.if_compare (t, op, l, f);
  END If_compare;

PROCEDURE If_then (t: ZType;  op: Cmp;  true, false: Label;  f: Frequency) =
  BEGIN
    IF Force2 ("If_compare", commute := TRUE) THEN
      op := M3CG.SwappedCompare [op];
    END;
    IF (true = No_label) THEN
      op := M3CG.NotCompare [op];
      true := false;
    END;
    cg.if_compare (t, op, true, f);
  END If_then;

PROCEDURE Case_jump (READONLY labels: ARRAY OF Label) =
  BEGIN
    Force1 ("Case_jump");
    cg.case_jump (Target.Integer.cg_type, labels);
  END Case_jump;

PROCEDURE Exit_proc (t: Type) =
  BEGIN
    IF (t # Type.Void) THEN  Force1 ("Exit_proc");  END;
    cg.exit_proc (t);
  END Exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE Load (v: Var;  o: Offset;  s: Size;  a: Alignment;  t: Type) =
  VAR
    size  := TargetMap.CG_Size [t];
    align := TargetMap.CG_Align [t];
    best_align : Alignment;
    best_size  : Size;
    best_type  : MType;
  BEGIN
    IF (size = s) AND ((a+o) MOD align) = 0 THEN
      (* a simple aligned load *)
      SimpleLoad (v, o, t);

    ELSIF (size < s) THEN
      Err ("load size too large");
      SimpleLoad (v, o, t);
      Force ();  (* to connect the error message to the bad code *)

    ELSIF (t = Target.Word.cg_type) OR (t = Target.Integer.cg_type)
     OR   (t = Target.Long.cg_type) OR (t = Target.Longint.cg_type) THEN
      best_type  := FindIntType (t, s, o, a);
      best_size  := TargetMap.CG_Size [best_type];
      best_align := TargetMap.CG_Align [best_type];
      align := (a+o) MOD best_align;
      IF (s = best_size) AND (align = 0) THEN
        (* this is a simple partial word load *)
        SimpleLoad (v, o, best_type);
      ELSE
        (* unaligned, partial load *)
        cg.load (v, AsBytes (o - align), best_type, StackType[t]);
        IF Target.Little_endian
          THEN cg.extract_mn (StackType[t], Target.SignedType[t],
                              align, s);
          ELSE cg.extract_mn (StackType[t], Target.SignedType[t],
                              best_size - align - s, s);
        END;
        SPush (t);
      END;
    ELSE
      (* unaligned non-integer value *)
      Err ("unaligned load  type="& Fmt.Int (ORD (t))
          & "  s/o/a=" & Fmt.Int (s) & "/" & Fmt.Int (o) & "/" & Fmt.Int (a));
      SimpleLoad (v, o, t);
      Force ();  (* to connect the error message to the bad code *)
    END;
  END Load;

PROCEDURE SimpleLoad (v: Var;  o: Offset;  t: Type) =
  BEGIN
    WITH x = stack [SCheck (0, "SimpleLoad")] DO
      x.kind      := VKind.Direct;
      x.type      := t;
      x.temp_base := FALSE;
      x.temp_bits := FALSE;
      x.align     := Target.Byte;
      x.base      := v;
      x.bits      := NIL;
      x.offset    := o;
      x.next      := NIL;
    END;
    INC (tos);
  END SimpleLoad;

PROCEDURE Load_addr_of (v: Var;  o: Offset;  a: Alignment) =
  BEGIN
    WITH x = stack [SCheck (0, "Load_addr_of")] DO
      x.kind      := VKind.Absolute;
      x.type      := Type.Addr;
      x.temp_base := FALSE;
      x.temp_bits := FALSE;
      x.align     := FixAlign (a) * Target.Byte;
      x.base      := v;
      x.bits      := NIL;
      x.offset    := o;
      x.next      := NIL;
    END;
    INC (tos);
  END Load_addr_of;

PROCEDURE Load_addr_of_temp (v: Var;  o: Offset;  a: Alignment) =
  BEGIN
    Load_addr_of (v, o, a);
    stack[tos-1].temp_base := TRUE;
  END Load_addr_of_temp;

PROCEDURE Load_int (t: IType;  v: Var;  o: Offset := 0) =
  BEGIN
    SimpleLoad (v, o, t);
  END Load_int;

PROCEDURE Load_addr (v: Var;  o: Offset) =
  BEGIN
    SimpleLoad (v, o, Type.Addr);
  END Load_addr;

PROCEDURE Load_indirect (t: Type;  o: Offset;  s: Size) =
  VAR
    size  := TargetMap.CG_Size [t];
    align := TargetMap.CG_Align [t];
    best_align : Alignment;
    best_size  : Size;
    best_type  : MType;
    a: INTEGER;
    base_align : INTEGER;
    bit_offset : INTEGER;
    save_bits  : Var;
    save_temp  : BOOLEAN;
    const_bits : INTEGER;
  BEGIN
    WITH x = stack [SCheck (1, "Load_indirect")] DO
      IF (x.kind = VKind.Direct) THEN
        (* there's no lazy form of MEM(x) *)
        Force ();
      ELSIF (x.kind = VKind.Indirect) THEN
        (* there's no lazy form of MEM(x) *)
        INC (o, x.offset);  x.offset := 0;
        Force ();
      END;

      IF (x.kind = VKind.Stacked) THEN
        <*ASSERT x.offset = 0*>
        <*ASSERT x.bits = NIL*>
        x.kind := VKind.Pointer;
      END;

      <*ASSERT x.kind = VKind.Pointer
            OR x.kind = VKind.Absolute *>

      INC (x.offset, o);
      a := LV_align (x);

      IF (size = s) AND (a MOD align) = 0 THEN
        (* a simple aligned load *)
        SimpleIndirectLoad (x, t);

      ELSIF (size = s) AND (a MOD Target.Byte) = 0 AND Target.Allow_packed_byte_aligned THEN
        (* a byte aligned load, used by packed structures, supported by the processor *)
        SimpleIndirectLoad (x, t);

      ELSIF (size < s) THEN
        Err ("load_indirect size too large");
        Force (); (* to connect the error message with the code *)
        SimpleIndirectLoad (x, t);

      ELSIF (t = Target.Word.cg_type) OR (t = Target.Integer.cg_type)
        OR  (t = Target.Long.cg_type) OR (t = Target.Longint.cg_type) THEN
        base_align := Base_align (x);
        best_type  := FindIntType (t, s, x.offset, base_align);
        best_size  := TargetMap.CG_Size [best_type];
        best_align := TargetMap.CG_Align [best_type];
        bit_offset := x.offset MOD best_align;
        IF (bit_offset = 0) AND (x.bits = NIL) THEN
          (* this is a simple partial word load *)
          SimpleIndirectLoad (x, best_type);
          (** x.type := TargetMap.CG_Base [best_type]; -- nope **)
          IF (s # best_size) THEN
            Force ();
            IF Target.Little_endian
              THEN cg.extract_mn (StackType[t],
                                  Target.SignedType[t], 0, s);
              ELSE cg.extract_mn (StackType[t],
                                  Target.SignedType[t], best_size - s, s);
            END;
          END;
        ELSIF (x.bits = NIL) THEN
          (* partial load with unaligned constant offset *)
          x.offset := x.offset - bit_offset;
          SimpleIndirectLoad (x, best_type);
          Force ();
          IF Target.Little_endian
            THEN cg.extract_mn (StackType[t], Target.SignedType[t],
                                bit_offset, s);
            ELSE cg.extract_mn (StackType[t], Target.SignedType[t],
                                best_size - bit_offset - s, s);
          END;
        ELSE
          (* unaligned, partial load with variable offset *)
          IF (best_align > x.align) THEN Err ("unaligned base variable"); END;

          a := MIN (base_align, TargetMap.CG_Size[t]);
          IF (best_size < a) THEN
            (* make sure we load the largest possible aligned value,
               because we can't tell how far the variable bit-offset
               will take us.  *)
            best_type  := FindIntType (t, MAX (s, a),
                                       x.offset MOD a, base_align);
            best_size  := TargetMap.CG_Size [best_type];
            best_align := TargetMap.CG_Align [best_type];
            bit_offset := x.offset MOD best_align;
          END;

          (* hide the bit offset *)
          save_bits := x.bits;       x.bits := NIL;
          save_temp := x.temp_bits;  x.temp_bits := FALSE;

          (* generate the aligned load *)
          const_bits := x.offset MOD best_align;
          DEC (x.offset, const_bits);
          SimpleIndirectLoad (x, best_type);
          Force ();

          (* compute the full bit offset *)
          IF Target.Little_endian THEN
            cg.load (save_bits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
            IF (const_bits # 0) THEN
              Push_int (const_bits);
              cg.add (Target.Integer.cg_type);
            END;
          ELSE (* big endian *)
            Push_int (best_size - const_bits - s);
            cg.load (save_bits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
            cg.subtract (Target.Integer.cg_type);
          END;

          (* extract the needed bits *)
          cg.extract_n (StackType[t], Target.SignedType[t], s);

          (* restore the hidden bit offset *)
          x.bits := save_bits;
          x.temp_bits := save_temp;
        END;
      ELSE
        (* unaligned non-integer value *)
        Err ("unaligned load_indirect  type="& Fmt.Int (ORD (t))
            & "  s/a=" & Fmt.Int (s) & "/" & Fmt.Int (a));
        Force ();  (* to connect the error message *)
        SimpleIndirectLoad (x, t);
        Force ();
      END;

    END;
  END Load_indirect;

PROCEDURE SimpleIndirectLoad (VAR x: ValRec;  t: Type) =
  VAR offs: INTEGER;
  BEGIN
    IF (x.kind = VKind.Absolute) THEN
      x.kind := VKind.Direct;
      x.type := t;
    ELSIF (x.kind = VKind.Pointer) OR (x.kind = VKind.Stacked) THEN
      offs := x.offset;  x.offset := 0;
      Force ();
      cg.load_indirect (AsBytes (offs), t, StackType[t]);
      x.type  := t;
      x.align := Target.Byte;
      x.kind  := VKind.Stacked;
    ELSE (* ?? *)
      ErrI (ORD (x.kind), "bad mode in SimpleIndirectLoad");
      Force ();
      cg.load_indirect (AsBytes (x.offset), t, StackType[t]);
      x.type  := t;
      x.align := Target.Byte;
      x.kind  := VKind.Stacked;
    END;
  END SimpleIndirectLoad;

PROCEDURE Store (v: Var;  o: Offset;  s: Size;  a: Alignment;  t: Type) =
  VAR
    size  := TargetMap.CG_Size [t];
    align := TargetMap.CG_Align [t];
    best_align : Alignment;
    best_size  : Size;
    best_type  : MType;
  BEGIN
    Force ();  (* materialize the value to be stored *)

    IF (size = s) AND ((a+o) MOD align) = 0 THEN
      (* a simple aligned store *)
      cg.store (v, AsBytes (o), StackType[t], t);
    ELSIF (size < s) THEN
      Err ("store size too large");
      cg.store (v, AsBytes (o), StackType[t], t);
    ELSIF (t = Target.Word.cg_type) OR (t = Target.Integer.cg_type)
      OR  (t = Target.Long.cg_type) OR (t = Target.Longint.cg_type) THEN
      best_type  := FindIntType (t, s, o, a);
      best_size  := TargetMap.CG_Size [best_type];
      best_align := TargetMap.CG_Align [best_type];
      align := (a+o) MOD best_align;
      IF (s = best_size) AND (align = 0) THEN
        (* this is a simple partial word store *)
        cg.store (v, AsBytes (o), StackType[t], best_type);
      ELSE
        (* unaligned, partial store *)
        cg.load (v, AsBytes (o - align), best_type, StackType[t]);
        cg.swap (t, t);
        IF Target.Little_endian
          THEN cg.insert_mn (StackType[t], align, s);
          ELSE cg.insert_mn (StackType[t], best_size - align - s, s);
        END;
        cg.store (v, AsBytes (o - align), StackType[t], best_type);
      END;
    ELSE
      (* unaligned non-integer value *)
      Err ("unaligned store  type="& Fmt.Int (ORD (t))
            & "  s/o/a=" & Fmt.Int (s) & "/" & Fmt.Int (o) & "/" & Fmt.Int(a));
      cg.store (v, ToBytes (o), Target.Integer.cg_type, t);
    END;
    SPop (1, "Store");
  END Store;

PROCEDURE Store_int (t: IType;  v: Var;  o: Offset := 0) =
  BEGIN
    Store (v, o, TargetMap.CG_Size[t], TargetMap.CG_Align[t], t);
  END Store_int;

PROCEDURE Store_addr (v: Var;  o: Offset := 0) =
  BEGIN
    Store (v, o, Target.Address.size, Target.Address.align, Type.Addr);
  END Store_addr;

PROCEDURE Store_indirect (t: Type;  o: Offset;  s: Size) =
  VAR
    size  := TargetMap.CG_Size [t];
    align := TargetMap.CG_Align [t];
    best_align : Alignment;
    best_size  : Size;
    best_type  : MType;
    a: INTEGER;
    tmp: Val;
    base_align: INTEGER;
    save_bits : Var     := NIL;
    save_temp : BOOLEAN := FALSE;
    const_bits: INTEGER := 0;
  BEGIN
    Force (); (* materialize the value to be stored *)

    WITH x = stack [SCheck (2, "Store_indirect-x")],
         y = stack [SCheck (1, "Store_indirect-y")] DO

      (* normalize the address and the value *)
      IF (x.kind = VKind.Stacked) THEN
        <*ASSERT x.offset = 0*>
        <*ASSERT x.bits = NIL*>
        const_bits := o MOD x.align;
        x.offset := o - const_bits;
        x.kind := VKind.Pointer;

        Force (); (* the rhs *)

      ELSIF (x.kind = VKind.Pointer) THEN
        (* save the bit offset *)
        save_bits := x.bits;  x.bits := NIL;
        save_temp := x.temp_bits;  x.temp_bits := FALSE;
        const_bits := (x.offset + o) MOD x.align;
        x.offset := x.offset + o - const_bits;

        Force (); (* the rhs *)

      ELSIF (x.kind = VKind.Direct) THEN
        EVAL Force_pair (commute := FALSE);  (* force both sides *)

        const_bits := o MOD x.align;
        x.offset := o - const_bits;
        x.kind := VKind.Pointer;

      ELSIF (x.kind = VKind.Absolute) THEN
        (* save the bit offset *)
        save_bits := x.bits;  x.bits := NIL;
        save_temp := x.temp_bits;  x.temp_bits := FALSE;
        const_bits := (x.offset + o) MOD x.align;
        x.offset := x.offset + o - const_bits;
(***
        INC (x.offset, o);
***)
        Force (); (* the rhs *)

      ELSIF (x.kind = VKind.Indirect) THEN
        (* save the bit offset *)
        save_bits := x.bits;  x.bits := NIL;
        save_temp := x.temp_bits;  x.temp_bits := FALSE;
        const_bits := (x.offset + o) MOD x.align;
        x.offset := x.offset + o - const_bits;

        EVAL Force_pair (commute := FALSE); (* both sides *)
        x.kind := VKind.Pointer;
      END;

      <*ASSERT x.kind = VKind.Pointer
            OR x.kind = VKind.Absolute *>

      (* restore the bit offset *)
      x.bits := save_bits;
      x.temp_bits := save_temp;
      INC (x.offset, const_bits);

      a := LV_align (x);

      IF (size = s) AND (a MOD align) = 0 THEN
        (* a simple aligned store *)
        SimpleIndirectStore (x, t);
      ELSIF (size = s) AND (a MOD Target.Byte) = 0 AND Target.Allow_packed_byte_aligned THEN
        (* a byte aligned store, used by packed structures, supported by the processor *)
        SimpleIndirectStore (x, t);
      ELSIF (size < s) THEN
        Err ("store_indirect size too large");
        SimpleIndirectStore (x, t);
      ELSIF (t = Target.Word.cg_type) OR (t = Target.Integer.cg_type)
        OR  (t = Target.Long.cg_type) OR (t = Target.Longint.cg_type) THEN
        base_align := Base_align (x);
        best_type  := FindIntType (t, s, x.offset, base_align);
        best_size  := TargetMap.CG_Size [best_type];
        best_align := TargetMap.CG_Align [best_type];
        const_bits := x.offset MOD best_align;
        IF (const_bits = 0) AND (s = best_size) AND (x.bits = NIL) THEN
          (* this is a simple partial word store *)
          SimpleIndirectStore (x, best_type);
        ELSIF (const_bits = 0) AND (x.bits = NIL) THEN
          (* this is an aligned, partial word store *)
          Swap ();
          tmp := Pop ();
          Push (tmp);  XForce ();
          SimpleIndirectLoad (stack [SCheck (1,"Store_indirect-3")],best_type);
          Swap ();
          EVAL Force_pair (commute := FALSE);
          IF Target.Little_endian
            THEN cg.insert_mn (StackType[t], 0, s);
            ELSE cg.insert_mn (StackType[t], best_size - s, s);
          END;
          SPop (1, "Store_indirect #1");
          Push (tmp);  XForce ();
          Swap ();
          SimpleIndirectStore (x, best_type);
          Free (tmp);
        ELSIF (x.bits = NIL) THEN
          (* partial store with unaligned constant offset *)
          x.offset := x.offset DIV best_align * best_align;
          Swap ();
          tmp := Pop ();
          Push (tmp);  XForce ();
          SimpleIndirectLoad (stack [SCheck (1, "Store_indirect-4")], best_type);
          Swap ();
          EVAL Force_pair (commute := FALSE);
          IF Target.Little_endian
            THEN cg.insert_mn (StackType[t], const_bits, s);
            ELSE cg.insert_mn (StackType[t], best_size - const_bits - s, s);
          END;
          SPop (1, "Store_indirect #2");
          Push (tmp);  XForce ();
          Swap ();
          SimpleIndirectStore (x, best_type);
          Free (tmp);
        ELSE
          (* unaligned, partial store with variable offset *)
          IF (best_align > x.align) THEN
            Err ("unaligned base variable in store");
          END;

          a := MIN (base_align, TargetMap.CG_Size[t]);
          IF (best_size < a) THEN
            (* make sure we load and store the largest possible aligned value,
               because we can't tell how far the variable bit-offset
               will take us.  *)
            best_type  := FindIntType (t, MAX (s, a),
                                       x.offset MOD a, base_align);
            best_size  := TargetMap.CG_Size [best_type];
            best_align := TargetMap.CG_Align [best_type];
            const_bits := x.offset MOD best_align;
          END;

          (* hide the bit offset *)
          save_bits := x.bits;       x.bits := NIL;
          save_temp := x.temp_bits;  x.temp_bits := FALSE;

          (* generate the aligned load *)
          const_bits := x.offset MOD best_align;
          DEC (x.offset, const_bits);
          Swap ();
          tmp := Pop ();
          Push (tmp);  Force ();
          SimpleIndirectLoad (y, best_type);
          Force ();

          (* stuff the bits *)
          Swap ();
          IF Target.Little_endian THEN
            cg.load (save_bits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
            IF (const_bits # 0) THEN
              Push_int (const_bits);
              cg.add (Target.Integer.cg_type);
            END;
          ELSE (* big endian *)
            Push_int (best_size - const_bits - s);
            cg.load (save_bits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
            cg.subtract (Target.Integer.cg_type);
          END;
          cg.insert_n (StackType[t], s);
          SPop (1, "Store_indirect #3");

          (* finally, store the result *)
          Push (tmp);  Force ();
          Swap ();
          SimpleIndirectStore (x, best_type);

          Free (tmp);
        END;
      ELSE
        (* unaligned non-integer value *)
        Err ("unaligned store_indirect  type="& Fmt.Int (ORD (t))
            & "  s/a=" & Fmt.Int (s) & "/" & Fmt.Int (a));
        SimpleIndirectStore (x, t);
      END;

    END;
    SPop (2, "Store_indirect");
  END Store_indirect;

PROCEDURE SimpleIndirectStore (READONLY x: ValRec;  t: MType)=
  BEGIN
    IF (x.kind = VKind.Absolute) THEN
      cg.store (x.base, AsBytes (x.offset), StackType [t], t);
    ELSIF (x.kind = VKind.Pointer) OR (x.kind = VKind.Stacked) THEN
      cg.store_indirect (AsBytes (x.offset), StackType [t], t);
    ELSE (* ?? *)
      ErrI (ORD (x.kind), "bad mode in SimpleIndirectStore");
      cg.store_indirect (AsBytes (x.offset), StackType[t], t);
    END;
  END SimpleIndirectStore;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE Load_nil () =
  BEGIN
    SPush (Type.Addr);
    cg.load_nil ();
    stack [tos-1].align := Target.Address.align;
  END Load_nil;

PROCEDURE Load_byte_address (x: INTEGER) =
  BEGIN
    SPush (Type.Addr);
    cg.load_nil ();
    cg.add_offset (x);
    stack [tos-1].align := Target.Byte;
  END Load_byte_address;

PROCEDURE Load_intt (i: INTEGER) =
  VAR val: Target.Int;  b := TInt.FromInt (i, val);
  BEGIN
    IF NOT b
      OR TInt.LT (val, Target.Integer.min)
      OR TInt.LT (Target.Integer.max, val)
    THEN ErrI (i, "integer not representable") END;
    Load_integer (Target.Integer.cg_type, val);
  END Load_intt;

PROCEDURE Load_integer (t: IType;  READONLY i: Target.Int) =
  BEGIN
    SPush (t);
    WITH x = stack[tos-1] DO
      x.kind := VKind.Integer;
      x.int  := i;
    END;
  END Load_integer;

PROCEDURE Load_float (READONLY f: Target.Float) =
  VAR t := TargetMap.Float_types [TFloat.Prec (f)].cg_type;
  BEGIN
    SPush (t);
    WITH x = stack[tos-1] DO
      x.kind  := VKind.Float;
      x.float := f;
    END;
  END Load_float;

(*------------------------------------------------------------ arithmetic ---*)
   
PROCEDURE Compare (t: ZType;  op: Cmp) =
  BEGIN
    IF Force_pair (commute := TRUE) THEN
      op := M3CG.SwappedCompare [op];
    END;
    cg.compare (t, Target.Integer.cg_type, op);
    SPop (2, "Compare");
    SPush (Type.Int32);
  END Compare;

PROCEDURE Add (t: AType) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    cg.add (t);
    SPop (2, "Add");
    SPush (t);
  END Add;

PROCEDURE Subtract (t: AType) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.subtract (t);
    SPop (2, "Subtract");
    SPush (t);
  END Subtract;

PROCEDURE Multiply (t: AType) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    cg.multiply (t);
    SPop (2, "Multiply");
    SPush (t);
  END Multiply;

PROCEDURE Divide (t: RType) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.divide (t);
    SPop (2, "Divide");
    SPush (t);
  END Divide;

PROCEDURE Negate (t: AType) =
  BEGIN
    Force ();
    cg.negate (t);
    SPop (1, "Negate");
    SPush (t);
  END Negate;

PROCEDURE Abs (t: AType) =
  BEGIN
    Force ();
    cg.abs (t);
    SPop (1, "Abs");
    SPush (t);
  END Abs;

PROCEDURE Max (t: ZType) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    cg.max (t);
    SPop (2, "Max");
    SPush (t);
  END Max;

PROCEDURE Min (t: ZType) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    cg.min (t);
    SPop (2, "Min");
    SPush (t);
  END Min;

PROCEDURE Cvt_int (t: RType;  u: IType;  op: Cvt) =
  BEGIN
    Force ();
    cg.cvt_int (t, u, op);
    SPop (1, "Cvt_int");
    SPush (u);
  END Cvt_int;

PROCEDURE Cvt_float (t: AType;  u: RType) =
  BEGIN
    Force ();
    cg.cvt_float (t, u);
    SPop (1, "Cvt_float");
    SPush (u);
  END Cvt_float;

PROCEDURE Div (t: IType;  a, b: Sign) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.div (t, a, b);
    SPop (2, "Div");
    SPush (t);
  END Div;

PROCEDURE Mod (t: IType;  a, b: Sign) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.mod (t, a, b);
    SPop (2, "Mod");
    SPush (t);
  END Mod;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE Set_union (s: Size) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    IF (s <= Target.Integer.size) THEN
      cg.or (Target.Integer.cg_type);
      SPop (1, "Set_union");
    ELSE
      cg.set_union (AsBytes (s));
      SPop (3, "Set_union");
    END;
  END Set_union;

PROCEDURE Set_difference (s: Size) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    IF (s <= Target.Integer.size) THEN
      cg.not (Target.Integer.cg_type);
      cg.and (Target.Integer.cg_type);
      SPop (1, "Set_diff");
    ELSE
      cg.set_difference (AsBytes (s));
      SPop (3, "Set_diff");
    END;
  END Set_difference;

PROCEDURE Set_intersection (s: Size) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    IF (s <= Target.Integer.size) THEN
      cg.and (Target.Integer.cg_type);
      SPop (1, "Set_inter");
    ELSE
      cg.set_intersection (AsBytes (s));
      SPop (3, "Set_inter");
    END;
  END Set_intersection;

PROCEDURE Set_sym_difference (s: Size) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    IF (s <= Target.Integer.size) THEN
      cg.xor (Target.Integer.cg_type);
      SPop (1, "Set_symd");
    ELSE
      cg.set_sym_difference (AsBytes (s));
      SPop (3, "Set_symd");
    END;
  END Set_sym_difference;

PROCEDURE Set_member (s: Size) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    IF (s <= Target.Integer.size) THEN
      cg.load_integer (Target.Integer.cg_type, TInt.One);
      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type);
      cg.shift_left (Target.Integer.cg_type);
      cg.and (Target.Integer.cg_type);
      cg.load_integer (Target.Integer.cg_type, TInt.Zero);
      cg.compare (Target.Word.cg_type, Target.Integer.cg_type, Cmp.NE);
    ELSE
      cg.set_member (AsBytes (s), Target.Integer.cg_type);
    END;
    SPop (2, "Set_member");
    SPush (Target.Integer.cg_type);
  END Set_member;

PROCEDURE Set_compare (s: Size;  op: Cmp) =
  VAR a: Val := NIL;
      b: Val := NIL;
  BEGIN

    (* a op b => BOOLEAN *)

    (* Comparison is commutative in that the comparison can be reversed
    if it is profitable to reverse the parameter order. *)

    IF Force_pair (commute := TRUE) THEN
      op := M3CG.SwappedCompare [op];
    END;

    IF (s <= Target.Integer.size) THEN

      (* The set fits in an integer, so handle things inline with integer operations
      NOTE that for the sake of code size, we should perhaps implement these with functions. *)

      IF (op = Cmp.EQ) OR (op = Cmp.NE) THEN

        Compare (Target.Word.cg_type, op);

      ELSE

        (* Set a is less than or equal to set b, if all of set a's members are in set b.
        (a <= b) = ((a & b) = a)
        (a <  b) = (a <= b AND a # b)
        (b >  a) = (a < b)
        *)

        IF (op = Cmp.GT) OR (op = Cmp.GE) THEN
          a := Pop ();
          b := Pop ();
        ELSE
          b := Pop ();
          a := Pop ();
        END;

        Push (a);
        Push (b);
        And (Target.Word.cg_type);

        Push (a);
        Compare (Target.Word.cg_type, Cmp.EQ);

        (* NOTE that short circuiting for < and > is probably desirable, if one
        knows how to set up the labels and branches. *)

        IF (op = Cmp.LT) OR (op = Cmp.GT) THEN
          Push (b);
          Push (a);
          Compare (Target.Word.cg_type, Cmp.EQ);
          And (Target.Integer.cg_type);
        END;

        Free (a);
        Free (b);
      END;
    ELSE
      cg.set_compare (AsBytes (s), op, Target.Integer.cg_type);
      SPop (2, "Set_compare");
      SPush (Type.Int32);
    END;
  END Set_compare;

PROCEDURE Set_range (s: Size) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    IF (s <= Target.Integer.size) THEN
      (* given x, a, b:  compute  x || {a..b} *)

      cg.load_integer (Target.Integer.cg_type, TInt.MOne);
        (* -1 = 16_ffffff = {0..N} *)
      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type);
      Push_int (Target.Integer.size-1);
      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type);
      cg.subtract (Target.Integer.cg_type);
      cg.shift_right (Target.Integer.cg_type);                  (*  x, a, {0..b} *)

      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type); (*  x, {0..b}, a *)

      cg.load_integer (Target.Integer.cg_type, TInt.MOne);
      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type);
      cg.shift_left (Target.Integer.cg_type);           (*  x, {0..b}, {a..N} *)

      cg.and (Target.Integer.cg_type);                  (*  x, {a..b} *)
      cg.or (Target.Integer.cg_type);                   (*  x || {a..b} *)
      SPop (3, "Set_range-a");
      SPush (Target.Integer.cg_type);
    ELSE
      cg.set_range (AsBytes (s), Target.Integer.cg_type);
      SPop (3, "Set_range-b");
    END;
  END Set_range;

PROCEDURE Set_singleton (s: Size) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    IF (s <= Target.Integer.size) THEN
      cg.load_integer (Target.Integer.cg_type, TInt.One);
      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type);
      cg.shift_left (Target.Integer.cg_type);
      cg.or (Target.Integer.cg_type);
      SPop (2, "Set_single-b");
      SPush (Target.Integer.cg_type);
    ELSE
      cg.set_singleton (AsBytes (s), Target.Integer.cg_type);
      SPop (2, "Set_single-b");
    END;
  END Set_singleton;

(*------------------------------------------ Word.T/Long.T bit operations ---*)

PROCEDURE Not (t: IType) =
  BEGIN
    Force ();
    cg.not (t);
    SPop (1, "Not");
    SPush (t);
  END Not;

PROCEDURE And (t: IType) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    cg.and (t);
    SPop (2, "And");
    SPush (t);
  END And;

PROCEDURE Or (t: IType) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    cg.or (t);
    SPop (2, "Or");
    SPush (t);
  END Or;

PROCEDURE Xor (t: IType) =
  BEGIN
    EVAL Force_pair (commute := TRUE);
    cg.xor (t);
    SPop (2, "Xor");
    SPush (t);
  END Xor;

PROCEDURE Shift (t: IType) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.shift (t);
    SPop (2, "Shift");
    SPush (t);
  END Shift;

PROCEDURE Shift_left (t: IType) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.shift_left (t);
    SPop (2, "Shift_left");
    SPush (t);
  END Shift_left;

PROCEDURE Shift_right (t: IType) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.shift_right (t);
    SPop (2, "Shift_right");
    SPush (t);
  END Shift_right;

PROCEDURE Rotate (t: IType) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.rotate (t);
    SPop (2, "Rotate");
    SPush (t);
  END Rotate;

PROCEDURE Rotate_left (t: IType) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.rotate_left (t);
    SPop (2, "Rotate_left");
    SPush (t);
  END Rotate_left;

PROCEDURE Rotate_right (t: IType) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.rotate_right (t);
    SPop (2, "Rotate_right");
    SPush (t);
  END Rotate_right;

PROCEDURE Extract (t: IType; sign: BOOLEAN) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.extract (t, sign);
    SPop (3, "Extract");
    SPush (t);
  END Extract;

PROCEDURE Extract_n (t: IType; sign: BOOLEAN;  n: INTEGER) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.extract_n (t, sign, n);
    SPop (2, "Extract_n");
    SPush (t);
  END Extract_n;

PROCEDURE Extract_mn (t: IType; sign: BOOLEAN;  m, n: INTEGER) =
  BEGIN
    Force ();
    cg.extract_mn (t, sign, m, n);
    SPop (1, "Extract_mn");
    SPush (t);
  END Extract_mn;

PROCEDURE Insert (t: IType) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.insert (t);
    SPop (4, "Insert");
    SPush (t);
  END Insert;

PROCEDURE Insert_n (t: IType; n: INTEGER) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.insert_n (t, n);
    SPop (3, "Insert_n");
    SPush (t);
  END Insert_n;

PROCEDURE Insert_mn (t: IType; m, n: INTEGER) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.insert_mn (t, m, n);
    SPop (2, "Insert_mn");
    SPush (t);
  END Insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE Swap () =
  VAR tmp: ValRec;
  BEGIN
    WITH xa = stack [SCheck (2, "Swap-a")],
         xb = stack [SCheck (1, "Swap-b")] DO

      (* exchange the underlying values *)
      IF ((xa.kind = VKind.Stacked) OR (xa.kind = VKind.Pointer))
        AND ((xb.kind = VKind.Stacked) OR (xb.kind = VKind.Pointer)) THEN
        (* both values are on the stack => must swap *)
        cg.swap (xa.type, xb.type);
      END;

      (* exchange the local copies *)
      tmp := xa;  xa := xb;  xb := tmp;
    END;
  END Swap;

PROCEDURE Discard (t: Type) =
  BEGIN
    SPop (1, "Discard");
    WITH x = stack [SCheck (0, "Pop")] DO
      IF (x.kind = VKind.Stacked) OR (x.kind = VKind.Pointer) THEN
        cg.pop (t);
      END;
      Release_temps (x);
    END;
  END Discard;

PROCEDURE Copy_n (s: Size;  overlap: BOOLEAN) =
  VAR t: MType;  z: Size;  a := MIN (SLV_align (2), SLV_align (3));
  BEGIN
    EVAL Force_pair (commute := FALSE);
    IF (a < Target.Byte) THEN ErrI (a, "unaligned copy_n") END;

    (* convert the count into a multiple of a machine type's size *)
    IF (s = Target.Byte) THEN
      t := AlignedType (s, Target.Byte);
      z := TargetMap.CG_Size [t];
      <*ASSERT z = Target.Byte*>
    ELSIF (s < Target.Byte) THEN
      IF (Target.Byte MOD s) # 0 THEN ErrI (s, "impossible copy_n size") END;
      t := AlignedType (s, Target.Byte);
      z := TargetMap.CG_Size [t];
      <*ASSERT z = Target.Byte*>
      Push_int (Target.Byte DIV s);
      cg.div (Target.Integer.cg_type, Sign.Positive, Sign.Positive);
    ELSE (* s > Target.Byte *)
      IF (s MOD Target.Byte) # 0 THEN ErrI (s, "impossible copy_n size") END;
      t := AlignedType (s, a);
      z := TargetMap.CG_Size [t];
      IF (z < s) THEN
        IF (s MOD z) # 0 THEN ErrI (s, "impossible copy_n size") END;
        Push_int (s DIV z);
        cg.multiply (Target.Integer.cg_type);
      END;
    END;

    cg.copy_n (Target.Integer.cg_type, t, overlap);
    SPop (3, "Copy_n");
  END Copy_n;

PROCEDURE Copy (s: Size;  overlap: BOOLEAN) =
  VAR
    a := MIN (SLV_align (2), SLV_align (1));
    t := AlignedType (s, a);
    z := TargetMap.CG_Size [t];
  BEGIN
    EVAL Force_pair (commute := FALSE);
    IF (s MOD z) # 0 THEN ErrI (s, "impossible copy size") END;
    cg.copy (s DIV z, t, overlap);
    SPop (2, "Copy");
  END Copy;

PROCEDURE Zero (s: Size) =
  VAR
    a := SLV_align (1);
    t := AlignedType (s, a);
    z := TargetMap.CG_Size [t];
  BEGIN
    Force ();
    IF (s MOD z) # 0 THEN ErrI (s, "impossible zero size") END;
    cg.zero (s DIV z, t);
    SPop (1, "Zero");
  END Zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE Loophole (from, to: Type) =
  BEGIN
    Force ();
    cg.loophole (from, to);
    SPop (1, "Loophole");
    SPush (to);
  END Loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE Abort (code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    cg.abort (code);
  END Abort;

PROCEDURE Check_nil (code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    Force ();
    cg.check_nil (code);
  END Check_nil;

PROCEDURE Check_lo (t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    Force ();
    cg.check_lo (t, i, code);
  END Check_lo;

PROCEDURE Check_hi (t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    Force ();
    cg.check_hi (t, i, code);
  END Check_hi;

PROCEDURE Check_range (t: IType;  READONLY a, b: Target.Int;
                       code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    Force ();
    cg.check_range (t, a, b, code);
  END Check_range;

PROCEDURE Check_index (code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    EVAL Force_pair (commute := FALSE);
    cg.check_index (Target.Integer.cg_type, code);
    SPop (1, "Check_index");
  END Check_index;

PROCEDURE Check_eq (t: IType;  code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    EVAL Force_pair (commute := TRUE);
    cg.check_eq (t, code);
    SPop (2, "Check_eq");
  END Check_eq;

PROCEDURE Check_byte_aligned () =
  VAR extra_bits: Var;  extra_is_temp: BOOLEAN;
  BEGIN
    WITH x = stack [SCheck (1, "Check_byte_aligned")] DO
      IF (x.align MOD Target.Byte) # 0 THEN
        Err ("unaligned base variable");
      ELSIF (x.offset MOD Target.Byte) # 0 THEN
        Err ("address's offset is not byte aligned");
      ELSIF (x.bits # NIL) THEN
        extra_bits := x.bits;   extra_is_temp := x.temp_bits;
        x.bits := NIL;          x.temp_bits := FALSE;
        EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
        cg.load (extra_bits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
        Push_int (Target.Byte - 1);  (*** Push_int (Target.Byte); ***)
        cg.and (Target.Integer.cg_type);
          (*** cg.mod (Target.Integer.cg_type, Sign.Unknown, Sign.Positive); ***)
        cg.load_integer (Target.Integer.cg_type, TInt.Zero);
        cg.check_eq (Target.Integer.cg_type, RuntimeError.UnalignedAddress);
        Boost_alignment (Target.Byte);
        Force ();
        cg.load (extra_bits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
        Push_int (Target.Byte);
        cg.div (Target.Integer.cg_type, Sign.Unknown, Sign.Positive);
        cg.index_address (Target.Integer.cg_type, 1);
        IF (extra_is_temp) THEN Free_temp (extra_bits); END;
      END;
    END;
  END Check_byte_aligned;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE Add_offset (i: INTEGER) =
  BEGIN
    WITH x = stack [SCheck (1, "Add_offset")] DO
      IF (x.type # Type.Addr) THEN
        Err ("add_offset on non-address");
        Force ();
      ELSIF (x.kind = VKind.Stacked) THEN
        x.kind := VKind.Pointer;
        x.offset := i;
      ELSIF (x.kind = VKind.Direct) THEN
        Force ();
        x.kind   := VKind.Pointer;
        x.offset := i;
      ELSIF (x.kind = VKind.Absolute) THEN
        INC (x.offset, i);
      ELSIF (x.kind = VKind.Indirect) THEN
        INC (x.offset, i);
      ELSIF (x.kind = VKind.Pointer) THEN
        INC (x.offset, i);
      ELSE
        Err ("add_offset on non-address form");
        Force ();
      END;
    END;
  END Add_offset;

PROCEDURE Index_bytes (size: INTEGER) =
  VAR align := SLV_align (2);
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.index_address (Target.Integer.cg_type, AsBytes (size));
    SPop (2, "Index_bytes");
    SPush (Type.Addr);
    stack [SCheck (1, "Index_bytes")].align := GCD (align, size);
  END Index_bytes;

PROCEDURE Index_bits () =
  VAR index := Pop_temp ();
  BEGIN
    WITH x = stack [SCheck (1, "Index_address")] DO
      IF (x.bits # NIL) THEN  Err ("index_bits applied twice");  END;
      IF (x.kind = VKind.Stacked) THEN x.kind := VKind.Pointer; END;
      x.bits := index.base;
      x.temp_bits := TRUE;
    END;
    (*** SPop (1, "Index_address"); ***)
  END Index_bits;

PROCEDURE Boost_alignment (a: Alignment) =
  BEGIN
    WITH x = stack [SCheck (1, "Boost_alignment")] DO
      x.align := MAX (x.align, a);
    END;
  END Boost_alignment;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE Start_call_direct (proc: Proc;  lev: INTEGER;  t: Type) =
  BEGIN
    SEmpty ("Start_call_direct");
    cg.start_call_direct (proc, lev, t);
  END Start_call_direct;

PROCEDURE Call_direct (p: Proc;  t: Type) =
  BEGIN
    SEmpty ("Call_direct");
    cg.call_direct (p, t);
    PushResult (t);
  END Call_direct;

PROCEDURE Start_call_indirect (t: Type;  cc: CallingConvention) =
  BEGIN
    SEmpty ("Start_call_indirect");
    cg.start_call_indirect (t, cc);
  END Start_call_indirect;

PROCEDURE Gen_Call_indirect (t: Type;  cc: CallingConvention) =
  BEGIN
    IF Host.doProcChk THEN Check_nil (RuntimeError.BadMemoryReference); END;
    Force ();
    cg.call_indirect (t, cc);
    SPop (1, "Call_indirect");
    SEmpty ("Call_indirect");
    PushResult (t);
  END Gen_Call_indirect;

PROCEDURE PushResult (t: Type) =
  BEGIN
    IF (t # Type.Void) THEN  SPush (t)  END;
  END PushResult;

PROCEDURE Pop_param (t: Type) =
  BEGIN
    Force ();
    cg.pop_param (t);
    SPop (1, "Pop_param");
    SEmpty ("Pop_param");
  END Pop_param;

PROCEDURE Pop_struct (t: TypeUID;  s: Size;  a: Alignment) =
  BEGIN
    Force ();
    cg.pop_struct (t, ToBytes (s), FixAlign (a));
    SPop (1, "Pop_struct");
    SEmpty ("Pop_struct");
  END Pop_struct;

PROCEDURE Pop_static_link () =
  BEGIN
    Force ();
    cg.pop_static_link ();
    SPop (1, "Pop_static_link");
  END Pop_static_link;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE Load_procedure (p: Proc) =
  BEGIN
    cg.load_procedure (p);
    SPush (Type.Addr);
  END Load_procedure;

PROCEDURE Load_static_link (p: Proc) =
  BEGIN
    cg.load_static_link (p);
    SPush (Type.Addr);
  END Load_static_link;

(*------------------------------------------------ builtin type operations --*)

PROCEDURE Ref_to_hdr () =
  BEGIN
    Boost_alignment (Target.Address.align);
    Load_indirect (Target.Integer.cg_type, -Target.Address.pack, Target.Address.size);
  END Ref_to_hdr;

PROCEDURE Hdr_to_info (offset, size: INTEGER) =
  VAR base: INTEGER;
  BEGIN
    Force ();
    IF Target.Little_endian
      THEN base := offset;
      ELSE base := Target.Integer.size - offset - size;
    END;
    cg.extract_mn (Target.Integer.cg_type, FALSE, base, size);
  END Hdr_to_info;

PROCEDURE Ref_to_info (offset, size: INTEGER) =
  BEGIN
    Ref_to_hdr ();
    Hdr_to_info (offset, size);
  END Ref_to_info;

(*------------------------------------------------------------ open arrays --*)

PROCEDURE Open_elt_ptr (a: Alignment) =
  BEGIN
    Boost_alignment (Target.Address.align);
    Load_indirect (Type.Addr, M3RT.OA_elt_ptr, Target.Address.size);
    (*** Boost_alignment (a); ***)
    WITH x = stack [SCheck (1, "Open_elt_ptr")] DO
      x.align := a;
    END;
  END Open_elt_ptr;

PROCEDURE Open_size (n: INTEGER) =
  BEGIN
    Boost_alignment (Target.Address.align);
    Load_indirect (Target.Integer.cg_type, M3RT.OA_sizes + n * Target.Integer.pack,
                   Target.Integer.size);
  END Open_size;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE If_closure (proc: Val;  true, false: Label;  freq: Frequency) =
  VAR skip := Next_label ();  nope := skip;
  BEGIN
    IF (false # No_label) THEN  nope := false; END;
    IF NOT Target.Aligned_procedures THEN
      Push (proc);
      Force ();
      cg.loophole (Type.Addr, Target.Integer.cg_type);
      Push_int (TargetMap.CG_Align_bytes[Target.Integer.cg_type] - 1);
      cg.and (Target.Integer.cg_type);
      cg.load_integer (Target.Integer.cg_type, TInt.Zero);
      cg.if_compare (Target.Integer.cg_type, Cmp.NE, nope, Always - freq);
      SPop (1, "If_closure-unaligned");
    END;
    Push (proc);
    Boost_alignment (Target.Address.align);
    Force ();
    cg.load_nil ();
    cg.if_compare (Type.Addr, Cmp.EQ, nope, Always - freq);
    Push (proc);
    Boost_alignment (Target.Integer.align);
    Load_indirect (Target.Integer.cg_type, M3RT.CL_marker, Target.Integer.size);
    Push_int (M3RT.CL_marker_value);
    IF (true # No_label)
      THEN cg.if_compare (Target.Integer.cg_type, Cmp.EQ, true, freq);
      ELSE cg.if_compare (Target.Integer.cg_type, Cmp.NE, false, freq);
    END;
    Set_label (skip);
    SPop (2, "If_closure");
  END If_closure;

PROCEDURE Closure_proc () =
  BEGIN
    Boost_alignment (Target.Address.align);
    Load_indirect (Type.Addr, M3RT.CL_proc, Target.Address.size);
  END Closure_proc;

PROCEDURE Closure_frame () =
  BEGIN
    Boost_alignment (Target.Address.align);
    Load_indirect (Type.Addr, M3RT.CL_frame, Target.Address.size);
  END Closure_frame;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE Comment (o: INTEGER;  is_const: BOOLEAN;  a, b, c, d: TEXT := NIL) =
  BEGIN
    IF (o < 0) THEN
      cg.comment (a, b, c, d);
    ELSE
      PushPending (NEW (CommentNode, o := o-1, a:=a, b:=b, c:=c, d:=d), is_const);
    END;
  END Comment;

PROCEDURE DumpComment (x: CommentNode) =
  BEGIN
    DumpNode (x);
    cg.comment (x.a, x.b, x.c, x.d);
  END DumpComment;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE Store_ordered (t: MType;  order: MemoryOrder) =
  BEGIN
    EVAL Force2 ("Store_ordered", commute := FALSE);
    cg.store_ordered (StackType[t], t, order);
  END Store_ordered;

PROCEDURE Load_ordered (t: MType;  order: MemoryOrder) =
  BEGIN
    Force1 ("Load_ordered");
    cg.load_ordered (t, StackType[t], order);
    SPush (StackType[t]);
  END Load_ordered;

PROCEDURE Exchange (t: MType;  order: MemoryOrder) =
  BEGIN
    EVAL Force2 ("Exchange", commute := FALSE);
    cg.exchange (t, StackType[t], order);
    SPush (StackType[t]);
  END Exchange;

PROCEDURE Compare_exchange (t: MType;  success, failure: MemoryOrder) =
  BEGIN
    EVAL Force_pair (commute := FALSE);
    cg.compare_exchange (t, StackType[t],
                         Target.Integer.cg_type, success, failure);
    SPop (3, "Compare_exchange");
    SPush (Type.Int32);
  END Compare_exchange;

PROCEDURE Fence (order: MemoryOrder) =
  BEGIN
    cg.fence (order);
  END Fence;

PROCEDURE Fetch_and_op (op: AtomicOp;  t: MType;  order: MemoryOrder) =
  BEGIN
    EVAL Force2 ("Fetch_and_op", commute := FALSE);
    cg.fetch_and_op (op, t, StackType[t], order);
    SPush (StackType[t]);
  END Fetch_and_op;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE FixAlign (a: Alignment): Alignment =
  BEGIN
    RETURN MAX (a, Target.Byte) DIV Target.Byte;
  END FixAlign;

PROCEDURE AlignedType (s: Size;  a: Alignment): MType =
  BEGIN
    IF IsAlignedMultiple (s, a, Target.Integer) THEN
      RETURN Target.Integer.cg_type;
    END;
    IF (Target.Int64.size <= Target.Integer.size)
       AND IsAlignedMultiple (s, a, Target.Int64) THEN
      RETURN Type.Int64;
    END;
    IF IsAlignedMultiple (s, a, Target.Int32)   THEN RETURN Type.Int32; END;
    IF IsAlignedMultiple (s, a, Target.Int16)   THEN RETURN Type.Int16; END;
    IF IsAlignedMultiple (s, a, Target.Int8)    THEN RETURN Type.Int8;  END;
    Err ("unaligned copy or zero:  s/a=" & Fmt.Int (s) & "/" & Fmt.Int (a));
    RETURN Target.Integer.cg_type;
  END AlignedType;

PROCEDURE IsAlignedMultiple (s: Size;  a: Alignment;
                             READONLY t: Target.Int_type): BOOLEAN =
  BEGIN
    RETURN (s MOD t.size = 0)
       AND ((a = t.align) OR (a MOD t.align = 0));
  END IsAlignedMultiple;

PROCEDURE ToVarSize (n: INTEGER;  a: Alignment): INTEGER =
  VAR n_bytes := (n + Target.Byte - 1) DIV Target.Byte;
      align   := FixAlign (a);
  BEGIN
    RETURN (n_bytes + align - 1) DIV align * align;
  END ToVarSize;

PROCEDURE ToBytes (n: INTEGER): INTEGER =
  BEGIN
    RETURN  (n + Target.Byte - 1) DIV Target.Byte;
  END ToBytes;

PROCEDURE AsBytes (n: INTEGER): INTEGER =
  VAR x := n DIV Target.Byte;
  BEGIN
    IF (x * Target.Byte # n) THEN ErrI (n, "unaligned offset") END;
    RETURN  x;
  END AsBytes;

PROCEDURE Push_int (i: INTEGER) =
  VAR val: Target.Int;  b := TInt.FromInt (i, val);
  BEGIN
    IF NOT b
      OR TInt.LT (val, Target.Integer.min)
      OR TInt.LT (Target.Integer.max, val)
    THEN ErrI (i, "integer not representable") END;
    cg.load_integer (Target.Integer.cg_type, val);
  END Push_int;

PROCEDURE Force_pair (commute: BOOLEAN): BOOLEAN =
  (* Returns TRUE if the items are stacked in the wrong order *)
  VAR s1 := stack [SCheck (1, "Force_pair")].kind = VKind.Stacked;
  VAR s2 := stack [SCheck (2, "Force_pair")].kind = VKind.Stacked;
  BEGIN
    IF s2 THEN (* second element is already stacked *)
        IF NOT s1 THEN (* first element is not already stacked *)
            Force ();
        END;
        RETURN FALSE;
    ELSIF s1 THEN (* first element is already stacked *)
        Swap ();
        Force ();
        IF NOT commute THEN Swap (); END;
    ELSE (* neither element is stacked *)
        IF NOT commute THEN Swap (); END;
        Force ();
        Swap ();
        Force ();
    END;
    RETURN commute;
  END Force_pair;

PROCEDURE SLV_align (n: INTEGER): INTEGER =
  BEGIN
    RETURN LV_align (stack [SCheck (n, "SLV_align")]);
  END SLV_align;

PROCEDURE LV_align (READONLY x: ValRec): INTEGER =
  VAR align := x.align;
  BEGIN
    IF (x.offset # 0) THEN align := GCD (align, x.offset) END;
    IF (x.bits # NIL) THEN align := 1  END;
    RETURN align;
  END LV_align;

PROCEDURE Base_align (READONLY x: ValRec): INTEGER =
  (* like LV_align, but ignore the constant offset *)
  BEGIN
    RETURN x.align;
    (***********
    IF (x.bits = NIL)
      THEN RETURN x.align;
      ELSE RETURN 1;
    END;
    ************)
  END Base_align;

PROCEDURE GCD (a, b: INTEGER): INTEGER =
  VAR c: INTEGER;
  BEGIN
    IF (a < 0) THEN a := -a END;
    IF (b < 0) THEN b := -b END;
    IF (b = 0) THEN RETURN a END;
    LOOP
      c := a MOD b;
      IF (c = 0) THEN RETURN b END;
      a := b; b := c;
    END;
  END GCD;

PROCEDURE FindIntType (t: Type;  s: Size;  o: Offset;  a: Alignment): MType =
  VAR best_t : Type;
  BEGIN
    IF Target.SignedType [t]
      THEN best_t := ScanTypes (TargetMap.Integer_types, t, s, o, a);
      ELSE best_t := ScanTypes (TargetMap.Word_types, t, s, o, a);
    END;
    IF (best_t = Type.Void) THEN
      best_t := t;
      Err ("unable to find integer type?  type=" & Target.TypeNames[t]
            & "  s/o/a=" & Fmt.Int (s) & "/" & Fmt.Int (o) & "/" & Fmt.Int (a));
    END;
    RETURN best_t;
  END FindIntType;

PROCEDURE ScanTypes (READONLY x: ARRAY [0..3] OF Target.Int_type;
                     t: Type;  s: Size;  o: Offset;  a: Alignment): Type =
  VAR
    best_s := TargetMap.CG_Size [t] + 1;
    best_a := TargetMap.CG_Align [t] + 1;
    best_t := Type.Void;
  BEGIN
    FOR i := FIRST (x) TO LAST (x) DO
      WITH z = x[i] DO
        IF (s <= z.size) AND (z.size < best_s)
          AND (z.align <= best_a)
          AND (a MOD z.align = 0)
          AND (s + (o MOD z.align) <= z.size) THEN
          (* remember this type *)
          best_t := z.cg_type;
          best_s := z.size;
          best_a := z.align;
        END;
      END;
    END;
    RETURN best_t;
  END ScanTypes;

PROCEDURE SPush (t: Type) =
  BEGIN
    WITH x = stack[tos] DO
      x.kind      := VKind.Stacked;
      x.type      := t;
      x.temp_base := FALSE;
      x.temp_bits := FALSE;
      x.align     := Target.Byte;
      x.base      := NIL;
      x.bits      := NIL;
      x.offset    := 0;
      x.int       := TInt.Zero;
      x.float     := TFloat.ZeroR;
      x.next      := NIL;
    END;
    INC (tos);
  END SPush;

PROCEDURE SPop (n: INTEGER;  tag: TEXT) =
  BEGIN
    IF (tos < n)
      THEN ErrI (n, "SPop: stack underflow in " & tag);  tos := 0;
      ELSE DEC (tos, n);
    END;
  END SPop;

PROCEDURE SCheck (n: INTEGER;  tag: TEXT): INTEGER =
  BEGIN
    IF (tos < n)
      THEN ErrI (n, "SCheck: stack underflow in " & tag); RETURN 0;
      ELSE RETURN tos - n;
    END;
  END SCheck;

PROCEDURE Err (msg: TEXT) =
  BEGIN
    msg := "** INTERNAL CG ERROR *** " & msg;
    Error.Msg (msg);
    cg.comment (msg);
  END Err;

PROCEDURE ErrI (n: INTEGER;  msg: TEXT) =
  BEGIN
    msg := "** INTERNAL CG ERROR *** " & msg;
    Error.Int (n, msg);
    cg.comment (msg, ": ", Fmt.Int (n));
  END ErrI;

PROCEDURE NewIntTbl (): IntIntTbl.T =
  BEGIN
    RETURN NEW (IntIntTbl.Default).init ();
  END NewIntTbl;

PROCEDURE NewNameTbl (): IntRefTbl.T =
  BEGIN
    RETURN NEW (IntRefTbl.Default).init ();
  END NewNameTbl;

(*------------------------------------------------------------- debugging ---*)
(**********
**********)

CONST
  Bool = ARRAY BOOLEAN OF TEXT { "F ", "T "};
CONST
  TypeName = ARRAY Type OF TEXT {
    "Word8  ", "Int8   ", "Word16 ", "Int16  ",
    "Word32 ", "Int32  ", "Word64 ", "Int64  ",
    "Reel   ", "LReel  ", "XReel  ",
    "Addr   ", "Struct ", "Void   "
  };
CONST
  VName = ARRAY VKind OF TEXT {
    "Integer  ",
    "Float    ",
    "Stacked  ",
    "Direct   ",
    "Absolute ",
    "Indirect ",
    "Pointer  " 
  };

PROCEDURE SDump (tag: TEXT) =
  VAR msg: TEXT;
  BEGIN
    cg.comment (tag);
    cg.comment ("------------ begin stack dump ------------");
    FOR i := tos-1 TO 0 BY -1 DO
      WITH x = stack[i] DO
        msg := VName [x.kind];
        msg := msg & TypeName [x.type];
        msg := msg & Bool [x.temp_base];
        msg := msg & Bool [x.temp_bits];
        msg := msg & Fmt.Int (x.align) & " ";
        msg := msg & Fmt.Int (x.offset);
        cg.comment (msg);
      END;
    END;
    cg.comment ("------------- end stack dump -------------");
  END SDump;

PROCEDURE SEmpty (tag: TEXT) =
  BEGIN
    IF (tos > 0) THEN
      Force ();
      ErrI (tos, "stack not empty, depth");
      SDump (tag);
    END;
  END SEmpty;

BEGIN
END CG.
