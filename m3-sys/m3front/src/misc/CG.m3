(* copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: CG.m3                                                 *)
(* Last modified on Tue Jun 20 15:58:21 PDT 1995 by kalsow     *)
(*      modified on Tue May 25 11:19:53 PDT 1993 by muller     *)

MODULE CG;

IMPORT Text, IntIntTbl, IntRefTbl, Fmt, Word;
IMPORT Scanner, Error, Module, RunTyme, WebInfo;
IMPORT M3, M3CG, M3CG_Ops, M3CG_Check, M3ID, RTParams;
IMPORT Host, Target, TInt, TFloat, TWord, TargetMap, M3RT (**, RTObject **);

VAR debug := FALSE;

CONST
  Max_init_chars = 256; (* max size of a single init_chars string *)

REVEAL
  Val = BRANDED "CG.Val" REF ValRec;

TYPE
  VKind = {      (* TYPE   VALUE                   *)
    Integer,     (* Int    int                     *)
    Float,       (* Float  float                   *)
    Stacked,     (* any    S0.type                 *)
    Direct,      (* any    Value(ADR(base) + offset);
                           bits=NIL; ADR(base) + offset satisfies old_align *)
    Absolute,    (* Addr   ADR(base) + OFFS        *)
    Indirect,    (* Addr   Value(base) + OFFS      *)
    Pointer      (* Addr   S0.A + OFFS             *)
  }; (* where OFFS == offset + Value(bits)         *)

TYPE
  ValRec = RECORD
    kind      : VKind;        (* type of descriptor *)
    type      : Type;         (* type of the value *)
    temp_base : BOOLEAN;      (* TRUE => base is a temp. *)
    temp_bits : BOOLEAN;      (* TRUE => bits is a temp. *)
    old_align : Alignment;    (* known alignment of ??? *)
    base_align: Alignment;    (* alignment of ADR(base). *)
    base_value_align: Alignment;
      (* ^Alignment of MEM(base) (Direct,Absolute, or Indirect),
                 or of MEM(S0.A) (Stacked or Pointer. *)
    addr_align: Alignment;    (* when type=Addr, alignment of VALUE. *)
    (* NOTE: Originally, field 'align' was highly inconsistent on whether it
             was the alignment where a value is (or could be) stored, or, when
             type=Addr, the alignment of where the value pointed.  Most calls
             outside of CG passed in the former, while most uses inside CG
             expected the latter.  This is quite complicated to unravel, so
             to reduce breakage risk and facilitate fixes thereof, three values
             are maintained.  'old_align' is as 'align' was.  'base_align' is
             the former, and 'addr_align' is the latter.  It should be possible
             to eliminate 'old_align' altogether, after ample testing and
             removal of all uses thereof. *) 
    base      : Var;          (* base address *)
    bits      : Var;          (* non-constant bit offset *)
    offset    : INTEGER;      (* constant bit offset *)
    next      : Val;          (* link for lists *)
    int       : Target.Int;   (* literal integer value *)
    float     : Target.Float; (* literal floating point value *)
  END;

PROCEDURE VKindImage (vkind: VKind): TEXT =
  BEGIN
    CASE vkind OF
    | VKind.Integer => RETURN "Integer";
    | VKind.Float => RETURN "Float";
    | VKind.Stacked => RETURN "Stacked";
    | VKind.Direct => RETURN "Direct";
    | VKind.Absolute => RETURN "Absolute";
    | VKind.Indirect => RETURN "Indirect";
    | VKind.Pointer => RETURN "Pointer";
    END; 
  END VKindImage; 

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

(* Stacking some per-procedure values for nested procedures: *) 
TYPE 
  ProcStackNodeRef = REF RECORD 
    link : ProcStackNodeRef := NIL; 
    free_temps  : TempWrapper := NIL;
    busy_temps  : TempWrapper := NIL;
    block_cnt   : INTEGER     := 0;
  END;

VAR ProcStackRoot : ProcStackNodeRef := NIL; 

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
  (* In free_temps and free_values, "free" means they are allocated at runtime,
     but not currently in use. *) 
  free_temps  : TempWrapper := NIL;
  busy_temps  : TempWrapper := NIL;
  free_values : Val         := NIL;
  busy_values : Val         := NIL;
  indirects   : IntIntTbl.T := NIL;
  variables   : IntRefTbl.T := NIL;
  procedures  : IntRefTbl.T := NIL;
  block_cnt   : INTEGER     := 0;
  tos         : CARDINAL    := 0;
  (* ^top-of-stack (lowest *unused* stack element.*)
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
    ProcStackRoot := NIL; 
    cg.begin_unit (optimize);
  END Begin_unit;

PROCEDURE End_unit () =
  BEGIN
    Free_all_values ();
    Free_all_temps ();
    <*ASSERT ProcStackRoot = NIL*> 
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
    IF n # M3ID.NoID THEN
      cg.declare_typename (t, n);
    END;
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

PROCEDURE Declare_indirect (target: TypeUID; target_typename: Name): TypeUID =
  VAR x: INTEGER;
  BEGIN
    IF (indirects = NIL) THEN indirects := NewIntTbl () END;
    IF NOT indirects.get (target, x) THEN
      x := Word.Not (target);  (* !! fingerprint HACK !! *)
      cg.declare_indirect (x, target, target_typename);
      WebInfo.Declare_indirect (x, target);
      EVAL indirects.put (target, x);
    END;
    RETURN x;
  END Declare_indirect;

PROCEDURE Declare_proctype (t: TypeUID;  n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention; result_typename: Name) =
  BEGIN
    cg.declare_proctype (t, n_formals, result, n_raises, cc, result_typename);
    WebInfo.Declare_proctype (t, n_formals, result, n_raises);
  END Declare_proctype;

PROCEDURE Declare_formal (n: Name;  t: TypeUID; typename: Name) =
  BEGIN
    cg.declare_formal (n, t, typename);
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

PROCEDURE Widechar_size (size: INTEGER) =
  BEGIN
    cg.widechar_size (size);
  END Widechar_size;

(*--------------------------------------------------------- RunTyme hooks ---*)

PROCEDURE Set_runtime_proc (n: Name;  p: Proc) =
  BEGIN
    cg.set_runtime_proc (n, p);
  END Set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE Import_global (n: Name;  s: Size;  a: Alignment;  t: Type;
                         m3t: TypeUID; typename: Name): Var =
  VAR ref: REFANY;  v: Var;
  BEGIN
    IF (variables = NIL) THEN variables := NewNameTbl () END;
    IF variables.get (n, ref) THEN RETURN ref END;
    v := cg.import_global (n, ToVarSize (s, a), ByteAlign (a), t, m3t, typename);
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
    cg.bind_segment (seg, ToVarSize (s, a), ByteAlign (a), t, exported, init);
    IF (init) THEN
      Begin_init (seg);
      DumpPendingNodes (is_const);
      End_init (seg);
    END;
  END Bind_segment;

PROCEDURE Declare_global (n: Name;  s: Size;  a: Alignment;  t: Type;
                          m3t: TypeUID;  exported, init: BOOLEAN): Var =
  BEGIN
    RETURN cg.declare_global (n, ToVarSize (s, a), ByteAlign (a),
                              t, m3t, exported, init);
  END Declare_global;

PROCEDURE Declare_constant (n: Name;  s: Size;  a: Alignment;  t: Type;
                            m3t: TypeUID;  exported, init: BOOLEAN): Var =
  BEGIN
    RETURN cg.declare_constant (n, ToVarSize (s, a), ByteAlign (a),
                                t, m3t, exported, init);
  END Declare_constant;

PROCEDURE Declare_local (n: Name;  s: Size;  a: Alignment;  t: Type;
                         m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  BEGIN
    RETURN cg.declare_local (n, ToVarSize (s, a), ByteAlign (a),
                             t, m3t, in_memory, up_level, f);
  END Declare_local;

PROCEDURE Declare_param (n: Name;  s: Size;  a: Alignment;  t: Type;
                         m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency; typename: Name): Var =
  BEGIN
    RETURN cg.declare_param (n, ToVarSize (s, a), ByteAlign (a),
                             t, m3t, in_memory, up_level, f, typename);
  END Declare_param;

(*----------------------------------------------------------- temporaries ---*)


PROCEDURE Declare_temp (s: Size;  a: Alignment;  t: Type;
                          in_memory: BOOLEAN): Var =
  (* If possible, get a temp off the free_temps list.  Otherwise, emit code to
     allocate a temp and use it.  Either way, put the temp on the busy_temps
     list. *) 
  VAR w : TempWrapper := free_temps;
  VAR last_w: TempWrapper := NIL;
  VAR tmp: Var;
  BEGIN
    LOOP
      IF (w = NIL) THEN
        (* we need to allocate a fresh one *)
        tmp := cg.declare_temp (ToVarSize (s, a), ByteAlign (a), t, in_memory);
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

PROCEDURE Declare_addr_temp (in_memory: BOOLEAN) : Var =
  BEGIN
    RETURN Declare_temp
             (Target.Address.size, Target.Address.align,
              Type.Addr, in_memory);
  END Declare_addr_temp; 

PROCEDURE Free_temp (<*UNUSED*> v: Var) =
  BEGIN
  END Free_temp;

PROCEDURE Free_temps () =
  (* Move all temps from the busy_temps wrapper list to the free_temps wrapper
     list.
     No deallocation of any CG.Var or TempWrapper.
     No backend free_temp code emitted. *) 
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
  (* Emit code to deallocate all busy and free temps.
     Allow TempWrappers and CG temps (Vars) to be GC'd. *) 
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
  (* Emit code to deallocate busy and free temps belonging to block.
     Allow TempWrappers and CG temps (Vars) to be GC'd. *) 
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
(* This only works if all temps of block are on the front of the list.  
   Either prev_w:=w here, or if the above is always true, stop the loop
   when the first unequal block is encountered. *) 
      w := w.next;
    END;
  END Free_block_temps;

(*--------------------------------------------- direct stack manipulation ---*)

PROCEDURE Pop (): Val =
(* POST: result.Kind IN {Direct, Absolute, *)
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

    (* put it on the busy_values list. *)
    v.next := busy_values;
    busy_values := v;

    (* If it's on the M3CG stack, pop into a temporary. *)
    IF (v.kind = VKind.Stacked) THEN
      z := Declare_temp (TargetMap.CG_Size [v.type], TargetMap.CG_Align [v.type],
                         v.type, in_memory := FALSE);
      cg.store (z, 0, StackType[v.type], v.type);
      v.kind      := VKind.Direct;
      v.temp_base := TRUE;
      v.temp_bits := FALSE;
      v.old_align := TargetMap.CG_Align [v.type];
      v.base      := z;
      v.base_align:= TargetMap.CG_Align [v.type];
      v.bits      := NIL;
      v.offset    := 0;

    ELSIF (v.kind = VKind.Pointer) THEN
      z := Declare_addr_temp (in_memory := FALSE);
      cg.store (z, 0, Type.Addr, Type.Addr);
      v.kind      := VKind.Indirect;
      v.type      := Type.Addr;
      v.temp_base := TRUE;
      v.temp_bits := FALSE;
      v.base      := z;
      v.base_align:= Target.Address.align
   (* v.bits      := NIL;
      ^This would violate the commented definition of Pointer, by losing 'bits'. *) 
    END;

    RETURN v;
  END Pop;

PROCEDURE Pop_temp (): Val =
(* POST: Result.Kind = Direct AND Result.offset = 0 AND Result.bits = NIL. *) 
  BEGIN
    ForceStacked ();
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
    IF v = NIL THEN RETURN END; 
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

    (* finally, free v's temps *)
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
        ForceStacked ();
      ELSIF (x.kind = VKind.Indirect) THEN
        offs := x.offset;  x.offset := 0;
        ForceStacked ();
        IF offs # 0 THEN
          x.offset := offs;
          x.kind := VKind.Pointer;
        END;
      END;
    END;
  END XForce;

PROCEDURE ForceStacked (s: Size := 0) =
(* s needs to be provided only if s0 could be structured and packed.
   Otherwise, ForceStacked will use the size of S0's type. *)
(* TODO: It would be a lot cleaner and more consistent if a ValRec had a size
         field (in bits), which could be used here. *)

  VAR OrigKind: VKind;

  BEGIN
    WITH x = stack [SCheck (1, "Force")] DO
      IF s = 0 THEN s := TargetMap.CG_Size[x.type]; END;

      OrigKind := x.kind;

      (* force the value onto the stack *)
      CASE (x.kind) OF

      | VKind.Integer =>
          IF x.type = Target.Word.cg_type THEN
            x.type := Target.Integer.cg_type;
          ELSIF x.type = Target.Long.cg_type THEN
            x.type := Target.Longint.cg_type;
          ELSIF x.type = Target.Integer.cg_type
                OR x.type = Target.Longint.cg_type THEN (* ok *)
          ELSE <* ASSERT FALSE *>
          END;
          cg.load_integer (x.type, x.int);

      | VKind.Float =>
          x.type := TargetMap.Float_types [TFloat.Prec (x.float)].cg_type;
          cg.load_float (x.type, x.float);

      | VKind.Stacked =>
          (* value is already on the M3CG stack *)

      | VKind.Direct =>
          IF (x.bits # NIL) THEN
            Err ("attempt to force a Direct from a bit-level address...");
          END;
(* REVIEW: Force_byte_align will need to use the value alignment of
           ADR(base+offset).  Right now, it is using base_value_align.
           Is this what base_value_align means? Or do does it only take
           into account the value of base, IWC we need to GCD in the offset
           here and pass to Force_byte_align. *) 
          Force_byte_align (x, s);
          cg.load (x.base, AsBytes (x.offset), x.type, StackType[x.type]);
          x.type := StackType[x.type];
          x.base_align := TargetMap.CG_Align[x.type];
       (* x.base_value_align := x.addr_align; Does not change. *)
 
      | VKind.Absolute =>
          IF (x.bits # NIL) THEN
            Err ("attempt to force Absolute with variable bits.");
          END;
          Force_byte_align (x, s);
          cg.load_address (x.base, AsBytes (x.offset));
          x.base_value_align := x.base_align;
          x.base_align := Target.Address.align; 

      | VKind.Indirect =>
          IF (x.bits # NIL) THEN
            Err ("attempt to force Indirect with variable bits.");
          END;
          Force_byte_align (x, s);
          cg.load  (x.base, 0, Type.Addr, Type.Addr);
          (* x.base_value_align is unchanged. *)
          IF (x.offset # 0) THEN
            cg.add_offset (AsBytes (x.offset));
            x.addr_align := GCD (x.addr_align, x.offset);
          END;
          x.base_align := Target.Address.align;

      | VKind.Pointer =>
          IF (x.bits # NIL) THEN
            Err ("attempt to force Pointer with variable bits.");
          END;
          Force_byte_align (x, s);
          (* x.base_value_align is unchanged. *)
          IF (x.offset # 0) THEN
            cg.add_offset (AsBytes (x.offset));
            x.addr_align := GCD (x.addr_align, x.offset);
          END;

      END;

      (* free any temps that we used *)
      Release_temps (x);

      (* finish the descriptor *)
      IF x.type # StackType[x.type] THEN

(*      Error.Warn
          (2, "Not StackType in ForceStacked, Kind = "
              & VKindImage (OrigKind)); *)
        x.type := StackType[x.type];
(* REVIEW: This seems peculiar.  It converts any Word* type to its
           same-sized Int* counterpart on the stack.  ZTypes (in
           M3CG.i3), which claims to be the set of arithmetic types
           used on the stack, includes Word* types.  Perhaps this is
           just an un-updated leftover from Modula2 or 2+.  Perhaps
           it all works because no direct stack operators care about
           signedness.  Operands of short memory types *are*
           sign-extended or zero-extended when loaded, as here. *)

      END; 
      x.kind      := VKind.Stacked;
      x.offset    := 0;
      x.next      := NIL;
    END;
  END ForceStacked;

PROCEDURE Force_byte_align (VAR x: ValRec; s: Size) =

(* FIXME: This code seems, from reading, to have several bugs.  A experiment
   shows that the copy case does not get executed, when compiling most of the
   Modula3 distribution and several other things.
   Problems:
     - The size parameter 's' passed to Load is wrong for what to load and
       store in the temp.  The needed size of the referenced value is not
       available here, but the remainder of the referenced word should be
       big enough, and unnecessary extra bits would be harmless. 
     - For Indirect and Pointer, OFFS should not be included in the
       address of what to load.  (But should, for Absolute and Direct).
     - Load requires its alignment parameter to be for where the base
       Var is stored, but x.align (now x.old_align) is often the
       alignment of the referenced location.  The needed value would be
       x.base_align.
     - For Pointer, x.base, passed to Load, is not meaningful.
     - Do we really know that this is always RHS side context, where making
       a copy of the referenced value is correct?
    2018-05-23, rodney.m.bates@acm.org. 
*) 

(* Force x's base alignment as large as statically known, but at least
   byte-aligned. *)
(* Make a copy, if necessary.  Works only on things that fit in a word. *) 
  VAR best_align: INTEGER;
      word_size := Target.Word.size;
      word_align := Target.Word.align;
      tmp: Var; 
      shift_TInt : Target.Int; 
  BEGIN
    best_align := LV_align (x);
    IF best_align MOD Target.Byte = 0 THEN (* Already byte-aligned. *) 
      x.old_align := best_align;
    END;
    IF x.type = Type.Addr AND x.base_value_align MOD Target.Byte # 0 THEN
      (* Address is not statically known to be byte-aligned. *)
      (* This could not happen unless Addr is smaller than Word. *)
      <* ASSERT TargetMap.CG_Size[x.type] <= word_size *>
      (* ^Because front end won't allow a BITS component to cross a word boundary. *)
      Error.Warn
        (2, "Forcing in Force_byte_align, Kind = "
            & VKindImage (x.kind));      
      (* Extract into a temp and change x to refer to it. *)
      Load (x.base, x.offset, s, x.base_align, word_align, Target.Word.cg_type);
      tmp := Declare_temp (word_size, word_align, Target.Word.cg_type, in_memory := TRUE);
      IF Target.endian = Target.Endian.Big AND word_size > s THEN
        (* Left-justify the value in tmp. *) 
        EVAL TInt.FromInt(word_size - s, (*VAR*) shift_TInt);
        cg.load_integer (Target.Word.cg_type, shift_TInt); 
        cg.shift_left(Target.Word.cg_type);
      END; 
      Store (tmp, 0, word_size, word_align, Target.Word.cg_type);
      x.kind := VKind.Absolute;
      x.type := Type.Addr;
      x.base := tmp;
      x.temp_base:= TRUE;
      x.offset := 0;
      x.bits := NIL;
      x.temp_bits := FALSE;
      x.old_align := word_align;
      x.base_align := Target.Address.align;
      x.base_value_align := word_align;
      x.addr_align := word_align;
    END;
  END Force_byte_align;

PROCEDURE Force_LValue (VAR x: ValRec) =
  BEGIN
    IF x.type # Type.Addr THEN
      Error.Warn
        (2, "####### not Addr in Force_LValue, Kind = " & VKindImage (x.kind) &
            " CGType = " & Target.TypeNames [x.type]);
      x.type := Type.Addr;
    END; 
    IF (x.bits # NIL) THEN
      Err ("####### attempt to force a bit-level L-value...");
    END;
  END Force_LValue;

PROCEDURE Release_temps (VAR x: ValRec) =
  BEGIN
    IF (x.temp_base) THEN Free_temp (x.base); END;
    IF (x.temp_bits) THEN Free_temp (x.bits); END;
    (* NOTE: Free_temp is a NOP. *)
    x.temp_base := FALSE;
    x.temp_bits := FALSE;
    x.base      := NIL;
    x.bits      := NIL;
  END Release_temps;

PROCEDURE ForceStacked1 (tag: TEXT) =
  BEGIN
    ForceStacked ();
    SPop (1, tag);
  END ForceStacked1;

PROCEDURE ForceStacked2 (tag: TEXT;  commute: BOOLEAN): BOOLEAN =
  VAR swapped := Force_pair (commute);
  BEGIN
    SPop (2, tag);
    RETURN swapped;
  END ForceStacked2;

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
    FOR i := 0 TO LAST (xx^) DO
      xx[i].dump ()
    END;
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
    (* <*ASSERT n_bytes >= 0*> *)
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
        ELSIF Target.endian = Target.Endian.Little
          AND TWord.Extract (init_bits, 0, size, tmp)
          AND TWord.Extract (init_bits, size, excess, new_bits) THEN
          cg.init_int (init_pc DIV Target.Byte, tmp, t);
          init_bits := new_bits;
        ELSIF Target.endian = Target.Endian.Big
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

VAR GDebugOffset: INTEGER;

PROCEDURE Init_int (o: Offset;  s: Size;  READONLY value: Target.Int;
                    is_const: BOOLEAN) =
  VAR bit_offset: CARDINAL;  itype: Type;  tmp: Target.Int;
  BEGIN
IF o = GDebugOffset THEN
  bit_offset := 17
END;
    <*ASSERT o >= 0 *>
    IF (NOT in_init) THEN
      PushPending (NEW (IntNode, o := o, s := s, v := value), is_const);
      RETURN;
    END;

    AdvanceInit (o);
    IF Target.endian = Target.Endian.Little
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
    <*ASSERT o >= 0 *>
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
    <*ASSERT o >= 0 *>
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
(* 'init_address_of_var' would have been a more meaningful name here. *)
  BEGIN
    <*ASSERT o >= 0 *>
    <*ASSERT o MOD Target.Address.align = 0 *>
    <*ASSERT bias MOD Target.Byte = 0*>
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
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
    <*ASSERT o >= 0 *>
    <*ASSERT o MOD Target.Integer.align = 0 *>
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
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
    <*ASSERT o >= 0 *>
    <*ASSERT o MOD Target.Char.align = 0 *>
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
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
    <*ASSERT o >= 0 *>
    <*ASSERT o MOD Target.Real.align = 0 *>
    IF (in_init) THEN
      AdvanceInit (o);
      <*ASSERT o = init_pc*>
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
    <* ASSERT offset >= 0 *>
    PushPending (NEW (CharsNode, o := offset, t := t), is_const);
    RETURN offset;
  END EmitText;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE Import_procedure (n: Name;  n_params: INTEGER;  ret_type: Type;
                            cc: CallingConvention;
                            VAR(*OUT*) new: BOOLEAN;
                            return_typeid: TypeUID := 0;
                            return_typename: Name): Proc =
  VAR ref: REFANY;  p: Proc;
  BEGIN
    IF (procedures = NIL) THEN procedures := NewNameTbl() END;
    IF procedures.get (n, ref) THEN new := FALSE;  RETURN ref END;
    p := cg.import_procedure (n, n_params, ret_type, cc, return_typeid, return_typename);
    EVAL procedures.put (n, p);
    new := TRUE;
    RETURN p;
  END Import_procedure;

PROCEDURE Declare_procedure (n: Name;  n_params: INTEGER;  ret_type: Type;
                             lev: INTEGER;  cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc;
                             return_typeid: TypeUID;
                             return_typename: Name): Proc =
  VAR p: Proc;
  BEGIN
    IF (procedures = NIL) THEN procedures := NewNameTbl() END;
    p := cg.declare_procedure (n, n_params, ret_type,
                               lev, cc, exported, parent,
                               return_typeid, return_typename);
    EVAL procedures.put (n, p);
    RETURN p;
  END Declare_procedure;

PROCEDURE Begin_procedure (p: Proc) =
  VAR N : ProcStackNodeRef := NEW(ProcStackNodeRef); 
  BEGIN 
    N.link := ProcStackRoot;
    ProcStackRoot := N; 
    N.free_temps := free_temps;  
    N.busy_temps := busy_temps;  
    N.block_cnt := block_cnt; 
    free_temps     := NIL;
    busy_temps    := NIL;
    block_cnt      := 0;
    cg.begin_procedure (p);
  END Begin_procedure;

PROCEDURE End_procedure (p: Proc) =
  VAR N : ProcStackNodeRef := ProcStackRoot; 
  BEGIN
    Free_all_values ();
    Free_all_temps ();
    <*ASSERT free_temps = NIL*>
    <*ASSERT busy_temps = NIL*>
    <*ASSERT block_cnt = 0*>
    <*ASSERT N # NIL*>
    free_temps := N.free_temps;  
    busy_temps := N.busy_temps;  
    block_cnt := N.block_cnt; 
    ProcStackRoot := N.link; 
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
    ForceStacked1 ("If_true");
    cg.if_true (Target.Integer.cg_type, l, f);
  END If_true;

PROCEDURE If_false (l: Label;  f: Frequency) =
  BEGIN
    ForceStacked1 ("If_false");
    cg.if_false (Target.Integer.cg_type, l, f);
  END If_false;

PROCEDURE If_compare (t: ZType;  op: Cmp;  l: Label;  f: Frequency) =
  BEGIN
    IF ForceStacked2 ("If_compare", commute := TRUE) THEN
      op := M3CG.SwappedCompare [op];
    END;
    cg.if_compare (t, op, l, f);
  END If_compare;

PROCEDURE If_then (t: ZType;  op: Cmp;  true, false: Label;  f: Frequency) =
  BEGIN
    IF ForceStacked2 ("If_compare", commute := TRUE) THEN
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
    ForceStacked1 ("Case_jump");
    cg.case_jump (Target.Integer.cg_type, labels);
  END Case_jump;

PROCEDURE Exit_proc (t: Type) =
  BEGIN
    IF (t # Type.Void) THEN  ForceStacked1 ("Exit_proc");  END;
    cg.exit_proc (t);
  END Exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE Load
  (v: Var; o: Offset; s: Size;
   base_align (*of ADR(v)*), addr_align (*of Value(v)*): Alignment; t: Type) =
(* push ; s0.t := Mem [ ADR(v) + o : s ] *)
(* v, o, and addr_align describe the memory operand to be loaded. t is the
   stack type to load into. *) 
  VAR
    tsize  := TargetMap.CG_Size [t];
    talign := TargetMap.CG_Align [t];
    best_align : Alignment;
    best_size  : Size;
    best_type  : Type;
  BEGIN
    IF (tsize = s) AND ((base_align+o) MOD talign) = 0 THEN
      (* a full t-size'd and t-align'ed load *)
      SimpleLoad (v, o, t, base_align, addr_align);

    ELSIF (tsize < s) THEN
      ErrI (s, "memory size exceeds to-be-loaded size.");
      SimpleLoad (v, o, t, base_align, addr_align);
      ForceStacked ();  (* to connect the error message to the bad code *)

    ELSIF (t = Target.Word.cg_type) OR (t = Target.Integer.cg_type)
     OR   (t = Target.Long.cg_type) OR (t = Target.Longint.cg_type) THEN
      best_type := MayFindIntType (t, s, o, base_align);
      IF best_type # Type.Void THEN 
        best_size  := TargetMap.CG_Size [best_type];
        best_align := TargetMap.CG_Align [best_type];
        talign := (base_align+o) MOD best_align;
        IF (s = best_size) AND (talign = 0) THEN
          (* this is a simple partial word load *)
          SimpleLoad (v, o, best_type, base_align, addr_align);
        ELSE
          (* unaligned, partial load *)
          cg.load (v, AsBytes (o - talign), best_type, StackType[t]);
          IF Target.endian = Target.Endian.Little
          THEN cg.extract_mn (StackType[t], Target.SignedType[t],
                              talign, s);
          ELSE cg.extract_mn (StackType[t], Target.SignedType[t],
                              best_size - talign - s, s);
          END;
          WITH x = stack [SCheck (0, "Load")] DO
            x.kind := VKind.Stacked;
            x.type := t;
            x.base := NIL;
            x.base_align := talign;
            x.base_value_align := addr_align;
            x.addr_align := addr_align;
            x.bits := NIL;
            x.offset := 0;
            x.temp_base := FALSE;
            x.temp_bits := FALSE;
            x.next:= NIL;
          END;
          INC (tos);
        END;
      ELSIF base_align MOD Target.Byte = 0 THEN
        Load_addr_of (v, o, base_align); 
        LoadIndirectStraddling (t, o, s);
(* IMPROVEME: A (direct) LoadStraddling could generate better code. *) 
      ELSE
        Err ("unaligned word-straddling load, type="& Target.TypeNames[t]
            & "  size/offset/align=" & Fmt.Int (s) & "/" & Fmt.Int (o)
            & "/" & Fmt.Int (base_align));
        SimpleLoad (v, o, t, base_align, addr_align);
        ForceStacked ();  (* to connect the error message to the bad code *)
      END 
    ELSE
      Err ("unaligned partial-word load, type="& Target.TypeNames[t]
          & "  size/offset/align=" & Fmt.Int (s) & "/" & Fmt.Int (o)
          & "/" & Fmt.Int (base_align));
      SimpleLoad (v, o, t, base_align, addr_align);
      ForceStacked ();  (* to connect the error message to the bad code *)
    END;
  END Load;

PROCEDURE SimpleLoad
  (v: Var;  o: Offset;  t: Type;  base_align, addr_align: Alignment) =
  BEGIN
    WITH x = stack [SCheck (0, "SimpleLoad")] DO
      x.kind      := VKind.Direct;
      x.type      := t;
      x.temp_base := FALSE;
      x.temp_bits := FALSE;
      x.old_align := Target.Byte;
      x.base_align:= base_align;
      x.base_value_align := addr_align;
      x.addr_align:= GCD (addr_align, o);
      x.base      := v;
      x.bits      := NIL;
      x.offset    := o;
      x.next      := NIL;
      INC (tos);
    END;
  END SimpleLoad;

PROCEDURE LoadIndirectStraddling (t: Type; o: Offset; s: Size) =
(* s0.t := Mem [s0.A + o : s] *)
(* PRE: (s0.A + o) is a byte multiple.
   PRE: s <= Target.Word.size.
   i.e., the field starts on a byte boundary but may end within a byte
   and may straddle one word boundary. *)

  VAR wordAddrMask, byteOffsetMask: INTEGER; 
  VAR byteAddr, word0Addr, bitOffset, word1BitCt: Var;
  VAR field: Var;
  VAR mergeLab := Next_label ();
  VAR altLab := Next_label ();
  VAR fieldMask: TInt.Int; 
  BEGIN
    IF Target.Word.size = Target.Word32.size THEN
      byteOffsetMask := 2_11;
    ELSIF Target.Word.size = Target.Word64.size THEN
       byteOffsetMask := 2_111;
    END;
    wordAddrMask := Word.Not (byteOffsetMask);

    cg.comment ("LoadIndirectStraddling:"); 

    field := cg.declare_temp
               (Target.Word.bytes,
                TargetMap.CG_Align_bytes [Target.Word.cg_type],
                Target.Word.cg_type,
                in_memory := FALSE); 

    (* This uses a mix of cg.* calls, which only alter the M3CG stack,
       and CG calls, which manipulate the CG stack and M3CG stack,
       keeping them consistent.  This gets confusing.  The comments
       show the contents of both stacks, CG first.  Items on the M3CG
       stack in parentheses are conditionally there, and can be put
       there unconditionally by a Force. call. *) 
    (* On CG stack: starting byte address, AKA Ba. *)
    (*                                        CG stack   ; M3CG stack *)
    (*                                        --------     ---------- *) 
                                           (* Ba.A       ; (Ba.A) *)

    (* Save the byte address, with offset added, in temp byteAddr, AKA Bao. *) 
    ForceStacked ();                       (* Ba.A       ; Ba.A *)
    IF o # 0 THEN
      cg.add_offset (AsBytes(o))           (* Ba+o.A     ; Ba+o.A *)
    END; 
    byteAddr := cg.declare_temp
               (Target.Address.bytes,
                TargetMap.CG_Align_bytes [Type.Addr],
                Type.Addr,
                in_memory := FALSE); 
    cg.store (byteAddr, 0, Type.Addr, Type.Addr);
                                           (* Ba+o.A     ; *)
    SPop (1, "");                          (*            ; *) 

    IF Target.endian = Target.Endian.Little THEN

      (* Compute and save address of full memory word zero in temp word0Addr,
         AKA w0a. *)
      word0Addr := cg.declare_temp
                    (Target.Address.bytes,
                     TargetMap.CG_Align_bytes [Type.Addr],
                     Type.Addr,
                     in_memory := FALSE);
      cg.load (byteAddr, 0, Type.Addr, Type.Addr);
                                           (*            ; Ba+o.A *)
      cg.loophole (Type.Addr, Target.Word.cg_type);
                                           (*            ; Ba+o.W *) 
      cg_load_word (wordAddrMask); (*AKA wam*)
                                           (*            ; wam.W Ba+o.W *)
      cg.and (Target.Word.cg_type);        (*            ; w0a.W *)
      cg.loophole (Target.Word.cg_type, Type.Addr);
                                           (*            ; w0a.A *) 
      cg.store (word0Addr, 0, Type.Addr, Type.Addr);
                                           (*            ; *)
     (* Compute and save bit offset within word zero in temp bitOffset,
         AKA bo. Byte offset is AKA Bo. *) 
      bitOffset := cg.declare_temp
                     (Target.Word.bytes,
                      TargetMap.CG_Align_bytes [Target.Word.cg_type],
                      Target.Word.cg_type,
                      in_memory := FALSE);
      cg.load (byteAddr, 0, Type.Addr, Type.Addr);
                                           (*            ; Ba+o.A *)
      cg.loophole (Type.Addr, Target.Word.cg_type);
                                           (*            ; Ba+o.W *) 
      cg_load_word (byteOffsetMask);       (*            ; Bom.W Ba+o.W *)
      cg.and (Target.Word.cg_type);        (*            ; Bo.W *)
      cg_load_word (Log2OfByte);           (*            ; shiftct.W, Bo.W *)
      cg.shift_left (Target.Word.cg_type); (*            ; bo.W *)
      cg.store (bitOffset, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; *)

      (* Load word zero and mask out the to-be-stored-into bits, AKA w0m. *)
      cg.load (word0Addr, 0, Type.Addr, Type.Addr);
                                           (*            ; w0a.A *)
      cg.load_indirect (0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; w0.W *) 

      (* Right shift by bit offset to right-justify and zero-extend
         word zero's subfield, AKA f0. *)
      cg.load (bitOffset, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; bo.W w0.W *)
      cg.shift_right (Target.Word.cg_type);(*            ; f0.W *)
      cg.store (field, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*             ; *)
      (* ^This could be the final result.  Or not. *) 

      (* Compute bit count from word one, AKA bc1. *)
      cg.load (bitOffset, 0, Target.Word.cg_type, Target.Integer.cg_type);
                                           (*            ; bo.I *)
      cg_load_intt (s - Target.Word.size); (*            ; const.I bo.I *)
                   (* ^May be negative. *)
      cg.add (Target.Integer.cg_type);     (*            ; bc1.I *)
      word1BitCt := cg.declare_temp
                     (Target.Integer.bytes,
                      TargetMap.CG_Align_bytes [Target.Integer.cg_type],
                      Target.Integer.cg_type,
                      in_memory := FALSE);
      cg.store (word1BitCt, 0, Target.Integer.cg_type, Target.Integer.cg_type);
                                           (*            ; *)

      (* Skip if no bytes from word one are involved. *)
      cg.load (word1BitCt, 0, Target.Integer.cg_type, Target.Integer.cg_type);
                                           (*            ; bc1.I *)
      cg_load_word (0);                    (*            ; 0.W bc1.I *)
      cg.if_compare (Target.Integer.cg_type, Cmp.LE, altLab, Maybe);
                                           (*            ; *) 

      (* Fetch word one, AKA w1. *) 
      cg.load (word0Addr, 0, Type.Addr, Type.Addr);
                                           (*            ; w0a.A *)
      cg.load_indirect
        (Target.Word.bytes, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; w1.W *)

      (* Mask out garbage bits of word one, giving its relevant subfield,
         AKA f1, which is already right-justified. *)
      cg_load_word (-1); (* All ones. *)   (*            ; ones.W w1.W *)
      cg.load (word1BitCt, 0, Target.Integer.cg_type, Target.Integer.cg_type);
                                           (*            ; bc1.I ones.W w1.W *) 
      cg.shift_left (Target.Word.cg_type); (*            ; negmask.W, w1.W *)
      cg.not (Target.Word.cg_type);        (*            ; mask.W w1.W *) 
      cg.and (Target.Word.cg_type);        (*            ; f1.W *) 

      (* Rotate word one right by bit count, to align its subfield,
         AKA f1a. *)
      cg.load (bitOffset, 0, Target.Word.cg_type, Target.Integer.cg_type);
                                           (*            ; bo.I f1.W *)
      cg.rotate_right(Target.Word.cg_type);(*            ; f1a.W *)

      (* Combine subfields from both words, AKA f. *)
      cg.load (field, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; f0.W f1a.W *) 
      cg.or (Target.Word.cg_type);         (*            ; f.W *) 
      cg.store (field, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; *)

      cg.jump (mergeLab); 
      cg.set_label (altLab);

      (* Zero-extend by masking out high bits. *)
      (* Here, f0 almost equals f *) 
      cg.load (field, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; f0.W *)
      TWord.nBitsOnRight (s, (*OUT*) fieldMask);
      cg.load_integer (Target.Word.cg_type, fieldMask);
                                           (*            ; mask.W f0.W *)
      cg.and (Target.Word.cg_type);        (*            ; f *)
      cg.store (field, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; *)

      cg.set_label (mergeLab);
      cg.load (field, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; f.W *) 
      SPush (Target.Word.cg_type);         (* f.W        ; f.W *) 

    ELSE (* Big endian. *)

(* 
      byteAddr := Pop (); (* Addr *)       (*            ; *)
      Push (byteAddr); ForceStacked ();    (* Ba         ; Ba *)

      (* Compute RT address of full memory word zero, AKA w0a *)  
      cg_load_addr (wordAddrMask); (*AKA wam*)
                                           (* Ba         ; wam, Ba *)
      cg.and (Type.Addr);                  (* w0a        ; w0a *)
      word0Addr := Pop (); (* Addr *)      (*            ; *)

      (* Fetch word zero, AKA w0. *) 
      Push (word0Addr); ForceStacked ();   (* w0a        ; w0a *)
      cg.load_indirect (0, Target.Word.cg_type, Target.Word.cg_type);
                                           (* w0a        ; w0 *)
      SPop (1, "" );                       (*            ; w0 *)

      (* Compute RT byte offset, within word zero, AKA Bo *) 
      Push (byteAddr); ForceStacked ();    (* Ba.A       ; Ba.A, w0 *)
      cg.loophole (Type.Addr, Target.Word.cg_type);
                                           (* Ba.A       ; Ba.W, w0 *)
      cg_load_word (byteOffsetMask);       (* Ba         ; Bom.W, Ba.W, w0 *)
      cg.and (Target.Word.cg_type);        (* Ba         ; Bo.W, w0 *)
      SPop (1, "LoadIndirectStrad, #1");   (*            ; Bo.W, w0 *)
      SPush (Target.Word.cg_type);         (* Bo.W       ; Bo.W, w0 *) 
      byteOffset := Pop (); (*Word*)       (*            ; w0 *)

      (* Compute shift count, AKA sc. *) 
      Push (byteOffset); ForceStacked ();  (* Bo.W       ; Bo.W, w0 *)
      cg.loophole (Target.Word.cg_type, Target.Integer.cg_type); 
                                           (* Bo.W       ; Bo.I, w0 *)
      cg_load_intt (s - Target.Word.size); (* Bo.W       ; const.I, Bo.I, w0 *)
      cg.add (Target.Integer.cg_type);     (* sc.W       ; sc.I, w0 *)
      shiftCt := Pop (); (*Int*)           (*            ; w0 *) 

      (* Shift word zero to field's final position. AKA f0. *)
      Push (shiftCt); ForceStacked ();     (* sc         ; sc.I, w0.W *)
      cg.shift (Target.Word.cg_type);      (* f0         ; f0.W *)
(* CHECK^ Does gcc-derived code generator accept this type mismatch? *) 

      (* If only word zero is involved, we are almost done. *) 
      Push (shiftCt); ForceStacked ();     (* sc, f0     ; sc.I, f0.W *)

      cg_load_intt (0);                    (* sc, f0     ; 0.I, sc.I , f0.W *)
      cg.if_compare (Target.Integer.cg_type, Cmp.LE, mergeLab, Maybe);
                                           (* sc, f0     ; f0.W *)
      SPop (1, "LoadIndirectStrad, #2");   (* f0         ; f0.W *)

      (* Fetch word one, AKA w1. *) 
      Push (word0Addr); ForceStacked ();   (* w0a, f0    ; w0a.A, f0.W *)
      cg.load_indirect
        (Target.Word.bytes, Target.Word.cg_type, Target.Word.cg_type);
                                           (* w0a, f0     ; w1.W, f0.W *)
      SPop (1, "LoadIndirectStrad, #3");   (* f0          ; w1.W, f0.W *)

      (* Right-justify the wanted field of word one, AKA f1. *) 
      Push (shiftCt); ForceStacked ();     (* sc, f0      ; sc.I, w1.W, f0.W *)
      cg.loophole (Target.Word.cg_type, Target.Integer.cg_type);
      cg.rotate (Target.Integer.cg_type);
                                           (* sc, f0      ; f1.W, f0.W *)
      SPop (1, "LoadIndirectStrad, #4");   (* f0          ; f1.W, f0.W *) 

      (* Mask out uninvolved bits of w1, i.e., zero extend,  AKA f1e *)
      cg_load_word ( -1 ); (* All ones. *) (* f0          ; -1.W, f1.W, f0.W *)
      Push (shiftCt); ForceStacked ();     (* sc, f0      ; sc.I -1.W, f1.W, f0.W *)
      cg.shift_left (Target.Word.cg_type); (* sc, f0      ; negmask.W, f1.W, f0.W *)
(* CHECK^ Does gcc-derived code generator accept this type mismatch? *) 
      cg.not (Target.Word.cg_type);        (* sc, f0      ; mask.W f1.W f0.W *)
      cg.and (Target.Word.cg_type);        (* sc, f0      ; f1e.W f0.W *)
      SPop (1, "LoadIndirectStrad, #5");   (* f0          ; fle.W f0.W *) 

      (* Combine masked and aligned fragments, AKA, f *)
      cg.or (Target.Word.cg_type);         (* f           ; f *) 
      
      (* Mask out the zero-extension. *) 
      cg.set_label (mergeLab);
      (* If we come here by branching, f0 = f, thus: *)
                                           (* f           ; f.W *)

      IF s < Target.Word.size THEN (* zero-extend by masking. *)
        Load_integer (Target.Word.cg_type, ZExtMasks[s DIV Target.Byte]);
                                           (* mask, f     ; (mask.W), f.W *) 
        And (Target.Word.cg_type);         (* f           ; f.W *)
      END;
*) 
    END; (* Endianness. *)  

    (* Fix the type of the result on the CG stack. *) 
    SPop (1, "LoadIndirectStrad, #6");     (*             ; f.I *)
    SPush (t);                             (* f.t         ; f.I *)
  END LoadIndirectStraddling; 

PROCEDURE Load_addr_of (v: Var;  o: Offset; addr_align: Alignment) =
(* push ; s0.A := ADR(v) + o *)
(* addr_align applies to where the address points. *) 
  BEGIN
    WITH x = stack [SCheck (0, "Load_addr_of")] DO
      x.kind      := VKind.Absolute;
      x.type      := Type.Addr;
      x.temp_base := FALSE;
      x.temp_bits := FALSE;
      x.old_align := ByteAlign (addr_align) * Target.Byte;
      x.base_align:= Target.Address.align;
      x.base_value_align := addr_align;
      x.addr_align:= GCD (addr_align, o);
      x.base      := v;
      x.bits      := NIL;
      x.offset    := o;
      x.next      := NIL;
    END;
    INC (tos);
  END Load_addr_of;

PROCEDURE Load_addr_of_temp (v: Var; o: Offset; addr_align: Alignment) =
  BEGIN
    Load_addr_of (v, o, addr_align);
    stack[SCheck(1,"Load_addr_of_temp")].temp_base := TRUE;
  END Load_addr_of_temp;

PROCEDURE Load_int (t: IType;  v: Var;  o: Offset := 0) =
  BEGIN
    SimpleLoad (v, o, t, TargetMap.CG_Align[t], Target.Word8.align);
  END Load_int;

PROCEDURE Load_addr
  (v: Var;  o: Offset; addr_align: Alignment := Target.Word8.align) =
(* Actually, this means load the *value* of v and give it type Addr. *) 
(* == Load (v, o, Target.Address.size, Target.Address.align, addr_align, Type.Addr) *)
  BEGIN
    SimpleLoad (v, o, Type.Addr, Target.Address.align, addr_align);
  END Load_addr;

PROCEDURE Load_indirect
  (t: Type;  addedOffset: Offset;  s: Size; addr_align: Alignment := Target.Word8.align) =
(* s0.t := Mem [s0.A + o : s] *)
(* If t=A, addr_align applies to where final s0.t points, otherwise irrelevant. *)
  VAR
    t_size     := TargetMap.CG_Size [t];
    t_align    := TargetMap.CG_Align [t];
    best_align : Alignment;
    best_size  : Size;
    best_type  : Type;
    fetch_align: INTEGER;
    oddStaticBitCt : INTEGER;
    save_bits  : Var := NIL;
    oddRTBits : Var;
    save_temp_bits : BOOLEAN;

best_type1  : Type;
a1          : INTEGER;
oddStaticBitCt1 : INTEGER;
x1, x2, x3, x4, x5, x6, x7, x8,x9, x10: ValRec;

  BEGIN (* Load_indirect *)
x1 := stack[SCheck(1,"Load_indirect-x1")];
    ForceAddr2SAP (addedOffset);
x2 := stack[SCheck(1,"Load_indirect-x2")];
    WITH x = stack [SCheck (1, "Load_indirect")] DO
      (* x.VKind IN {Stacked, Absolute, Pointer} *) 
a1 := x.addr_align;

      IF s = t_size AND x.addr_align MOD t_align = 0 THEN
        (* A full t_size'd and t_align'ed load. *)
        SimpleIndirectLoad (x, t, addr_align);

      ELSIF s = t_size  AND x.addr_align MOD Target.Byte = 0
            AND Target.Allow_packed_byte_aligned THEN
        (* A full t_size'd and byte-aligned load, supported by the processor.
           This is used by packed structures. *)
        SimpleIndirectLoad (x, t, addr_align);

      ELSIF s > t_size THEN
        Err ("In Load_indirect, memory size exceeds stack size.");
        ForceStacked (); (* to connect the error message with the code *)
x3 := stack[SCheck(1,"Load_indirect-x3")];
        SimpleIndirectLoad (x, t, addr_align);

      ELSIF (t = Target.Word.cg_type) OR (t = Target.Integer.cg_type)
            OR (t = Target.Long.cg_type) OR (t = Target.Longint.cg_type)
      THEN
        best_type := MayFindIntType (t, s, x.offset, x.base_value_align);
        IF best_type # Type.Void THEN
 best_type1 := best_type;
          best_size := TargetMap.CG_Size [best_type];
          best_align := TargetMap.CG_Align [best_type];
          oddStaticBitCt := x.offset MOD best_align;
 oddStaticBitCt1 := oddStaticBitCt; 
          IF (oddStaticBitCt = 0) AND (x.bits = NIL) THEN
            (* A best_align'ed load. *)
            SimpleIndirectLoad (x, best_type, addr_align);
            (* x.kind IN {Direct, Stacked} *)
            IF (s # best_size) THEN (* NOT all of best_size to be loaded. *)
              ForceStacked ();
x4 := stack[SCheck(1,"Load_indirect-x4")];
              IF Target.endian = Target.Endian.Little
              THEN cg.extract_mn (StackType[t],
                                  Target.SignedType[t], 0, s);
              ELSE cg.extract_mn (StackType[t],
                                  Target.SignedType[t], best_size - s, s);
              END;
              FinishLoadIndirect (x, t, addr_align);
            END;            
          ELSIF (x.bits = NIL) THEN
            (* Unaligned, partial best_size'd load. *)
            DEC (x.offset, oddStaticBitCt);
x5 := stack[SCheck(1,"Load_indirect-x5")];
            SimpleIndirectLoad (x, best_type, addr_align);
x6 := stack[SCheck(1,"Load_indirect-x6")];
            ForceStacked ();
x7 := stack[SCheck(1,"Load_indirect-x7")];
            IF Target.endian = Target.Endian.Little
            THEN cg.extract_mn (StackType[t], Target.SignedType[t],
                                oddStaticBitCt, s);
            ELSE cg.extract_mn (StackType[t], Target.SignedType[t],
                                best_size - oddStaticBitCt - s, s);
            END;
            FinishLoadIndirect (x, t, addr_align);
          ELSE (* bits # NIL *)
            (* Unaligned, partial-word load with RT variable offset. *)
            fetch_align := MIN (x.addr_align, t_size);
            IF (best_size < fetch_align) THEN
              (* make sure we load the largest possible aligned value,
                 because we can't tell how far the variable bit-offset
                 will take us.  *)
              best_type := MustFindIntType 
                             (t, MAX (s, fetch_align), x.offset MOD fetch_align,
                              x.base_value_align);
              best_size  := TargetMap.CG_Size [best_type];
              best_align := TargetMap.CG_Align [best_type];
              oddStaticBitCt := x.offset MOD best_align;
            END;
            IF (x.base_value_align < best_align) THEN 
              Err ("unaligned fetch type in Load_indirect."); 
            END;

            oddRTBits := ReduceBits (best_align); 

            (* hide the runtime bit offset value. *)
            save_bits := x.bits;       x.bits := NIL;
            save_temp_bits := x.temp_bits;  x.temp_bits := FALSE;

            (* generate the aligned load *)
            DEC (x.offset, oddStaticBitCt);
x8 := stack[SCheck(1,"Load_indirect-x8")];
            SimpleIndirectLoad (x, best_type, addr_align);
x9 := stack[SCheck(1,"Load_indirect-x9")];
            ForceStacked ();
x10 := stack[SCheck(1,"Load_indirect-x10")];

            (* Runtime compute the full bit offset *)
            IF Target.endian = Target.Endian.Little THEN
              cg.load (oddRTBits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
              IF (oddStaticBitCt # 0) THEN
                cg_load_intt (oddStaticBitCt);
                cg.add (Target.Integer.cg_type);
              END;
            ELSE (* big endian *)
              cg_load_intt (best_size - oddStaticBitCt - s);
              cg.load (oddRTBits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
              cg.subtract (Target.Integer.cg_type);
            END;

            (* extract the needed bits *)
            cg.extract_n (StackType[t], Target.SignedType[t], s);
            FinishLoadIndirect (x, t, addr_align);

            Free_temp (oddRTBits);
            IF save_temp_bits THEN Free_temp (save_bits) END; 
          END;
        ELSIF x.addr_align MOD Target.Byte = 0 THEN
          LoadIndirectStraddling (t, addedOffset, s); 
        ELSE 
          Err ("unaligned word-straddling load_indirect, type="& Target.TypeNames[t]
              & "  s/a=" & Fmt.Int (s) & "/" & Fmt.Int (x.addr_align));
          ForceStacked ();  (* to connect the error message *)
          SimpleIndirectLoad (x, t, addr_align);
          ForceStacked ();
        END
      ELSE
        Err ("unaligned partial-word load_indirect, type="& Target.TypeNames[t]
            & "  s/a=" & Fmt.Int (s) & "/" & Fmt.Int (x.addr_align));
        ForceStacked ();  (* to connect the error message *)
        SimpleIndirectLoad (x, t, addr_align);
        ForceStacked ();
      END;
    END (*WITH*);
  END Load_indirect;

PROCEDURE FinishLoadIndirect
  (VAR x: ValRec; t: Type; addr_align: Alignment) =
  BEGIN
    x.kind := VKind.Stacked;
    x.type := StackType[t];
    x.base := NIL;
    x.offset := 0;
    x.temp_base := FALSE;
    x.bits := NIL;
    x.temp_bits := FALSE;
    x.old_align := Target.Byte;
    x.base_align := TargetMap.CG_Align [x.type];
    x.base_value_align := addr_align;
    x.addr_align := addr_align;
  END FinishLoadIndirect; 

PROCEDURE SimpleIndirectLoad (VAR x: ValRec;  t: MType; addr_align: Alignment) =
(* PRE: x.kind IN {Stacked, Absolute, Pointer}. *)
(* Load full t-size'd and t-align'ed. *)
  VAR offs: INTEGER;
  VAR stackType: Type;
  BEGIN
    CASE x.kind OF
    | VKind.Absolute =>
      x.kind := VKind.Direct;
      x.type := t;
      x.addr_align := addr_align;
      
    | VKind.Pointer, VKind.Stacked =>
      offs := x.offset;  x.offset := 0;
      stackType := StackType[t];
      cg.load_indirect (AsBytes (offs), t, stackType);
      FinishLoadIndirect (x, t, addr_align);
      
    ELSE (* ?? *)
      ErrI (ORD (x.kind), "bad VKind in SimpleIndirectLoad");
      ForceStacked ();
      stackType := StackType[t];
      cg.load_indirect (0, t, stackType);
      FinishLoadIndirect (x, t, addr_align);
    END;
  END SimpleIndirectLoad;

PROCEDURE Store
  (v: Var;  o: Offset;  s: Size;  base_align (*of ADR(v)*): Alignment;
   t: Type) =
  VAR
    t_size     := TargetMap.CG_Size [t];
    t_align    := TargetMap.CG_Align [t];
    best_align : Alignment;
    best_size  : Size;
    best_type  : Type;
  BEGIN
    ForceStacked ();  (* materialize the value to be stored *)

    IF s = t_size AND ((base_align+o) MOD t_align) = 0 THEN
      (* a simple aligned store *)
      cg.store (v, AsBytes (o), StackType[t], t);
    ELSIF (t_size < s) THEN
      Err ("store size too large");
      cg.store (v, AsBytes (o), StackType[t], t);
    ELSIF (t = Target.Word.cg_type) OR (t = Target.Integer.cg_type)
      OR  (t = Target.Long.cg_type) OR (t = Target.Longint.cg_type) THEN
      best_type  := MayFindIntType (t, s, o, base_align);
      IF best_type # Type.Void THEN 
        best_size  := TargetMap.CG_Size [best_type];
        best_align := TargetMap.CG_Align [best_type];
        t_align := (base_align+o) MOD best_align;
        IF (s = best_size) AND (t_align = 0) THEN
          (* this is a simple partial word store *)
          cg.store (v, AsBytes (o), StackType[t], best_type);
        ELSE
          (* unaligned, partial store *)
          cg.load (v, AsBytes (o - t_align), best_type, StackType[t]);
          cg.swap (t, t);
          IF Target.endian = Target.Endian.Little
            THEN cg.insert_mn (StackType[t], t_align, s);
            ELSE cg.insert_mn (StackType[t], best_size - t_align - s, s);
          END;
          cg.store (v, AsBytes (o - t_align), StackType[t], best_type);
        END;
      ELSIF base_align MOD Target.Byte = 0 THEN
        Load_addr_of (v, o, base_align);
        Swap ();
        StoreIndirectStraddling (t, o, s);
      ELSE
        Err ("unaligned word-straddling store, type="& Target.TypeNames[t]
              & "  size/offset/align=" & Fmt.Int (s) & "/" & Fmt.Int (o) & "/" & Fmt.Int(base_align));
        cg.store (v, ToBytes (o), Target.Integer.cg_type, t);
      END;
    ELSE
      Err ("unaligned partial-word store, type="& Target.TypeNames[t]
            & "  size/offset/align=" & Fmt.Int (s) & "/" & Fmt.Int (o) & "/" & Fmt.Int(base_align));
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

PROCEDURE ForceAddr2SAP (addedOffset: Offset) =
  (* Force s0.A to have kind IN {Stacked, Absolute, Pointer}, while
     incrementing its value by addedOffset. *)
  BEGIN
    WITH x = stack [SCheck (1, "ForceAddr2SAP")] DO
      <* ASSERT x.type = Type.Addr *>
      <* ASSERT x.base_align = Target.Address.align *>
      CASE x.kind OF
      | VKind.Stacked => 
        <*ASSERT x.offset = 0*>
        IF addedOffset # 0 THEN 
          x.kind := VKind.Pointer;
          x.base_align := Target.Address.align;
          x.offset := addedOffset;
          x.addr_align := GCD (x.addr_align, addedOffset);
        END;

      | VKind.Pointer, VKind.Absolute =>
        INC (x.offset, addedOffset);
        x.addr_align := GCD (x.addr_align, addedOffset);

      | VKind.Direct => 
        <*ASSERT x.bits = NIL *>
        IF addedOffset = 0 THEN
          cg.load (x.base, AsBytes (x.offset), Type.Addr, Type.Addr);
          x.kind := VKind.Stacked;
          x.base_align := Target.Address.align;
       (* x.base_value_align := x.addr_align; Does not change. *)
          x.offset := 0;
        ELSE
          cg.load (x.base, AsBytes (x.offset), Type.Addr, Type.Addr);
          x.kind := VKind.Pointer;
          x.base_align := Target.Address.align;
       (* x.base_value_align := x.addr_align; Does not change. *)
          x.offset := addedOffset;
          x.addr_align := GCD (x.addr_align, addedOffset);
        END; 

      | VKind.Indirect =>
        cg.load  (x.base, 0, Type.Addr, Type.Addr);
        x.kind := VKind.Pointer;
        x.base_align := Target.Address.align;
        INC (x.offset, addedOffset);
     (* x.base_value_align := x.addr_align; Does not change. *)
        x.addr_align := GCD (x.addr_align, addedOffset);
      ELSE 
      END;
    END; 
  END ForceAddr2SAP; 

PROCEDURE Store_indirect (t: Type; addedOffset: Offset;  s: Size) =
(* Mem [s1.A + o : s] := s0.t ; pop (2) *)
  VAR
    t_size     := TargetMap.CG_Size [t];
    t_align    := TargetMap.CG_Align [t];
    best_align : Alignment;
    best_size  : Size;
    best_type  : Type;
    addr_align : INTEGER;
    tmp        : Val;
    oddStaticBitCt : INTEGER;
    save_bits  : Var := NIL;
    oddRTBits : Var;
    save_temp_bits : BOOLEAN := FALSE;
  BEGIN
    Swap ();
    ForceAddr2SAP (addedOffset);
    (* Address wherein to store is now Stacked, Absolute, or Pointer. *)
    Swap ();
    ForceStacked (); (* the value to be stored *)

    WITH x = stack [SCheck (2, "Store_indirect-addr")] DO

      IF s = t_size AND (x.addr_align MOD t_align) = 0 THEN
        (* A full t_size'd and t_align'ed store. *)
        SimpleIndirectStore (x, t);
      ELSIF s = t_size
            AND (x.addr_align MOD Target.Byte) = 0
            AND Target.Allow_packed_byte_aligned THEN
        (* A full t_size'd and byte-aligned store, supported by the processor.
           This is used by structures containing packed components. *)
        SimpleIndirectStore (x, t);

      ELSIF s > t_size THEN
        Err ("In Store_indirect, memory size exceeds stack size.");
        SimpleIndirectStore (x, t);

      ELSIF (t = Target.Word.cg_type) OR (t = Target.Integer.cg_type)
        OR  (t = Target.Long.cg_type) OR (t = Target.Longint.cg_type) THEN
        best_type := MayFindIntType (t, s, x.offset, x.base_value_align);
        IF best_type # Type.Void THEN 
          best_size := TargetMap.CG_Size [best_type];
          best_align := TargetMap.CG_Align [best_type];
          oddStaticBitCt := x.offset MOD best_align;
          IF (oddStaticBitCt = 0) AND (s = best_size) AND (x.bits = NIL) THEN
            (* A full best_size'd and best_align'ed store. *)
            SimpleIndirectStore (x, best_type);
          ELSIF (oddStaticBitCt = 0) AND (x.bits = NIL) THEN
            (* A best_align'ed but only partial best_size'd store. *)
            Swap (); (* Address on top *)
            tmp := Pop ();
            Push (tmp);
            ForceAddr2SAP (0);
            SimpleIndirectLoad (* Fetched word on top. *)
              (stack [SCheck (1,"Store_indirect-1")], best_type, 1);

            (* stuff the bits *)
            ForceStacked ();
            Swap (); 
            ForceStacked ();
            IF Target.endian = Target.Endian.Little
              THEN cg.insert_mn (StackType[t], 0, s);
              ELSE cg.insert_mn (StackType[t], best_size - s, s);
            END;
            SPop (1, "Store_indirect #1");
            Push (tmp);
            ForceAddr2SAP (0);
            Swap (); (* Value on top. *) 
            SimpleIndirectStore (x, best_type);
            Free (tmp);
          ELSIF (x.bits = NIL) THEN
            (* Unaligned, partial best_size'd store. *)
            DEC (x.offset, oddStaticBitCt);
            Swap (); (* Address on top *)
            tmp := Pop ();
            Push (tmp);
            ForceAddr2SAP (0);
            SimpleIndirectLoad (* Fetched word on top. *)
              (stack [SCheck (1, "Store_indirect-2")], best_type, 1);

            (* stuff the bits *)
            ForceStacked ();
            Swap (); 
            ForceStacked ();
            IF Target.endian = Target.Endian.Little
              THEN cg.insert_mn (StackType[t], oddStaticBitCt, s);
              ELSE cg.insert_mn (StackType[t], best_size - oddStaticBitCt - s, s);
            END;
            SPop (1, "Store_indirect #2");
            
            (* finally, store the result *)
            Push (tmp);
            ForceAddr2SAP (0);
            Swap (); (* Stuffed word on top, address below. *) 
            SimpleIndirectStore (x, best_type);
            Free (tmp);
          ELSE (* bits # NIL *)
            (* Unaligned, partial best_size'd store with RT variable offset. *)
            addr_align := MIN (x.addr_align, t_size);
            IF (best_size < addr_align) THEN
              (* make sure we load and store the largest possible aligned value,
                 because we can't tell how far the variable bit-offset
                 will take us.  *)
              best_type
                := MustFindIntType
                     (t, MAX (s, addr_align), x.offset MOD addr_align,
                      x.base_value_align);
              best_size  := TargetMap.CG_Size [best_type];
              best_align := TargetMap.CG_Align [best_type];
              oddStaticBitCt := x.offset MOD best_align;
            END;
            IF (x.base_value_align < best_align) THEN 
              Err ("unaligned fetch type in Store_indirect");
            END;

            Swap (); (* Address on top *) 
            oddRTBits := ReduceBits (best_align); 
            Swap (); (* Value back on top. *) 

            (* Hide the unreduced runtime bit offset. *)
            save_bits := x.bits; x.bits := NIL;
            save_temp_bits := x.temp_bits; x.temp_bits:= FALSE;

            (* generate the aligned load *)
            DEC (x.offset, oddStaticBitCt);
            Swap (); (* Address on top *) 
            tmp := Pop ();
            Push (tmp);
            ForceAddr2SAP (0);
            SimpleIndirectLoad
              (stack [SCheck (1, "Store_indirect-3")], best_type, addr_align);
              (* ^Fetched word on top, to-store value below. *)
               
            (* Code to compute the full bit offset. *)
            ForceStacked ();
            Swap (); 
            (* Value on top, fetched word below. *) 
            ForceStacked ();
            IF Target.endian = Target.Endian.Little THEN
              cg.load
                (oddRTBits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
              IF (oddStaticBitCt # 0) THEN
                cg_load_intt (oddStaticBitCt);
                cg.add (Target.Integer.cg_type);
              END;
            ELSE (* big endian *)
              cg_load_intt (best_size - oddStaticBitCt - s);
              cg.load
                (oddRTBits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
              cg.subtract (Target.Integer.cg_type);
            END;
            (* RT bit count, value, fetched word. *) 
            
            (* stuff the bits *)
            cg.insert_n (StackType[t], s);
            SPop (1, "Store_indirect #3");

            (* finally, store the result *)
            Push (tmp);
            ForceAddr2SAP (0);
            Swap (); (* Stuffed word on top, address below. *) 
            SimpleIndirectStore (x, best_type);

            Free (tmp);
            Free_temp (oddRTBits);
            IF save_temp_bits THEN Free_temp (save_bits) END;
          END;
        ELSIF x.addr_align MOD Target.Byte = 0 THEN
          StoreIndirectStraddling (t, addedOffset, s);
          RETURN; (* ^This pops the operands. *)
        ELSE
          Err ("unaligned word-straddling store_indirect, type="& Target.TypeNames[t]
              & "  s/a=" & Fmt.Int (s) & "/" & Fmt.Int (x.addr_align));
          SimpleIndirectStore (x, t);
          END 
      ELSE
        Err ("unaligned partial-word store_indirect, type="& Target.TypeNames[t]
            & "  s/a=" & Fmt.Int (s) & "/" & Fmt.Int (x.addr_align));
        SimpleIndirectStore (x, t);
      END;

    END;
    SPop (2, "Store_indirect");
  END Store_indirect;

PROCEDURE SimpleIndirectStore (READONLY x: ValRec;  t: MType)=
(* PRE: x.kind IN {Stacked, Absolute, Pointer}. *)
(* Store full t-size'd and t-align'ed. *)
  BEGIN
    CASE x.kind OF
    | VKind.Absolute =>
      cg.store (x.base, AsBytes (x.offset), StackType [t], t);
    | VKind.Pointer, VKind.Stacked =>
      cg.store_indirect (AsBytes (x.offset), StackType [t], t);
    ELSE (* ?? *)
      ErrI (ORD (x.kind), "bad VKind in SimpleIndirectStore");
      cg.store_indirect (AsBytes (x.offset), StackType[t], t);
    END;
  END SimpleIndirectStore;

PROCEDURE StoreIndirectStraddling
  (<*UNUSED*> t: Type; o: Offset; s: Size; PleaseZextField:= TRUE) =
(* Mem [s1.A + o : s] := s0.t ; pop (2) *)
(* PRE: (s0.A + o) is a byte multiple.
   PRE: s <= Target.Word.size.
   i.e., the memory field starts on a byte boundary but may end on
   any bit and may straddle one word boundary. *)
  VAR field, byteAddr, word0Addr, bitOffset, word1Mask: Var; 
  VAR fieldMask: TInt.Int;
  VAR byteOffsetMask, wordAddrMask: INTEGER; 
  VAR altLab := Next_label ();
  BEGIN
     IF Target.Word.size = Target.Word32.size THEN
       byteOffsetMask := 2_11;
     ELSIF Target.Word.size = Target.Word64.size THEN
        byteOffsetMask := 2_111;
     END;
     wordAddrMask := Word.Not (byteOffsetMask);

     cg.comment ("StoreIndirectStraddling:"); 
    (* This uses a mix of cg.* calls, which manipulate only the M3CG
       stack; SPush and SPop, which manipulate only the CG stack, and
       CG calls, which manipulate them both, keeping them consistent.
       This can get very confusing.  The comments show the contents of
       both stacks, CG first.  Items on the M3CG stack in parentheses
       are conditionally there, and can be put there unconditionally
       by a call on Force, when at the TOS. *) 
    (* On CG stack: word-to-store, AKA ws, and starting byte address, AKA Ba. *)
    (* Initially:                             CG stack   ; M3CG stack *)
    (*                                        --------     ---------- *) 
                                           (* ws.I Ba.A  ; (ws.I) (Ba.A) *)
    ForceStacked ();                       (* ws.I Ba.A  ; ws.I (Ba.A) *)
    cg.loophole (Target.Integer.cg_type, Target.Word.cg_type);
                                           (* ws.W Ba.A  ; ws.W (Ba.A) *)

    (* If requested, mask out garbage bits to the field's left, AKA f.
       This is a static mask. *) 
    field := cg.declare_temp
               (Target.Word.bytes,
                TargetMap.CG_Align_bytes [Target.Word.cg_type],
                Target.Word.cg_type,
                in_memory := FALSE);
    TWord.nBitsOnRight (s, (*OUT*) fieldMask);
    IF PleaseZextField THEN 
      cg.load_integer (Target.Word.cg_type, fieldMask);
                                           (* ws.W Ba.A  ; mask.W f.W (Ba.A) *)
      cg.and (Target.Word.cg_type);        (* ws.W Ba.A  ; f.W (Ba.A) *)
    END; 
    cg.store (field, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (* ws.W Ba.A  ; (Ba) *)
    SPop (1, "");                          (* Ba.A       ; (Ba) *)

    (* Save the byte address, with offset added, in temp byteAddr, AKA Bao. *) 
    ForceStacked ();                       (* Ba.A       ; Ba.A *)
    IF o # 0 THEN
      cg.add_offset (AsBytes(o))           (* Ba+o.A     ; Ba+o.A *)
    END; 
    byteAddr := cg.declare_temp
               (Target.Address.bytes,
                TargetMap.CG_Align_bytes [Type.Addr],
                Type.Addr,
                in_memory := FALSE); 
    cg.store (byteAddr, 0, Type.Addr, Type.Addr);
                                           (* Ba+o.A     ; *)
    SPop (1, "");                          (*            ; *) 

    IF Target.endian = Target.Endian.Little THEN
      (* Compute and save address of full memory word zero in temp word0Addr,
         AKA w0a. *)
      word0Addr := cg.declare_temp
                    (Target.Address.bytes,
                     TargetMap.CG_Align_bytes [Type.Addr],
                     Type.Addr,
                     in_memory := FALSE);
      cg.load (byteAddr, 0, Type.Addr, Type.Addr);
                                           (*            ; Ba+o.A *)
      cg.loophole (Type.Addr, Target.Word.cg_type);
                                           (*            ; Ba+o.W *) 
      cg_load_word (wordAddrMask); (*AKA wam*)
                                           (*            ; wam.W Ba+o.W *)
      cg.and (Target.Word.cg_type);        (*            ; w0a.W *)
      cg.loophole (Target.Word.cg_type, Type.Addr);
                                           (*            ; w0a.A *) 
      cg.store (word0Addr, 0, Type.Addr, Type.Addr);
                                           (*            ; *)

      (* Compute and save bit offset within word zero in temp bitOffset,
         AKA bo. Byte offset is AKA Bo. *) 
      bitOffset := cg.declare_temp
                     (Target.Word.bytes,
                      TargetMap.CG_Align_bytes [Target.Word.cg_type],
                      Target.Word.cg_type,
                      in_memory := FALSE);
      cg.load (byteAddr, 0, Type.Addr, Type.Addr);
                                           (*            ; Ba+o.A *)
      cg.loophole (Type.Addr, Target.Word.cg_type);
                                           (*            ; Ba+o.W *) 
      cg_load_word (byteOffsetMask);       (*            ; Bom.W Ba+o.W *)
      cg.and (Target.Word.cg_type);        (*            ; Bo.W *)
      cg_load_word (Log2OfByte);           (*            ; shiftct.W, Bo.W *)
      cg.shift_left (Target.Word.cg_type); (*            ; bo.W *)
      cg.store (bitOffset, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; *)

      (* Load word zero and mask out the to-be-stored-into bits, AKA w0m. *)
      cg.load (word0Addr, 0, Type.Addr, Type.Addr);
                                           (*            ; w0a.A *)
      (* ^Keep this underneath, for future re-store of word-zero. *) 
      cg.load (word0Addr, 0, Type.Addr, Type.Addr);
                                           (*            ; w0a.A w0a.A *)
      cg.load_indirect (0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; w0.W w0a.A *) 
      cg.load_integer (Target.Word.cg_type, fieldMask);
                                           (*            ; fm.W w0.W w0a.A *)
      cg.load (bitOffset, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; bo.W fm.W w0.W w0a.A *)
      cg.shift_left (Target.Word.cg_type); (*            ; fms.W w0.W w0a.A *)
      cg.not (Target.Word.cg_type);        (*            ; fmsNeg.W w0.W w0a.A *)
      cg.and (Target.Word.cg_type);        (*            ; w0m.W w0a.A *)

      (* Combine components of updated word zero and re-store. *)
      cg.load (field, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; f.W w0m.W w0a.A *)
      cg.load (bitOffset, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; bo.W f.W w0m.W w0a.A *)
      cg.shift_left (Target.Word.cg_type); (*            ; fs.W w0m.W w0a.A *)
      cg.or (Target.Word.cg_type);         (*            ; w0Final.W w0a.A*)
      cg.store_indirect (0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; *)

      (* See if we are done, after storing into only word0 *)
      cg.load (bitOffset, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; bo.W *) 
      cg_load_word (s);                    (*            ; s.W bo.W *)
      cg.add (Target.Word.cg_type);        (*            ; s+bo.W *)
      cg_load_word (Target.Word.size);     (*            ; sw.W s+bo.W *)
      cg.if_compare (Target.Word.cg_type, Cmp.LE, altLab, Maybe); 
                                           (*            ; *)

    (* Take care of contribution to word one. *)
      cg.load (word0Addr, 0, Type.Addr, Type.Addr);
                                           (*            ; w0a.A *)
      (* ^Keep this underneath, for future re-store of word-one. *) 

      (* Rotate original field to right-justify its contribution to word one. *)
      cg.load (field, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; f.W w0a.W *) 
      cg.load (bitOffset, 0, Target.Word.cg_type, Target.Integer.cg_type);
      (* ^!!! cm3cg will crash on rotate_left or rotate_right, if the loaded
              type is not Integer. *)  
                                           (*            ; bo.W f.W w0a.A *) 
      cg.rotate_left (Target.Word.cg_type);(*            ; fr.W w0a.A *) 

      (* Compute a mask for the word-one contribution, AKA w1m. *)
      word1Mask := cg.declare_temp
                     (Target.Word.bytes,
                      TargetMap.CG_Align_bytes [Target.Word.cg_type],
                      Target.Word.cg_type,
                      in_memory := FALSE);
      cg.load_integer (Target.Word.cg_type, TInt.MOne);
                                           (*            ; -1.W fr.W w0a.A *)
      cg_load_word(2*Target.Word.size - s);(*            ; 2wms.W -1.W fr.W w0a.A *)
      cg.load (bitOffset, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; bo.W 2wms.W -1.W fr.W w0a.A *)
      cg.subtract (Target.Integer.cg_type);(*            ; shiftct.W -1.W fr.W w0a.A *)
      cg.shift_right (Target.Word.cg_type);(*            ; w1m.W fr.W w0a.A *)
      cg.store (word1Mask, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; fr.W w0a.A *)

      (* Mask out garbage bits of word-one contribution of rotated field. *) 
      cg.load (word1Mask, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; w1m.W fr.W w0a.A *)
      cg.and (Target.Word.cg_type);        (*            ; w1c.W w0a.A *)

      (* Bit-flip word-one mask. *) 
      cg.load (word1Mask, 0, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; w1m.W w1c.W w0a.A *)
      cg.not (Target.Word.cg_type);        (*            ; w1mNeg.W w1c.W w0a.A *)


      (* Mask out to-be-stored-into bits of word one. *) 
      cg.load (word0Addr, 0, Type.Addr, Type.Addr);
                                           (*            ; w0a.A w1mNeg.W w1c.W w0a.A *)
      cg.load_indirect
        (Target.Word.bytes, Target.Word.cg_type, Target.Word.cg_type);
                                           (*            ; w1.W w1mNeg.W w1c.W w0a.A *)
      cg.and (Target.Word.cg_type);        (*            ; w1m.W w1c.W w0a.A *)

      (* Combine components of updated word one and re-store. *)
      cg.or (Target.Word.cg_type);         (*            ; w1Final.W w0a.A *)
      cg.loophole (Target.Word.cg_type, Target.Integer.cg_type); 
      cg.store_indirect
        (Target.Word.bytes, Target.Integer.cg_type, Target.Integer.cg_type);
                                           (*            ; *)
      (* Merge the word-zero-only path here. *) 
      cg.set_label (altLab);       
    ELSE (* Big endian target. *)
      Err ( "Big endian StoreIndirectStraddling not implemented." ) 
    END (* Endianness. *) 

  END StoreIndirectStraddling; 

(*-------------------------------------------------------------- literals ---*)

PROCEDURE Load_nil () =
  BEGIN
    SPush (Type.Addr);
    cg.load_nil ();
    WITH x = stack [SCheck(1,"Load_nil")] DO
      x.old_align := Target.Address.align;
      x.base_align := Target.Address.align;
      x.base_value_align := Target.Word64.align;
      x.addr_align := Target.Word.align;
    END;
  END Load_nil;

PROCEDURE Load_byte_address (intVal: INTEGER) =
(*push ; s0.A := intVal bytes *)
  BEGIN
    SPush (Type.Addr);
    cg.load_nil ();
    cg.add_offset (intVal);
    WITH x = stack [SCheck(1,"Load_byte_address")] DO
      x.old_align := Target.Byte;
      x.base_align := Target.Address.align;
      x.addr_align := AlignOfInt (intVal);
      x.base_value_align := x.addr_align;
    END;
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

PROCEDURE Load_integer (t: IType;  READONLY tintVal: Target.Int) =
  BEGIN
    SPush (t);
    WITH x = stack[SCheck(1,"Load_integer")] DO
      x.kind := VKind.Integer;
      x.int  := tintVal;
      x.base_value_align := AlignOfTInt (tintVal);
      x.addr_align := x.base_value_align;
    END;
  END Load_integer;

PROCEDURE Load_float (READONLY f: Target.Float) =
  VAR t := TargetMap.Float_types [TFloat.Prec (f)].cg_type;
  BEGIN
    SPush (t);
    WITH x = stack[SCheck(1,"Load_float")] DO
      x.kind  := VKind.Float;
      x.base_align := TargetMap.CG_Align [t];
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
    ForceStacked ();
    cg.negate (t);
    SPop (1, "Negate");
    SPush (t);
  END Negate;

PROCEDURE Abs (t: AType) =
  BEGIN
    ForceStacked ();
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
    ForceStacked ();
    cg.cvt_int (t, u, op);
    SPop (1, "Cvt_int");
    SPush (u);
  END Cvt_int;

PROCEDURE Cvt_float (t: AType;  u: RType) =
  BEGIN
    ForceStacked ();
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
   (* IF s <= target word,
        s2.I := s2.I || {S1.I..S0.I} pop(3)
         --- S2.A must be forced
      ELSE
        s2.A[s1.I..s0.I] := 1...1; pop(3)
         --- S2.A must be forced *)
  BEGIN
    EVAL Force_pair (commute := FALSE);
    IF (s <= Target.Word.size) THEN
      (* Initial value on CG stack, in s2.  Final value will be left in s0. *)
      (* given x, a, b:  compute  x || {a..b} *)

      cg.load_integer (Target.Integer.cg_type, TInt.MOne);
        (* -1 = 16_ffffff = {0..N} *)
      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type);
      cg_load_intt (Target.Integer.size-1);
      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type);
      cg.subtract (Target.Integer.cg_type);
      cg.shift_right (Target.Integer.cg_type);                  (* x, a, {0..b} *)

      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type); (* x, {0..b}, a *)

      cg.load_integer (Target.Integer.cg_type, TInt.MOne);
      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type);
      cg.shift_left (Target.Integer.cg_type);               (* x, {0..b}, {a..N} *)

      cg.and (Target.Integer.cg_type);                      (* x, {a..b} *)
      cg.or (Target.Integer.cg_type); (* s0 is the value *) (* x || {a..b} *)

      SPop (3, "Set_range-a");
      SPush (Target.Integer.cg_type);
    ELSE (* s2 is the *address* of the value. *)
      (* CG stack s2 is the *address* of the value, which will be updated in
         memory, leaving nothing on either stack. *)
      cg.set_range (AsBytes (s), Target.Integer.cg_type);
      SPop (3, "Set_range-b");
    END;
  END Set_range;

PROCEDURE Set_singleton (s: Size) =
  (* IF s <= target word,
       s1.I := s1.I || Word.Shift (1, s0.I); pop(2)
     ELSE
       s1.A [s0.I] := 1; pop(2) *)
  BEGIN
    EVAL Force_pair (commute := FALSE);                         (* cg stack: *)
    IF (s <= Target.Integer.size) THEN (* x is the value *)     (* x, a *)
      (* Initial value is on CG stack, in s2.  Final value will be left in s0. *)
      cg.load_integer (Target.Integer.cg_type, TInt.One);       (* x, a, 1 *)
      cg.swap (Target.Integer.cg_type, Target.Integer.cg_type); (* x, 1 , a *)
      cg.shift_left (Target.Integer.cg_type);                   (* x, 10...0 *)
      cg.or (Target.Integer.cg_type); (* y is the result. *)    (* y *)
      SPop (2, "Set_singleton-a");
      SPush (Target.Integer.cg_type);
    ELSE (* mem is the *addr* of the value (and result) *)      (* mem, a *)
      (* CG stack s1 is the *address* of the value, which will be updated in
         memory, leaving nothing on either stack. *)
      cg.set_singleton (AsBytes (s), Target.Integer.cg_type);
      SPop (2, "Set_singleton-b");
    END;
  END Set_singleton;

(*------------------------------------------ Word.T/Long.T bit operations ---*)

PROCEDURE Not (t: IType) =
  BEGIN
    ForceStacked ();
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
    ForceStacked ();
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
        (* Both values are on the M3CG stack => must swap them there. *)
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
(* Mem[s2.A:s0.I*s] := Mem[s1.A:s0.I*s]; pop(3)
   -- s2.A & s1.A must have byte value-alignment and must have been
   ForceStacked by caller.  Copied data need not end on a byte boundary. *) 
  CONST GMaskToShift = TInt.Int {16_00, 16_FF, ..}; 
  
  VAR t: MType;  z: Size;
  VAR a: INTEGER;
  VAR lhs_align, rhs_align: INTEGER;
  VAR fromAddr, toAddr, ec, toByteAddr, byteCt, extractMask: Val;
  VAR rshift, lshift, ecmask: INTEGER;
  
  BEGIN
    rhs_align := stack_addr_align(2);
    lhs_align := stack_addr_align(3);
    IF lhs_align MOD Target.Byte # 0 THEN
      ErrI (lhs_align, "non-byte-aligned LHS in Copy_n")
    END;
    IF rhs_align MOD Target.Byte # 0 THEN
      ErrI (rhs_align, "non-byte-aligned RHS in Copy_n")
    END;
    a := MIN (rhs_align, lhs_align);
    (* This uses a mix of cg.* calls, which only alter the M3CG stack,
       and CG calls, which manipulate the CG stack and M3CG stack,
       keeping them consistent.  This gets confusing.  The comments
       show the contents of both stacks, CG first.  Items on the M3CG
       stack in parentheses are conditionally there, and can be put
       there unconditionally by a Force. call.  Growth is leftward. *)
    (* Subscript AKA ec, from-address AKA fa, to-address AKA ta. *)
    (*                                          CG stack   ; M3CG stack *)
    (*                                          --------     ---------- *) 
                                             (* ec, fa, ta ; (ec.I), fa.A, ta.A *)
    ForceStacked ();                         (* ec, fa, ta ; ec, fa, ta *)

    IF (s = Target.Byte) THEN
    (* Elements are exactly byte-sized. *) 
      t := AlignedType (s, Target.Byte);
      <*ASSERT TargetMap.CG_Size [t] = Target.Byte*>
      cg.copy_n (Target.Word.cg_type, t, overlap);(* ec, fa, ta ; *)
      SPop (3, "Copy_n, exactly bytes" );    (*            ; *)
    ELSIF (s < Target.Byte) THEN
    (* Partial-byte elements.  Elements are 1, 2, or 4 bits. *)
      cg.comment ("Copy_n, partial-byte elements:"); 
      IF (Target.Byte MOD s) # 0 THEN
        ErrI (s, "CM3 restriction: copy bitsize must be a power of 2 or multiple of 8")
      END;
      t := Target.Word8.cg_type;
      <*ASSERT TargetMap.CG_Size [t] = Target.Byte*>

      CASE s <* NOWARN *> OF
        1 => rshift := 3; ecmask := 2_111; lshift := 0;
      | 2 => rshift := 2; ecmask := 2_11;  lshift := 1;
      | 4 => rshift := 1; ecmask := 2_1;   lshift := 2;
      END;
                                             (* ec, fa, ta ; ec, fa, (ta) *)
      (* Compute number of whole bytes to copy. *) 
      ec := Pop();                           (* fa, ta     ; fa, (ta) *) 
      fromAddr := Pop (); (* Addr. *)        (* ta         ; (ta) *) 
      toAddr := Pop (); (* Addr. *)          (*            ; *) 
      Push (ec);                             (* ec         ; (ec) *) 
      cg_load_word (rshift);                 (* ec         ; rs (ec) *)
      SPush (Target.Word.cg_type);           (* rs, ec     ; rs (ec) *)
      Shift_right (Target.Word.cg_type); (* Whole byte count, AKA Bc. *)
                                             (* Bc         ; Bc*) 
      byteCt := Pop (); (* Whole byte count, Word. *)
                                             (*            ; *) 

      (* Copy whole bytes. *) 
      Push (toAddr); ForceStacked ();        (* ta         ; ta *) 
      Push (fromAddr); ForceStacked ();      (* fa, ta     ; fa, ta *) 
      Push (byteCt); ForceStacked ();        (* Bc, fa, ta ; Bc, fa, ta *) 
      cg.copy_n (Target.Integer.cg_type, Target.Word8.cg_type, overlap);
                                             (* Bc, fa, ta ; *)
      SPop (3, "bitpacked Copy_n #1");       (*            ; *)

      (* Fetch leftover byte of from-array. *)
      Push (fromAddr); ForceStacked ();      (* fa         ; fa *) 
      Push (byteCt); ForceStacked ();        (* Bc, fa     ; Bc, fa *)
      cg.index_address (Target.Word.cg_type, 1);
        (* Address of leftover from byte, AKA fBa. *)
                                             (* Bc, fa     ; fBa *)
      SPop (2, "bitpacked Copy_n #2");       (*            ; fBa *) 
      SPush (Type.Addr);                     (* fBa        ; fBa *) 
      Load_indirect (Target.Word.cg_type, 0, Target.Byte);
      ForceStacked (); (* TOS: Leftover from-byte, AKA fB. *)
                                             (* fB         ; fB *) 

      (* Compute bit mask for extracting elements. *)
      cg.load_integer (Target.Word.cg_type, GMaskToShift);
                                             (* fB         ; ms, fB *) 
      Push (ec); ForceStacked ();            (* ec, fB     ; ec, ms, fB *) 
      cg_load_word (ecmask);                 (* ec, fB     ; ecm, ec, ms, fB *)
      cg.and (Target.Word.cg_type); (* subscript w/in leftover byte, AKA swb *)
                                             (* ec, fB     ; swb, ms, fB *)
      IF lshift > 0 THEN
        cg_load_word (lshift);               (* ec, fB     ; ls, swb, ms, fB *)
        cg.shift_left (Target.Word.cg_type); (* Bit count to be copied, AKA bc. *)
                                             (* ec, fB     ; bc, ms, fB *)
      ELSE (* bc = swb, thus: *)             (* ec, fB     ; bc, ms, fB *)
      END;
      IF Target.endian = Target.Endian.Little THEN
        cg.loophole (Target.Word.cg_type, Target.Integer.cg_type);
        cg.rotate_left (Target.Word.cg_type); (* Extract mask, AKA em. *)
      ELSE
        cg.shift_right (Target.Word.cg_type);(* em *)
      END;                                   (* ec, fB     ; em, fB *)
      SPush (Target.Word.cg_type);           (* em, ec, fB ; em, fB *)
      extractMask := Pop (); (* Word. *)     (* ec, fB     ; fB *)
  
      (* Extract leftover from-bits. *)
      Push (extractMask); ForceStacked ();   (* em, ec, fB ; em, fB *)
      cg.and (Target.Word.cg_type);
      (* from-bits in LSB, possible garbage bits higher, AKA fb.*)
                                             (* em, ec, fB ; fb *)
      SPop (3, "bitpacked Copy_n #3");       (*            ; fb *) 

      (* Fetch leftover to-byte. *)
      Push (toAddr); ForceStacked ();        (* ta         ; ta, fb *) 
      Push (byteCt); ForceStacked ();        (* Bc, ta     ; Bc, ta, fb *) 
      cg.index_address(Target.Word.cg_type, 1);
        (* Address of leftover to-byte, AKA tBa. *) 
                                             (* Bc, ta     ; tBa, fb *)
      SPop (2, "bitpacked Copy_n #4");       (*            ; tBa, fb *)
      SPush (Type.Addr);                     (* tBa        ; tBa, fb *)
      toByteAddr := Pop (); (* Addr *)       (*            ; fb *) 
      Push (toByteAddr); ForceStacked ();    (* tBa        ; tBa, fb*)
      Load_indirect (Target.Word.cg_type, 0, Target.Byte);
                                             (* tB         ; tB, fb *)
      SPop (1, "bitpacked Copy_n #5");       (*            ; tB, fb *) 

      (* Zero to-be-filled bits of to-byte. *)
      Push (extractMask); ForceStacked ();   (* em         ; em, tB, fb *)
      cg.not (Target.Word.cg_type); (* Mask for blanking to byte, AKA bm *)
                                             (* em         ; bm, tB, fb *)
      SPop (1, "bitpacked Copy_n #6");       (*            ; bm, tB, fb *) 
      cg.and (Target.Word.cg_type); (* blanked to-byte, AKA btB *) 
                                             (*            ; btB, fb *)

      (* Combine bits. *)
      cg.or (Target.Word.cg_type); (* byte to store, AKA stB. *) 
                                             (*            ; stB *) 
      SPush (Target.Word.cg_type);           (* stB        ; stB *)

      (* Store leftover to-byte. *)
      Push (toByteAddr); ForceStacked ();    (* tBa, stB   ; tBa, stB *) 
      Swap ();                               (* stB, tBa   ; stB, tBa *) 
      cg.store_indirect (0, Target.Word.cg_type, Target.Word8.cg_type);
                                             (* stB, tBa   ; *) 
      SPop (2, "bitpacked Copy_n #7");       (*            ; *)

      (* Temps used: fromAddr, toAddr, ec, toByteAddr, byteCt, extractMask *) 

    ELSE (* s > Target.Byte *)               (* ec, fa, ta ; ec, fa, (ta) *)
      IF (s MOD Target.Byte) # 0 THEN
        ErrI (s, "CM3 restriction: copy bitsize must be a power of 2 or multiple of 8");
      END;
      t := AlignedType (s, GCD (lhs_align, rhs_align));
      z := TargetMap.CG_Size [t];
      IF (z < s) THEN
        (* convert the count into a multiple of a machine type's size *)
        IF (s MOD z) # 0 THEN ErrI (s, "impossible copy_n size") END;
        cg_load_intt (s DIV z);              (* ec, fa, ta ; es, ec, fa, (ta) *)
        cg.multiply (Target.Integer.cg_type);(* ec, fa, ta ; Bc, fa, (ta) *)
      END;
(* CHECK ^ Force ta? *) 
      cg.copy_n (Target.Integer.cg_type, t, overlap);
                                             (* ec, fa, ta ; *)
      SPop (3, "Copy_n");                    (*            ; *)
    END;
  END Copy_n;

PROCEDURE Copy (s: Size;  overlap: BOOLEAN) =
  VAR a: INTEGER;
  VAR lhs_align, rhs_align: INTEGER;
  VAR t: MType;
  VAR z: Size; 
  BEGIN
    rhs_align := stack_addr_align (1); 
    lhs_align := stack_addr_align (2); 
    a := MIN (stack_addr_align(2), stack_addr_align(1));
    (* ^We can count on both lhs and rhs addresses being 'a'-aligned. *) 
    IF lhs_align MOD Target.Byte # 0
       OR rhs_align MOD Target.Byte # 0
       OR s MOD Target.Byte # 0 THEN (* Must do the copy by fetch and store. *)
      cg.comment ("Copy, non-byte aligned or sized, via load/store:"); 
      IF s > Target.Integer.size THEN
        ErrI (s, "CM3 restriction: non-byte-aligned or non-byte-multiple-sized "
                 & "copy must fit in a word.");
      END; 
      Load_indirect (Target.Word.cg_type, 0, s, rhs_align); 
      Store_indirect (Target.Word.cg_type, 0, s); 
    ELSE (* memory-to-memory copy, byte or larger natural units. *) 
      t := AlignedType (s, GCD (lhs_align, rhs_align));
      z := TargetMap.CG_Size [t];
      IF Force_pair (commute := FALSE) THEN Swap (); END;
      IF (s MOD z) # 0 THEN ErrI (s, "impossible copy size") END;
      cg.copy (s DIV z, t, overlap);
      SPop (2, "Copy");
    END; 
  END Copy;

PROCEDURE Zero (s: Size) =
(* Mem[s0.A:s] := 0; pop(1) *)
  VAR
    a := stack_addr_align (1);
    t := AlignedType (s, a);
    z := TargetMap.CG_Size [t];
  BEGIN
    ForceStacked ();
    IF (s MOD z) # 0 THEN ErrI (s, "impossible zero size") END;
    cg.zero (s DIV z, t);
    SPop (1, "Zero");
  END Zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE Loophole (from, to: Type) =
  BEGIN
    ForceStacked ();
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
    ForceStacked ();
    cg.check_nil (code);
  END Check_nil;

PROCEDURE Check_lo (t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    ForceStacked ();
    cg.check_lo (t, i, code);
  END Check_lo;

PROCEDURE Check_hi (t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    ForceStacked ();
    cg.check_hi (t, i, code);
  END Check_hi;

PROCEDURE Check_range (t: IType;  READONLY a, b: Target.Int;
                       code: RuntimeError) =
  BEGIN
    EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
    ForceStacked ();
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
(* Emit RT code to ensure s0.a has byte addr_align. *) 
  VAR extra_bits: Var;  extra_is_temp: BOOLEAN;
  BEGIN
(* REVIEW: What VKinds can happen here? *) 
    WITH x = stack [SCheck (1, "Check_byte_aligned")] DO
      IF x.type # Type.Addr THEN
        Err ("Check_byte_aligned called for non-address.");
      END;
      IF (x.addr_align MOD Target.Byte) # 0 THEN
        Err ("unaligned addr_align in Check_byte_aligned.");
      ELSIF (x.offset MOD Target.Byte) # 0 THEN
        Err ("address's offset is not byte aligned");
      ELSIF (x.bits # NIL) THEN
        extra_bits := x.bits;   extra_is_temp := x.temp_bits;
        x.bits := NIL;          x.temp_bits := FALSE;
        EVAL RunTyme.LookUpProc (RunTyme.Hook.Abort);
        cg.load (extra_bits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
        cg_load_intt (Target.Byte - 1);
        cg.and (Target.Integer.cg_type);
        cg.load_integer (Target.Integer.cg_type, TInt.Zero);
        cg.check_eq (Target.Integer.cg_type, RuntimeError.UnalignedAddress);
        Boost_addr_alignment (Target.Byte);
        ForceStacked ();
        cg.load (extra_bits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
        cg_load_intt (Target.Byte);
        cg.div (Target.Integer.cg_type, Sign.Unknown, Sign.Positive);
        cg.index_address (Target.Integer.cg_type, 1);
        IF (extra_is_temp) THEN Free_temp (extra_bits); END;
      END;
    END;
  END Check_byte_aligned;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE Add_offset (i: INTEGER) =
(* s0.A := s0.A + i bits *)
  BEGIN
    WITH x = stack [SCheck (1, "Add_offset")] DO
      IF (x.type # Type.Addr) THEN
        Err ("add_offset on non-address");
        ForceStacked ();
      ELSE
        IF i = 0 THEN RETURN END;
        CASE x.kind OF
        | VKind.Stacked =>
          x.kind := VKind.Pointer;
          x.offset := i;
          x.addr_align := GCD (x.addr_align, i); 
        | VKind.Direct =>
          ForceStacked ();
          x.kind := VKind.Pointer;
          x.offset := i;
          x.addr_align := GCD (x.addr_align, i); 
        | VKind.Absolute, VKind.Indirect, VKind.Pointer =>
          INC (x.offset, i);
          x.addr_align := GCD (x.addr_align, i); 
        ELSE
          Err ("add_offset on non-address form");
          ForceStacked ();
        END;
      END;
    END;
  END Add_offset;

PROCEDURE EnsureTempBase (t: Type; VAR x: ValRec) =
  (* Ensure x has a base and it is a temp.  Its runtime contents will
     be uninitialized. *) 
  BEGIN
    IF x.base = NIL OR NOT x.temp_base THEN
      x.base := Declare_temp
        (TargetMap.CG_Size[t], TargetMap.CG_Align[t], t, in_memory := FALSE);
      x.base_align := TargetMap.CG_Align[t];
      x.temp_base := TRUE;
    END 
  END EnsureTempBase; 

PROCEDURE Index_bytes (size: INTEGER) =
(* s1.A := s1.A + s0.I * size ; pop -- size must be a multiple of
   Target.Byte. *)
  VAR old_align, addr_align: Alignment;
  VAR index: Val; 
  BEGIN
    <* ASSERT size MOD Target.Byte = 0 *>
    WITH addr = stack [SCheck (2, "Index_bytes")] DO
      <* ASSERT addr.type = Type.Addr *> 
      old_align := addr.old_align;
      addr_align := addr.addr_align;
      ForceStacked (); (* The index *)
      CASE (addr.kind) OF

      | VKind.Stacked, VKind.Pointer => (* Address already on cg stack *) 
        cg.index_address (Target.Integer.cg_type, AsBytes (size));
        SPop (1, "Index_bytes, Stacked|Pointer");
        RETURN
        
      | VKind.Direct =>
        index := Pop ();
        <* ASSERT index.offset = 0 AND index.bits = NIL *>
        ForceStacked (); (* The starting address, now stacked. *) 
        cg.load (index.base, 0, Target.Integer.cg_type, Target.Integer.cg_type);
        cg.index_address (Target.Integer.cg_type, AsBytes (size));
        EnsureTempBase (Type.Addr, addr); 
        cg.store (addr.base, 0, Type.Addr, Type.Addr);
        addr.kind := VKind.Indirect;
        addr.offset := 0;
        addr.bits := NIL; 
        addr.temp_bits := FALSE;  

      | VKind.Absolute =>
        index := Pop ();
        <* ASSERT index.offset = 0 AND index.bits = NIL *>
        cg.load_address (addr.base, AsBytes (addr.offset));
        cg.load (index.base, 0, Target.Integer.cg_type, Target.Integer.cg_type);
        cg.index_address (Target.Integer.cg_type, AsBytes (size));
        EnsureTempBase (Type.Addr, addr); 
        cg.store (addr.base, 0, Type.Addr, Type.Addr);
        addr.kind := VKind.Indirect;
        addr.offset := 0;
        (* Leave bits unchanged. *) 

      | VKind.Indirect =>
        index := Pop ();
        <* ASSERT index.offset = 0 AND index.bits = NIL *>
        cg.load (addr.base, 0, Type.Addr, Type.Addr);
        cg.load (index.base, 0, Target.Integer.cg_type, Target.Integer.cg_type);
        cg.index_address (Target.Integer.cg_type, AsBytes (size));
        EnsureTempBase (Type.Addr, addr); 
        cg.store (addr.base, 0, Type.Addr, Type.Addr);
        (* Leave offset and bits unchanged. *) 
        
      ELSE <* ASSERT FALSE *> 
      END;
      addr.old_align := GCD (addr.old_align, size);
      addr.addr_align := GCD (addr.addr_align, size);
    END;
  END Index_bytes;

PROCEDURE Index_bits (bits_addr_align: Alignment := 1) =
(* s1.A := s1.A + s0.I ; pop -- note that s0.I must be less than
  or equal to the alignment of s1.A, otherwise bad code will be generated. *)
(* TODO: Remove this restriction by moving code from ArrayType.GenIndex in here,
         and removing it from any other client sites. *)
  VAR index := Pop_temp (); (* Direct, no offset or bits. *) 
  BEGIN
    WITH x = stack [SCheck (1, "Index_bits")] DO
      <* ASSERT index.temp_base *> 
      IF x.bits # NIL THEN
        cg.load (index.base, 0, Target.Integer.cg_type, Target.Integer.cg_type);
        cg.load (x.bits, 0, Target.Integer.cg_type, Target.Integer.cg_type);
        cg.add (Target.Integer.cg_type); 
        cg.store (index.base, 0, Target.Integer.cg_type, Target.Integer.cg_type);
      END;
      IF (x.kind = VKind.Stacked) THEN x.kind := VKind.Pointer; END;
      x.bits := index.base;
      x.temp_bits := TRUE;
      x.addr_align := GCD (x.addr_align, bits_addr_align);
      index.temp_base := FALSE; 
      Free (index);
    END;
  END Index_bits;

PROCEDURE Boost_addr_alignment (a: Alignment) =
  BEGIN
    WITH x = stack [SCheck (1, "Boost_addr_alignment")] DO
      x.old_align := MAX (x.old_align, a);
      x.addr_align := MAX (x.addr_align, a);
      IF x.addr_align > x.base_value_align THEN
        x.base_value_align := x.addr_align;
      END;
    END;
  END Boost_addr_alignment;

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
    ForceStacked ();
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
    ForceStacked ();
    cg.pop_param (t);
    SPop (1, "Pop_param");
    SEmpty ("Pop_param");
  END Pop_param;

PROCEDURE Pop_struct (t: TypeUID;  s: Size;  a: Alignment) =
  BEGIN
    ForceStacked (s);
    cg.pop_struct (t, ToBytes (s), ByteAlign (a));
    SPop (1, "Pop_struct");
    SEmpty ("Pop_struct");
  END Pop_struct;

PROCEDURE Pop_static_link () =
  BEGIN
    ForceStacked ();
    cg.pop_static_link ();
    SPop (1, "Pop_static_link");
  END Pop_static_link;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE ProcAlign ( ): Alignment =
  BEGIN
    IF Target.Aligned_procedures THEN RETURN Target.Integer.align;
    ELSE RETURN Target.Byte;
    END; 
  END ProcAlign; 

PROCEDURE Load_procedure (p: Proc) =
  BEGIN
    cg.load_procedure (p);
    SPush (Type.Addr, ProcAlign());
    
  END Load_procedure;

PROCEDURE Load_static_link (p: Proc) =
  BEGIN
    cg.load_static_link (p);
    SPush (Type.Addr, Target.Address.align);
  END Load_static_link;

(*------------------------------------------------ builtin type operations --*)

PROCEDURE Ref_to_object_hdr () =
  BEGIN
    Boost_addr_alignment (Target.Address.align);
    Load_indirect
      (Target.Integer.cg_type, -Target.Address.pack, Target.Address.size,
       Target.Address.align);
  END Ref_to_object_hdr;

PROCEDURE Hdr_to_info (offset, size: INTEGER) =
  VAR base: INTEGER;
  BEGIN
    ForceStacked ();
    IF Target.endian = Target.Endian.Little
      THEN base := offset;
      ELSE base := Target.Integer.size - offset - size;
    END;
    cg.extract_mn (Target.Integer.cg_type, FALSE, base, size);
    (* REVIEW: No updates to CG stack, whose properties have changed? *) 
  END Hdr_to_info;

PROCEDURE Ref_to_info (offset, size: INTEGER) =
  BEGIN
    Ref_to_object_hdr ();
    Hdr_to_info (offset, size);
  END Ref_to_info;

(*------------------------------------------------------------ open arrays --*)

PROCEDURE Open_elt_ptr (a: Alignment) =
  BEGIN
    Boost_addr_alignment (Target.Address.align);
    Load_indirect
      (Type.Addr, M3RT.OA_elt_ptr, Target.Address.size, a);
    WITH x = stack [SCheck (1, "Open_elt_ptr")] DO
(* Review: does Load_indirect take care of any of the below? *)
      x.old_align := a;
      x.base_align := Target.Address.align;
      x.base_value_align := a;
      x.addr_align := a;
    END;
  END Open_elt_ptr;

PROCEDURE Open_size (n: INTEGER) =
  BEGIN
    Boost_addr_alignment (Target.Address.align);
    Load_indirect (Target.Integer.cg_type,
                   M3RT.OA_sizes + n * Target.Integer.pack, Target.Integer.size);
  END Open_size;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE If_closure (proc: Val;  true, false: Label;  freq: Frequency) =
  VAR skip := Next_label ();  nope := skip;
  BEGIN
    IF (false # No_label) THEN  nope := false; END;
    IF NOT Target.Aligned_procedures THEN
      Push (proc);
      ForceStacked ();
      cg.loophole (Type.Addr, Target.Integer.cg_type);
      cg_load_intt (TargetMap.CG_Align_bytes[Target.Integer.cg_type] - 1);
      cg.and (Target.Integer.cg_type);
      cg.load_integer (Target.Integer.cg_type, TInt.Zero);
      cg.if_compare (Target.Integer.cg_type, Cmp.NE, nope, Always - freq);
      SPop (1, "If_closure-unaligned");
    END;
    Push (proc);
    Boost_addr_alignment (Target.Address.align);
    ForceStacked ();
    cg.load_nil ();
    cg.if_compare (Type.Addr, Cmp.EQ, nope, Always - freq);
    Push (proc);
    Boost_addr_alignment (Target.Address.align);
    Load_indirect (Target.Integer.cg_type, M3RT.CL_marker, Target.Integer.size);
    cg_load_intt (M3RT.CL_marker_value);
    IF (true # No_label)
      THEN cg.if_compare (Target.Integer.cg_type, Cmp.EQ, true, freq);
      ELSE cg.if_compare (Target.Integer.cg_type, Cmp.NE, false, freq);
    END;
    Set_label (skip);
    SPop (2, "If_closure");
  END If_closure;

PROCEDURE Closure_proc () =
  BEGIN
    Boost_addr_alignment (Target.Address.align);
    Load_indirect (Type.Addr, M3RT.CL_proc, Target.Address.size, ProcAlign ());
  END Closure_proc;

PROCEDURE Closure_frame () =
  BEGIN
    Boost_addr_alignment (Target.Address.align);
    Load_indirect
      (Type.Addr, M3RT.CL_frame, Target.Address.size, Target.Address.align);
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
    EVAL ForceStacked2 ("Store_ordered", commute := FALSE);
    cg.store_ordered (StackType[t], t, order);
  END Store_ordered;

PROCEDURE Load_ordered (t: MType;  order: MemoryOrder) =
  BEGIN
    ForceStacked1 ("Load_ordered");
    cg.load_ordered (t, StackType[t], order);
    SPush (StackType[t]);
  END Load_ordered;

PROCEDURE Exchange (t: MType;  order: MemoryOrder) =
  BEGIN
    EVAL ForceStacked2 ("Exchange", commute := FALSE);
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
    EVAL ForceStacked2 ("Fetch_and_op", commute := FALSE);
    cg.fetch_and_op (op, t, StackType[t], order);
    SPush (StackType[t]);
  END Fetch_and_op;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE ByteAlign (a: Alignment): Alignment =
  BEGIN
    RETURN MAX (a, Target.Byte) DIV Target.Byte;
  END ByteAlign;

PROCEDURE AlignedType (s: Size;  a: Alignment): MType =
  (* Largest target integer type whose size evenly divides s and whose
     alignment evenly divides a.  Generate CG error, if none exists. *) 
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
    Err ("UnalignedType, no possible type:  s/a=" & Fmt.Int (s) & "/" & Fmt.Int (a));
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
      align   := ByteAlign (a);
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
    IF (x * Target.Byte # n) THEN ErrI (n, "non-byte-aligned offset") END;
    RETURN  x;
  END AsBytes;

PROCEDURE cg_load_intt (i: INTEGER) =
(* Push host integer i on the M3CG stack, with target type Integer.
  No action on CG stack. *) 
  VAR val: Target.Int;  b := TInt.FromInt (i, val);
  BEGIN
    IF NOT b
      OR TInt.LT (val, Target.Integer.min)
      OR TInt.LT (Target.Integer.max, val)
    THEN ErrI (i, "integer not representable") END;
    cg.load_integer (Target.Integer.cg_type, val);
  END cg_load_intt;

PROCEDURE cg_load_word (i: INTEGER) =
(* Push host integer i on the M3CG stack, with target type Word.
  No action on CG stack. *) 
  VAR val: Target.Int;  b := TInt.FromInt (i, val);
  BEGIN
    IF NOT b
      OR TInt.LT (val, Target.Integer.min)
      OR TInt.LT (Target.Integer.max, val)
    THEN ErrI (i, "integer not representable") END;
    cg.load_integer (Target.Word.cg_type, val);
  END cg_load_word;

PROCEDURE cg_load_addr (addr: INTEGER) =
(* push addr on the M3CG stack, with type Address.
   No action on CG stack. *) 
  VAR val: Target.Int;  b := TInt.FromInt (addr, val);
  BEGIN
    IF NOT b
      OR TInt.LT (val, Target.Integer.min)
      OR TInt.LT (Target.Integer.max, val)
    THEN ErrI (addr, "integer address not representable") END;
    cg.load_integer (Target.Word.cg_type, val);
    cg.loophole (Target.Word.cg_type, Type.Addr); 
  END cg_load_addr;

PROCEDURE Force_pair (commute: BOOLEAN): BOOLEAN =
  (* Returns TRUE if it leaves the items are stacked in the wrong order *)
  VAR s1 := stack [SCheck (1, "Force_pair")].kind = VKind.Stacked;
  VAR s2 := stack [SCheck (2, "Force_pair")].kind = VKind.Stacked;
  BEGIN
    IF s2 THEN (* deeper element is already stacked *)
        IF NOT s1 THEN (* only the top element needs to become stacked *)
            ForceStacked ();
        END;
        RETURN FALSE;
    ELSIF s1 THEN (* only the deeper element needs to become stacked *)
        Swap ();
        ForceStacked ();
        IF NOT commute THEN Swap (); END;
    ELSE (* neither element is stacked *)
        IF NOT commute THEN Swap (); END;
        ForceStacked ();
        Swap ();
        ForceStacked ();
    END;
    RETURN commute;
  END Force_pair;

PROCEDURE stack_old_align (n: INTEGER): INTEGER =
  BEGIN
    RETURN stack [SCheck (n, "stack_addr_align")].old_align; 
  END stack_old_align;

PROCEDURE stack_addr_align (n: INTEGER): INTEGER =
  BEGIN
    RETURN stack [SCheck (n, "stack_addr_align")].addr_align; 
  END stack_addr_align;

PROCEDURE SLV_align (n: INTEGER): INTEGER =
  BEGIN
    RETURN LV_align (stack [SCheck (n, "SLV_align")]);
  END SLV_align;

PROCEDURE LV_align (READONLY x: ValRec): INTEGER =
  (* Largest alignment x is statically sure to have. *) 
  VAR align := x.old_align;
  BEGIN
    IF (x.offset # 0) THEN align := GCD (align, x.offset) END;
    IF (x.bits # NIL) THEN
      align := 1
(* TODO: Probably: align := GCD (align, x.bits.old_align? *) 
    END;
    RETURN align;
  END LV_align;

PROCEDURE GCD (a, b: INTEGER): INTEGER =
  VAR c: INTEGER;
  BEGIN
    a := ABS (a);
    b := ABS (b);
    IF (b = 0) THEN RETURN a END;
    LOOP
      c := a MOD b;
      IF (c = 0) THEN RETURN b END;
      a := b; b := c;
    END;
  END GCD;

PROCEDURE MustFindIntType
  (t: Type;  s: Size;  o: Offset;  base_align: Alignment): MType =
  (* base_align is what the caller guarantees to give the returned type. *) 
  VAR best_t : Type;
  BEGIN
    best_t := MayFindIntType (t, s, o, base_align ); 
    IF best_t = Type.Void THEN
      best_t := t;
      Err ("unable to find integer type?  type=" & Target.TypeNames[t]
            & "  size/offset/align=" & Fmt.Int (s) & "/" & Fmt.Int (o) & "/" & Fmt.Int (base_align));
    END;
    RETURN best_t;
  END MustFindIntType;

PROCEDURE MayFindIntType
  (t: Type;  s: Size;  o: Offset;  base_align: Alignment): Type =
  (* base_align is what the caller guarantees to give the returned type. *) 
  VAR best_t : Type;
  BEGIN
    IF Target.SignedType [t]
      THEN best_t := ScanTypes (TargetMap.Integer_types, t, s, o, base_align);
      ELSE best_t := ScanTypes (TargetMap.Word_types, t, s, o, base_align);
    END;
    RETURN best_t;
  END MayFindIntType;

PROCEDURE ScanTypes (READONLY Types: ARRAY [0..3] OF Target.Int_type;
                     t: Type;  s: Size;  o: Offset;  base_align: Alignment): Type =
  (* From Types, find and return a CG type 'tt', with that satisfies:
     1) align(tt) evenly divides base_align (thus align(tt) <= base_align)
     2) A field of size(tt) and aligned align(tt) will fully contain a field
        of size s, that starts at offset o from a point with alignment
        base_align (thus size(tt) >= s).
     3) size(tt) <= size(t)
     4) align(tt) <= align(t)
     5) no other GC type in Types has smaller size or alignment.
  *) 
  VAR
    best_s := TargetMap.CG_Size [t] + 1;
    best_a := TargetMap.CG_Align [t] + 1;
    best_t := Type.Void;
  BEGIN
    FOR i := FIRST (Types) TO LAST (Types) DO
      WITH z = Types[i] DO
        IF (z.size < best_s) (* size is better than we've previously found. *)
          AND (z.align <= best_a) (* alignment is no worse. *)  
          AND (s <= z.size) (* It's big enough for the field. *) 
          AND (base_align MOD z.align = 0)
              (* Its alignment is satisfied by caller. *) 
          AND (s + (o MOD z.align) <= z.size) (* The offset field fits. *)
        THEN (* remember this type *)
          best_t := z.cg_type;
          best_s := z.size;
          best_a := z.align;
        END;
      END;
    END;
    RETURN best_t;
  END ScanTypes;

PROCEDURE AlignOfInt (IntVal: INTEGER): Alignment =
  VAR I : INTEGER;
  VAR TrialAlign: Alignment;
  BEGIN
    I := LAST (TargetMap.Word_types);
    LOOP
      IF I < FIRST (TargetMap.Word_types) THEN RETURN 1; END;
      TrialAlign := TargetMap.Word_types [I].align;
      IF IntVal MOD TrialAlign = 0 THEN RETURN TrialAlign; END;
      DEC (I);
    END; 
  END AlignOfInt; 

PROCEDURE AlignOfTInt (IntVal: Target.Int): Alignment =
  VAR I : INTEGER;
  VAR TrialAlign: Alignment;
  VAR TrialTInt, ModVal: Target.Int; 
  BEGIN
    I := LAST (TargetMap.Word_types);
    LOOP
      IF I < FIRST (TargetMap.Word_types) THEN RETURN 1; END;
      TrialAlign := TargetMap.Word_types [I].align;
      IF TInt.FromInt (TrialAlign, (*OUT*)TrialTInt)
         AND TInt.Mod (IntVal, TrialTInt, (*OUT*)ModVal)
         AND ModVal = TInt.Zero THEN
        RETURN TrialAlign;
      END;
      DEC (I);
    END; 
  END AlignOfTInt; 

PROCEDURE SPush (t: Type; addr_align: Alignment := Target.Word8.align) =
  BEGIN
    WITH x = stack[SCheck(0,"SPush")] DO
      x.kind      := VKind.Stacked;
      x.type      := StackType[t];
      x.temp_base := FALSE;
      x.temp_bits := FALSE;
      x.old_align := TargetMap.CG_Align[t] (*Was Target.Byte*);
      x.base_align:= TargetMap.CG_Align[x.type];
      x.base_value_align:= addr_align;
      x.addr_align:= addr_align;
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
    IF tos < n THEN
      ErrI (n, "CG.SCheck: stack underflow in " & tag); RETURN 0;
    ELSIF tos - n > LAST (stack) THEN
      ErrI (n, "CG.SCheck: stack overflow in " & tag); RETURN 0;
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
        msg := msg & Target.TypeNamesFixedWidth [x.type];
        msg := msg & Bool [x.temp_base];
        msg := msg & Bool [x.temp_bits];
        msg := msg & Fmt.Int (x.old_align) & " ";
        msg := msg & Fmt.Int (x.base_align) & " ";
        msg := msg & Fmt.Int (x.addr_align) & " ";
        msg := msg & Fmt.Int (x.base_value_align) & " ";
        msg := msg & Fmt.Int (x.offset);
        cg.comment (msg);
      END;
    END;
    cg.comment ("------------- end stack dump -------------");
  END SDump;

PROCEDURE SEmpty (tag: TEXT) =
  BEGIN
    IF (tos > 0) THEN
      ForceStacked ();
      ErrI (tos, "stack not empty, depth");
      SDump (tag);
    END;
  END SEmpty;

PROCEDURE Log2OfAlign (align: Alignment): INTEGER =
  (* PRE: align in {8, 16, 32, 64} *)
  BEGIN
    CASE align OF
    | 8 => RETURN 3;
    | 16 => RETURN 4;
    | 32 => RETURN 5;
    | 64 => RETURN 6;
    ELSE RETURN -1;
    END; 
  END Log2OfAlign; 

PROCEDURE ReduceBits (align: Alignment) : Var =
  (* PRE: TOS.type = Addr. *)
  (* POST: Result is a temp var containing, at RT, count of bits
           within align.  It needs to be freed. *) 
  (* Gen RT code to reduce TOS.bits to < align and compensate
     by increasing the Byte-aligned component of TOS. *) 
  VAR bitCt: INTEGER;
  VAR bitsMask: Target.Int;
  VAR save_bits: Var;
  VAR new_bits: Var := NIL;
  BEGIN
    WITH AddrVal = stack [SCheck (1, "ReduceBits1")] DO
      save_bits := AddrVal.bits;
      <* ASSERT AddrVal.type = Type.Addr *> 
      AddrVal.bits := NIL;
    END;
    ForceStacked (); (* The whole-byte portion onto the cg stack. *) 
    IF save_bits # NIL THEN 
      WITH AddrVal = stack [SCheck (1, "ReduceBits2")] DO
        new_bits := Declare_temp (Target.Word.size, Target.Word.align,
                Target.Word.cg_type, in_memory:= FALSE);
        bitCt := Log2OfAlign (align); 
        TWord.nBitsOnRight (bitCt, (*OUT*) bitsMask);
        cg.load (save_bits, 0, Target.Word.cg_type, Target.Word.cg_type);
        cg_load_intt (bitCt); 
        cg.shift_right(Target.Word.cg_type);
        cg.index_address (Target.Word.cg_type, align DIV Target.Byte);

        cg.load (save_bits, 0, Target.Word.cg_type, Target.Word.cg_type);
        cg.load_integer (Target.Word.cg_type, bitsMask);
        cg.and (Target.Word.cg_type);
        cg.store (new_bits, 0, Target.Word.cg_type, Target.Word.cg_type);
        AddrVal.kind := VKind.Pointer; 
        AddrVal.bits := new_bits;
      END; 
    END;
    RETURN new_bits; 
  END ReduceBits; 
VAR Log2OfByte : INTEGER := 3; 

BEGIN
  <* ASSERT Word.LeftShift (1,Log2OfByte) = Target.Byte *>
  debug := RTParams.IsPresent ("m3front-debug-cg");
END CG.

