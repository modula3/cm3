(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 20 16:03:47 PDT 1995 by kalsow     *)
(*      modified on Tue Jun  1 14:56:30 PDT 1993 by muller     *)

MODULE M3CG_Check;

IMPORT Wr, Thread, Fmt, Text, Stdio, IntIntTbl;
IMPORT M3ID, M3CG, M3CG_Ops, Target, TargetMap;

FROM M3CG IMPORT Name, ByteOffset, CallingConvention;
FROM M3CG IMPORT ByteSize, Alignment, Frequency, RuntimeError;
FROM M3CG IMPORT Var, Proc, Label, Sign, CompareOp, ConvertOp, AtomicOp;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT MemoryOrder, TypeUID;

TYPE (* stack data types *)
  ST = { Addr, Int32, Int64, Reel, LReel, XReel, Void,
         IType, RType, AType,
         Any, Missing, DontCare, Match };

VAR (* CONST after "begin_unit" call *)
  T_to_ST := ARRAY Type OF ST {ST.Void,..};

CONST
  ST_name = ARRAY ST OF TEXT {
    "Addr ", "Int32", "Int64", "Real ", "LReal ", "ExReal ", "Void ",
    "W,I ", "R,L,E ", "W,I,R,L,E ",
    "any ", "", "", "<=match "
  };

TYPE
  U = M3CG.T OBJECT
        clean_stores := FALSE;
        clean_jumps  := FALSE;
        nested_calls := TRUE;
        nested_procs := FALSE;
        cur_line     : INTEGER := 0;
        next_var     := 1;
        next_proc    := 1;
        next_scope   := 1;
        n_errors     := 0;
        proc_count   := 0;
        block_count  := 0;
        call_count   := 0;
        top_of_stack := 0;
        in_init      := 0;
        init_cursor  := 0;
        note_error   : M3CG_Ops.ErrorHandler := NIL;
        runtime      : IntIntTbl.T := NIL;  (* Name -> BOOL *)
(*        temps        : IntIntTbl.T  := NIL; (* Var -> line number *) *)
        stack        : ARRAY [0..50] OF Type;
      METHODS
        s_pop (s0, s1, s2, s3 := ST.DontCare) := Stack_Pop;
        s_push (t: Type) := Stack_Push;
        s_repush () := Stack_Repush;
        s_empty () := Stack_Empty;
      OVERRIDES
        set_error_handler := set_error_handler;
        begin_unit := begin_unit;
        end_unit   := end_unit;
        set_source_line := set_source_line;
        set_runtime_proc := set_runtime_proc;
        bind_segment := bind_segment;
        declare_temp   := declare_temp;
        free_temp := free_temp;
        widechar_size := widechar_size;
        begin_init := begin_init;
        end_init := end_init;
        init_int := init_int;
        init_proc := init_proc;
        init_label := init_label;
        init_var := init_var;
        init_offset := init_offset;
        init_chars := init_chars;
        init_float := init_float;
        begin_procedure := begin_procedure;
        end_procedure := end_procedure;
        begin_block := begin_block;
        end_block := end_block;
        note_procedure_origin := note_procedure_origin;
        set_label := set_label;
        jump := jump;
        if_true  := if_true;
        if_false := if_false;
        if_compare := if_compare;
        case_jump := case_jump;
        exit_proc := exit_proc;
        load  := load;
        store := store;
        load_address := load_address;
        load_indirect := load_indirect;
        store_indirect := store_indirect;
        load_nil      := load_nil;
        load_integer  := load_integer;
        load_float    := load_float;
        compare  := compare;
        add      := add;
        subtract := subtract;
        multiply := multiply;
        divide   := divide;
        div      := div;
        mod      := mod;
        negate   := negate;
        abs      := abs;
        max      := max;
        min      := min;
        cvt_int  := cvt_int;
        cvt_float := cvt_float;
        set_union          := set_union;
        set_difference     := set_difference;
        set_intersection   := set_intersection;
        set_sym_difference := set_sym_difference;
        set_member         := set_member;
        set_compare  := set_compare;
        set_range    := set_range;
        set_singleton := set_singleton;
        not := not;
        and := and;
        or  := or;
        xor := xor;
        shift        := shift;
        shift_left   := shift_left;
        shift_right  := shift_right;
        rotate       := rotate;
        rotate_left  := rotate_left;
        rotate_right := rotate_right;
        widen := widen;
        chop := chop;
        extract := extract;
        extract_n := extract_n;
        extract_mn := extract_mn;
        insert  := insert;
        insert_n  := insert_n;
        insert_mn  := insert_mn;
        swap := swap;
        pop  := pop;
        copy := copy;
        copy_n := copy_n;
        zero := zero;
        zero_n := zero_n;
        loophole := loophole;
        abort := abort;
        check_nil := check_nil;
        check_lo := check_lo;
        check_hi := check_hi;
        check_range := check_range;
        check_index := check_index;
        check_eq := check_eq;
        add_offset := add_offset;
        index_address := index_address;
        start_call_direct := start_call_direct;
        call_direct := call_direct;
        start_call_indirect := start_call_indirect;
        call_indirect := call_indirect;
        pop_param := pop_param;
        pop_struct := pop_struct;
        pop_static_link := pop_static_link;
        load_procedure := load_procedure;
        load_static_link := load_static_link;
        store_ordered := store_ordered;
        load_ordered := load_ordered;
        exchange := exchange;
        compare_exchange := compare_exchange;
        fence := fence;
        fetch_and_op := fetch_and_op;
      END;


(*----------------------------------------------- binary/ASCII conversion ---*)


VAR Ints := ARRAY [0..1023] OF TEXT { NIL, .. };

PROCEDURE Int (i: INTEGER): TEXT =
  BEGIN
    IF (FIRST (Ints) <= i) AND (i <= LAST (Ints)) THEN
      IF (Ints[i] = NIL) THEN Ints [i] := " " & Fmt.Int (i) END;
      RETURN Ints [i];
    ELSE
      RETURN " " & Fmt.Int (i);
    END;
  END Int;

(*--------------------------------------------------------- low level I/O ---*)

PROCEDURE PutErr (u: U;  a, b, c: TEXT := NIL) =
  BEGIN
    u.note_error ("********* M3CG_Check ERROR *********** " & a & b & c);
    u.child.comment ("********* M3CG_Check ERROR *********** ", a, b, c);
    INC (u.n_errors);
  END PutErr;

(*-------------------------------------------------------- stack checking ---*)

PROCEDURE Stack_Get (self: U;  depth: INTEGER): ST =
  VAR x := self.top_of_stack - depth - 1;
  BEGIN
    IF (FIRST (self.stack) <= x) AND (x <= LAST (self.stack))
      THEN RETURN T_to_ST [self.stack [x]];
      ELSE RETURN ST.Missing;
    END;
  END Stack_Get;

PROCEDURE IsOK (need, got, prev: ST): BOOLEAN =
  BEGIN
    CASE need OF
    | ST.IType =>
        RETURN (T_to_ST [FIRST (IType)] <= got) AND (got <= T_to_ST [LAST (IType)]);
    | ST.RType =>
        RETURN (T_to_ST [FIRST (RType)] <= got) AND (got <= T_to_ST [LAST (RType)]);
    | ST.AType =>
        RETURN (T_to_ST [FIRST (AType)] <= got) AND (got <= T_to_ST [LAST (AType)]);
    | ST.Any =>
        RETURN (T_to_ST [FIRST (Type)]  <= got) AND (got <= T_to_ST [LAST (Type)]);
    | ST.Void     => RETURN (got = ST.Missing);
    | ST.DontCare => RETURN TRUE;
    | ST.Match    => RETURN got = prev;
    ELSE             RETURN (got = need);
    END;
  END IsOK;

PROCEDURE ST_Name (a, prev: ST): TEXT =
  BEGIN
    IF (a = ST.Match) THEN a := prev END;
    RETURN ST_name [a];
  END ST_Name;

PROCEDURE Stack_Pop (self: U;  a, b, c, d: ST) =
  VAR
    s0 := Stack_Get (self, 0);
    s1 := Stack_Get (self, 1);
    s2 := Stack_Get (self, 2);
    s3 := Stack_Get (self, 3);
  BEGIN
    IF IsOK (a, s0, a) AND IsOK (b, s1, a)
      AND IsOK (c, s2, b) AND IsOK (d, s3, c) THEN
      (* no error *)
    ELSE
      PutErr (self, "bad stack:  expected [ ",
        ST_Name (a, a) & " " & ST_Name (b, a) & " "
        & ST_Name (c, b) & " " & ST_Name (d, c),
        " ] got [ " &
        ST_Name (s0, s0) & " " & ST_Name (s1, s0) & " "
          & ST_Name (s2, s1) & " " & ST_Name (s3, s2) & " ]");
    END;
    IF    (d # ST.DontCare) THEN DEC (self.top_of_stack, 4)
    ELSIF (c # ST.DontCare) THEN DEC (self.top_of_stack, 3)
    ELSIF (b # ST.DontCare) THEN DEC (self.top_of_stack, 2)
    ELSE (*a # ST.DontCare*)     DEC (self.top_of_stack)
    END;
    IF (self.top_of_stack < 0) THEN self.top_of_stack := 0 END;
  END Stack_Pop;

PROCEDURE Stack_Push (self: U;  t: Type) =
  BEGIN
    IF (self.top_of_stack <= LAST (self.stack))
      THEN self.stack [self.top_of_stack] := t;
      ELSE PutErr (self, "stack overflow");
    END;
    INC (self.top_of_stack);
  END Stack_Push;

PROCEDURE Stack_Repush (self: U) =
  BEGIN
    INC (self.top_of_stack);
  END Stack_Repush;

PROCEDURE Stack_Empty (self: U) =
  BEGIN
    IF (self.top_of_stack > 0) THEN
      PutErr (self, "non-empty stack: ", Stack_Dump (self));
      self.top_of_stack := 0;
    END;
  END Stack_Empty;

(*************** DEBUGGING ********
PROCEDURE SDump (self: U) =
  BEGIN
    self.child.comment ("**** ", Stack_Dump (self));
  END SDump;
****************************************)

PROCEDURE Stack_Dump (self: U): TEXT =
  VAR s := "[ ";
  BEGIN
    FOR i := 0 TO MIN (self.top_of_stack - 1, 4) DO
      s := s & ST_name [Stack_Get (self, i)];
    END;
    IF (self.top_of_stack > 5) THEN
      s := s & "... ";
    END;
    s := s & "]";
    RETURN s;
  END Stack_Dump;

(*---------------------------------------------------------------------------*)

PROCEDURE New (child: M3CG.T;
               clean_jumps, clean_stores: BOOLEAN;
               nested_calls, nested_procs: BOOLEAN): M3CG.T =
  BEGIN
    child.set_error_handler (CrashAndBurn);
    RETURN NEW (U,
                child        := child,
                note_error   := CrashAndBurn,
                runtime      := NEW (IntIntTbl.Default).init (20),
                clean_jumps  := clean_jumps,
                clean_stores := clean_stores,
                nested_calls := nested_calls,
                nested_procs := nested_procs
               );
  END New;

PROCEDURE CrashAndBurn (msg: TEXT) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    Wr.PutText (Stdio.stdout, "Unhandled M3CG_Check error: " & msg);
    Wr.Flush (Stdio.stdout);
    Wr.Flush (Stdio.stderr);
    <*ASSERT FALSE*>
  END CrashAndBurn;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (self: U;  p: M3CG_Ops.ErrorHandler) =
  BEGIN
    self.note_error := p;
    self.child.set_error_handler (p);
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (self: U;  optimize : INTEGER) =
  (* called before any other method to initialize the compilation unit *)
  CONST IntMap = ARRAY [0..1] OF ST {ST.Int32, ST.Int64};
  VAR ST_Int := IntMap[Target.Integer.bytes DIV 8];
  BEGIN
    (* initialize the mapping of stack types *)
    T_to_ST := ARRAY Type OF ST
        {ST_Int, ST_Int, ST_Int, ST_Int,      (* Word8, Int8, Word16, Int16 *)
         ST_Int, ST_Int, ST.Int64, ST.Int64,  (* Word32, Int32, Word64, Int64 *)
         ST.Reel, ST.LReel, ST.XReel,         (* Reel, LReel, XReel *)
         ST.Addr, ST.Addr,                    (* Addr, Struct *)
         ST.Void};

    self.s_empty ();
    self.child.begin_unit (optimize);
  END begin_unit;

PROCEDURE end_unit   (self: U) =
  (* called after all other methods to finalize the unit and write the
     resulting object *)
  BEGIN
    self.s_empty ();
    self.child.end_unit ();
    IF (self.n_errors <= 0) THEN
      (* ok *)
    ELSIF (self.n_errors = 1) THEN
      self.note_error ("1 code generation error");
    ELSE (*self.n_errors > 1 *)
      self.note_error (Int (self.n_errors) & " code generation errors");
    END;
  END end_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_line (self: U; line: INTEGER) =
  BEGIN
    self.cur_line := line;
    self.child.set_source_line (line);
  END set_source_line;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (self: U;  n: Name;  p: Proc) =
  BEGIN
    CheckProc (self, p);
    IF self.runtime.put (n, 0) THEN
      PutErr (self, "redefined runtime proc: ", M3ID.ToText (n));
    END;
    self.child.set_runtime_proc (n, p);
  END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE CheckVar (self: U;  v: Var) =
  BEGIN
    IF (v = NIL) THEN
      PutErr (self, "NIL variable");
    END;
  END CheckVar;

PROCEDURE bind_segment (self: U;  seg: Var;  s: ByteSize;  a: Alignment;
                        t: Type;  exported, inited: BOOLEAN) =
  BEGIN
    CheckVar (self, seg);
    self.child.bind_segment (seg, s, a, t, exported, inited);
  END bind_segment;

PROCEDURE declare_temp   (self: U;  s: ByteSize;  a: Alignment;  t: Type;
                          in_memory:BOOLEAN): Var =
  VAR v: Var;
  BEGIN
(*
    IF (self.temps = NIL) THEN
      self.temps := NEW (IntIntTbl.Default).init (); END;
*)
    v := self.child.declare_temp (s, a, t, in_memory);
(*
    IF self.temps.put (v, self.cur_line) THEN
      PutErr (self, "temporary reused while live!");
    END;
*)
    RETURN v;
  END declare_temp;

PROCEDURE free_temp (self: U;  v: Var) =
  (* VAR line: INTEGER; *)
  BEGIN
    CheckVar (self, v);
(*
    IF (self.temps = NIL) THEN
      self.temps := NEW (IntIntTbl.Default).init (); END;
    IF NOT self.temps.delete (v, line) THEN
      PutErr (self, "temp freed twice");
    END;
*)
    self.child.free_temp (v);
  END free_temp;

PROCEDURE widechar_size (self:U; size: INTEGER) =
  BEGIN 
    IF size # 16 AND size #32 THEN
      PutErr (self, "invalid widechar size"); 
    END; 
    self.child.widechar_size(size); 
  END widechar_size; 

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (self: U;  v: Var) =
  BEGIN
    CheckVar (self, v);
    IF (self.in_init > 0) THEN
      PutErr (self, "nested static initialization");
    END;
    INC (self.in_init);
    self.init_cursor := 0;
    self.child.begin_init (v);
  END begin_init;

PROCEDURE end_init (self: U;  v: Var) =
  BEGIN
    CheckVar (self, v);
    IF (self.in_init > 0)
      THEN DEC (self.in_init);  self.init_cursor := 0;
      ELSE PutErr (self, "missing begin_init");
    END;
    self.child.end_init (v);
  END end_init;

PROCEDURE DoInit (self: U;  o: ByteOffset;  s: ByteSize) =
  BEGIN
    IF (self.in_init <= 0) THEN PutErr (self, "missing begin_init") END;
    IF (o >= self.init_cursor)
      THEN self.init_cursor := o + s;
      ELSE PutErr (self, "decreasing offsets");
    END;
  END DoInit;

PROCEDURE init_int (self: U;  o: ByteOffset;  READONLY value: Target.Int;
                    t: Type) =
  BEGIN
    DoInit (self, o, TargetMap.CG_Bytes[t]);
    self.child.init_int (o, value, t);
  END init_int;

PROCEDURE init_proc (self: U;  o: ByteOffset;  value: Proc) =
  BEGIN
    DoInit (self, o, Target.Address.bytes);
    self.child.init_proc (o, value);
  END init_proc;

PROCEDURE init_label (self: U;  o: ByteOffset;  value: Label) =
  BEGIN
    DoInit (self, o, Target.Address.bytes);
    self.child.init_label (o, value);
  END init_label;

PROCEDURE init_var (self: U;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
  BEGIN
    DoInit (self, o, Target.Address.bytes);
    self.child.init_var (o, value, bias);
  END init_var;

PROCEDURE init_offset (self: U;  o: ByteOffset;  value: Var) =
  BEGIN
    DoInit (self, o, Target.Integer.bytes);
    self.child.init_offset (o, value);
  END init_offset;

PROCEDURE init_chars (self: U;  o: ByteOffset;  value: TEXT) =
  BEGIN
    DoInit (self, o, Text.Length (value) * Target.Char.bytes);
    self.child.init_chars (o, value);
  END init_chars;

PROCEDURE init_float (self: U;  o: ByteOffset;  READONLY f: Target.Float) =
  BEGIN
    DoInit (self, o, TargetMap.Float_types[f.pre].bytes);
    self.child.init_float (o, f);
  END init_float;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE CheckProc (self: U;  p: Proc) =
  BEGIN
    IF (p = NIL) THEN
      PutErr (self, "NIL procedure");
    END;
  END CheckProc;

PROCEDURE begin_procedure (self: U;  p: Proc) =
  BEGIN
    CheckProc (self, p);
    IF (self.proc_count > 0) AND (NOT self.nested_procs) THEN
      PutErr (self, "nested procedure declaration");
    END;
    INC (self.proc_count);
    self.child.begin_procedure (p);
  END begin_procedure;

PROCEDURE end_procedure (self: U;  p: Proc) =
  BEGIN
    CheckProc (self, p);
    IF (self.proc_count > 0)
      THEN DEC (self.proc_count);
      ELSE PutErr (self, "missing begin_procedure");
    END;
    IF (self.block_count > 0) AND (NOT self.nested_procs) THEN
      PutErr (self, "missing end_blocks: ", Int (self.block_count));
      self.block_count := 0;
    END;
    self.s_empty ();
(*
    IF (self.temps # NIL) THEN
      VAR it := self.temps.iterate (); k: REFANY; line: INTEGER; BEGIN
        WHILE it.next (tag, line) DO
          PutErr (self, "temp not freed, created on line ", Int (line));
        END;
      END;
    END;
*)
    self.child.end_procedure (p);
  END end_procedure;

PROCEDURE begin_block (self: U) =
  (* marks the beginning of a nested anonymous block *)
  BEGIN
    IF (self.proc_count <= 0) THEN
      PutErr (self, "begin_block not in procedure");
    END;
    self.s_empty ();
    INC (self.block_count);
    self.child.begin_block ();
  END begin_block;

PROCEDURE end_block (self: U) =
  (* marks the ending of a nested anonymous block *)
  BEGIN
    IF (self.block_count > 0)
      THEN DEC (self.block_count);
      ELSE PutErr (self, "missing begin_block");
    END;
    self.s_empty ();
    self.child.end_block ();
  END end_block;

PROCEDURE note_procedure_origin (self: U;  p: Proc) =
  BEGIN
    CheckProc (self, p);
    self.s_empty ();
    self.child.note_procedure_origin (p);
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE CheckLabel (self: U;  l: Label) =
  BEGIN
    IF (l < 0) (*OR (self.next_label <= l)*) THEN
      PutErr (self, "undefined label: ", Int (l));
    END;
  END CheckLabel;

PROCEDURE set_label (self: U;  l: Label;  barrier: BOOLEAN) =
  (* define 'l' to be at the current pc *)
  BEGIN
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.set_label (l, barrier);
  END set_label;

PROCEDURE jump (self: U; l: Label) =
  (* GOTO l *)
  BEGIN
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.jump (l);
  END jump;

PROCEDURE if_true  (self: U;  t: IType;  l: Label;  f: Frequency) =
  (* IF (s0.t # 0) GOTO l ; pop *)
  BEGIN
    self.s_pop (T_to_ST[t]);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_true (t, l, f);
  END if_true;

PROCEDURE if_false (self: U;  t: IType;  l: Label;  f: Frequency) =
  (* IF (s0.t = 0) GOTO l ; pop *)
  BEGIN
    self.s_pop (T_to_ST[t]);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_false (t, l, f);
  END if_false;

PROCEDURE if_compare (self: U;  t: ZType;  op: CompareOp;  l: Label; f: Frequency) =
  (* IF (s1.t op s0.t) GOTO l ; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_compare (t, op, l, f);
  END if_compare;

PROCEDURE case_jump (self: U;  t: IType;  READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.t] ; pop" with no range checking on s0.t *)
  BEGIN
    self.s_pop (T_to_ST[t]);
    IF (self.clean_jumps) THEN self.s_empty () END;
    FOR i := FIRST (labels) TO LAST (labels) DO
      CheckLabel (self, labels [i]);
    END;
    self.child.case_jump (t, labels);
  END case_jump;

PROCEDURE exit_proc (self: U; t: Type) =
  (* Returns s0.t if the stack is non-empty, otherwise returns no value. *)
  BEGIN
    IF (t # Type.Void) THEN self.s_pop (T_to_ST [t]) END;
    self.s_empty ();
    self.child.exit_proc (t);
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

TYPE ZMap = ARRAY ZType OF BOOLEAN;
TYPE MMap = ARRAY MType OF BOOLEAN;

CONST
  LegalLoad = ARRAY MType, ZType OF BOOLEAN {
  (*               W32   I32   W64   I64   Reel  LReel XReel Addr *)
(* Word8  *) ZMap{ TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Int8   *) ZMap{ TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Word16 *) ZMap{ TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Int16  *) ZMap{ TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Word32 *) ZMap{ TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Int32  *) ZMap{ TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Word64 *) ZMap{ FALSE,FALSE,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Int64  *) ZMap{ FALSE,FALSE,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Reel   *) ZMap{ FALSE,FALSE,FALSE,FALSE,TRUE ,FALSE,FALSE,FALSE },
(* LReel  *) ZMap{ FALSE,FALSE,FALSE,FALSE,FALSE,TRUE ,FALSE,FALSE },
(* XReel  *) ZMap{ FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE ,FALSE },
(* Addr   *) ZMap{ FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE  }
  };

CONST
  LegalStore = ARRAY ZType, MType OF BOOLEAN {
  (*      Word8 Int8  W16   I16   W32   I32   W64   I64   Reel  LReel XReel Addr *)
(* Word32 *)
    MMap{ TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE },
(* Int32  *)
    MMap{ TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE },
(* Word64 *)
    MMap{ TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Int64  *)
    MMap{ TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE },
(* Reel   *)
    MMap{ FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE ,FALSE,FALSE,FALSE },
(* LReel  *)
    MMap{ FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE ,FALSE,FALSE },
(* XReel  *)
    MMap{ FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE ,FALSE },
(* Addr   *)
    MMap{ FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE  }
  };

PROCEDURE load  (self: U;  v: Var;  o: ByteOffset;  t: MType;  u: ZType) =
  BEGIN
    CheckVar (self, v);
    IF NOT LegalLoad [t, u] THEN PutErr (self, "illegal load conversion"); END;
    self.s_push (u);
    self.child.load (v, o, t, u);
  END load;

PROCEDURE store  (self: U;  v: Var;  o: ByteOffset;  t: ZType;  u: MType) =
  BEGIN
    CheckVar (self, v);
    IF NOT LegalStore [t, u] THEN PutErr (self, "illegal store conversion"); END;
    self.s_pop (T_to_ST [t]);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.store (v, o, t, u);
  END store;

PROCEDURE load_address (self: U;  v: Var;  o: ByteOffset) =
  BEGIN
    CheckVar (self, v);
    self.s_push (Type.Addr);
    self.child.load_address (v, o);
  END load_address;

PROCEDURE load_indirect (self: U;  o: ByteOffset;  t: MType;  u: ZType) =
  BEGIN
    IF NOT LegalLoad [t, u] THEN PutErr (self, "illegal load conversion"); END;
    self.s_pop (ST.Addr);
    self.s_push (u);
    self.child.load_indirect (o, t, u);
  END load_indirect;

PROCEDURE store_indirect (self: U;  o: ByteOffset;  t: ZType;  u: MType) =
  BEGIN
    IF NOT LegalStore [t, u] THEN PutErr (self, "illegal store conversion"); END;
    self.s_pop (T_to_ST [t], ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.store_indirect (o, t, u);
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (self: U) =
  (* push ; s0.A := a *)
  BEGIN
    self.s_push (Type.Addr);
    self.child.load_nil ();
  END load_nil;

PROCEDURE load_integer  (self: U;  t: IType;  READONLY i: Target.Int) =
  (* push ; s0.t := i *)
  BEGIN
    self.s_push (t);
    self.child.load_integer (t, i);
  END load_integer;

PROCEDURE load_float    (self: U;  t: RType;  READONLY f: Target.Float) =
  (* push ; s0.t := f *)
  CONST FType = ARRAY Target.Precision OF RType
                { Type.Reel, Type.LReel, Type.XReel };
  BEGIN
    IF t # FType [f.pre] THEN
      PutErr (self, "floating-point literal doesn't match type");
    END;
    self.s_push (t);
    self.child.load_float (t, f);
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE Binary (self: U;  lhs, rhs: Type) =
  (* s1.lhs := s1.rhs 'op' s0.rhs ; pop *)
  BEGIN
    self.s_pop (T_to_ST [rhs], ST.Match);
    self.s_push (lhs);
  END Binary;

PROCEDURE Unary (self: U;  lhs, rhs: Type) =
  (* s1.lhs := 'op' (s1.rhs) *)
  BEGIN
    self.s_pop (T_to_ST [rhs]);
    self.s_push (lhs);
  END Unary;

PROCEDURE compare (self: U;  t: ZType;  u: IType;  op: CompareOp) =
  (* s1.u := (s1.t op s0.t)  ; pop *)
  BEGIN
    Binary (self, u, t);
    self.child.compare (t, u, op);
  END compare;

PROCEDURE add (self: U;  t: AType) =
  (* s1.t := s1.t + s0.t ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.add (t);
  END add;

PROCEDURE subtract (self: U;  t: AType) =
  (* s1.t := s1.t - s0.t ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.subtract (t);
  END subtract;

PROCEDURE multiply (self: U;  t: AType) =
  (* s1.t := s1.t * s0.t ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.multiply (t);
  END multiply;

PROCEDURE divide (self: U;  t: RType) =
  (* s1.t := s1.t / s0.t ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.divide (t);
  END divide;

PROCEDURE div (self: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t DIV s0.t ; pop *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    self.s_push (t);
    self.child.div (t, a, b);
  END div;

PROCEDURE mod (self: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t MOD s0.t ; pop *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    self.s_push (t);
    self.child.mod (t, a, b);
  END mod;

PROCEDURE negate (self: U;  t: AType) =
  (* s0.t := - s0.t *)
  BEGIN
    Unary (self, t, t);
    self.child.negate (t);
  END negate;

PROCEDURE abs      (self: U;  t: AType) =
  (* s0.t := ABS (s0.t) (noop on Words) *)
  BEGIN
    Unary (self, t, t);
    self.child.abs (t);
  END abs;

PROCEDURE max      (self: U;  t: ZType) =
  (* s1.t := MAX (s1.t, s0.t) ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.max (t);
  END max;

PROCEDURE min      (self: U;  t: ZType) =
  (* s1.t := MIN (s1.t, s0.t) ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.min (t);
  END min;

PROCEDURE cvt_int    (self: U;  t: RType;  u: IType;  op: ConvertOp) =
  (* s0.u := op (s0.t) *)
  BEGIN
    Unary (self, u, t);
    self.child.cvt_int (t, u, op);
  END cvt_int;

PROCEDURE cvt_float    (self: U;  t: AType;  u: RType) =
  (* s0.u := FLOAT (s0.t, u) *)
  BEGIN
    Unary (self, u, t);
    self.child.cvt_float (t, u);
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (self: U;  s: ByteSize) =
  (* s2.B := s1.B + s0.B ; pop(3) *)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr, ST.Addr);
    self.child.set_union (s);
  END set_union;

PROCEDURE set_difference (self: U;  s: ByteSize) =
  (* s2.B := s1.B - s0.B ; pop(3) *)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr, ST.Addr);
    self.child.set_difference (s);
  END set_difference;

PROCEDURE set_intersection (self: U;  s: ByteSize) =
  (* s2.B := s1.B * s0.B ; pop(3) *)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr, ST.Addr);
    self.child.set_intersection (s);
  END set_intersection;

PROCEDURE set_sym_difference (self: U;  s: ByteSize) =
  (* s2.B := s1.B / s0.B ; pop(3) *)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr, ST.Addr);
    self.child.set_sym_difference (s);
  END set_sym_difference;

PROCEDURE set_member (self: U;  s: ByteSize;  t: IType) =
  (* s1.t := (s0.t IN s1.B) ; pop *)
  BEGIN
    self.s_pop (T_to_ST[t], ST.Addr);
    self.s_push (t);
    self.child.set_member (s, t);
  END set_member;

PROCEDURE set_compare (self: U;  s: ByteSize;  op: CompareOp;  t: IType) =
  (* s1.t := (s1.B op s0.B)  ; pop *)
  BEGIN
    Binary (self, t, Type.Addr);
    self.child.set_compare (s, op, t);
  END set_compare;

PROCEDURE set_range (self: U;  s: ByteSize;  t: IType) =
  (* s2.A [s1.t .. s0.t] := 1's; pop(3)*)
  BEGIN
    self.s_pop (T_to_ST[t], T_to_ST[t], ST.Addr);
    self.child.set_range (s, t);
  END set_range;

PROCEDURE set_singleton (self: U;  s: ByteSize;  t: IType) =
  (* s1.A [s0.t] := 1; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST[t], ST.Addr);
    self.child.set_singleton (s, t);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (self: U;  t: IType) =
  (* s0.t := Word.Not (s0.t) *)
  BEGIN
    Unary (self, t, t);
    self.child.not (t);
  END not;

PROCEDURE and (self: U;  t: IType) =
  (* s1.t := Word.And (s1.t, s0.t) ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.and (t);
  END and;

PROCEDURE or  (self: U;  t: IType) =
  (* s1.t := Word.Or  (s1.t, s0.t) ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.or (t);
  END or;

PROCEDURE xor (self: U;  t: IType) =
  (* s1.t := Word.Xor (s1.t, s0.t) ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.xor (t);
  END xor;

PROCEDURE shift (self: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t) ; pop *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type], T_to_ST [t]);
    self.s_push (t);
    self.child.shift (t);
  END shift;

PROCEDURE shift_left (self: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t) ; pop *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type], T_to_ST [t]);
    self.s_push (t);
    self.child.shift_left (t);
  END shift_left;

PROCEDURE shift_right  (self: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, -s0.t) ; pop *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type], T_to_ST [t]);
    self.s_push (t);
    self.child.shift_right (t);
  END shift_right;

PROCEDURE rotate (self: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t) ; pop *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type], T_to_ST [t]);
    self.s_push (t);
    self.child.rotate (t);
  END rotate;

PROCEDURE rotate_left  (self: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t) ; pop *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type], T_to_ST [t]);
    self.s_push (t);
    self.child.rotate_left (t);
  END rotate_left;

PROCEDURE rotate_right (self: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, -s0.t) ; pop *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type], T_to_ST [t]);
    self.s_push (t);
    self.child.rotate_right (t);
  END rotate_right;

PROCEDURE widen (self: U;  sign: BOOLEAN) =
  (* s0.I64 := s0.I32;  IF sign THEN Sign-extend s0;  *)
  BEGIN
    self.s_pop (ST.Int32);
    self.s_push (Type.Int64);
    self.child.widen (sign);
  END widen;

PROCEDURE chop (self: U) =
  (* s0.I32 := Word.And (s0.I64, 16_ffffffff);  *)
  BEGIN
    self.s_pop (ST.Int64);
    self.s_push (Type.Int32);
    self.child.chop ();
  END chop;

PROCEDURE extract (self: U;  t: IType;  sign: BOOLEAN) =
  (* s2.t := Word.Extract(s2.t, s1.t, s0.t);
     IF sign THEN SignExtend s2 END; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type],
                T_to_ST[Target.Integer.cg_type],
                T_to_ST [t]);
    self.s_push (t);
    self.child.extract (t, sign);
  END extract;

PROCEDURE extract_n (self: U;  t: IType;  sign: BOOLEAN;  n: CARDINAL) =
  (* s1.t := Word.Extract(s1.t, s0.t, n);
     IF sign THEN SignExtend s1 END; pop(1) *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type],
                T_to_ST [t]);
    self.s_push (t);
    self.child.extract_n (t, sign, n);
  END extract_n;

PROCEDURE extract_mn (self: U;  t: IType;  sign: BOOLEAN;  m, n: CARDINAL) =
  (* s0.t := Word.Extract(s0.t, m, n);
     IF sign THEN SignExtend s0 END; *)
  BEGIN
    Unary (self, t, t);
    self.child.extract_mn (t, sign, m, n);
  END extract_mn;

PROCEDURE insert  (self: U;  t: IType) =
  (* s3.t := Word.Insert (s3.t, s2.t, s1.t, s0.t) ; pop(3) *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type],
                T_to_ST[Target.Integer.cg_type],
                T_to_ST[t], T_to_ST[t]);
    self.s_push (t);
    self.child.insert (t);
  END insert;

PROCEDURE insert_n  (self: U;  t: IType;  n: CARDINAL) =
  (* s2.t := Word.Insert (s2.t, s1.t, s0.t, n) ; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST[Target.Integer.cg_type], T_to_ST[t], T_to_ST[t]);
    self.s_push (t);
    self.child.insert_n (t, n);
  END insert_n;

PROCEDURE insert_mn  (self: U;  t: IType;  m, n: CARDINAL) =
  (* s1.t := Word.Insert (s1.t, s0.t, m, n) ; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST[t], T_to_ST[t]);
    self.s_push (t);
    self.child.insert_mn (t, m, n);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (self: U;  a, b: Type) =
  (* tmp := s1 ; s1 := s0 ; s0 := tmp *)
  BEGIN
    self.s_pop (T_to_ST [b], T_to_ST [a]);
    self.s_push (b);
    self.s_push (a);
    self.child.swap (a, b);
  END swap;

PROCEDURE pop  (self: U;  t: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    self.s_pop (T_to_ST [t]);
    self.child.pop (t);
  END pop;

PROCEDURE copy_n (self: U;  u: IType;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:s0.t] := Mem[s1.A:s0.t]; pop(3)*)
  BEGIN
    self.s_pop (T_to_ST[u], ST.Addr, ST.Addr);
    self.child.copy_n (u, t, overlap);
  END copy_n;

PROCEDURE copy (self: U;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:sz] := Mem[s1.A:sz]; pop(2)*)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr);
    self.child.copy (n, t, overlap);
  END copy;

PROCEDURE zero_n (self: U;  u: IType;  t: MType) =
  (* Mem[s1.A:s0.u] := 0; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST[u], ST.Addr);
    self.child.zero_n (u, t);
  END zero_n;

PROCEDURE zero (self: U;  n: INTEGER;  t: MType) =
  (* Mem[s1.A:sz] := 0; pop(1) *)
  BEGIN
    self.s_pop (ST.Addr);
    self.child.zero (n, t);
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (self: U;  from, two: ZType) =
  (* s0.to := LOOPHOLE(s0.from, to) *)
  BEGIN
    self.s_pop (T_to_ST [from]);
    self.s_push (two);
    self.child.loophole (from, two);
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (self: U;  code: RuntimeError) =
  BEGIN
    self.s_empty ();
    self.child.abort (code);
  END abort;

PROCEDURE check_nil (self: U;  code: RuntimeError) =
  (* IF (s0.A = NIL) THEN abort(code) *)
  BEGIN
    self.s_pop (ST.Addr);
    self.s_push (Type.Addr);
    self.child.check_nil (code);
  END check_nil;

PROCEDURE check_lo (self: U;  t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  (* IF (s0.t < i) THEN abort(code) *)
  BEGIN
    Unary (self, t,  t);
    self.child.check_lo (t, i, code);
  END check_lo;

PROCEDURE check_hi (self: U;  t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  (* IF (i < s0.t) THEN abort(code) *)
  BEGIN
    Unary (self, t, t);
    self.child.check_hi (t, i, code);
  END check_hi;

PROCEDURE check_range (self: U;  t: IType;  READONLY a, b: Target.Int;  code: RuntimeError) =
  (* IF (s0.t < a) OR (b < s0.t) THEN abort(code) *)
  BEGIN
    Unary (self, t, t);
    self.child.check_range (t, a, b, code);
  END check_range;

PROCEDURE check_index (self: U;  t: IType;  code: RuntimeError) =
  (* IF NOT (0 <= s1.t < s0.t) THEN abort(code) END; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.check_index (t, code);
  END check_index;

PROCEDURE check_eq (self: U;  t: IType;  code: RuntimeError) =
  (* IF (s0.I # s1.I) THEN abort(code);  Pop (2) *)
  BEGIN
    self.s_pop (T_to_ST[t], T_to_ST[t]);
    self.child.check_eq (t, code);
  END check_eq;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (self: U; i: INTEGER) =
  (* s0.A := s0.A + i *)
  BEGIN
    self.s_pop (ST.Addr);
    self.s_push (Type.Addr);
    self.child.add_offset (i);
  END add_offset;

PROCEDURE index_address (self: U;  t: IType;  size: INTEGER) =
  (* s1.A := s1.A + s0.t * size ; pop *)
  BEGIN
    self.s_pop (T_to_ST[t], ST.Addr);
    self.s_push (Type.Addr);
    self.child.index_address (t, size);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE start_call_direct (self: U;  p: Proc;  lev: INTEGER;  t: Type) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    CheckProc (self, p);
    IF (self.clean_jumps) THEN self.s_empty () END;
    IF (self.call_count > 0) AND (NOT self.nested_calls) THEN
      PutErr (self, "nested procedure call");
    END;
    INC (self.call_count);
    self.child.start_call_direct (p, lev, t);
  END start_call_direct;

PROCEDURE start_call_indirect (self: U;  t: Type;  cc: CallingConvention) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    IF (self.clean_jumps) THEN self.s_empty () END;
    IF (self.call_count > 0) AND (NOT self.nested_calls) THEN
      PutErr (self, "nested procedure call");
    END;
    INC (self.call_count);
    self.child.start_call_indirect (t, cc);
  END start_call_indirect;

PROCEDURE pop_param (self: U;  t: MType) =
  (* pop s0 and make it the "next" parameter in the current call *)
  BEGIN
    IF (self.call_count <= 0) THEN PutErr (self, "missing start_call") END;
    self.s_pop (T_to_ST [t]);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.pop_param (t);
  END pop_param;

PROCEDURE pop_struct (self: U;  t: TypeUID;  s: ByteSize;  a: Alignment) =
  (* pop s0 and make it the "next" parameter in the current call *)
  BEGIN
    IF (self.call_count <= 0) THEN PutErr (self, "missing start_call") END;
    self.s_pop (ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.pop_struct (t, s, a);
  END pop_struct;

PROCEDURE pop_static_link (self: U) =
  BEGIN
    IF (self.call_count <= 0) THEN PutErr (self, "missing start_call") END;
    self.s_pop (ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.pop_static_link ();
  END pop_static_link;

PROCEDURE DoCall (self: U) =
  BEGIN
    IF (self.clean_jumps) THEN self.s_empty () END;
    IF (self.call_count > 0)
      THEN DEC (self.call_count);
      ELSE PutErr (self, "missing start_call");
    END;
  END DoCall;

PROCEDURE call_direct (self: U; p: Proc;  t: Type) =
  (* call the procedure identified by block b.  The procedure
     returns a value of type t. *)
  BEGIN
    CheckProc (self, p);
    DoCall (self);
    IF (t # Type.Void) THEN self.s_push (t) END;
    self.child.call_direct (p, t);
  END call_direct;

PROCEDURE call_indirect (self: U; t: Type;  cc: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0.  The
     procedure returns a value of type t. *)
  BEGIN
    self.s_pop (ST.Addr);
    DoCall (self);
    IF (t # Type.Void) THEN self.s_push (t) END;
    self.child.call_indirect (t, cc);
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (self: U;  p: Proc) =
  (* push; s0.A := ADDR (p's body) *)
  BEGIN
    CheckProc (self, p);
    self.s_push (Type.Addr);
    self.child.load_procedure (p);
  END load_procedure;

PROCEDURE load_static_link (self: U;  p: Proc) =
  (* push; s0.A := (static link needed to call p, NIL for top-level procs) *)
  BEGIN
    CheckProc (self, p);
    self.s_push (Type.Addr);
    self.child.load_static_link (p);
  END load_static_link;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered (self: U;  t: ZType;  u: MType;  order: MemoryOrder) =
  BEGIN
    IF NOT LegalStore [t, u] THEN PutErr (self, "illegal store conversion"); END;
    CASE order OF
    | MemoryOrder.Acquire, MemoryOrder.AcquireRelease =>
      PutErr (self, "illegal store memory order");
    ELSE (* ok *) END;
    self.s_pop (T_to_ST [t], ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.store_ordered (t, u, order);
  END store_ordered;

PROCEDURE load_ordered (self: U;  t: MType;  u: ZType;  order: MemoryOrder) =
  BEGIN
    IF NOT LegalLoad [t, u] THEN PutErr (self, "illegal load conversion"); END;
    CASE order OF
    | MemoryOrder.Release, MemoryOrder.AcquireRelease =>
      PutErr (self, "illegal load memory order");
    ELSE (* ok *) END;
    self.s_pop (ST.Addr);
    self.s_push (u);
    self.child.load_ordered (t, u, order);
  END load_ordered;

PROCEDURE exchange (self: U;  t: MType;  u: ZType;  order: MemoryOrder) =
  BEGIN
    IF NOT LegalStore [u, t] THEN PutErr (self, "illegal store conversion"); END;
    IF NOT LegalLoad  [t, u] THEN PutErr (self, "illegal load conversion");  END;
    self.s_pop (T_to_ST [u], ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.s_push (u);
    self.child.exchange (t, u, order);
  END exchange;

PROCEDURE compare_exchange (self: U;  t: MType;  u: ZType;  r: IType;
                            success, failure: MemoryOrder) =
  BEGIN
    IF NOT LegalStore [u, t] THEN PutErr (self, "illegal store conversion"); END;
    IF NOT LegalLoad  [t, u] THEN PutErr (self, "illegal load conversion");  END;
    CASE failure OF
    | MemoryOrder.Release, MemoryOrder.AcquireRelease =>
      PutErr (self, "illegal load memory order");
    ELSE (* ok *) END;
    IF failure > success THEN PutErr (self, "failure stronger than success"); END;
    self.s_pop (T_to_ST [u], T_to_ST [u], ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.s_push (r);
    self.s_push (u);
    self.child.compare_exchange (t, u, r, success, failure);
  END compare_exchange;

PROCEDURE fence (self: U;  order: MemoryOrder) =
  BEGIN
    self.child.fence (order);
  END fence;

PROCEDURE fetch_and_op (self: U;  op: AtomicOp;  t: MType;  u: ZType;
                        order: MemoryOrder) =
  BEGIN
    IF NOT LegalStore [u, t] THEN PutErr (self, "illegal store conversion"); END;
    IF NOT LegalLoad  [t, u] THEN PutErr (self, "illegal load conversion");  END;
    self.s_pop (T_to_ST[u], ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.s_push (u);
    self.child.fetch_and_op (op, t, u, order);
  END fetch_and_op;

BEGIN
END M3CG_Check.
