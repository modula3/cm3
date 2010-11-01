(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 20 16:19:09 PDT 1995 by kalsow     *)
(*      modified on Wed Nov 23 13:57:47 PST 1994 by isard      *)

MODULE M3C;

IMPORT TextSeq, Wr, Text, Fmt;
IMPORT M3CG, M3CG_Ops, Target;
IMPORT TIntN, Stdio;
IMPORT M3ObjFile, TargetMap;
FROM TargetMap IMPORT CG_Bytes;
FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, RuntimeError, MemoryOrder, AtomicOp;
FROM Target IMPORT CGType;
FROM M3CG_Ops IMPORT ErrorHandler;
IMPORT Wrx86;

REVEAL
  U = Public BRANDED "M3C.U" OBJECT
        wr     : Wrx86.T := NIL;
        c      : Wr.T := NIL;
        debug  := FALSE;
        stack  : TextSeq.T := NIL;
      OVERRIDES
        next_label := next_label;
        set_error_handler := set_error_handler;
        begin_unit := begin_unit;
        end_unit   := end_unit;
        import_unit := import_unit;
        export_unit := export_unit;
        set_source_file := set_source_file;
        set_source_line := set_source_line;
        declare_typename := declare_typename;
        declare_array := declare_array;
        declare_open_array := declare_open_array;
        declare_enum := declare_enum;
        declare_enum_elt := declare_enum_elt;
        declare_packed  := declare_packed;
        declare_record := declare_record;
        declare_field := declare_field;
        declare_set := declare_set;
        declare_subrange := declare_subrange;
        declare_pointer := declare_pointer;
        declare_indirect := declare_indirect;
        declare_proctype := declare_proctype;
        declare_formal := declare_formal;
        declare_raises := declare_raises;
        declare_object := declare_object;
        declare_method := declare_method;
        declare_opaque := declare_opaque;
        reveal_opaque := reveal_opaque;
        set_runtime_proc := set_runtime_proc;
        import_global  := import_global;
        declare_segment := declare_segment;
        bind_segment := bind_segment;
        declare_global := declare_global;
        declare_constant := declare_constant;
        declare_local  := declare_local;
        declare_param  := declare_param;
        declare_temp   := declare_temp;
        free_temp := free_temp;
        declare_exception := declare_exception;
        begin_init := begin_init;
        end_init := end_init;
        init_int := init_int;
        init_proc := init_proc;
        init_label := init_label;
        init_var := init_var;
        init_offset := init_offset;
        init_chars := init_chars;
        init_float := init_float;
        import_procedure := import_procedure;
        declare_procedure := declare_procedure;
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
        set_compare   := set_compare;
        set_range     := set_range;
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
        pop  := cg_pop;
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
        comment := comment;
        store_ordered := store_ordered;
        load_ordered := load_ordered;
        exchange := exchange;
        compare_exchange := compare_exchange;
        fence := fence;
        fetch_and_op := fetch_and_op;
      END;

(*---------------------------------------------------------------------------*)

<*NOWARN*>CONST Prefix = ARRAY OF TEXT {
"#include <stddef.h>",
"",
"#ifdef __cplusplus",
"extern \"C\" {",
"#endif",
"",
"/* const is extern const in C, but static const in C++,",
" * but gcc gives a warning for the correct portable form \"extern const\"",
" */",
"",
"#if defined(__cplusplus) || !defined(__GNUC__)",
"#define EXTERN_CONST extern const",
"#else",
"#define EXTERN_CONST const",
"#endif",
"",
"#if !defined(_MSC_VER) && !defined(__cdecl)",
"#define __cdecl /* nothing */",
"#endif",
"",
"typedef   signed char       INT8;",
"typedef unsigned char      UINT8, WORD8;",
"typedef   signed short      INT16;",
"typedef unsigned short     UINT16, WORD16;",
"typedef   signed int        INT32;",
"typedef unsigned int       UINT32, WORD32;",
"#if defined(_MSC_VER) || defined(__DECC)",
"typedef   signed __int64  INT64, LONGINT;",
"typedef unsigned __int64 UINT64, WORD64, LONGCARD;",
"#else",
"typedef   signed long long  INT64, LONGINT;",
"typedef unsigned long long UINT64, WORD64, LONGCARD;",
"#endif",
"",
"/* WORD_T/INTEGER are always exactly the same size as a pointer.",
" * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers.",
" */",
"#if __INITIAL_POINTER_SIZE == 64",
"typedef __int64 INTEGER;",
"typedef unsigned __int64 WORD_T, CARDINAL;",
"#else",
"typedef ptrdiff_t INTEGER;",
"typedef size_t WORD_T, CARDINAL;",
"#endif",
"typedef void *ADDRESS, *PVOID, *TEXT, *STRUCT;",
"typedef float REAL;",
"typedef double LONGREAL, EXTENDED;"
  };

<*NOWARN*>CONST Suffix = ARRAY OF TEXT {
"",
"#ifdef __cplusplus",
"} /* extern \"C\" */",
"#endif"
};

CONST TypeNames = ARRAY CGType OF TEXT {
    "UINT8",  "INT8",
    "UINT16", "INT16",
    "UINT32", "INT32",
    "UINT64", "INT64",
    "REAL", "LONGREAL", "EXTENDED",
    "PVOID",
    "STRUCT",
    "void"
  };

<*NOWARN*>CONST CompareOpSymbols = ARRAY CompareOp OF TEXT { "==", "!=", ">", ">=", "<", "<=" };
CONST ConvertOpName = ARRAY ConvertOp OF TEXT { " round", " trunc", " floor", " ceiling" };
CONST CompareOpName = ARRAY CompareOp OF TEXT { " EQ", " NE", " GT", " GE", " LT", " LE" };

(*---------------------------------------------------------------------------*)

PROCEDURE pop(u: U): TEXT =
  BEGIN
    RETURN u.stack.remlo();
  END pop;

<*NOWARN*>PROCEDURE push(u: U; t: TEXT) =
  BEGIN
    u.stack.addlo(t);
  END push;

PROCEDURE get(u: U; n: CARDINAL): TEXT =
  BEGIN
    RETURN u.stack.get(n);
  END get;

PROCEDURE print(u: U; t: TEXT) = <*FATAL ANY*>
  BEGIN
    Wr.PutText(u.c, t)
  END print;
  
(*---------------------------------------------------------------------------*)

PROCEDURE New (logfile: Wr.T; <*NOWARN*>obj: M3ObjFile.T): M3CG.T =
  VAR u := NEW (U);
  BEGIN
    IF logfile # NIL THEN
      u.debug := TRUE;
      u.wr := Wrx86.New (logfile);
    ELSE
      u.wr := NIL;
    END;
    u.c := Stdio.stdout;
    u.stack := NEW(TextSeq.T).init();
    RETURN NIL;
  END New;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (<*NOWARN*>u: U;  <*NOWARN*>n: INTEGER := 1): Label =
  BEGIN
    RETURN -1;
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (<*NOWARN*>u: U; <*NOWARN*>p: ErrorHandler) =
  BEGIN
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (u: U;  optimize : INTEGER) =
  (* called before any other method to initialize the compilation unit *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd ("begin_unit");
      u.wr.Int (optimize);
      u.wr.NL  ();
    END;
  END begin_unit;

PROCEDURE end_unit   (u: U) =
  (* called after all other methods to finalize the unit and write the
     resulting object *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd ("end_unit");
      u.wr.NL  ();
    END;
  END end_unit;

PROCEDURE import_unit (u: U;  n: Name) =
  (* note that the current compilation unit imports the interface 'n' *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("import_unit");
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END import_unit;

PROCEDURE export_unit (u: U;  n: Name) =
  (* note that the current compilation unit exports the interface 'n' *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("export_unit");
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (u: U; file: TEXT) =
  (* Sets the current source file name.  Subsequent statements
     and expressions are associated with this source location. *)
  BEGIN
    IF u.debug THEN
      u.wr.OutT ("\t\t\t\t\t-----FILE ");
      u.wr.OutT (file);
      u.wr.OutT ("  -----");
      u.wr.NL ();
    END;
  END set_source_file;

PROCEDURE set_source_line (u: U; line: INTEGER) =
  (* Sets the current source line number.  Subsequent statements
   and expressions are associated with this source location. *)
  BEGIN
    IF u.debug THEN
      u.wr.OutT ("\t\t\t\t\t-----LINE");
      u.wr.Int  (line);
      u.wr.OutT ("  -----");
      u.wr.NL ();
    END;
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (u: U;  type: TypeUID;  n: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_typename");
      u.wr.Tipe  (type);
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END declare_typename;

PROCEDURE declare_array (u: U;  type, index, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_array");
      u.wr.Tipe (type);
      u.wr.Tipe (index);
      u.wr.Tipe (elt);
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_array;

PROCEDURE declare_open_array (u: U;  type, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_open_array");
      u.wr.Tipe (type);
      u.wr.Tipe (elt);
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_open_array;

PROCEDURE declare_enum (u: U;  type: TypeUID;  n_elts: INTEGER;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_enum");
      u.wr.Tipe (type);
      u.wr.Int  (n_elts);
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_enum;

PROCEDURE declare_enum_elt (u: U;  n: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_enum_elt");
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END declare_enum_elt;

PROCEDURE declare_packed  (u: U;  type: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_packed");
      u.wr.Tipe (type);
      u.wr.BInt (s);
      u.wr.Tipe (base);
      u.wr.NL   ();
    END
  END declare_packed;

PROCEDURE declare_record (u: U; type: TypeUID;  s: BitSize; n_fields: INTEGER) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_record");
      u.wr.Tipe (type);
      u.wr.BInt (s);
      u.wr.Int  (n_fields);
      u.wr.NL   ();
    END
  END declare_record;

PROCEDURE declare_field (u: U; n: Name; o: BitOffset; s: BitSize; type: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_field");
      u.wr.ZName (n);
      u.wr.BInt  (o);
      u.wr.BInt  (s);
      u.wr.Tipe  (type);
      u.wr.NL    ();
    END
  END declare_field;

PROCEDURE declare_set (u: U;  type, domain: TypeUID;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_set");
      u.wr.Tipe (type);
      u.wr.Tipe (domain);
      u.wr.BInt (s);
      u.wr.NL    ();
    END
  END declare_set;

PROCEDURE declare_subrange (u: U; type, domain: TypeUID;
                            READONLY min, max: Target.Int;
                            s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_subrange");
      u.wr.Tipe (type);
      u.wr.Tipe (domain);
      u.wr.TInt (TIntN.FromTargetInt(min, NUMBER(min))); (* What about s for size? *)
      u.wr.TInt (TIntN.FromTargetInt(max, NUMBER(max))); (* What about s for size? *)
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_subrange;

PROCEDURE declare_pointer (u: U; type, target: TypeUID; brand: TEXT; traced: BOOLEAN) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_pointer");
      u.wr.Tipe (type);
      u.wr.Tipe (target);
      u.wr.Txt  (brand);
      u.wr.Bool (traced);
      u.wr.NL   ();
    END
  END declare_pointer;


PROCEDURE declare_indirect (u: U;  type, target: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_indirect");
      u.wr.Tipe (type);
      u.wr.Tipe (target);
      u.wr.NL   ();
    END
  END declare_indirect;


PROCEDURE declare_proctype (u: U;  type: TypeUID;  n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_proctype");
      u.wr.Tipe (type);
      u.wr.Int  (n_formals);
      u.wr.Tipe (result);
      u.wr.Int  (n_raises);
      u.wr.Txt  (cc.name);
      u.wr.NL   ();
    END
  END declare_proctype;

PROCEDURE declare_formal (u: U;  n: Name;  type: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_formal");
      u.wr.ZName (n);
      u.wr.Tipe  (type);
      u.wr.NL    ();
    END
  END declare_formal;

PROCEDURE declare_raises (u: U;  n: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_raises");
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END declare_raises;


PROCEDURE declare_object (u: U;  type, super: TypeUID;
                          brand: TEXT;  traced: BOOLEAN;
                          n_fields, n_methods: INTEGER;
                          field_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_object");
      u.wr.Tipe (type);
      u.wr.Tipe (super);
      u.wr.Txt  (brand);
      u.wr.Bool (traced);
      u.wr.Int  (n_fields);
      u.wr.Int  (n_methods);
      u.wr.BInt (field_size);
      u.wr.NL   ();
    END
  END declare_object;

PROCEDURE declare_method (u: U;  n: Name;  signature: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_method");
      u.wr.ZName (n);
      u.wr.Tipe  (signature);
      u.wr.NL    ();
    END
  END declare_method;

PROCEDURE declare_opaque (u: U;  type, super: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_opaque");
      u.wr.Tipe  (type);
      u.wr.Tipe  (super);
      u.wr.NL    ();
    END
  END declare_opaque;

PROCEDURE reveal_opaque (u: U;  lhs, rhs: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("reveal_opaque");
      u.wr.Tipe  (lhs);
      u.wr.Tipe  (rhs);
      u.wr.NL    ();
    END
  END reveal_opaque;

PROCEDURE declare_exception (u: U;  n: Name;  arg_type: TypeUID;
                           raise_proc: BOOLEAN;  base: Var;  offset: INTEGER) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_exception");
      u.wr.ZName (n);
      u.wr.Tipe  (arg_type);
      u.wr.Bool  (raise_proc);
      u.wr.VName (base);
      u.wr.Int   (offset);
      u.wr.NL    ();
    END
  END declare_exception;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (u: U;  n: Name;  p: Proc) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_runtime_proc");
      u.wr.ZName (n);
      u.wr.PName (p);
      u.wr.NL    ();
    END;
  END set_runtime_proc;

PROCEDURE set_runtime_hook (u: U;  n: Name;  v: Var;  o: ByteOffset) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_runtime_hook");
      u.wr.ZName (n);
      u.wr.VName (v);
      u.wr.Int   (o);
      u.wr.NL    ();
    END;
  END set_runtime_hook;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global (u: U;  n: Name;  s: ByteSize;  a: Alignment; type: Type;  m3t: TypeUID): Var =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("import_global");
      u.wr.ZName (n);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Tipe  (m3t);
      (*u.wr.VName (v);*)
      u.wr.NL    ();
    END;
    RETURN NIL;
  END import_global;

PROCEDURE declare_segment (u: U;  n: Name;  m3t: TypeUID;  is_const: BOOLEAN): Var =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_segment");
      u.wr.ZName (n);
      u.wr.Tipe  (m3t);
      u.wr.Bool  (is_const);
      (*u.wr.VName (v);*)
      u.wr.NL    ();
    END;
    RETURN NIL;
  END declare_segment;

PROCEDURE bind_segment (u: U;  v: Var;  s: ByteSize;  a: Alignment;
                        type: Type;  exported, inited: BOOLEAN) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("bind_segment");
      u.wr.VName (v);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Bool  (exported);
      u.wr.Bool  (inited);
      u.wr.NL    ();
    END
  END bind_segment;

PROCEDURE declare_global (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     type: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN DeclareGlobal(u, n, s, a, type, m3t, exported, inited, FALSE);
  END declare_global;

PROCEDURE declare_constant (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     type: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN DeclareGlobal(u, n, s, a, type, m3t, exported, inited, TRUE);
  END declare_constant;

PROCEDURE DeclareGlobal (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         type: Type;  m3t: TypeUID;
                         exported, inited, is_const: BOOLEAN): Var =
  CONST DeclTag = ARRAY BOOLEAN OF TEXT { "declare_global", "declare_constant" };
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   (DeclTag [is_const]);
      u.wr.ZName (n);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Tipe  (m3t);
      u.wr.Bool  (exported);
      u.wr.Bool  (inited);
      (*u.wr.VName (v);*)
      u.wr.NL    ();
    END;
    RETURN NIL;
  END DeclareGlobal;

PROCEDURE declare_local (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         type: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_local");
      u.wr.ZName (n);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Tipe  (m3t);
      u.wr.Bool  (in_memory);
      u.wr.Bool  (up_level);
      u.wr.Int   (f);
      (*u.wr.VName (v);
      u.wr.Int   (v.offset);*)
      u.wr.NL    ();
    END;
    RETURN NIL;
  END declare_local;

PROCEDURE declare_param (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         type: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_param");
      u.wr.ZName (n);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Tipe  (m3t);
      u.wr.Bool  (in_memory);
      u.wr.Bool  (up_level);
      u.wr.Int   (f);
      (*u.wr.VName (v);
      u.wr.Int   (v.offset);*)
      u.wr.NL    ();
    END;

    RETURN NIL;
  END declare_param;

PROCEDURE declare_temp (u: U; s: ByteSize; a: Alignment; type: Type; in_memory:BOOLEAN): Var =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_temp");
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Bool  (in_memory);
      (*u.wr.VName (v);
      u.wr.Int   (v.offset);*)
      u.wr.NL    ();
    END;
    RETURN NIL;
  END declare_temp;

PROCEDURE free_temp (u: U;  v: Var) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("free_temp");
      u.wr.VName (v);
      u.wr.NL    ();
    END;
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (u: U;  v: Var) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_init");
      u.wr.VName (v);
      u.wr.NL    ();
    END;
  END begin_init;

PROCEDURE end_init (u: U;  v: Var) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_init");
      u.wr.VName (v);
      u.wr.NL    ();
    END;
  END end_init;

PROCEDURE init_int (u: U; o: ByteOffset; READONLY value: Target.Int; type: Type) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_int");
      u.wr.Int   (o);
      u.wr.TInt  (TIntN.FromTargetInt(value, CG_Bytes[type]));
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END init_int;

PROCEDURE init_proc (u: U;  o: ByteOffset;  value: Proc) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_proc");
      u.wr.Int   (o);
      u.wr.PName (value);
      u.wr.NL    ();
    END;

  END init_proc;

PROCEDURE init_label (u: U;  o: ByteOffset;  value: Label) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_label");
      u.wr.Int   (o);
      u.wr.Lab   (value);
      u.wr.NL    ();
    END;
  END init_label;

PROCEDURE init_var (u: U;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_var");
      u.wr.Int   (o);
      u.wr.VName (value);
      u.wr.Int   (bias);
      u.wr.NL    ();
    END;
  END init_var;

PROCEDURE init_offset (u: U;  o: ByteOffset;  value: Var) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_offset");
      u.wr.Int   (o);
      u.wr.VName (value);
      u.wr.NL    ();
    END;
  END init_offset;

PROCEDURE init_chars (u: U;  o: ByteOffset;  value: TEXT) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_chars");
      u.wr.Int   (o);
      u.wr.Txt   (value);
      u.wr.NL    ();
    END;
  END init_chars;

PROCEDURE init_float (u: U;  o: ByteOffset;  READONLY f: Target.Float) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_float");
      u.wr.Int   (o);
      u.wr.Flt   (f);
      u.wr.NL    ();
    END;
  END init_float;

(*------------------------------------------------------------ PROCEDUREs ---*)

PROCEDURE import_procedure (u: U;  n: Name;  n_params: INTEGER;
                            ret_type: Type;  cc: CallingConvention): Proc =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("import_procedure");
      u.wr.ZName (n);
      u.wr.Int   (n_params);
      u.wr.TName (ret_type);
      u.wr.Txt   (cc.name);
      (*u.wr.PName (p);*)
      u.wr.NL    ();
    END;
    RETURN NIL;
  END import_procedure;

PROCEDURE declare_procedure (u: U;  n: Name;  n_params: INTEGER;
                             return_type: Type;  lev: INTEGER;
                             cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc): Proc =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_procedure");
      u.wr.ZName (n);
      u.wr.Int   (n_params);
      u.wr.TName (return_type);
      u.wr.Int   (lev);
      u.wr.Txt   (cc.name);
      u.wr.Bool  (exported);
      u.wr.PName (parent);
      (*u.wr.PName (p);*)
      u.wr.NL    ();
    END;
    RETURN NIL;
  END declare_procedure;

PROCEDURE begin_procedure (u: U;  p: Proc) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_procedure");
      u.wr.PName (p);
      u.wr.NL    ();
    END;
  END begin_procedure;

PROCEDURE end_procedure (u: U;  p: Proc) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_procedure");
      u.wr.PName (p);
      u.wr.NL    ();
    END;
  END end_procedure;

PROCEDURE begin_block (u: U) =
  (* marks the beginning of a nested anonymous block *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_block");
      u.wr.NL    ();
    END;
  END begin_block;

PROCEDURE end_block (u: U) =
  (* marks the ending of a nested anonymous block *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_block");
      u.wr.NL    ();
    END;
  END end_block;

PROCEDURE note_procedure_origin (u: U;  p: Proc) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("note_procedure_origin");
      u.wr.PName (p);
      u.wr.NL    ();
    END
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (u: U;  label: Label;  <*UNUSED*> barrier: BOOLEAN) =
  (* define 'label' to be at the current pc *)
  BEGIN
    print(u, "L" & Fmt.Int(label) & ":;\n");
  END set_label;

PROCEDURE jump (u: U; label: Label) =
  (* GOTO label *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("jump");
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    print(u, "goto L" & Fmt.Int(label) & ";\n");
  END jump;

PROCEDURE if_true  (u: U;  type: IType;  label: Label; <*UNUSED*> f: Frequency) =
  (* IF (s0.type # 0) GOTO label ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_true");
      u.wr.TName (type);
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;    
    print(u, "if (" & pop(u) & ") goto L" & Fmt.Int(label) & ";\n");
  END if_true;

PROCEDURE if_false (u: U;   type: IType;  label: Label; <*UNUSED*> f: Frequency) =
  (* IF (s0.type = 0) GOTO label ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_false");
      u.wr.TName (type);
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    print(u, "if (!(" & pop(u) & ")) goto L" & Fmt.Int(label) & ";\n");
  END if_false;

PROCEDURE if_compare (u: U;  type: ZType;  op: CompareOp;  label: Label;
                      <*UNUSED*> f: Frequency) =
  (* IF (s1.type  op  s0.type) GOTO label ; pop(2) *)
  VAR s0 := pop(u);
      s1 := pop(u);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_compare");
      u.wr.TName (type);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    print(u, "\n/* if_compare */\n");
    print(u, "if ((" & s1 & ")" & "op" & "(" & s0 & ")) goto L" & Fmt.Int(label) & ";\n");
  END if_compare;

PROCEDURE case_jump (u: U;  type: IType;  READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.type] ; pop" with no range checking on s0.type *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("case_jump");
      u.wr.TName (type);
      u.wr.Int   (NUMBER(labels));
      FOR i := FIRST (labels) TO LAST (labels) DO  u.wr.Lab (labels [i]);  END;
      u.wr.NL    ();
    END;
    print(u, "\n/* case_jump */\n");
  END case_jump;

PROCEDURE exit_proc (u: U; type: Type) =
  (* Returns s0.type if type is not Void, otherwise returns no value. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exit_proc");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* exit_proc */\n");
    print(u, "goto LExit;\n");
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load  (u: U;  v: Var;  o: ByteOffset;  type: MType;  type_multiple_of_32: ZType) =
(* push; s0.u := Mem [ ADR(v) + o ].type ;  The only allowed (type->u) conversions
   are {Int,Word}{8,16} -> {Int,Word}{32,64} and {Int,Word}32 -> {Int,Word}64.
   The source type, type, determines whether the value is sign-extended or
   zero-extended. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load");
      u.wr.VName (v);
      u.wr.Int   (o);
      u.wr.TName (type);
      u.wr.TName (type_multiple_of_32);
      u.wr.NL    ();
    END;
    print(u, "/* load */\n");
    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>
  END load;

PROCEDURE store (u: U;  v: Var;  o: ByteOffset;  type_multiple_of_32: ZType;  type: MType;  ) =
(* Mem [ ADR(v) + o ].u := s0.type; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store");
      u.wr.VName (v);
      u.wr.Int   (o);
      u.wr.TName (type_multiple_of_32);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* store */\n");
    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>
  END store;

PROCEDURE load_address (u: U;  v: Var;  o: ByteOffset) =
(* push; s0.A := ADR(v) + o *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_address");
      u.wr.VName (v);
      u.wr.Int   (o);
      u.wr.NL    ();
    END;
    print(u, "\n/* load_address */\n");
  END load_address;

PROCEDURE load_indirect (u: U;  o: ByteOffset;  type: MType;  type_multiple_of_32: ZType) =
(* s0.type_multiple_of_32 := Mem [s0.A + o].type  *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_indirect");
      u.wr.Int   (o);
      u.wr.TName (type);
      u.wr.TName (type_multiple_of_32);
      u.wr.NL    ();
    END;
    print(u, "\n/* load_indirect */\n");
    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>
  END load_indirect;

PROCEDURE store_indirect (u: U;  o: ByteOffset;  type_multiple_of_32: ZType;  type: MType) =
(* Mem [s1.A + o].type := s0.type_multiple_of_32; pop (2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store_indirect");
      u.wr.Int   (o);
      u.wr.TName (type_multiple_of_32);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* store_indirect */\n");
    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (u: U) =
  (* push ; s0.A := a *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_nil");
      u.wr.NL    ();
    END;
    print(u, "\n/* load_nil */\n");
  END load_nil;

PROCEDURE load_integer  (u: U;  type: IType;  READONLY j: Target.Int) =
  (* push ; s0.type := i *)
  VAR i := TIntN.FromTargetInt(j, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_integer");
      u.wr.TName (type);
      u.wr.TInt  (i);
      u.wr.NL    ();
    END;
    print(u, "\n/* load_integer */\n");
  END load_integer;

PROCEDURE load_float    (u: U;  type: RType;  READONLY f: Target.Float) =
  (* push ; s0.type := f *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_float");
      u.wr.TName (type);
      u.wr.Flt   (f);
      u.wr.NL    ();
    END;
    print(u, "\n/* load_float */\n");
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE compare (u: U;  type: ZType; result_type: IType;  op: CompareOp) =
  (* s1.result_type := (s1.type  op  s0.type)  ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("compare");
      u.wr.TName (type);
      u.wr.TName (result_type);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.NL    ();
    END;
    print(u, "\n/* compare */\n");
    (* ASSERT cond # Cond.Z AND cond # Cond.NZ *)
  END compare;

PROCEDURE add (u: U;  type: AType) =
  (* s1.type := s1.type + s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* add */\n");
  END add;

PROCEDURE subtract (u: U;  type: AType) =
  (* s1.type := s1.type - s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("subtract");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* subtract */\n");
  END subtract;

PROCEDURE multiply (u: U;  type: AType) =
  (* s1.type := s1.type * s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("multiply");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* multiply */\n");
    (*
      stack[1] := m3_div & "type(" ((type)stack[1]) & "/" & (type)stack[0] & ")"
      pop();
    *)
  END multiply;

PROCEDURE divide (u: U;  type: RType) =
  (* s1.type := s1.type / s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("divide");
      u.wr.TName (type);
      u.wr.NL    ();
    END;    
    print(u, "\n/* divide */\n");
    (*
      stack[1] := ((type)stack[1]) & "/" & (type)stack[0]
      pop();
    *)
  END divide;

CONST SignName = ARRAY Sign OF TEXT { " P", " N", " X" };

PROCEDURE div (u: U;  type: IType;  a, b: Sign) =
  (* s1.type := s1.type DIV s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("div");
      u.wr.TName (type);
      u.wr.OutT  (SignName [a]);
      u.wr.OutT  (SignName [b]);
      u.wr.NL    ();
    END;
    print(u, "\n/* div */\n");
    (*
      stack[1] := m3_div & "type(" ((type)stack[1]) & "/" & (type)stack[0] & ")"
      pop();
    *)
  END div;

PROCEDURE mod (u: U;  type: IType;  a, b: Sign) =
  (* s1.type := s1.type MOD s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("mod");
      u.wr.TName (type);
      u.wr.OutT  (SignName [a]);
      u.wr.OutT  (SignName [b]);
      u.wr.NL    ();
    END;
    print(u, "\n/* mod */\n");
    (*
      stack[1] := m3_div & "type(" ((type)stack[1]) % "/" & (type)stack[0] & ")"
      pop();
    *)
  END mod;

PROCEDURE negate (u: U;  type: AType) =
  (* s0.type := - s0.type *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("negate");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* negate */\n");
    (*
      stack[0] := - "(type)" & stack[0]
    *)
  END negate;

PROCEDURE abs (u: U;  type: AType) =
  (* s0.type := ABS (s0.type) (noop on Words) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abs");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* abs */\n");
    (*
      stack[0] := "m3_abs&type(" & stack[0] & ")"
    *)
  END abs;

PROCEDURE max (u: U;  type: ZType) =
  (* s1.type := MAX (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("max");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* max */\n");
    (*
      stack[1] := "m3_max&type(" & stack[0] & "," stack[1] & ")"
      pop();
    *)
  END max;

PROCEDURE min (u: U;  type: ZType) =
  (* s1.type := MIN (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("min");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* min */\n");
    (*
      stack[1] := "m3_min&type(" & stack[0] & "," stack[1] & ")"
      pop();
    *)
  END min;

PROCEDURE cvt_int (u: U;  type: RType;  x: IType;  op: ConvertOp) =
  (* s0.x := ROUND (s0.type) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_int");
      u.wr.TName (type);
      u.wr.TName (x);
      u.wr.OutT  (ConvertOpName [op]);
      u.wr.NL    ();
    END;
    print(u, "\n/* cvt_int */\n");
    (*
      stack[0] := "((long)(" & stack[0] & ")"
    *)
  END cvt_int;

PROCEDURE cvt_float (u: U;  type: AType;  x: RType) =
  (* s0.x := FLOAT (s0.type, x) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_float");
      u.wr.TName (type);
      u.wr.TName (x);
      u.wr.NL    ();
    END;
    print(u, "\n/* cvt_float */\n");
    (*
      stack[0] := "((double)(" & stack[0] & ")"
    *)
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_op3(u: U;  s: ByteSize; op: TEXT) =
  (* s2.B := s1.B op s0.B ; pop(3) *)
  BEGIN
    IF u.debug THEN
      (*u.wr.Cmd   (BuiltinDesc[builtin].name);*)
      u.wr.Int   (s);
      u.wr.NL    ();
    END;  
    print(u, "\n/* " & op & " */\n");
    (*
      stack[2] := "m3_" & op & "(" & stack[0] & stack[1] & stack[2] & ")"
      pop();
      pop();
    *)
  END set_op3;

PROCEDURE set_union (u: U;  s: ByteSize) =
  (* s2.B := s1.B + s0.B ; pop(2) *)
  BEGIN
    set_op3(u, s, "set_union");
  END set_union;

PROCEDURE set_difference (u: U;  s: ByteSize) =
  (* s2.B := s1.B - s0.B ; pop(2) *)
  BEGIN
    set_op3(u, s, "set_difference");
  END set_difference;

PROCEDURE set_intersection (u: U;  s: ByteSize) =
  (* s2.B := s1.B * s0.B ; pop(2) *)
  BEGIN
    set_op3(u, s, "set_intersection");
  END set_intersection;

PROCEDURE set_sym_difference (u: U;  s: ByteSize) =
  (* s2.B := s1.B / s0.B ; pop(2) *)
  BEGIN
    set_op3(u, s, "set_sym_difference");
  END set_sym_difference;

PROCEDURE set_member (u: U;  s: ByteSize;  type: IType) =
  (* s1.type := (s0.type IN s1.B) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_member");
      u.wr.Int   (s);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* set_member */\n");
  END set_member;

PROCEDURE set_compare (u: U;  s: ByteSize;  op: CompareOp;  type: IType) =
  (* s1.type := (s1.B  op  s0.B)  ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_compare");
      u.wr.Int   (s);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* set_compare */\n");
  END set_compare;

PROCEDURE set_range (u: U;  s: ByteSize;  type: IType) =
  (* s2.A [s1.type .. s0.type] := 1's; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_range");
      u.wr.Int   (s);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* set_range */\n");
  END set_range;

PROCEDURE set_singleton (u: U;  s: ByteSize;  type: IType) =
  (* s1.A [s0.type] := 1; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_singleton");
      u.wr.Int   (s);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* set_singleton */\n");
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (u: U;  type: IType) =
  (* s0.type := Word.Not (s0.type) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("not");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* not */\n");
    (*
    stack[0] := "((type)~(type)" & stack[0] & ")";
    *)
  END not;

PROCEDURE and (u: U;  type: IType) =
  (* s1.type := Word.And (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("and");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* and */\n");
    (*
    stack[1] := "(((type)" & stack[0] & ") & (type)(stack[1] & "))";
    pop();
    *)
  END and;

PROCEDURE or  (u: U;  type: IType) =
  (* s1.type := Word.Or  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("or");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* or */\n");
    (*
    stack[1] := "(((type)" & stack[0] & ") | (type)(stack[1] & "))";
    pop();
    *)
  END or;

PROCEDURE xor (u: U;  type: IType) =
  (* s1.type := Word.Xor (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("xor");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* xor */\n");
    (*
    stack[1] := "(((type)" & stack[0] & ") ^ (type)(stack[1] & "))";
    pop();
    *)
  END xor;

PROCEDURE shift_left   (u: U;  type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_left");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* shift_left */\n");
    (*
    stack[1] := "(((type)" & stack[1] & ") << (type)(stack[0] & "))";
    pop();
    *)
  END shift_left;

PROCEDURE shift_right  (u: U;  type: IType) =
  (* s1.type := Word.Shift  (s1.type, -s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_right");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* shift_right */\n");
    (*
    stack[1] := "((type)(((unsigned type)" & stack[1] & ") >> (type)(stack[0] & ")))";
    pop();
    *)
  END shift_right;

PROCEDURE shift (u: U;  type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* shift */\n");
  END shift;

PROCEDURE rotate (u: U;  type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* rotate */\n");
  END rotate;

PROCEDURE rotate_left  (u: U;  type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_left");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* rotate_left */\n");
  END rotate_left;

PROCEDURE rotate_right (u: U;  type: IType) =
  (* s1.type := Word.Rotate (s1.type, -s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_right");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* rotate_right */\n");
  END rotate_right;

PROCEDURE widen (u: U;  sign_extend: BOOLEAN) =
  (* s0.I64 := s0.I32;  IF sign_extend THEN SignExtend s0;  *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("widen");
      u.wr.Bool  (sign_extend);
      u.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END widen;

PROCEDURE chop (u: U) =
  (* s0.I32 := Word.And (s0.I64, 16_ffffffff);  *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("chop");
      u.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END chop;

PROCEDURE extract (u: U;  type: IType;  sign_extend: BOOLEAN) =
  (* s2.type := Word.Extract(s2.type, s1.type, s0.type);
     IF sign_extend THEN SignExtend s2 END; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.NL    ();
    END;
    print(u, "\n/* extract */\n");
  END extract;

PROCEDURE extract_n (u: U;  type: IType;  sign_extend: BOOLEAN;  n: CARDINAL) =
  (* s1.type := Word.Extract(s1.type, s0.type, n);
     IF sign_extend THEN SignExtend s1 END; pop(1) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract_n");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
    print(u, "\n/* extract_m */\n");
  END extract_n;

PROCEDURE extract_mn (u: U;  type: IType;  sign_extend: BOOLEAN;  m, n: CARDINAL) =
  (* s0.type := Word.Extract(s0.type, m, n);
     IF sign_extend THEN SignExtend s0 END; *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract_mn");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.Int   (m);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
    print(u, "\n/* extract_mn */\n");
  END extract_mn;

PROCEDURE insert  (u: U;  type: IType) =
  (* s3.type := Word.Insert (s3.type, s2.type, s1.type, s0.type) ; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* insert */\n");
  END insert;

PROCEDURE insert_n  (u: U;  type: IType;  n: CARDINAL) =
  (* s2.type := Word.Insert (s2.type, s1.type, s0.type, n) ; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_n");
      u.wr.TName (type);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
    print(u, "\n/* insert_n */\n");
  END insert_n;

PROCEDURE insert_mn  (u: U;  type: IType;  m, n: CARDINAL) =
  (* s1.type := Word.Insert (s1.type, s0.type, m, n) ; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_mn");
      u.wr.TName (type);
      u.wr.Int   (m);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
    print(u, "\n/* insert_mn */\n");
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (u: U;  a, b: Type) =
  (* tmp := s1 ; s1 := s0 ; s0 := tmp *)
  VAR temp := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("swap");
      u.wr.TName (a);
      u.wr.TName (b);
      u.wr.NL    ();
    END;
    u.stack.put(1, get(u, 0));
    u.stack.put(0, temp);
  END swap;

PROCEDURE cg_pop (u: U;  type: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* pop */\n");
    EVAL pop(u);
  END cg_pop;

PROCEDURE copy_n (u: U;  type_multiple_of_32: IType;  type: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:s0.type_multiple_of_32] := Mem[s1.A:s0.type_multiple_of_32]; pop(3)*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy_n");
      u.wr.TName (type_multiple_of_32);
      u.wr.TName (type);
      u.wr.Bool  (overlap);
      u.wr.NL    ();
    END;
    print(u, "\n/* copy_n */\n");
  END copy_n;

PROCEDURE copy (u: U;  n: INTEGER;  type: MType;  overlap: BOOLEAN) =
  (* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy");
      u.wr.Int   (n);
      u.wr.TName (type);
      u.wr.Bool  (overlap);
      u.wr.NL    ();
    END;
    print(u, "\n/* copy */\n");
  END copy;

PROCEDURE zero_n (u: U;  type_multiple_of_32: IType;  type: MType) =
  (* Mem[s1.A:s0.type_multiple_of_32] := 0; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("zero_n");
      u.wr.TName (type_multiple_of_32);
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    <* ASSERT FALSE *>

    (* zero_n is implemented incorrectly in the gcc backend,
     * therefore it must not be used.
    *)
  END zero_n;

PROCEDURE zero (u: U;  n: INTEGER;  type: MType) =
  (* Mem[s0.A:sz] := 0; pop(1) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("zero");
      u.wr.Int   (n);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "\n/* zero */\n");
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (u: U;  from, to: ZType) =
  (* s0.to := LOOPHOLE(s0.from, to) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("loophole");
      u.wr.TName (from);
      u.wr.TName (to);
      u.wr.NL    ();
    END;
    print(u, "\n/* loophole */\n");
    (* If type is already a pointer, then we should not add pointer here.
     * As well, if type does not contain a pointer, then we should store the
     * value in a non-stack-packed temporary and use its address.
     * We don't have code to queue up temporary declarations.
     * (for that matter, to limit/reuse temporaries)
     *)
    u.stack.put(0, "*(" & TypeNames[to] & "*)&" & get(u, 0));
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (u: U;  code: RuntimeError) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abort");
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "\n/* abort */\n");
    print(u, "{ m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END abort;

PROCEDURE check_nil (u: U;  code: RuntimeError) =
  (* IF (s0.A = NIL) THEN abort(code) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_nil");
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "\n/* check_nil */\n");
    print(u, "{ const PVOID _s0 = " & get(u, 0) & ";\n");
    print(u, "  if (_s0 == NULL) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_nil;

PROCEDURE check_lo (u: U;  type: IType;  READONLY j: Target.Int;  code: RuntimeError) =
  (* IF (s0.type < i) THEN abort(code) *)
  VAR typename := TypeNames[type];
      i := TIntN.FromTargetInt(j, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_lo");
      u.wr.TName (type);
      u.wr.TInt  (i);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "\n/* check_lo */\n");
    print(u, "{ const " & typename & " _i = " & TIntN.ToText(i) & ";\n");
    print(u, "  const " & typename & " _s0 = " & get(u, 0) & ";\n");
    print(u, "  if (_s0 < _i) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_lo;

PROCEDURE check_hi (u: U;  type: IType;  READONLY j: Target.Int;  code: RuntimeError) =
  (* IF (i < s0.type) THEN abort(code) *)
  VAR typename := TypeNames[type];
      i := TIntN.FromTargetInt(j, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_hi");
      u.wr.TName (type);
      u.wr.TInt  (i);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "\n/* check_hi */\n");
    print(u, "{ const " & typename & " _i = " & TIntN.ToText(i) & ";\n");
    print(u, "  const " & typename & " _s0 = " & get(u, 0) & ";\n");
    print(u, "  if (_i < _s0) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_hi;

PROCEDURE check_range (u: U;  type: IType;  READONLY xa, xb: Target.Int;  code: RuntimeError) =
  (* IF (s0.type < a) OR (b < s0.type) THEN abort(code) *)
  VAR typename := TypeNames[type];
      a := TIntN.FromTargetInt(xa, CG_Bytes[type]);
      b := TIntN.FromTargetInt(xb, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_range");
      u.wr.TInt  (a);
      u.wr.TInt  (b);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "\n/* check_range */\n");
    print(u, "{ const " & typename & " _a = " & TIntN.ToText(a) & ";\n");
    print(u, "  const " & typename & " _b = " & TIntN.ToText(b) & ";\n");
    print(u, "  const " & typename & " _s0 = " & get(u, 0) & ";\n");
    print(u, "  if ((_s0 < _a) || (_b < _s0)) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_range;

PROCEDURE check_index (u: U;  type: IType;  code: RuntimeError) =
  (* IF NOT (0 <= s1.type < s0.type) THEN
       abort(code)
     END;
     pop *)
  (* s0.type is guaranteed to be positive so the unsigned
     check (s0.W <= s1.W) is sufficient. *)
  VAR typename := TypeNames[type];
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_index");
      u.wr.TName (type);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "\n/* check_index */\n");
    print(u, "{ const " & typename & " _array_size = " & pop(u) & ";\n");
    print(u, "  const " & typename & " _index = " & get(u, 0) & ";\n");
    print(u, "  if (_array_size <= _index) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_index;

PROCEDURE check_eq (u: U;  type: IType;  code: RuntimeError) =
  (* IF (s0.type # s1.type) THEN
       abort(code);
       Pop (2) *)
  VAR typename := TypeNames[type];
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_eq");
      u.wr.TName (type);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "\n/* check_eq */\n");
    print(u, "{ const " & typename & " _s0 = " & pop(u) & ";\n");
    print(u, "  const " & typename & " _s1 = " & pop(u) & ";\n");
    print(u, "  if (_s0 != s_1) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_eq;

<*NOWARN*>PROCEDURE reportfault (u: U;  code: RuntimeError) =
  BEGIN
  END reportfault;

<*NOWARN*>PROCEDURE makereportproc (u: U) =
  BEGIN
  END makereportproc;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (u: U; i: INTEGER) =
  (* s0.A := s0.A + i *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add_offset");
      u.wr.Int   (i);
      u.wr.NL    ();
    END;
  END add_offset;

PROCEDURE index_address (u: U;  type: IType;  size: INTEGER) =
  (* s1.A := s1.A + s0.type * size ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("index_address");
      u.wr.TName (type);
      u.wr.Int   (size);
      u.wr.NL    ();
    END;
  END index_address;

(*------------------------------------------------------- PROCEDURE calls ---*)

PROCEDURE start_call_direct (u: U;  p: Proc;  lev: INTEGER;  type: Type) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_direct");
      u.wr.PName (p);
      u.wr.Int   (lev);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END start_call_direct;

PROCEDURE start_call_indirect (u: U;  type: Type;  cc: CallingConvention) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_indirect");
      u.wr.TName (type);
      u.wr.Txt   (cc.name);
      u.wr.NL    ();
    END;
  END start_call_indirect;

PROCEDURE pop_param (u: U;  type: MType) =
  (* pop s0 and make it the "next" parameter in the current call *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_param");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END pop_param;

PROCEDURE pop_struct (u: U;  type: TypeUID;  s: ByteSize;  a: Alignment) =
  (* pop s0 and make it the "next" parameter in the current call
   * NOTE: it is passed by value *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_struct");
      u.wr.Tipe  (type);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.NL    ();
    END;
  END pop_struct;

PROCEDURE pop_static_link (u: U) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_static_link");
      u.wr.NL    ();
    END;
  END pop_static_link;

PROCEDURE call_direct (u: U; p: Proc;  type: Type) =
  (* call the procedure identified by block b.  The procedure
     returns a value of type type. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_direct");
      u.wr.PName (p);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END call_direct;

PROCEDURE call_indirect (u: U; type: Type;  cc: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0.  The
     procedure returns a value of type type. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_indirect");
      u.wr.TName (type);
      u.wr.Txt   (cc.name);
      u.wr.NL    ();
    END;
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (u: U;  p: Proc) =
  (* push; s0.A := ADDR (p's body) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_procedure");
      u.wr.PName (p);
      u.wr.NL    ();
    END;
  END load_procedure;

PROCEDURE load_static_link (u: U;  p: Proc) =
  (* push; s0.A := (static link needed to call p, NIL for top-level procs) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_static_link");
      u.wr.PName (p);
      u.wr.NL    ();
    END;
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (u: U;  a, b, c, d: TEXT := NIL) =
  VAR i: INTEGER := -1;
  BEGIN
    Cmt (u, a, i);
    Cmt (u, b, i);
    Cmt (u, c, i);
    Cmt (u, d, i);
    Cmt (u, "\n", i);
  END comment;

PROCEDURE Cmt (u: U;  text: TEXT;  VAR width: INTEGER) =
  VAR ch: CHAR;
  BEGIN
    IF (NOT u.debug) OR (text = NIL) THEN
      RETURN
    END;
    FOR i := 0 TO Text.Length (text) - 1 DO
      ch := Text.GetChar (text, i);
      IF (ch = '\n' OR ch = '\r') THEN
        u.wr.OutC (ch);
        width := -1;
      ELSE
        IF (width = -1) THEN
          u.wr.OutT ("\t# ");
          width := 0;
        END;
        u.wr.OutC (ch);
      END
    END;
  END Cmt;


(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered (x: U; type_multiple_of_32: ZType; type: MType; <*UNUSED*>order: MemoryOrder) =
(* Mem [s1.A].u := s0.type;
   pop (2) *)
  BEGIN
    IF x.debug THEN
      x.wr.Cmd   ("store_ordered");
      x.wr.TName (type_multiple_of_32);
      x.wr.TName (type);
      x.wr.NL    ();
    END;
  END store_ordered;

PROCEDURE load_ordered (x: U; type: MType; type_multiple_of_32: ZType; <*UNUSED*>order: MemoryOrder) =
(* s0.type_multiple_of_32 := Mem [s0.A].type  *)
  BEGIN
    IF x.debug THEN
      x.wr.Cmd   ("load_ordered");
      x.wr.TName (type);
      x.wr.TName (type_multiple_of_32);
      x.wr.NL    ();
    END;
  END load_ordered;

PROCEDURE exchange (u: U; type: MType; type_multiple_of_32: ZType; <*UNUSED*>order: MemoryOrder) =
(* tmp := Mem [s1.A + o].type;
   Mem [s1.A + o].type := s0.type_multiple_of_32;
   s0.type_multiple_of_32 := tmp;
   pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exchange");
      u.wr.TName (type);
      u.wr.TName (type_multiple_of_32);
      u.wr.NL    ();
    END;
  END exchange;

PROCEDURE compare_exchange (x: U; type: MType; type_multiple_of_32: ZType; result_type: IType;
                            <*UNUSED*>success, failure: MemoryOrder) =
(* original := Mem[s2.A].type;
   spurious_failure := whatever;
   IF original = Mem[s1.A].type AND NOT spurious_failure THEN
     Mem [s2.A].type := s0.type_multiple_of_32;
     s2.result_type := 1;
   ELSE
     Mem [s2.A].type := original; x86 really does rewrite the original value, atomically
     s2.result_type := 0;
   END;
   pop(2);
   This is permitted to fail spuriously.
   That is, even if Mem[s2.a] = Mem[s1.a], we might
     still go down the then branch.
*)
  BEGIN

    IF x.debug THEN
      x.wr.Cmd   ("compare_exchange");
      x.wr.TName (type);
      x.wr.TName (type_multiple_of_32);
      x.wr.TName (result_type);
      x.wr.NL    ();
    END;
  END compare_exchange;

PROCEDURE fence (u: U; <*UNUSED*>order: MemoryOrder) =
(*
 * x86: Exchanging any memory with any register is a serializing instruction.
 *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("fence");
      u.wr.NL    ();
    END;
  END fence;

CONST AtomicOpName = ARRAY AtomicOp OF TEXT { "add", "sub", "or", "and", "xor" };

PROCEDURE fetch_and_op (x: U; atomic_op: AtomicOp; type: MType; type_multiple_of_32: ZType;
                        <*UNUSED*>order: MemoryOrder) =
(* original := Mem [s1.A].type;
   Mem [s1.A].type := original op s0.type_multiple_of_32;
   s1.type_multiple_of_32 := original;
   pop

=> store the new value, return the old value

Generally we use interlocked compare exchange loop.
Some operations can be done better though.
*)
  BEGIN
    IF x.debug THEN
      x.wr.Cmd   ("fetch_and_op");
      x.wr.OutT  (AtomicOpName[atomic_op]);
      x.wr.TName (type);
      x.wr.TName (type_multiple_of_32);
      x.wr.NL    ();
    END;
  END fetch_and_op;

BEGIN
END M3C.
