(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE M3CG_Binary;

(* NOTE: If you alter this file substantively, do the following: 

   1. Please update CONST Version, so front/back end mismatches will be
      detected. 

   2. Also make matching changes to cm3/m3-sys/m3cggen/src/Main.m3.

   3. Recompile cm3, m3cggen and lots of packages they depend on, in order.
      cm3/scripts/do-cm3-front.sh will do it.  

   4. Run the executable m3cggen, copying its std output to 
      cm3/m3-sys/m3cc/gcc/gcc/m3cg/m3cg.h.

   5. Recompile m3cc.

   This will create a compatible cm3/cm3cg pair.  
*) 

CONST
  Version = 16_110;  (* version 1.10 *)
  (* Please update Version if you change the codes in this file. *) 

TYPE
  Op = {
    begin_unit, end_unit, import_unit, export_unit, set_source_file,
    set_source_line, declare_typename, declare_array,
    declare_open_array, declare_enum, declare_enum_elt,
    declare_packed, declare_record, declare_field, declare_set,
    declare_subrange, declare_pointer, declare_indirect,
    declare_proctype, declare_formal, declare_raises, declare_object,
    declare_method, declare_opaque, reveal_opaque, declare_exception,
    set_runtime_proc, import_global,
    declare_segment, bind_segment, declare_global, declare_constant,
    declare_local, declare_param, declare_temp, free_temp, begin_init,
    end_init, init_int, init_proc, init_label, init_var, init_offset,
    init_chars, init_float, import_procedure, declare_procedure,
    begin_procedure, end_procedure, begin_block, end_block,
    note_procedure_origin, set_label, jump, if_true, if_false,
    if_eq, if_ne, if_gt, if_ge, if_lt, if_le,       (* file only; in-memory uses if_compare *)
    case_jump, exit_proc, load,
    load_address, load_indirect, store, store_indirect,
    load_nil, load_integer, load_float,
    eq, ne, gt, ge, lt,  le,                        (* file only; in-memory uses compare *)
    add, subtract, multiply, divide, negate, abs, max,
    min,
    round, trunc, floor, ceiling,                   (* file only; in-memory uses cvt_int *)
    cvt_float, div, mod, set_union,
    set_difference, set_intersection, set_sym_difference, set_member,
    set_eq, set_ne, set_lt, set_le, set_gt, set_ge, (* file only; in-memory uses set_compare *)
    set_range,
    set_singleton, not, and, or, xor, shift, shift_left, shift_right,
    rotate, rotate_left, rotate_right, widen, chop, extract, extract_n,
    extract_mn, insert, insert_n, insert_mn, swap, pop, copy_n, copy,
    zero_n, zero, loophole, abort, check_nil, check_lo, check_hi,
    check_range, check_index, check_eq, add_offset, index_address,
    start_call_direct, call_direct, start_call_indirect,
    call_indirect, pop_param, pop_struct, pop_static_link,
    load_procedure, load_static_link, comment,
    store_ordered, load_ordered, exchange, compare_exchange, fence,
    fetch_and_add,      (* file only; in-memory uses fetch_and_op *)
    fetch_and_sub,      (* file only; in-memory uses fetch_and_op *)
    fetch_and_or,       (* file only; in-memory uses fetch_and_op *)
    fetch_and_and,      (* file only; in-memory uses fetch_and_op *)
    fetch_and_xor,      (* file only; in-memory uses fetch_and_op *)
    widechar_size ,     (* file only; communicates byte size WIDECHAR *)
    set_error_handler,  (* in-memory only; contains a pointer, so silently skipped in files *)
    compare,            (* in-memory only; file converts to eq/ne/etc. *)
    cvt_int,            (* in-memory only; file converts to trunc/ceiling/etc. *)
    fetch_and_op,       (* in-memory only; file converts to fetch_and_add/fetch_and_sub/etc. *)
    if_compare,         (* in-memory only; file converts to if_eq/if_ne/etc. *)
    set_compare         (* in-memory only; file converts to set_eq/set_ne/etc. *)
  };

(* Integers are encoded as sequences of unsigned bytes, [0..255].
   The length and format of the encoding is determined by the
   first byte.  The special first-byte values are:  *)
CONST
  Int1  = 255;  (* Int1,x        => x                           *)
  NInt1 = 254;  (* NInt1,x       => -x                          *)
  Int2  = 253;  (* Int2,x,y      => x + 2^8*y                   *)
  NInt2 = 252;  (* NInt2,x,y     => - (Int2,x,y)                *)
  Int4  = 251;  (* Int4,a,b,c,d  => a + 2^8*b + 2^16*c + 2^24*d *)
  NInt4 = 250;  (* NInt4,a,b,c,d => - (Int4,a,b,c,d)            *)
  Int8  = 249;  (* Int8,a,...,h  => a + 2^8*b + ... + 2^56*h    *)
  NInt8 = 248;  (* NInt8,a,...,h => - (Int8,a,...,h)            *)
  LastRegular = 247;
  (* Integer values in [0..247] are simply passed thru as single bytes *)

END M3CG_Binary.

(*
  The binary intermediate code has the following format:

    <I-code>     ::= <version> { <cmd> }
    <version>    ::= I(Version)
    <cmd>        ::= <method> { <arg> }
    <method>     ::= I(ORD(Op.<method>))
    <arg>        ::= <int> | <text> | <name> | <typeUID> | <bool>
                     <callconv> | <var> | <proc> | <type> | <label>
                     <float> | <casetbl> | <sign>
    <int:i>      ::= I(ORD(i))
    <text:t>     ::= I(Text.Length(t)) S(t)
    <name:n>     ::= I(Text.Length(M3ID.ToText(n))) S(M3ID.ToText(n))
    <typeUID>    ::= I(ORD(uid))
    <bool:b>     ::= I(ORD(b))
    <callconv:c> ::= I(c.m3cg_id)
    <var:v>      ::= I(v.uid)
    <proc:p>     ::= I(p.uid)
    <type:t>     ::= I(ORD(t))
    <label:l>    ::= I(l)
    <float:f>    ::= ??
    <casetbl:t>  ::= I(NUMBER(t)) I(t[0]) I(t[1]) ...
    <sign:s>     ::= I(ORD(s))

  Where I(x) is the variable length encoding of the integer value 'x'
  and S(x) are the bytes of the string 'x'.
*)
