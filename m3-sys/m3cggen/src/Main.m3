MODULE Main;

(* generate a C .h file that corresponds to M3CG_Binary.  *)

(* NOTE: The CONST Map must match cm3/m3-sys/m3middle/src/M3CG_Binary.i3. 
         If you change them see the notes in that file. 
*) 

IMPORT M3CG_Binary, Wr, Stdio, Text, ASCII, Fmt;

TYPE
  Op   = M3CG_Binary.Op;
  Desc = RECORD name: TEXT;  op: Op;  END;

CONST
  Map = ARRAY [Op.begin_unit..Op.widechar_size] OF Desc {
    Desc { "begin_unit", Op.begin_unit }, 
    Desc { "end_unit", Op.end_unit }, 
    Desc { "import_unit", Op.import_unit }, 
    Desc { "export_unit", Op.export_unit }, 
    Desc { "set_source_file", Op.set_source_file },
    
    Desc { "set_source_line", Op.set_source_line }, 
    Desc { "declare_typename", Op.declare_typename }, 
    Desc { "declare_array", Op.declare_array },
    
    Desc { "declare_open_array", Op.declare_open_array }, 
    Desc { "declare_enum", Op.declare_enum }, 
    Desc { "declare_enum_elt", Op.declare_enum_elt },
    
    Desc { "declare_packed", Op.declare_packed }, 
    Desc { "declare_record", Op.declare_record }, 
    Desc { "declare_field", Op.declare_field }, 
    Desc { "declare_set", Op.declare_set },
    
    Desc { "declare_subrange", Op.declare_subrange }, 
    Desc { "declare_pointer", Op.declare_pointer }, 
    Desc { "declare_indirect", Op.declare_indirect },
    
    Desc { "declare_proctype", Op.declare_proctype }, 
    Desc { "declare_formal", Op.declare_formal }, 
    Desc { "declare_raises", Op.declare_raises }, 
    Desc { "declare_object", Op.declare_object },
    
    Desc { "declare_method", Op.declare_method }, 
    Desc { "declare_opaque", Op.declare_opaque }, 
    Desc { "reveal_opaque", Op.reveal_opaque }, 
    Desc { "declare_exception", Op.declare_exception },
    
    Desc { "set_runtime_proc", Op.set_runtime_proc }, 
    Desc { "import_global", Op.import_global },
    
    Desc { "declare_segment", Op.declare_segment }, 
    Desc { "bind_segment", Op.bind_segment }, 
    Desc { "declare_global", Op.declare_global }, 
    Desc { "declare_constant", Op.declare_constant },
    
    Desc { "declare_local", Op.declare_local }, 
    Desc { "declare_param", Op.declare_param }, 
    Desc { "declare_temp", Op.declare_temp }, 
    Desc { "free_temp", Op.free_temp }, 
    Desc { "begin_init", Op.begin_init },
    
    Desc { "end_init", Op.end_init }, 
    Desc { "init_int", Op.init_int }, 
    Desc { "init_proc", Op.init_proc }, 
    Desc { "init_label", Op.init_label }, 
    Desc { "init_var", Op.init_var }, 
    Desc { "init_offset", Op.init_offset },
    
    Desc { "init_chars", Op.init_chars }, 
    Desc { "init_float", Op.init_float }, 
    Desc { "import_procedure", Op.import_procedure }, 
    Desc { "declare_procedure", Op.declare_procedure },
    
    Desc { "begin_procedure", Op.begin_procedure }, 
    Desc { "end_procedure", Op.end_procedure }, 
    Desc { "begin_block", Op.begin_block }, 
    Desc { "end_block", Op.end_block },
    
    Desc { "note_procedure_origin", Op.note_procedure_origin }, 
    Desc { "set_label", Op.set_label }, 
    Desc { "jump", Op.jump }, 
    Desc { "if_true", Op.if_true }, 
    Desc { "if_false", Op.if_false }, 
    Desc { "if_eq", Op.if_eq },
    
    Desc { "if_ne", Op.if_ne }, 
    Desc { "if_gt", Op.if_gt }, 
    Desc { "if_ge", Op.if_ge }, 
    Desc { "if_lt", Op.if_lt }, 
    Desc { "if_le", Op.if_le }, 
    Desc { "case_jump", Op.case_jump }, 
    Desc { "exit_proc", Op.exit_proc }, 
    Desc { "load", Op.load },
    
    Desc { "load_address", Op.load_address }, 
    Desc { "load_indirect", Op.load_indirect }, 
    Desc { "store", Op.store }, 
    Desc { "store_indirect", Op.store_indirect },
    
    Desc { "load_nil", Op.load_nil }, 
    Desc { "load_integer", Op.load_integer }, 
    Desc { "load_float", Op.load_float }, 
    Desc { "eq", Op.eq }, 
    Desc { "ne", Op.ne },
    
    Desc { "gt", Op.gt }, 
    Desc { "ge", Op.ge }, 
    Desc { "lt", Op.lt }, 
    Desc { "le", Op.le }, 
    Desc { "add", Op.add }, 
    Desc { "subtract", Op.subtract }, 
    Desc { "multiply", Op.multiply }, 
    Desc { "divide", Op.divide }, 
    Desc { "negate", Op.negate }, 
    Desc { "abs", Op.abs }, 
    Desc { "max", Op.max },
    
    Desc { "min", Op.min }, 
    Desc { "round", Op.round }, 
    Desc { "trunc", Op.trunc }, 
    Desc { "floor", Op.floor }, 
    Desc { "ceiling", Op.ceiling }, 
    Desc { "cvt_float", Op.cvt_float }, 
    Desc { "div", Op.div }, 
    Desc { "mod", Op.mod }, 
    Desc { "set_union", Op.set_union },
    
    Desc { "set_difference", Op.set_difference }, 
    Desc { "set_intersection", Op.set_intersection }, 
    Desc { "set_sym_difference", Op.set_sym_difference }, 
    Desc { "set_member", Op.set_member },
    
    Desc { "set_eq", Op.set_eq }, 
    Desc { "set_ne", Op.set_ne }, 
    Desc { "set_lt", Op.set_lt }, 
    Desc { "set_le", Op.set_le }, 
    Desc { "set_gt", Op.set_gt }, 
    Desc { "set_ge", Op.set_ge }, 
    Desc { "set_range", Op.set_range },
    
    Desc { "set_singleton", Op.set_singleton }, 
    Desc { "not", Op.not }, 
    Desc { "and", Op.and }, 
    Desc { "or", Op.or }, 
    Desc { "xor", Op.xor }, 
    Desc { "shift", Op.shift }, 
    Desc { "shift_left", Op.shift_left }, 
    Desc { "shift_right", Op.shift_right },
    
    Desc { "rotate", Op.rotate }, 
    Desc { "rotate_left", Op.rotate_left }, 
    Desc { "rotate_right", Op.rotate_right }, 
    Desc { "widen", Op.widen }, 
    Desc { "chop", Op.chop }, 
    Desc { "extract", Op.extract }, 
    Desc { "extract_n", Op.extract_n },
    
    Desc { "extract_mn", Op.extract_mn }, 
    Desc { "insert", Op.insert }, 
    Desc { "insert_n", Op.insert_n }, 
    Desc { "insert_mn", Op.insert_mn }, 
    Desc { "swap", Op.swap }, 
    Desc { "pop", Op.pop }, 
    Desc { "copy_n", Op.copy_n }, 
    Desc { "copy", Op.copy },
    
    Desc { "zero_n", Op.zero_n }, 
    Desc { "zero", Op.zero }, 
    Desc { "loophole", Op.loophole }, 
    Desc { "abort", Op.abort }, 
    Desc { "check_nil", Op.check_nil }, 
    Desc { "check_lo", Op.check_lo }, 
    Desc { "check_hi", Op.check_hi },
    
    Desc { "check_range", Op.check_range }, 
    Desc { "check_index", Op.check_index }, 
    Desc { "check_eq", Op.check_eq }, 
    Desc { "add_offset", Op.add_offset }, 
    Desc { "index_address", Op.index_address },
    
    Desc { "start_call_direct", Op.start_call_direct }, 
    Desc { "call_direct", Op.call_direct }, 
    Desc { "start_call_indirect", Op.start_call_indirect },
    
    Desc { "call_indirect", Op.call_indirect }, 
    Desc { "pop_param", Op.pop_param }, 
    Desc { "pop_struct", Op.pop_struct }, 
    Desc { "pop_static_link", Op.pop_static_link },
    
    Desc { "load_procedure", Op.load_procedure }, 
    Desc { "load_static_link", Op.load_static_link }, 
    Desc { "comment", Op.comment },

    Desc { "store_ordered", Op.store_ordered },
    Desc { "load_ordered", Op.load_ordered },
    Desc { "exchange", Op.exchange },
    Desc { "compare_exchange", Op.compare_exchange },
    Desc { "fence", Op.fence },
    Desc { "fetch_and_add", Op.fetch_and_add },
    Desc { "fetch_and_sub", Op.fetch_and_sub },
    Desc { "fetch_and_or", Op.fetch_and_or },
    Desc { "fetch_and_and", Op.fetch_and_and },
    Desc { "fetch_and_xor", Op.fetch_and_xor },
    Desc { "widechar_size", Op.widechar_size }
  };

PROCEDURE Out (a, b, c, d, e, f, g : TEXT := NIL) =
  <*FATAL ANY*>
  VAR wr := Stdio.stdout;
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
    IF (e # NIL) THEN Wr.PutText (wr, e); END;
    IF (f # NIL) THEN Wr.PutText (wr, f); END;
    IF (g # NIL) THEN Wr.PutText (wr, g); END;
    Wr.PutText (wr, Wr.EOL);
    Wr.Flush (wr);
  END Out;

PROCEDURE Pad (txt: TEXT): TEXT =
  CONST z = ARRAY [0..22] OF TEXT {
    "                      ", 
    "                     ", 
    "                    ", 
    "                   ", 
    "                  ", 
    "                 ", 
    "                ", 
    "               ", 
    "              ", 
    "             ", 
    "            ", 
    "           ", 
    "          ", 
    "         ", 
    "        ", 
    "       ", 
    "      ", 
    "     ", 
    "    ", 
    "   ", 
    "  ", 
    " ", 
    "" };
  BEGIN
    RETURN z [MAX (FIRST (z), MIN (Text.Length (txt), LAST(z)))];
  END Pad;

PROCEDURE Upper (txt: TEXT): TEXT =
  VAR
    len := Text.Length (txt);
    buf : ARRAY [0..99] OF CHAR;
  BEGIN
    <*ASSERT len <= NUMBER (buf) *>
    Text.SetChars (buf, txt);
    FOR i := 0 TO len-1 DO
      buf[i] := ASCII.Upper [buf[i]];
    END;
    RETURN Text.FromChars (SUBARRAY (buf, 0, len));
  END Upper;

BEGIN
  Out ("/* C version of M3CG_Binary. */");
  Out ("/* This file is generated by m3cggen. */");
  Out ("");

  Out ("#define M3CG_Version  0x", Fmt.Int (M3CG_Binary.Version, 16));
  Out ("");

  Out ("typedef enum {");
  FOR op := FIRST(Map) TO LAST(Map) DO
    <*ASSERT op = Map[op].op*>
    Out ("  M3CG_", Upper (Map[op].name), ", ",
         Pad (Map[op].name), "/* ", Fmt.Int (ORD (op)), " */");
  END;
  Out ("  LAST_OPCODE } M3CG_opcode;");
  Out ("");

  Out ("static const char *M3CG_opnames[] = {");
  FOR op := FIRST(Map) TO LAST(Map) DO
    <*ASSERT op = Map[op].op*>
    Out ("  \"", Map[op].name, "\", ",
         Pad(Map[op].name), "/* ", Fmt.Int (ORD (op)), " */");
  END;
  Out ("  0 };");
  Out ("");

  Out ("");
  Out ("#define M3CG_Int1        ", Fmt.Int (M3CG_Binary.Int1));
  Out ("#define M3CG_NInt1       ", Fmt.Int (M3CG_Binary.NInt1));
  Out ("#define M3CG_Int2        ", Fmt.Int (M3CG_Binary.Int2));
  Out ("#define M3CG_NInt2       ", Fmt.Int (M3CG_Binary.NInt2));
  Out ("#define M3CG_Int4        ", Fmt.Int (M3CG_Binary.Int4));
  Out ("#define M3CG_NInt4       ", Fmt.Int (M3CG_Binary.NInt4));
  Out ("#define M3CG_Int8        ", Fmt.Int (M3CG_Binary.Int8));
  Out ("#define M3CG_NInt8       ", Fmt.Int (M3CG_Binary.NInt8));
  Out ("#define M3CG_LastRegular ", Fmt.Int (M3CG_Binary.LastRegular));
  
END Main.
