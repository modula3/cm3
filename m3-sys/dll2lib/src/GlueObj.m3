(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

MODULE GlueObj;

IMPORT CoffTime, Fmt, LibFile, Text, WinNT, Word;

TYPE
  State = REF RECORD
    dll_file : TEXT;

    (* object file contents *)
    len : INTEGER := 0;
    buf : ARRAY [0..4095] OF CHAR;

    (* string table *)
    n_strings     : INTEGER := 0;
    total_strings : INTEGER := 0;
    strings       : ARRAY [0..9] OF TEXT;
    string_offset : ARRAY [0..9] OF INTEGER;
  END;

  Section = RECORD
    offset  : INTEGER; (* file offset of raw data *)
    size    : INTEGER; (* size of raw data *)
    n_reloc : INTEGER; (* # of relocations *)
    reloc   : INTEGER; (* file offset of relocations *)
    next    : INTEGER; (* file offset of next section *)
  END;

VAR
  now := CoffTime.Now ();
  (***
  now := 16_2FC11CAD;  (* to match comdlg32.lib *)
  **)

CONST
  CheckArgs   = "_m3_arg_check";
  CheckProc   = "_RTHeapDep__CheckArgs";
  (**
  LinkStamp   = "Microsoft LINK 2.60.5112 (NT)";
  ***)
  LinkStamp   = "Critical Mass dll2lib 1.00";
  ThunkFlag   = "\177";  (* What's this?? *)
  ALIGN_1BYTE = 16_00100000;  (* 1 byte alignment for section *)
  ALIGN_2BYTE = 16_00200000;  (* 2 byte alignment for section *)
  ALIGN_4BYTE = 16_00300000;  (* 4 byte alignment for section *)
  RW_DATA     = WinNT.IMAGE_SCN_CNT_INITIALIZED_DATA
                + WinNT.IMAGE_SCN_MEM_READ
                + WinNT.IMAGE_SCN_MEM_WRITE;

PROCEDURE Gen (dll_name, entry: TEXT;  std_call: BOOLEAN;
               ordinal, n_parms, n_checks: INTEGER;
               READONLY checks: ARRAY OF INTEGER;
               direct_calls: BOOLEAN): LibFile.Obj =
  VAR
    s := NewState (dll_name);
    C_entry, imp_entry: TEXT;
    dll_import: TEXT;
    sec1, sec2, sec3, sec4, sec5, sec6: Section;
    text_length: INTEGER;
    text_relocs: INTEGER;
    n_syms: INTEGER;
  BEGIN
    IF std_call
      THEN C_entry := "_" & entry & "@" & Fmt.Int (n_parms);
      ELSE C_entry := "_" & entry;
    END;
    imp_entry := "__imp_" & C_entry;
    dll_import := "__IMPORT_DESCRIPTOR_" & dll_name;

    n_syms := 14;
    text_length := 0;
    text_relocs := 0;
    IF (n_checks > 0) THEN
      INC (text_relocs);
      INC (text_length, 16); (* push mask / load [arg_check] / call / pop mask *)
      INC (n_syms);
      (***********
      INC (text_length, 2);  (* MOV EBX,ESP *)
      FOR i := 0 TO n_checks-1 DO
        IF (checks[i] <= 120)
          THEN INC (text_length, 9);   (* MOV(disp8)/AND/JZ/MOV *)
          ELSE INC (text_length, 12);  (* MOV(disp32)/AND/JZ/MOV *)
        END;
      END;
      ***********)
    END;
    INC (text_length, 6);  (* JMP [moffset32] *)
    INC (text_relocs);

    (* size & layout the various sections *)
    sec1.next := WinNT.IMAGE_SIZEOF_FILE_HEADER
                   + 6 * WinNT.IMAGE_SIZEOF_SECTION_HEADER;
    InitSection (sec1, 22+Text.Length(s.dll_file)+Text.Length(LinkStamp), 0, sec1);
    InitSection (sec2, text_length, text_relocs, sec1);
    InitSection (sec3, 30 + Text.Length (entry), 2, sec2);
    InitSection (sec4, 4 (* pointer to ".idata$6" entry *), 1, sec3);
    InitSection (sec5, 4 (* pointer to ".idata$6" entry *), 1, sec4);
    InitSection (sec6, 2 + Text.Length (entry) + 1, 0, sec5);
    sec6.size := RoundUp (sec6.size);

    (* write the file header *)
    OutS (s, WinNT.IMAGE_FILE_MACHINE_I386);  (* == Intel 386 *)
    OutS (s, 6);   (* == # sections *)
    OutI (s, now); (* == file time *)
    OutI (s, sec6.next); (* file offset of symbol table *)
    OutI (s, n_syms);      (* # symbols *)
    OutS (s, 0);       (* size of optional header *)
    OutS (s, WinNT.IMAGE_FILE_32BIT_MACHINE);
                       (* flags => +reloc, -exec, +lineno, +locals, +32Bit *)

    (* section headers *)
    GenSection (s, sec1, ".debug$S", WinNT.IMAGE_SCN_CNT_INITIALIZED_DATA
           + ALIGN_1BYTE + WinNT.IMAGE_SCN_MEM_DISCARDABLE
           + WinNT.IMAGE_SCN_MEM_READ);
    GenSection (s, sec2, ".text",    WinNT.IMAGE_SCN_CNT_CODE
           + WinNT.IMAGE_SCN_LNK_COMDAT + ALIGN_2BYTE
           + WinNT.IMAGE_SCN_MEM_READ + WinNT.IMAGE_SCN_MEM_EXECUTE);
    GenSection (s, sec3, ".debug$S", WinNT.IMAGE_SCN_CNT_INITIALIZED_DATA
           + WinNT.IMAGE_SCN_LNK_COMDAT + ALIGN_1BYTE
           + WinNT.IMAGE_SCN_MEM_DISCARDABLE + WinNT.IMAGE_SCN_MEM_READ);
    GenSection (s, sec4, ".idata$5", RW_DATA
           + WinNT.IMAGE_SCN_LNK_COMDAT + ALIGN_4BYTE);
    GenSection (s, sec5, ".idata$4",  RW_DATA
           + WinNT.IMAGE_SCN_LNK_COMDAT + ALIGN_4BYTE);
    GenSection (s, sec6, ".idata$6",  RW_DATA
           + WinNT.IMAGE_SCN_LNK_COMDAT + ALIGN_2BYTE);

    (**** RAW DATA & RELOCATION DATA ******)

    (* .debug$S section -- raw data *)
    OutI (s, 1);  (* CV4 marker *)
    OutI (s, 16_00090013);
    OutI (s, 16_00000000);
    OutC (s, VAL (Text.Length (s.dll_file), CHAR));
    OutT (s, s.dll_file);
    OutI (s, 16_00010024);
    OutI (s, 16_08000703);
    OutC (s, VAL (Text.Length (LinkStamp), CHAR));
    OutT (s, LinkStamp);
    PadSection (s);

    (* .text section -- raw data *)
    IF (n_checks > 0) THEN
      OutC (s, VAL (16_68, CHAR));  (* PUSH imm32 *)
      OutI (s, CheckMask (n_checks, checks));
      IF (direct_calls) THEN
        OutS (s, 16_058D);  (* LEA EAX,$(arg_check) *)
        OutI (s, 0);
        OutS (s, 16_D0FF);  (* CALL [EAX] *)
      ELSE
        OutS (s, 16_058B);  (* MOVE EAX,[arg_check] *)
        OutI (s, 0);
        OutS (s, 16_D0FF);  (* CALL [EAX] *)
      END;
      OutS (s, 16_c483);  (* ADD ESP,imm8 *)
      OutC (s, '\004');
      (************
      OutS (s, 16_DC8B); (* mov EBX,ESP *)
      FOR i := 0 TO n_checks-1 DO
        IF (checks[i] <= 120) THEN
          OutS (s, 16_438B);  (*  mov EAX,disp8[EBX] *)
          OutC (s, VAL (checks[i]+4, CHAR));
          OutS (s, 16_C023);  (*  and EAX,EAX *)
          OutS (s, 16_0274);  (*  jz *+2 *)
          OutS (s, 16_008A);  (*  mov AL,[EAX] *)
        ELSE
          OutS (s, 16_838B);  (*  mov EAX,disp32[EBX] *)
          OutI (s, checks[i]+4);
          OutS (s, 16_C023);  (*  and EAX,EAX *)
          OutS (s, 16_0274);  (*  jz *+2 *)
          OutS (s, 16_008A);  (*  mov AL,[EAX] *)
        END;
      END;
      **************)
    END;
    OutS (s, 16_25FF);  (* jmp [indirect] *)
    OutI (s, 0);
    PadSection (s);

    (* .text section -- relocation *)
    IF (n_checks > 0) THEN
      OutI (s, 7); (* offset of relocation (== target of CALL) *)
      OutI (s, 14); (* symbol index => CheckArgs/CheckProc *)
      OutS (s, WinNT.IMAGE_REL_I386_DIR32);
    END;
    OutI (s, sec2.size-4);   (* offset of relocation *)
    OutI (s, 8);   (* symbol index => "__imp__foo" symbol *)
    OutS (s, WinNT.IMAGE_REL_I386_DIR32);

    (* .debug$S section -- raw data *)
    OutS (s, sec3.size - 6);
    OutS (s, 16_0206);
    OutI (s, 16_00000000);
    OutI (s, 16_00000000);
    OutI (s, 16_00000000);
    OutI (s, 16_00000000);
    OutI (s, 16_00060000);
    OutC (s, '\000');
    OutC (s, VAL (Text.Length (entry), CHAR));
    OutT (s, entry);
    OutI (s, 16_00060002);
    PadSection (s);

    (* .debug$S section -- relocations *)
    OutI (s, 16);  (* offset of relocation *)
    OutI (s, 3);   (* index of symbol ==> "_foo@xx" *)
    OutS (s, WinNT.IMAGE_REL_I386_SECREL);
    OutI (s, 20);  (* offset of relocation *)
    OutI (s, 3);   (* index of symbol ==> "_foo@xx" *)
    OutS (s, WinNT.IMAGE_REL_I386_SECTION);

    (* .idata$5 section -- raw data *)
    OutI (s, 0);  (* pointer to .idata$6 *)

    (* .idata$5 section -- relocation *)
    OutI (s, 0);   (* offset of relocation *)
    OutI (s, 11);   (* symbol index => ".idata$6" symbol *)
    OutS (s, WinNT.IMAGE_REL_I386_DIR32NB);

    (* .idata$4 section -- raw data *)
    OutI (s, 0);  (* pointer to .idata$6 *)

    (* .idata$4 section -- relocation *)
    OutI (s, 0);   (* offset of relocation *)
    OutI (s, 11);   (* symbol index => ".idata$6" symbol *)
    OutS (s, WinNT.IMAGE_REL_I386_DIR32NB);

    (* .idata$6 section -- raw data *)
    OutS (s, ordinal);
    OutT (s, entry);
    OutC (s, '\000');
    PadSection (s);

    (**** Symbol table *****)

    (* #0 *)
    OutN (s, ".debug$S");
    OutI (s, 0); (* symbol value *)
    OutS (s, 1); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_STATIC, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (* #1 *)
    OutN (s, ".text");
    OutI (s, 0); (* symbol value *)
    OutS (s, 2); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_STATIC, CHAR)); (* Storage Class *)
    OutC (s, '\001'); (* # of aux symbols *)

    (* #2 *)
    OutI (s, sec2.size);  (* section raw data length *)
    OutS (s, sec2.n_reloc);  (* # relocations *)
    OutS (s, 0); (* # line numbers *)
    OutI (s, 0); (* checksum *)
    OutS (s, 0); (* section # to associate with *)
    OutS (s, WinNT.IMAGE_COMDAT_SELECT_NODUPLICATES); (* communal selection type *)
    OutS (s, 16_17); (* ?? filler ?? *)

    (* #3 *)
    OutN (s, C_entry);  (* "_foo@nn" *)
    OutI (s, 0); (* symbol value *)
    OutS (s, 2); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_DTYPE_FUNCTION * 16); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_EXTERNAL, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (* #4 *)
    OutN (s, ".debug$S");
    OutI (s, 0); (* symbol value *)
    OutS (s, 3); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_STATIC, CHAR)); (* Storage Class *)
    OutC (s, '\001'); (* # of aux symbols *)

    (* #5 *)
    OutI (s, sec3.size);  (* section raw data length *)
    OutS (s, sec3.n_reloc);  (* # relocations *)
    OutS (s, 0); (* # line numbers *)
    OutI (s, 0); (* checksum *)
    OutS (s, 2); (* section # to associate with *)
    OutS (s, WinNT.IMAGE_COMDAT_SELECT_ASSOCIATIVE); (* communal selection type *)
    OutS (s, 16_17); (* ?? filler ?? *)

    (* #6 *)
    OutN (s, ".idata$5");
    OutI (s, 0); (* symbol value *)
    OutS (s, 4); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_STATIC, CHAR)); (* Storage Class *)
    OutC (s, '\001'); (* # of aux symbols *)

    (* #7 *)
    OutI (s, sec4.size);  (* section raw data length *)
    OutS (s, sec4.n_reloc);  (* # relocations *)
    OutS (s, 0); (* # line numbers *)
    OutI (s, 0); (* checksum *)
    OutS (s, 0); (* section # to associate with *)
    OutS (s, WinNT.IMAGE_COMDAT_SELECT_NODUPLICATES); (* communal selection type *)
    OutS (s, 16_17); (* ?? filler ?? *)

    (* #8 *)
    OutN (s, imp_entry);
    OutI (s, 0); (* symbol value *)
    OutS (s, 4); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_EXTERNAL, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (* #9 *)
    OutN (s, ".idata$4");
    OutI (s, 0); (* symbol value *)
    OutS (s, 5); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_STATIC, CHAR)); (* Storage Class *)
    OutC (s, '\001'); (* # of aux symbols *)

    (* #10 *)
    OutI (s, sec5.size);  (* section raw data length *)
    OutS (s, sec5.n_reloc);  (* # relocations *)
    OutS (s, 0); (* # line numbers *)
    OutI (s, 0); (* checksum *)
    OutS (s, 4); (* section # to associate with *)
    OutS (s, WinNT.IMAGE_COMDAT_SELECT_ASSOCIATIVE); (* communal selection type *)
    OutS (s, 16_17); (* ?? filler ?? *)

    (* #11 *)
    OutN (s, ".idata$6");
    OutI (s, 0); (* symbol value *)
    OutS (s, 6); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_STATIC, CHAR)); (* Storage Class *)
    OutC (s, '\001'); (* # of aux symbols *)

    (* #12 *)
    OutI (s, sec6.size);  (* section raw data length *)
    OutS (s, sec6.n_reloc);  (* # relocations *)
    OutS (s, 0); (* # line numbers *)
    OutI (s, 0); (* checksum *)
    OutS (s, 4); (* section # to associate with *)
    OutS (s, WinNT.IMAGE_COMDAT_SELECT_ASSOCIATIVE); (* communal selection type *)
    OutS (s, 16_17); (* ?? filler ?? *)

    (* #13 *)
    OutN (s, dll_import);
    OutI (s, 0); (* symbol value *)
    OutS (s, 0); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_EXTERNAL, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    IF (n_checks > 0) THEN
      (* #14 *)
      IF (direct_calls)
        THEN OutN (s, CheckProc);
        ELSE OutN (s, CheckArgs);
      END;
      OutI (s, 0); (* symbol value *)
      OutS (s, 0); (* Section number *)
      OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
      OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_EXTERNAL, CHAR)); (* Storage Class *)
      OutC (s, '\000'); (* # of aux symbols *)
    END;

    (**** String table *****)

    GenStrings (s);

    RETURN MakeObject (s, C_entry, imp_entry);
  END Gen;

PROCEDURE CheckMask (n_checks : INTEGER;
            READONLY checks   : ARRAY OF INTEGER): INTEGER =
  VAR mask := 0;  offset: INTEGER;
  BEGIN
    FOR i := 0 TO n_checks - 1 DO
      offset := checks[i];
      <*ASSERT Word.And (offset, 3) = 0*>
      offset := Word.RightShift (offset, 2);
      <*ASSERT offset < BITSIZE (mask)*>
      mask := Word.Or (mask, Word.LeftShift (1, offset));
    END;
    RETURN mask;
  END CheckMask;

PROCEDURE GenHeader1 (dll_name: TEXT): LibFile.Obj =
  VAR
    s := NewState (dll_name);
    dll_import, thunk_sym: TEXT;
    sec1, sec2: Section;
  BEGIN
    dll_import := "__IMPORT_DESCRIPTOR_" & dll_name;
    thunk_sym  := ThunkFlag & dll_name & "_NULL_THUNK_DATA";

    (* size & layout the various sections *)
    sec1.next := WinNT.IMAGE_SIZEOF_FILE_HEADER
                   + WinNT.IMAGE_SIZEOF_NT_OPTIONAL_HEADER
                   + 2 * WinNT.IMAGE_SIZEOF_SECTION_HEADER;
    InitSection (sec1, 20, 3, sec1);
    InitSection (sec2, RoundUp (1 + Text.Length (s.dll_file)), 0, sec1);

    (* write the file header *)
    OutS (s, WinNT.IMAGE_FILE_MACHINE_I386);  (* == Intel 386 *)
    OutS (s, 2);   (* == # sections *)
    OutI (s, now); (* == file time *)
    OutI (s, sec2.next); (* file offset of symbol table *)
    OutI (s, 7);      (* # symbols *)
    OutS (s, WinNT.IMAGE_SIZEOF_NT_OPTIONAL_HEADER); (* size of optional header *)
    OutS (s, WinNT.IMAGE_FILE_32BIT_MACHINE);
                       (* flags => +reloc, -exec, +lineno, +locals, +32Bit *)

    (* write the NT optional file header *)
    OutS (s, 16_10b);    (* magic *)
    OutC (s, '\002');    (* Major linker version # *)
    OutC (s, '\074');    (* Minor linker version # *)
    OutI (s, 0);         (* size of code *)
    OutI (s, 0);         (* size of initialized data *)
    OutI (s, 0);         (* size of uninitialized data *)
    OutI (s, 0);         (* address of entry point *)
    OutI (s, 0);         (* base of code *)
    OutI (s, 0);         (* base of data *)

    (* NT extensions *)

    OutI (s, 0);         (* image base *)
    OutI (s, 16_1000);   (* section alignment *)
    OutI (s, 16_200);    (* file alignment *)
    OutS (s, 4);         (* Major OS version *)
    OutS (s, 0);         (* Minor OS version *)
    OutS (s, 0);         (* Major image version *)
    OutS (s, 0);         (* Minor image version *)
    OutS (s, 0);         (* Major subsystem version *)
    OutS (s, 0);         (* Minor subsystem version *)
    OutI (s, 0);         (* RESERVED *)
    OutI (s, 0);         (* size of image *)
    OutI (s, 0);         (* size of headers *)
    OutI (s, 0);         (* check sum *)
    OutS (s, 0);         (* subsystem *)
    OutS (s, 0);         (* DLL characteristics *)
    OutI (s, 16_100000); (* size of stack reserve *)
    OutI (s, 16_1000);   (* size of stack commit *)
    OutI (s, 16_100000); (* size of heap reserve *)
    OutI (s, 16_1000);   (* size of heap commit *)
    OutI (s, 0);         (* address of TLS index *)

    OutI (s, 16);        (* number of RVA & Size entries in directory *)
    OutI (s, 0);         (* address of Export Directory *)
    OutI (s, 0);         (* size of Export Directory *)
    OutI (s, 0);         (* address of Import Directory *)
    OutI (s, 0);         (* size of Import Directory *)
    OutI (s, 0);         (* address of Resource Directory *)
    OutI (s, 0);         (* size of Resource Directory *)
    OutI (s, 0);         (* address of Exception Directory *)
    OutI (s, 0);         (* size of Exception Directory *)
    OutI (s, 0);         (* address of Security Directory *)
    OutI (s, 0);         (* size of Security Directory *)
    OutI (s, 0);         (* address of Base Relocation Directory *)
    OutI (s, 0);         (* size of Base Relocation Directory *)
    OutI (s, 0);         (* address of Debug Directory *)
    OutI (s, 0);         (* size of Debug Directory *)
    OutI (s, 0);         (* address of Description Directory *)
    OutI (s, 0);         (* size of Description Directory *)
    OutI (s, 0);         (* address of Special Directory *)
    OutI (s, 0);         (* size of Special Directory *)
    OutI (s, 0);         (* address of Thread Storage Directory *)
    OutI (s, 0);         (* size of Thread Storage Directory *)
    OutI (s, 0);         (* address of Load Configuration Directory *)
    OutI (s, 0);         (* size of Load Configuration Directory *)
    OutI (s, 0);         (* address of Bound Import Directory *)
    OutI (s, 0);         (* size of Bound Import Directory *)
    OutI (s, 0);         (* address of Import Address Table Directory *)
    OutI (s, 0);         (* size of Import Address Table Directory *)
    OutI (s, 0);         (* address of Reserved Directory *)
    OutI (s, 0);         (* size of Reserved Directory *)
    OutI (s, 0);         (* address of Reserved Directory *)
    OutI (s, 0);         (* size of Reserved Directory *)
    OutI (s, 0);         (* address of Reserved Directory *)
    OutI (s, 0);         (* size of Reserved Directory *)

    (* section headers *)
    GenSection (s, sec1, ".idata$2", RW_DATA + ALIGN_1BYTE);
    GenSection (s, sec2, ".idata$6", RW_DATA + ALIGN_2BYTE);

    (**** RAW DATA & RELOCATION DATA ******)

    (* .idata$2 section -- raw data *)
    OutI (s, 0);
    OutI (s, 0);
    OutI (s, 0);
    OutI (s, 0);
    OutI (s, 0);

    (* .idata$2 section -- relocations *)
    OutI (s, 12);   (* offset of relocation *)
    OutI (s, 2);    (* symbol index => ".idata$6" symbol *)
    OutS (s, WinNT.IMAGE_REL_I386_DIR32NB);
    OutI (s, 0);    (* offset of relocation *)
    OutI (s, 3);    (* symbol index => ".idata$4" symbol *)
    OutS (s, WinNT.IMAGE_REL_I386_DIR32NB);
    OutI (s, 16);   (* offset of relocation *)
    OutI (s, 4);    (* symbol index => ".idata$5" symbol *)
    OutS (s, WinNT.IMAGE_REL_I386_DIR32NB);

    (* .idata$6 section -- raw data *)
    OutT (s, s.dll_file);
    OutC (s, '\000');
    PadSection (s);
    
    (**** Symbol table *****)

    (* #0 *)
    OutN (s, dll_import);
    OutI (s, 0); (* symbol value *)
    OutS (s, 1); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_EXTERNAL, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (* #1 *)
    OutN (s, ".idata$2");
    OutI (s, RW_DATA);  (* symbol value *)
    OutS (s, 1); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_SECTION, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (* #2 *)
    OutN (s, ".idata$6");
    OutI (s, 0); (* symbol value *)
    OutS (s, 2); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_STATIC, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (* #3 *)
    OutN (s, ".idata$4");
    OutI (s, RW_DATA);  (* symbol value *)
    OutS (s, 0); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_SECTION, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (* #4 *)
    OutN (s, ".idata$5");
    OutI (s, RW_DATA);  (* symbol value *)
    OutS (s, 0); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_SECTION, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (* #5 *)
    OutN (s, "__NULL_IMPORT_DESCRIPTOR");
    OutI (s, 0); (* symbol value *)
    OutS (s, 0); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_EXTERNAL, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (* #6 *)
    OutN (s, thunk_sym);
    OutI (s, 0); (* symbol value *)
    OutS (s, 0); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_EXTERNAL, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (**** String table *****)

    GenStrings (s);

    RETURN MakeObject (s, dll_import);
  END GenHeader1;

PROCEDURE GenHeader2 (dll_name: TEXT): LibFile.Obj =
  VAR
    s := NewState (dll_name);
    sec1: Section;
  BEGIN
    (* size & layout the various sections *)
    sec1.next := WinNT.IMAGE_SIZEOF_FILE_HEADER
                   + 1 * WinNT.IMAGE_SIZEOF_SECTION_HEADER;
    InitSection (sec1, 20, 0, sec1);

    (* write the file header *)
    OutS (s, WinNT.IMAGE_FILE_MACHINE_I386);  (* == Intel 386 *)
    OutS (s, 1);   (* == # sections *)
    OutI (s, now); (* == file time *)
    OutI (s, sec1.next); (* file offset of symbol table *)
    OutI (s, 1);       (* # symbols *)
    OutS (s, 0);       (* size of optional header *)
    OutS (s, WinNT.IMAGE_FILE_32BIT_MACHINE);
                       (* flags => +reloc, -exec, +lineno, +locals, +32Bit *)

    (* section headers *)
    GenSection (s, sec1, ".idata$3", RW_DATA + ALIGN_1BYTE);

    (**** RAW DATA & RELOCATION DATA ******)

    (* .idata$3 section -- raw data *)
    OutI (s, 0);
    OutI (s, 0);
    OutI (s, 0);
    OutI (s, 0);
    OutI (s, 0);

    (**** Symbol table *****)

    (* #0 *)
    OutN (s, "__NULL_IMPORT_DESCRIPTOR");
    OutI (s, 0); (* symbol value *)
    OutS (s, 1); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_EXTERNAL, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (**** String table *****)

    GenStrings (s);

    RETURN MakeObject (s, "__NULL_IMPORT_DESCRIPTOR");
  END GenHeader2;

PROCEDURE GenHeader3 (dll_name: TEXT): LibFile.Obj =
  VAR
    s := NewState (dll_name);
    thunk_sym: TEXT;
    sec1, sec2: Section;
  BEGIN
    thunk_sym := ThunkFlag & dll_name & "_NULL_THUNK_DATA";

    (* size & layout the various sections *)
    sec1.next := WinNT.IMAGE_SIZEOF_FILE_HEADER
                   + 2 * WinNT.IMAGE_SIZEOF_SECTION_HEADER;
    InitSection (sec1, 4, 0, sec1);
    InitSection (sec2, 4, 0, sec1);

    (* write the file header *)
    OutS (s, WinNT.IMAGE_FILE_MACHINE_I386);  (* == Intel 386 *)
    OutS (s, 2);   (* == # sections *)
    OutI (s, now); (* == file time *)
    OutI (s, sec2.next); (* file offset of symbol table *)
    OutI (s, 1);      (* # symbols *)
    OutS (s, 0);       (* size of optional header *)
    OutS (s, WinNT.IMAGE_FILE_32BIT_MACHINE);
                       (* flags => +reloc, -exec, +lineno, +locals, +32Bit *)

    (* section headers *)
    GenSection (s, sec1, ".idata$5", RW_DATA + ALIGN_4BYTE);
    GenSection (s, sec2, ".idata$4", RW_DATA + ALIGN_4BYTE);

    (**** RAW DATA & RELOCATION DATA ******)

    (* .idata$5 section -- raw data *)
    OutI (s, 0);

    (* .idata$4 section -- raw data *)
    OutI (s, 0);

    (**** Symbol table *****)

    (* #0 *)
    OutN (s, thunk_sym);
    OutI (s, 0); (* symbol value *)
    OutS (s, 1); (* Section number *)
    OutS (s, WinNT.IMAGE_SYM_TYPE_NULL); (* Type *)
    OutC (s, VAL (WinNT.IMAGE_SYM_CLASS_EXTERNAL, CHAR)); (* Storage Class *)
    OutC (s, '\000'); (* # of aux symbols *)

    (**** String table *****)

    GenStrings (s);

    RETURN MakeObject (s, thunk_sym);
  END GenHeader3;

(*------------------------------------------------------ generic State ---*)

PROCEDURE NewState (dll_name: TEXT): State =
  VAR s := NEW (State);
  BEGIN
    s.dll_file := dll_name & ".dll";
    s.total_strings := BYTESIZE (INTEGER);  (* include the byte-count header *)
    RETURN s;
  END NewState;

PROCEDURE MakeObject (s: State;  sym1, sym2: TEXT := NIL): LibFile.Obj =
  VAR obj := NEW (LibFile.Obj);
  BEGIN
    obj.next     := NIL;
    obj.filename := s.dll_file;
    obj.time     := now;
    obj.body     := Text.FromChars (SUBARRAY (s.buf, 0, s.len));

    IF (sym1 # NIL) AND (sym2 # NIL) THEN
      obj.symbols := NEW (REF ARRAY OF TEXT, 2);
      obj.symbols[0] := sym1;
      obj.symbols[1] := sym2;
    ELSIF (sym1 # NIL) THEN
      obj.symbols := NEW (REF ARRAY OF TEXT, 1);
      obj.symbols[0] := sym1;
    ELSE
      obj.symbols := NIL;
    END;

    RETURN obj;
  END MakeObject;

(*---------------------------------------------------- generic COFF ---*)

PROCEDURE InitSection (VAR sec: Section;  size, n_reloc: INTEGER;
                       READONLY prev: Section) =
  BEGIN
    sec.offset  := prev.next;
    sec.size    := size;
    sec.n_reloc := n_reloc;
    sec.reloc   := RoundUp (sec.offset + size);
    sec.next    := RoundUp (sec.reloc + n_reloc * WinNT.IMAGE_SIZEOF_RELOCATION);
    IF (n_reloc = 0) THEN sec.reloc := 0; END;
  END InitSection;

PROCEDURE GenSection (s: State;  READONLY sec: Section;  nm: TEXT;
                      flags: INTEGER) =
  BEGIN
    OutN (s, nm); (* section name *)
    OutI (s, 0);  (* physical address *)
    OutI (s, 0);  (* virtual address *)
    OutI (s, sec.size); (* size of raw data *)
    OutI (s, sec.offset); (* offset of raw data *)
    OutI (s, sec.reloc); (* offset of relocation *)
    OutI (s, 0); (* offset of line numbers *)
    OutS (s, sec.n_reloc); (* # relocations *)
    OutS (s, 0); (* # line numbers *)
    OutI (s, flags);
  END GenSection;

PROCEDURE PadSection (s: State) =
  (* pad the output file to an even length *)
  BEGIN
    WHILE Word.And (s.len, 1) # 0 DO OutC (s, '\000'); END;
  END PadSection;

PROCEDURE RoundUp (n: INTEGER): INTEGER =
  (* round up to a multiple of 2 *)
  BEGIN
    RETURN Word.And (n + 1, Word.Not (1));
  END RoundUp;

PROCEDURE GenStrings (s: State) =
  BEGIN
    OutI (s, s.total_strings);
    FOR i := 0 TO s.n_strings-1 DO
      OutT (s, s.strings [i]);
      OutC (s, '\000');
    END;
    (* PadSection (s); *)
  END GenStrings;

(*--------------------------------------------- low-level output routines ---*)

PROCEDURE OutN (s: State;  nm: TEXT) =
  (* writes an 8-byte name *)
  VAR len := Text.Length (nm);
  BEGIN
    IF (len <= 8) THEN
      FOR i := 0 TO len-1 DO OutC (s, Text.GetChar (nm, i)); END;
      FOR i := len TO 7   DO OutC (s, '\000'); END;
    ELSE
      OutI (s, 0);
      OutI (s, AddString (s, nm));
    END;
  END OutN;

PROCEDURE OutI (s: State;  i: INTEGER) =
  BEGIN
    OutC (s, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (s, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (s, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (s, VAL (Word.And (i, 16_ff), CHAR));
  END OutI;

PROCEDURE OutS (s: State;  i: INTEGER) =
  BEGIN
    OutC (s, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (s, VAL (Word.And (i, 16_ff), CHAR));
  END OutS;

PROCEDURE OutT (s: State;  txt: TEXT) =
  VAR len := Text.Length (txt);
  BEGIN
    FOR i := 0 TO len-1 DO OutC (s, Text.GetChar (txt, i)); END;
  END OutT;

PROCEDURE OutC (s: State;  c: CHAR) =
  BEGIN
    s.buf [s.len] := c;
    INC (s.len);
  END OutC;

PROCEDURE AddString (s: State;  txt: TEXT): INTEGER =
  VAR i := 0;
  BEGIN
    s.strings [s.n_strings] := txt;
    s.string_offset [s.n_strings] := s.total_strings;
    WHILE NOT Text.Equal (s.strings[i], txt) DO INC (i); END;
    IF (i = s.n_strings) THEN
      INC (s.total_strings, 1 + Text.Length (txt));
      INC (s.n_strings);
    END;
    RETURN s.string_offset[i];
  END AddString;

BEGIN
END GlueObj.
