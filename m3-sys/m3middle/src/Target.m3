(* Copyright (C) 1993, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(*                                                              *)
(* File: Target.m3                                              *)
(* Last Modified On Tue Jan 24 09:58:43 PST 1995 By kalsow      *)
(*      Modified On Fri Dec  9 12:56:47 PST 1994 By Olaf Wagner *)

MODULE Target;

IMPORT Text, TargetMap, M3RT;

VAR (*CONST*)
  CCs : REF ARRAY OF CallingConvention;

PROCEDURE Init (system: TEXT; in_OS_name: TEXT; backend_mode: M3BackendMode_t): BOOLEAN =
  CONST FF = 16_ff;
  VAR sys := 0;  max_align := 64;
  BEGIN
    (* lookup the system *)
    IF (system = NIL) THEN RETURN FALSE END;
    WHILE NOT Text.Equal (system, SystemNames[sys]) DO
      INC (sys);  IF (sys >= NUMBER (SystemNames)) THEN RETURN FALSE END;
    END;
    System := VAL(sys, Systems);
    System_name := SystemNames[sys];

    (* build a generic 32-bit/IEEE system description *)

    Int8.cg_type     := CGType.Int8;
    Int8.size        := 8;
    Int8.align       := 8;
    Int8.min         := Int{NUMBER(IBytes), IBytes{16_80,FF,..}};
    Int8.max         := Int{NUMBER(IBytes), IBytes{16_7f,00,..}};

    Int16.cg_type    := CGType.Int16;
    Int16.size       := 16;
    Int16.align      := 16;
    Int16.min        := Int{NUMBER(IBytes), IBytes{00,16_80,FF,..}};
    Int16.max        := Int{NUMBER(IBytes), IBytes{FF,16_7f,00,..}};

    Int32.cg_type    := CGType.Int32;
    Int32.size       := 32;
    Int32.align      := 32;
    Int32.min        := Int{NUMBER(IBytes), IBytes{00,00,00,16_80,FF,..}};
    Int32.max        := Int{NUMBER(IBytes), IBytes{FF,FF,FF,16_7f,00,..}};

    Int64.cg_type    := CGType.Int64;
    Int64.size       := 64;
    Int64.align      := 64;
    Int64.min        := Int{NUMBER(IBytes), IBytes{00,00,00,00,00,00,00,16_80}};
    Int64.max        := Int{NUMBER(IBytes), IBytes{FF,FF,FF,FF,FF,FF,FF,16_7f}};

    Word8.cg_type    := CGType.Word8;
    Word8.size       := 8;
    Word8.align      := 8;
    Word8.min        := Int{NUMBER(IBytes), IBytes{00,00,..}};
    Word8.max        := Int{NUMBER(IBytes), IBytes{FF,00,..}};

    Word16.cg_type   := CGType.Word16;
    Word16.size      := 16;
    Word16.align     := 16;
    Word16.min       := Int{NUMBER(IBytes), IBytes{00,00,00,..}};
    Word16.max       := Int{NUMBER(IBytes), IBytes{FF,FF,00,..}};

    Word32.cg_type   := CGType.Word32;
    Word32.size      := 32;
    Word32.align     := 32;
    Word32.min       := Int{NUMBER(IBytes), IBytes{00,00,00,00,00,..}};
    Word32.max       := Int{NUMBER(IBytes), IBytes{FF,FF,FF,FF,00,..}};

    Word64.cg_type   := CGType.Word64;
    Word64.size      := 64;
    Word64.align     := 64;
    Word64.min       := Int{NUMBER(IBytes), IBytes{00,00,00,00,00,00,00,00}};
    Word64.max       := Int{NUMBER(IBytes), IBytes{FF,FF,FF,FF,FF,FF,FF,FF}};

    Integer          := Int32;  (* default for the 32-bit platforms *)
    Longint          := Int64;
    Word             := Word32; (* default for the 32-bit platforms *)
    Long             := Word64;
    Address          := Word32;  Address.cg_type := CGType.Addr;
    Char             := Word8;

    Void.cg_type     := CGType.Void;
    Void.size        := 0;
    Void.align       := Byte;
    Void.min         := Int{0};
    Void.max         := Int{0};

    Real.cg_type     := CGType.Reel;
    Real.pre         := Precision.Short;
    Real.size        := 32;
    Real.align       := 32;
    Real.min         := Float { Precision.Short, 0, -3.40282346638528860x+38 };
    Real.max         := Float { Precision.Short, 0,  3.40282346638528860x+38 };

    Longreal.cg_type := CGType.LReel;
    Longreal.pre     := Precision.Long;
    Longreal.size    := 64;
    Longreal.align   := 64;
    Longreal.min     := Float { Precision.Long, 0,-1.79769313486231570x+308 };
    Longreal.max     := Float { Precision.Long, 0, 1.79769313486231570x+308 };

    Extended.cg_type := CGType.XReel;
    Extended.pre     := Precision.Extended;
    Extended.size    := 64;
    Extended.align   := 64;
    Extended.min     := Float{Precision.Extended, 0,-1.79769313486231570x+308};
    Extended.max     := Float{Precision.Extended, 0, 1.79769313486231570x+308};

    Alignments[0] := 8;
    Alignments[1] := 16;
    Alignments[2] := 32;
    Alignments[3] := 64;

    CCs := NIL;

    Allow_packed_byte_aligned := FALSE;
		
    (* add the system-specific customization *)

    OS_name := in_OS_name;
    EOL                       := "\n";
    Jumpbuf_align             := Address.align;
    All_floats_legal          := TRUE;
    Checks_integer_ops        := FALSE;
    Has_stack_walker          := FALSE;
    Aligned_procedures        := TRUE;
    Bitfield_can_overlap      := FALSE;
    Global_handler_stack      := TRUE;
    First_readable_addr       := 0;
    Guard_page_size           := 0;

    CASE System OF
    |  Systems.AIX386 =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 8;
                 Jumpbuf_size              := 25 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    |  Systems.ALPHA_OSF =>
                 Integer := Int64;
                 Word    := Word64;
                 Address := Word64;   Address.cg_type := CGType.Addr;

                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 16_400000;
                 Jumpbuf_size              := 84 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 Has_stack_walker          := TRUE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := TRUE;
                 Global_handler_stack      := FALSE; (* may use pthreads *)
                 Aligned_procedures        := FALSE;

    |  Systems.AP3000 =>
                 max_align                 := 16;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Bitfield_can_overlap      := TRUE;
                 Jumpbuf_size              := 83 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    |  Systems.ARM =>
                 max_align                 := 32;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 32;
                 Jumpbuf_size              := 16 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    |  Systems.DS3100 =>
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 16_400000;
                 Jumpbuf_size              := 84 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 Has_stack_walker          := TRUE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := TRUE;

    |  Systems.FreeBSD, Systems.FreeBSD2, Systems.FreeBSD3, Systems.FreeBSD4 =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 4096 * Char.size;
                 Jumpbuf_size              := 11 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";
                 Global_handler_stack      := FALSE; (* may use pthreads *)

    |  Systems.HP300 =>
                 max_align                 := 16;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Bitfield_can_overlap      := TRUE;
                 Jumpbuf_size              := 100 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    |  Systems.HPPA =>
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 16;
                 First_readable_addr       := 16_1000;
                 Jumpbuf_size              := 53 * Address.size;
                 Jumpbuf_align             := max_align;
                 Fixed_frame_size          := 8 * Address.size;
                 Setjmp                    := "_setjmp";
                 Aligned_procedures        := FALSE;

    |  Systems.IBMR2 =>
                 max_align                 := 32;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 32;
                 Jumpbuf_size              := 65 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    | Systems.IBMRT =>
                 max_align                 := 32;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 8;
                 Jumpbuf_size              := 17 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    |  Systems.IRIX5 =>
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 16_400000;
                 Jumpbuf_size              := 28 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 Setjmp                    := "setjmp";

    |  Systems.LINUX, Systems.LINUXELF =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 16_1000;
                 Jumpbuf_size              := 8 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "__setjmp";

    | Systems.NEXT =>
                 max_align                 := 16;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Jumpbuf_size              := 39 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    | Systems.NT386, Systems.NT386GNU =>

                 (* Cygwin is 13, Visual C++ is 16. Always use 16
                 so the code has a chance of interoperating.
                 Cygwin provides setjmp and _setjmp that resolve the same.
                 Visual C++ provides only _setjmp.
                 Visual C++ also has _setjmp3 that the compiler generates
                 a call to. In fact _setjmp appears to only use 8 ints
                 and _setjmp3 appears to use more. Consider switching to _setjmp3.
                 *)
                 Jumpbuf_size := (16 * Address.size);
                 Setjmp := "_setjmp";

                 IF Text.Equal(OS_name, "WIN32") THEN
                   EOL := "\r\n";
                 (*
                 ELSIF Text.Equal(OS_name, "POSIX") THEN
                   EOL := "\n";
                 ELSE
                   RETURN FALSE;
                 *)
                 END;

                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 (* initial experiments indicate that the first 64K of
                    a process's memory on NT are "free" and unreadable.
                    --- WKK  9/9/94 *)
                 First_readable_addr       := 4096;
                 Fixed_frame_size          := 0;
                 Guard_page_size           := 4096 * Char.size;
                 Global_handler_stack      := FALSE; (* uses NT or pthreads over NT threads *)

                 (* m3back doesn't handle 64 bit integers *)
                 IF BackendIntegrated[backend_mode] THEN
                   Longint := Int32;
                   Long    := Word32;
                 END;

                 (* 0 as third argument is __cdecl, while 1 is __stdcall *)

                 CCs := NEW (REF ARRAY OF CallingConvention, 9);
                 NTCall (0, "C",          0, backend_mode); (* __cdecl *)
                 NTCall (1, "WINAPI",     1, backend_mode); (* __stdcall *)
                 NTCall (2, "CALLBACK",   1, backend_mode); (* __stdcall *)
                 NTCall (3, "WINAPIV",    0, backend_mode); (* __cdecl *)
                 NTCall (4, "APIENTRY",   1, backend_mode); (* __stdcall *)
                 NTCall (5, "APIPRIVATE", 1, backend_mode); (* __stdcall *)
                 NTCall (6, "PASCAL",     1, backend_mode); (* __stdcall *)
                 NTCall (7, "__cdecl",    0, backend_mode); (* __cdecl *)
                 NTCall (8, "__stdcall",  1, backend_mode); (* __stdcall *)

    | Systems.OKI =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 32;
                 Jumpbuf_size              := 22 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    |  Systems.OS2 =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Jumpbuf_size              := 8 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "__setjmp";
                 EOL                       := "\n"; (* really? *)

    | Systems.SEQUENT =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Jumpbuf_size              := 84 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 Setjmp                    := "_setjmp";

    | Systems.SOLgnu, Systems.SOLsun =>
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 8192;
                 Jumpbuf_size              := 19 * Address.size;
                 Fixed_frame_size          := 20 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 Has_stack_walker          := TRUE;
                 Setjmp                    := "setjmp";
                 Global_handler_stack      := FALSE; (* may use pthreads *)

    | Systems.SPARC =>
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 8192;
                 Jumpbuf_size              := 10 * Address.size;
                 Fixed_frame_size          := 20 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 Setjmp                    := "_setjmp";

    | Systems.SUN3 =>
                 max_align                 := 16;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Jumpbuf_size              := 79 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 1024 * Char.size;
                 Setjmp                    := "_setjmp";

    | Systems.SUN386 =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 8;
                 Jumpbuf_size              := 8 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    | Systems.UMAX =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Jumpbuf_size              := 10 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    | Systems.VAX =>
                 Real.min.fraction     := -1.70111x+38;
                 Real.max.fraction     := -1.70111x+38;
                 Longreal.min.fraction := -1.70111x+38;
                 Longreal.max.fraction := -1.70111x+38;
                 Extended.min.fraction := -1.70111x+38;
                 Extended.max.fraction := -1.70111x+38;

                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Jumpbuf_size              := 10 * Address.size;
                 Fixed_frame_size          := 12 * Address.size;
                 Guard_page_size           := 1024 * Char.size;
                 All_floats_legal          := FALSE;
                 Setjmp                    := "_setjmp";

    |  Systems.LINUXLIBC6 =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Jumpbuf_size              := 40 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";
                 Global_handler_stack      := FALSE; (* may use pthreads *)

    |  Systems.AMD64_LINUX =>
                 Integer := Int64;
                 Word    := Word64;
                 Address := Word64;
                 Address.cg_type := CGType.Addr;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Jumpbuf_size              := 200 * Char.size;
                 Fixed_frame_size          := 8 * Char.size;
                 Setjmp                    := "_setjmp";
                 Global_handler_stack      := FALSE; (* may use pthreads *)

    |  Systems.I386_DARWIN =>
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 4096 * Char.size;
                 Jumpbuf_size              := 18 * Address.size;
                 Fixed_frame_size          := 8 * Address.size;
                 Setjmp                    := "setjmp";
                 Global_handler_stack      := FALSE; (* may use pthreads *)

    |  Systems.AMD64_DARWIN =>
                 Integer := Int64;
                 Word    := Word64;
                 Address := Word64;   Address.cg_type := CGType.Addr;

                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 4096 * Char.size;
                 Jumpbuf_size              := 19 * Address.size;
                 Fixed_frame_size          := 8 * Address.size;
                 Setjmp                    := "setjmp";
                 Global_handler_stack      := FALSE; (* may use pthreads *)

    |  Systems.PPC_DARWIN =>
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 4096 * Char.size;
                 Jumpbuf_size              := (26 + 36 + 129 + 1 + 1) * 
                                              Address.size;
                 Jumpbuf_align             := Word64.align;
                 Fixed_frame_size          := 8 * Address.size;
                 Setjmp                    := "setjmp";
                 Global_handler_stack      := FALSE; (* may use pthreads *)
                 (* Allow_packed_byte_aligned := TRUE; use <*LAZYALIGN*>*)

    | Systems.BSDI4 =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 4096;
                 Jumpbuf_size              := 10 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 Setjmp                    := "_setjmp";

    |  Systems.PPC_LINUX => 
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 4096 * Char.size;
                 Jumpbuf_size              := 58 * Address.size + 
                                              32 * Address.size + 4;
                 Jumpbuf_align             := Word64.align;
                 Fixed_frame_size          := 8 * Address.size;
                 Setjmp                    := "_setjmp";

    | Systems.NetBSD2_i386 =>
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 First_readable_addr       := 4096 * Char.size;
                 Jumpbuf_size              := 14 * Address.size;
                 Fixed_frame_size          := 4 * Address.size;
                 Setjmp                    := "_setjmp";

    ELSE RETURN FALSE;
    END;

    IF (CCs = NIL) THEN
      CCs := NEW (REF ARRAY OF CallingConvention, 1);
      VAR cc := NEW (CallingConvention);  BEGIN
        CCs[0] := cc;
        cc.name               := "C";
        cc.m3cg_id            := 0;
        cc.args_left_to_right := TRUE;
        cc.results_on_left    := FALSE;
        cc.standard_structs   := TRUE;
      END;
    END;
    DefaultCall := CCs[0];

    (* fill in the "bytes" and "pack" fields *)
    FixI (Address, max_align);
    FixI (Integer, max_align);
    FixI (Word, max_align);
    FixI (Longint, max_align);
    FixI (Long, max_align);
    FixF (Real, max_align);
    FixF (Longreal, max_align);
    FixF (Extended, max_align);
    FixI (Int8, max_align);
    FixI (Int16, max_align);
    FixI (Int32, max_align);
    FixI (Int64, max_align);
    FixI (Word8, max_align);
    FixI (Word16, max_align);
    FixI (Word32, max_align);
    FixI (Word64, max_align);
    FixI (Void, max_align);
    FixI (Char, max_align);

    (* sets are always treated as an array of integers *)    
    Set_grain := Integer.size;
    Set_align := Integer.align;

    (* fix the alignments *)
    FOR i := FIRST (Alignments) TO LAST (Alignments) DO
      Alignments[i] := MIN (Alignments[i], max_align);
    END;

    (* initialize the other target-specific modules *)
    TargetMap.Init ();
    M3RT.Init ();

    RETURN TRUE;
  END Init;

PROCEDURE NTCall (x: INTEGER;  nm: TEXT;  id: INTEGER; backend_mode: M3BackendMode_t) =
  BEGIN
 (* The external backend handles more calling convention
    details than the integrated backend -- reversing parameter
    order and knowing how to return structs. *)
    CCs[x] := NEW (CallingConvention,
                     name := nm,
                     m3cg_id := id,
                     args_left_to_right := NOT BackendIntegrated[backend_mode],
                     results_on_left := TRUE,
                     standard_structs := NOT BackendIntegrated[backend_mode]);
  END NTCall;

PROCEDURE FixI (VAR i: Int_type;  max_align: INTEGER) =
  BEGIN
    i.align := MIN (i.align, max_align);
    i.bytes := i.size DIV Byte;
    i.pack  := (i.size + i.align - 1) DIV i.align * i.align;
  END FixI;

PROCEDURE FixF (VAR f: Float_type;  max_align: INTEGER) =
  BEGIN
    f.align := MIN (f.align, max_align);
    f.bytes := f.size DIV Byte;
    (* f.pack  := (f.size + f.align - 1) DIV f.align * f.align; *)
  END FixF;

PROCEDURE FindConvention (nm: TEXT): CallingConvention =
  VAR cc: CallingConvention;
  BEGIN
    FOR i := 0 TO LAST (CCs^) DO
      cc := CCs[i];
      IF (cc # NIL) AND Text.Equal (nm, cc.name) THEN RETURN cc; END;
    END;
    RETURN NIL;
  END FindConvention;

PROCEDURE ConventionFromID (id: INTEGER): CallingConvention =
  VAR cc: CallingConvention;
  BEGIN
    FOR i := 0 TO LAST (CCs^) DO
      cc := CCs[i];
      IF (cc # NIL) AND (cc.m3cg_id = id) THEN RETURN cc; END;
    END;
    RETURN NIL;
  END ConventionFromID;

BEGIN
END Target.

