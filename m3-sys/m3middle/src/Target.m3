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

PROCEDURE Init64 () =
  BEGIN
    Integer := Int64;
    Word := Word64;
    Address := Word64;
    Address.cg_type := CGType.Addr;
    Jumpbuf_align := Address.align;
  END Init64;

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
    OS_name := in_OS_name;

    (* common values *)

    Allow_packed_byte_aligned := FALSE;
    EOL                       := "\n";
    Jumpbuf_align             := Address.align;
    All_floats_legal          := TRUE;
    PCC_bitfield_type_matters := TRUE;
    Structure_size_boundary   := 8;
    Little_endian             := TRUE;
    Setjmp                    := "_setjmp";

    (* There is no portable stack walker, and therefore few systems have one.
    Having a stack walker theoretically speeds up everything nicely.
    If you are familiar with NT exception handling, all but x86 have a stack walker.
    Not having a stack walker means that functions that have try/except/finally/raise
    incur a codegen cost even if there is never a raise -- for having the try.
    Having a stack walker means "zero cost" for exception handling code that
    does not actually raise an exception.

    If there is NOT a stack walker, prologues/epilogues for functions that try/except/finally/raise
    call PushEFrame / PopEFrame, essentially to build a walkable parallel stack.

    If there is a stack walker, then raise can discover what PushEFrame / PopEFrame effectively recorded.

    NT/x86 has a highly optimized equivalent of PushEFrame / PopEFrame, not currently used by Modula-3.
    *)
    Has_stack_walker          := FALSE;

    (* "Closures" in Modula-3 -- function pointers to nested functions,
    are represented as a pointer to -1 (of size?) followed by other data.
    -1 is assumed to be invalid code. Prior to calling any function pointer,
    the generated code first checks for the marker -1, to decide how to call it.
    On systems where data alignment matters, but functions are not aligned,
    this can result in an alignment fault. Most systems either don't care
    about alignment (x86, AMD64) or have fixed sized and aligned instructions (PowerPC),
    in which case the check for -1 can just be a direct read, in which case Aligned_procedures := TRUE.
    This logic can break down on 64bit platforms, where the -1 is perhaps 64bits, but
    the fixed size instructions may be 32bits. Or of course, on systems that care
    about data alignment but not code alignment, or in which the alignments differ.

    Setting this to FALSE is safe, but results in slower code -- code that
    checks function pointers for alignment before checking for the -1, and if they
    aren't aligned, doing a byte-wise read instead of an integer-wise read.

    We can probably do better here, such as ensuring the marker is 4 bytes instead of 8,
    if that works (for 64 bit platforms, that care about alignment, but with fixed
    sized aligned 32 bit instructions, which probably describes some e.g. MIPS64 and SPARC64)
    *)
    Aligned_procedures        := TRUE;

    (* The affect of First_readable_addr is that (static?) array indices (offsets)
    lower than it (and positive?) do not have a NULL check on the array base.
    Reading NULL + an offset less than First_readable_addr is assumed to access
    violate the same as reading NULL. It is a nice optimization.
    Setting the value too low results in correct but suboptimal code.
    However just setting it to a small non-zero number should provide most of the benefit.
    Setting the value too high results in missing NULL checks -- a loss of safety enforcement.
    Typically setting it to one hardware page is a good estimate, since if NULL is not accessible,
    nor is any address on the same page. As well, setting it to getpagesize, whatever
    the granularity of mmap/VirtualAlloc, often larger than a hardware page, is another good guess.
    *)
    First_readable_addr       := 4096 * Char.size;

    (* add the system-specific customization *)

    (* 64bit *)

    CASE System OF
    | Systems.ALPHA_OSF,
(*    Systems.ALPHA_LINUX,
      Systems.ALPHA_FREEBSD,
      Systems.ALPHA_OPENBSD,
      Systems.ALPHA_NETBSD,
      Systems.ALPHA_VMS,

      Systems.AMD64_CYGWIN, *)
      Systems.AMD64_DARWIN,
      Systems.AMD64_FREEBSD,
(*    Systems.AMD64_NETBSD,
      Systems.AMD64_OPENBSD, *)
      Systems.AMD64_LINUX,
(*    Systems.AMD64_NT,
      Systems.AMD64_SOLARIS,

      Systems.PA64_LINUX,
*)
      Systems.PA64_HPUX,
(*
      Systems.IA64_FREEBSD,
      Systems.IA64_HPUX,
      Systems.IA64_LINUX,
      Systems.IA64_NT,
      Systems.IA64_VMS,

      Systems.MIPS64_LINUX,
      Systems.MIPS64_IRIX, *)
      Systems.MIPS64_OPENBSD,
(*    Systems.MIPS64_NETBSD,

      Systems.PPC64_AIX,
      Systems.PPC64_LINUX,
      Systems.PPC64_DARWIN,

      Systems.SPARC64_FREEBSD,
      Systems.SPARC64_NETBSD, *)
      Systems.SPARC64_OPENBSD,
      Systems.SPARC64_LINUX,
      Systems.SPARC64_SOLARIS
    =>
        Init64();
    ELSE
    END;

    (* big endian *)

    CASE System OF
    | 
      Systems.PA32_HPUX,
      Systems.PA64_HPUX,
(*    Systems.PA32_LINUX,
      Systems.PA64_LINUX,

      Systems.MIPS32_LINUX,
      Systems.MIPS32_IRIX,
      Systems.MIPS32_OPENBSD,
      Systems.MIPS32_NETBSD,
      Systems.MIPS64_LINUX,
      Systems.MIPS64_IRIX, *)
      Systems.MIPS64_OPENBSD,
(*    Systems.MIPS64_NETBSD, *)

      Systems.PPC_LINUX,
      Systems.PPC_DARWIN,
(*    Systems.PPC32_AIX,
      Systems.PPC32_FREEBSD,
      Systems.PPC32_NETBSD,
      Systems.PPC32_OPENBSD,
      Systems.PPC64_AIX,
      Systems.PPC64_LINUX,
      Systems.PPC64_DARWIN, *)

      Systems.SPARC32_LINUX,
(*    Systems.SPARC32_SOLARIS,
      Systems.SPARC64_FREEBSD,
      Systems.SPARC64_NETBSD, *)
      Systems.SPARC64_OPENBSD,
      Systems.SPARC64_LINUX,
      Systems.SPARC64_SOLARIS,

      Systems.SOLgnu,
      Systems.SOLsun,

      Systems.AP3000,
      Systems.ARM,
      Systems.HP300,
      Systems.HPPA,
      Systems.IBMR2,
      Systems.IBMRT,
      Systems.IRIX5,
      Systems.NEXT,
      Systems.SPARC,
      Systems.SUN3
    =>
        Little_endian := FALSE;
    ELSE
    END;

    (* Solaris (should we put all "setjmp" platforms here?) *)

    CASE System OF
    (* SPARC32_SOLARIS *)
    | Systems.SOLgnu,
      Systems.SOLsun,
(*    Systems.I386_SOLARIS,
      Systems.AMD64_SOLARIS, *)
      Systems.SPARC64_SOLARIS
    =>
        Setjmp := "setjmp";
    ELSE
    END;

    (* SPARC *)

    CASE System OF
    | Systems.SOLgnu,
      Systems.SOLsun,
      Systems.SPARC,
      Systems.SPARC32_LINUX,
      Systems.SPARC64_LINUX,
      Systems.SPARC64_OPENBSD,
      Systems.SPARC64_SOLARIS
    =>
        First_readable_addr := 8192 * Char.size;
    ELSE
    END;

    CASE System OF
    |  Systems.AIX386 =>
                 max_align                 := 32;
                 PCC_bitfield_type_matters := FALSE;
                 Jumpbuf_size              := 25 * Address.size;

    |  Systems.ALPHA_OSF =>
                 First_readable_addr       := 16_400000 * Char.size;
                 Jumpbuf_size              := 84 * Address.size;
                 Has_stack_walker          := TRUE;
                 Aligned_procedures        := FALSE;

    |  Systems.AP3000 =>
                 max_align                 := 16;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Jumpbuf_size              := 83 * Address.size;

    |  Systems.ARM =>
                 max_align                 := 32;
                 Structure_size_boundary   := 32;
                 Jumpbuf_size              := 16 * Address.size;

    |  Systems.DS3100 =>
                 First_readable_addr       := 16_400000 * Char.size;
                 Jumpbuf_size              := 84 * Address.size;
                 Has_stack_walker          := TRUE;

    |  Systems.FreeBSD, Systems.FreeBSD2, Systems.FreeBSD3, Systems.FreeBSD4 =>
                 max_align                 := 32;
                 First_readable_addr       := 4096;
                 Jumpbuf_size              := 11 * Address.size;

    |  Systems.AMD64_FREEBSD =>
                 Jumpbuf_size              := 16_80 * Char.size;

    |  Systems.HP300 =>
                 max_align                 := 16;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Jumpbuf_size              := 100 * Address.size;

    |  Systems.HPPA =>
                 Structure_size_boundary   := 16;
                 First_readable_addr       := 16_1000;
                 Jumpbuf_size              := 53 * Address.size;
                 Jumpbuf_align             := max_align;
                 Aligned_procedures        := FALSE;

    |  Systems.PA32_HPUX =>
                 Structure_size_boundary   := 16;
                 First_readable_addr       := 16_1000;
                 (* 200 bytes with 8 byte alignment *)
                 Jumpbuf_size              := 50 * Address.size;
                 Jumpbuf_align             := 8;
                 Aligned_procedures        := FALSE;

    |  Systems.PA64_HPUX =>
                 Structure_size_boundary   := 16;
                 First_readable_addr       := 16_1000;
                 (* 640 bytes with 16 byte alignment *)
                 Jumpbuf_size              := 80 * Address.size;
                 Jumpbuf_align             := 128;
                 Aligned_procedures        := FALSE;

    |  Systems.IBMR2 =>
                 max_align                 := 32;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 32;
                 Jumpbuf_size              := 65 * Address.size;

    | Systems.IBMRT =>
                 max_align                 := 32;
                 PCC_bitfield_type_matters := FALSE;
                 Jumpbuf_size              := 17 * Address.size;

    |  Systems.IRIX5 =>
                 First_readable_addr       := 16_400000 * Char.size;
                 Jumpbuf_size              := 28 * Address.size;
                 Setjmp                    := "setjmp";

    |  Systems.MIPS64_OPENBSD =>
                 Jumpbuf_size              := 16_53 * Address.size;
                 Aligned_procedures        := FALSE;

    |  Systems.LINUX, Systems.LINUXELF =>
                 max_align                 := 32;
                 Jumpbuf_size              := 8 * Address.size;
                 Setjmp                    := "__setjmp";

    | Systems.NEXT =>
                 max_align                 := 16;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Jumpbuf_size              := 39 * Address.size;

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
                 Structure_size_boundary   := 32;
                 Jumpbuf_size              := 22 * Address.size;

    |  Systems.OS2 =>
                 max_align                 := 32;
                 Jumpbuf_size              := 8 * Address.size;
                 Setjmp                    := "__setjmp";
                 EOL                       := "\n"; (* really? *)

    | Systems.SEQUENT =>
                 max_align                 := 32;
                 Jumpbuf_size              := 84 * Address.size;

    | Systems.SOLgnu,
      Systems.SOLsun,
      Systems.SPARC,
      Systems.SPARC32_LINUX,
      Systems.SPARC64_LINUX,
      Systems.SPARC64_OPENBSD,
      Systems.SPARC64_SOLARIS =>

                 (* common characteristics of all SPARC targets *)

                 First_readable_addr       := 8192 * Char.size;

                 CASE System OF <* NOWARN *>
                 (* SPARC32_SOLARIS *)
                 | Systems.SOLgnu, Systems.SOLsun =>
                   Jumpbuf_size              := 19 * Address.size;
                   Has_stack_walker          := TRUE;

                 | Systems.SPARC32_LINUX =>
                   Jumpbuf_size              := 16_90 * Char.size;

                 | Systems.SPARC64_OPENBSD =>
                   Jumpbuf_size := 16_70 * Char.size;
                   Aligned_procedures := FALSE;

                 | Systems.SPARC64_LINUX =>
                   Jumpbuf_size := 16_280 * Char.size;
                   Aligned_procedures := FALSE;

                 | Systems.SPARC64_SOLARIS =>
                   Jumpbuf_size := 16_90 * Char.size;
                   Aligned_procedures := FALSE;

(*               |  Systems.I386_SOLARIS =>
                   Jumpbuf_size := 16_280 * Char.size; TBD *)

                 | Systems.SPARC =>
                   Jumpbuf_size              := 10 * Address.size;

                  END;

    | Systems.SUN3 =>
                 max_align                 := 16;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Jumpbuf_size              := 79 * Address.size;

    | Systems.SUN386 =>
                 max_align                 := 32;
                 PCC_bitfield_type_matters := FALSE;
                 Jumpbuf_size              := 8 * Address.size;

    | Systems.UMAX =>
                 max_align                 := 32;
                 Jumpbuf_size              := 10 * Address.size;

    | Systems.VAX =>
                 Real.min.fraction     := -1.70111x+38;
                 Real.max.fraction     := -1.70111x+38;
                 Longreal.min.fraction := -1.70111x+38;
                 Longreal.max.fraction := -1.70111x+38;
                 Extended.min.fraction := -1.70111x+38;
                 Extended.max.fraction := -1.70111x+38;

                 max_align                 := 32;
                 Jumpbuf_size              := 10 * Address.size;
                 All_floats_legal          := FALSE;

    |  Systems.LINUXLIBC6 =>
                 max_align                 := 32;
                 Jumpbuf_size              := 40 * Address.size;

    |  Systems.AMD64_LINUX =>
                 Jumpbuf_size              := 200 * Char.size;

    |  Systems.I386_DARWIN =>
                 Jumpbuf_size              := 18 * Address.size;
                 Setjmp                    := "setjmp";

    |  Systems.AMD64_DARWIN =>
                 Jumpbuf_size              := 19 * Address.size;
                 Setjmp                    := "setjmp";

    |  Systems.PPC_DARWIN =>
                 Jumpbuf_size              := (26 + 36 + 129 + 1 + 1) * 
                                              Address.size;
                 Jumpbuf_align             := Word64.align;
                 Setjmp                    := "setjmp";
                 (* Allow_packed_byte_aligned := TRUE; use <*LAZYALIGN*>*)

    | Systems.BSDI4 =>
                 max_align                 := 32;
                 Jumpbuf_size              := 10 * Address.size;

    |  Systems.PPC_LINUX => 
                 Jumpbuf_size              := 16_94 * Address.size;
                 Jumpbuf_align             := Word64.align;

    |  Systems.PPC32_OPENBSD => 
                 Jumpbuf_size              := 16_190 * Char.size;
                 Jumpbuf_align             := Word64.align; (* ? *)

    | Systems.NetBSD2_i386 =>
                 max_align                 := 32;
                 Jumpbuf_size              := 14 * Address.size;

(*    | Systems.I386_MSDOS =>
                 Jumpbuf_size              := 172 * Char.size; TBD *)

    | Systems.I386_OPENBSD =>
                 Jumpbuf_size              := 10 * Address.size;

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

