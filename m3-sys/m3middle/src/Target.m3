(* Copyright (C) 1993, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(*                                                              *)
(* File: Target.m3                                              *)
(* Last Modified On Tue Jan 24 09:58:43 PST 1995 By kalsow      *)
(*      Modified On Fri Dec  9 12:56:47 PST 1994 By Olaf Wagner *)

MODULE Target;

IMPORT Text, TargetMap, M3RT, TextUtils;

VAR (*CONST*)
  CCs : ARRAY [0..8] OF CallingConvention;

PROCEDURE Init64 () =
  BEGIN
    Integer := Int64;
    Word := Word64;
    Address := Word64;
    Address.cg_type := CGType.Addr;
  END Init64;

PROCEDURE IsX86(): BOOLEAN =
  BEGIN
    IF TextUtils.StartsWith(System_name, "I") AND
          (TextUtils.StartsWith(System_name, "I386_")
        OR TextUtils.StartsWith(System_name, "I486_")
        OR TextUtils.StartsWith(System_name, "I586_")
        OR TextUtils.StartsWith(System_name, "I686_")) THEN
      RETURN TRUE;
    END;
    CASE System OF
    | Systems.FreeBSD4, Systems.NT386, Systems.LINUXLIBC6 => RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END IsX86;

PROCEDURE IsAMD64(): BOOLEAN =
  BEGIN
    RETURN TextUtils.StartsWith(System_name, "AMD64_");
  END IsAMD64;

PROCEDURE IsSPARC(): BOOLEAN =
  BEGIN
    RETURN (TextUtils.StartsWith(System_name, "S")
            AND (TextUtils.StartsWith(System_name, "SPARC")
              OR TextUtils.StartsWith(System_name, "SOL")));
  END IsSPARC;
  
PROCEDURE Init (system: TEXT; in_OS_name: TEXT; backend_mode: M3BackendMode_t): BOOLEAN =
  VAR sys := 0;  max_align := 64;
  BEGIN
    (* lookup the system -- linear search *)
    IF (system = NIL) THEN RETURN FALSE END;
    WHILE NOT Text.Equal (system, SystemNames[sys]) DO
      INC (sys);  IF (sys >= NUMBER (SystemNames)) THEN RETURN FALSE END;
    END;
    System := VAL(sys, Systems);
    System_name := SystemNames[sys];

    OS_name := in_OS_name;

    (* common values *)

    Atomic_lock_free :=
        ARRAY [CGType.Word8..CGType.Addr] OF BOOLEAN { TRUE, .. };
    (* this is overly optimistic... *)

    Allow_packed_byte_aligned := FALSE;
    All_floats_legal          := TRUE;
    PCC_bitfield_type_matters := TRUE;
    Structure_size_boundary   := 8;
    Little_endian             := TRUE;
    Setjmp                    := "_setjmp";

    (* There is no portable stack walker, and therefore few systems have one.
       Having a stack walker theoretically speeds up everything nicely.  If
       you are familiar with NT exception handling, all but x86 have a stack
       walker.  Not having a stack walker means that functions that have
       try/except/finally/raise incur a codegen cost even if there is never a
       raise -- for having the try.  Having a stack walker means "zero cost"
       for exception handling code that does not actually raise an exception.

       If there is NOT a stack walker, prologues/epilogues for functions that
       try/except/finally/raise call PushEFrame / PopEFrame, essentially to
       build a walkable parallel stack.

       If there is a stack walker, then raise can discover what PushEFrame /
       PopEFrame effectively recorded.

       NT/x86 has a highly optimized equivalent of PushEFrame / PopEFrame, not
       currently used by Modula-3. *)

    Has_stack_walker          := FALSE;

    (* "Closures" in Modula-3 -- function pointers to nested functions, are
       represented as a pointer to -1 (of size?) followed by other data.  -1
       is assumed to be invalid code. Prior to calling any function pointer,
       the generated code first checks for the marker -1, to decide how to
       call it.  On systems where data alignment matters, but functions are
       not aligned, this can result in an alignment fault. Most systems either
       don't care about alignment (x86, AMD64) or have fixed sized and aligned
       instructions (PowerPC), in which case the check for -1 can just be a
       direct read, in which case Aligned_procedures := TRUE.  This logic can
       break down on 64bit platforms, where the -1 is perhaps 64bits, but the
       fixed size instructions may be 32bits. Or of course, on systems that
       care about data alignment but not code alignment, or in which the
       alignments differ.

       Setting this to FALSE is safe, but results in slower code -- code that
       checks function pointers for alignment before checking for the -1, and
       if they aren't aligned, doing a byte-wise read instead of an
       integer-wise read.

       We can probably do better here, such as ensuring the marker is 4 bytes
       instead of 8, if that works (for 64 bit platforms, that care about
       alignment, but with fixed sized aligned 32 bit instructions, which
       probably describes some (e.g., MIPS64 and SPARC64) *)

    Aligned_procedures := TRUE;

    (* The effect of First_readable_addr is that (static?) array indices
       (offsets) lower than it (and positive?) do not have a NULL check on the
       array base.  Reading NULL + an offset less than First_readable_addr is
       assumed to access violate the same as reading NULL. It is a nice
       optimization.  Setting the value too low results in correct but
       suboptimal code.  However just setting it to a small non-zero number
       should provide most of the benefit.  Setting the value too high results
       in missing NULL checks -- a loss of safety enforcement.  Typically
       setting it to one hardware page is a good estimate, since if NULL is
       not accessible, nor is any address on the same page. As well, setting
       it to getpagesize, whatever the granularity of mmap/VirtualAlloc, often
       larger than a hardware page, is another good guess.  *)

    First_readable_addr := 4096 * Char.size;

    (* add the system-specific customization *)

    (* 64bit *)

    IF TextUtils.StartsWith(system, "ALPHA_")
        OR TextUtils.Contains(system, "64") THEN
      Init64();
    END;

    (* ALPHA, SPARC64, HPPA, MIPS64: aligned_procedures *)

    IF TextUtils.StartsWith(system, "SPARC64")
        OR TextUtils.StartsWith(system, "ARMEL")
        OR TextUtils.StartsWith(system, "ALPHA_")
        OR TextUtils.StartsWith(system, "ALPHA64")
        OR TextUtils.StartsWith(system, "PA")
        OR TextUtils.StartsWith(system, "HPPA")
        OR TextUtils.StartsWith(system, "MIPS64") THEN
      Aligned_procedures := FALSE;
    END;

    (* big endian *)

    IF TextUtils.StartsWith(system, "PA")
        OR (TextUtils.StartsWith(system, "MIPS")
          AND NOT TextUtils.StartsWith(system, "MIPSEL")
          AND NOT TextUtils.StartsWith(system, "MIPS32EL")
          AND NOT TextUtils.StartsWith(system, "MIPS64EL"))
        OR TextUtils.StartsWith(system, "PPC")  (* ambiguous *)
        OR TextUtils.StartsWith(system, "SPARC")
        OR TextUtils.StartsWith(system, "SOL") THEN
      Little_endian := FALSE;
    END;

    (* SPARC: 8K pages *)

    IF IsSPARC() THEN
      First_readable_addr := 8192 * Char.size;
    END;
    
    (* x86 and AMD64 allow unaligned loads/stores *)

    IF IsX86() OR IsAMD64() THEN
      Allow_packed_byte_aligned := TRUE;
      Aligned_procedures := TRUE; (* Assume aligned => unaligned is ok. *)
    END;

    IF System IN SET OF Systems{Systems.ALPHA32_VMS,
                                Systems.ALPHA64_VMS} THEN
        Setjmp := "decc$setjmp";
    END;

    IF System IN SET OF Systems{Systems.PA32_HPUX,
                                Systems.PA64_HPUX} THEN
        Structure_size_boundary := 16;
    END;

    CASE System OF
    
    |  Systems.ALPHA_LINUX => Jumpbuf_size := 34 * Address.size;
    |  Systems.ALPHA_OPENBSD => Jumpbuf_size := 81 * Address.size;
    |  Systems.ALPHA_OSF => Jumpbuf_size := 84 * Address.size;

    |  Systems.I386_FREEBSD, Systems.FreeBSD4 =>
                 Jumpbuf_size              := 11 * Address.size;

    |  Systems.AMD64_NETBSD,
       Systems.AMD64_OPENBSD,
       Systems.AMD64_FREEBSD =>
                 Jumpbuf_size              := 12 * Address.size;

    | Systems.ARM_LINUX,
      Systems.ARMEL_LINUX =>
                 Jumpbuf_size := 64 * Int64.size; (* 392 bytes = 49 * Int64.size on Raspberry Pi *)

    |  Systems.PA32_HPUX =>
                 (* 200 bytes with 8 byte alignment *)
                 Jumpbuf_size              := 50 * Address.size;

    |  Systems.PA64_HPUX =>
                 (* 640 bytes with 16 byte alignment *)
                 Jumpbuf_size              := 80 * Address.size;

    |  Systems.MIPS64_OPENBSD,
       Systems.MIPS64EL_OPENBSD =>
                 Jumpbuf_size              := 16_53 * Address.size;

    | Systems.I386_INTERIX =>

                (* Visual C++'s 16 plus 2 ints: is sigmask saved, its value. *)

                Jumpbuf_size := 18 * Address.size;

    | Systems.NT386, Systems.I386_NT, Systems.I386_CYGWIN, Systems.I386_MINGW =>

                 (* Cygwin: 13, Visual C++: 16, Interix: 18.
                    Use 18 for interop.
                    Cygwin's setjmp.h is wrong by a factor of 4.
                    Cygwin provides setjmp and _setjmp that resolve the same.
                    Visual C++ provides only _setjmp.
                    Visual C++ also has _setjmp3 that the compiler generates
                    a call to. In fact _setjmp appears to only use 8 ints
                    and _setjmp3 appears to use more. Consider using _setjmp3.
                 *)
                 Jumpbuf_size := 18 * Address.size;

    | Systems.AMD64_NT =>
                 (* 256 bytes with 16 byte alignment *)
                 Jumpbuf_size := 32 * Int64.size;
                 Setjmp := "setjmp";

    | Systems.IA64_FREEBSD, Systems.IA64_HPUX,
      Systems.IA64_LINUX, Systems.IA64_NETBSD, Systems.IA64_NT,
      Systems.IA64_OPENBSD, Systems.IA64_VMS =>
                 (* random guess: 1K *)
                 Jumpbuf_size     := 128 * Address.size;

    | Systems.SPARC32_SOLARIS, Systems.SOLgnu, Systems.SOLsun =>
                 (* 76 bytes with 4 byte alignment *)
                 Jumpbuf_size     := 19 * Address.size;

    | Systems.SPARC32_LINUX =>
                 Jumpbuf_size              := 16_90 * Char.size;

    | Systems.SPARC64_OPENBSD =>
                 Jumpbuf_size := 14 * Address.size;

    | Systems.SPARC64_LINUX =>
                 Jumpbuf_size := 16_280 * Char.size;

    | Systems.SPARC64_SOLARIS =>
                 (* 96 bytes with 8 byte alignment *)
                 Jumpbuf_size     := 12 * Address.size;

    |  Systems.I386_SOLARIS =>
                 (* 40 bytes with 4 byte alignment *)
                 Jumpbuf_size := 10 * Address.size;

    |  Systems.AMD64_SOLARIS =>
                 (* 64 bytes with 8 byte alignment *)
                 Jumpbuf_size := 8 * Address.size;

    |  Systems.I386_LINUX, Systems.LINUXLIBC6 =>
                 Jumpbuf_size              := 39 * Address.size;

    |  Systems.AMD64_LINUX =>
                 Jumpbuf_size              := 25 * Address.size;

    |  Systems.I386_DARWIN =>
                 Jumpbuf_size              := 18 * Address.size;

     | Systems.AMD64_DARWIN =>
                 Jumpbuf_size              := ((9 * 2) + 3 + 16) * Int32.size;

    |  Systems.ARM_DARWIN =>
                 Jumpbuf_size              := 28 * Address.size;

    |  Systems.PPC_DARWIN =>
                 Jumpbuf_size  := 768 * Word8.size;

    | Systems.PPC64_DARWIN =>
                 Jumpbuf_size  := 872 * Word8.size;

    |  Systems.PPC_LINUX => 
                 Jumpbuf_size              := 74 * Int64.size;
                 (* ideal alignment is 16 bytes, but 4 is ok *)

    |  Systems.PPC32_OPENBSD => 
                 Jumpbuf_size              := 100 * Address.size;

    | Systems.I386_NETBSD =>
                 Jumpbuf_size              := 14 * Address.size; (* 13? *)
                 
    | Systems.ALPHA32_VMS,
      Systems.ALPHA64_VMS =>
                 Jumpbuf_size              := 68 * Word64.size;

(*  | Systems.I386_MSDOS =>
                 Jumpbuf_size              := 172 * Char.size; TBD *)

    | Systems.I386_OPENBSD =>
                 Jumpbuf_size              := 10 * Address.size;

    ELSE RETURN FALSE;
    END;

    InitCallingConventions (backend_mode,
                            System IN SET OF Systems{Systems.I386_INTERIX,
                                                     Systems.NT386,
                                                     Systems.I386_NT,
                                                     Systems.I386_CYGWIN,
                                                     Systems.I386_MINGW});

    (* check the "bytes" and "pack" fields *)
    CheckI (Address, max_align);
    CheckI (Integer, max_align);
    CheckI (Word, max_align);
    CheckI (Longint, max_align);
    CheckI (Long, max_align);
    CheckF (Real, max_align);
    CheckF (Longreal, max_align);
    CheckF (Extended, max_align);
    CheckI (Int8, max_align);
    CheckI (Int16, max_align);
    CheckI (Int32, max_align);
    CheckI (Int64, max_align);
    CheckI (Word8, max_align);
    CheckI (Word16, max_align);
    CheckI (Word32, max_align);
    CheckI (Word64, max_align);
    CheckI (Void, max_align);
    CheckI (Char, max_align);

    (* fix the alignments *)
    FOR i := FIRST (Alignments) TO LAST (Alignments) DO
      <* ASSERT Alignments[i] = MIN (Alignments[i], max_align) *>
    END;

    (* initialize the other target-specific modules *)
    TargetMap.Init ();
    M3RT.Init ();

    RETURN TRUE;
  END Init;

PROCEDURE InitCallingConventions(backend_mode: M3BackendMode_t;
                                 target_has_calling_conventions: BOOLEAN) =
  PROCEDURE New(name: TEXT; id: [0..1]): CallingConvention =
    VAR cc := NEW(CallingConvention, name := name);
    BEGIN
      (* This stuff seems messed up. *)
      IF (*backend_mode = M3BackendMode_t.C OR*) target_has_calling_conventions THEN
        cc.m3cg_id            := id;
      ELSE
        cc.m3cg_id            := 0;
      END;
      IF backend_mode = M3BackendMode_t.C THEN
        cc.args_left_to_right := TRUE;
        cc.results_on_left    := TRUE;
        cc.standard_structs   := TRUE;
      ELSIF integrated THEN
        cc.args_left_to_right := FALSE;
        cc.results_on_left    := TRUE;
        cc.standard_structs   := FALSE;
      ELSE
        cc.args_left_to_right := TRUE;
        cc.results_on_left    := FALSE;
        cc.standard_structs   := TRUE;
      END;
      RETURN cc;
    END New;
  VAR integrated := BackendIntegrated[backend_mode];    
  BEGIN
    (* 0 is __cdecl, 1 is __stdcall. *)
    CCs := ARRAY OF CallingConvention{ New("__cdecl",    0), (* must be first *)
                                       New("__stdcall",  1), (* must be second *)
                                       New("C",          0),
                                       New("WINAPIV",    0),
                                       New("WINAPI",     1),
                                       New("CALLBACK",   1),
                                       New("APIENTRY",   1),
                                       New("APIPRIVATE", 1),
                                       New("PASCAL",     1) };
    DefaultCall := CCs[0];
  END InitCallingConventions;

PROCEDURE CheckI (READONLY i: Int_type; max_align: INTEGER) =
  BEGIN
    <* ASSERT i.align = MIN (i.align, max_align) *>
    <* ASSERT i.bytes = i.size DIV Byte *>
    <* ASSERT i.pack  = (i.size + i.align - 1) DIV i.align * i.align *>
  END CheckI;

PROCEDURE CheckF (READONLY f: Float_type; max_align: INTEGER) =
  BEGIN
    <* ASSERT f.align = MIN (f.align, max_align) *>
    <* ASSERT f.bytes = f.size DIV Byte *>
    (* ASSERT f.pack  = (f.size + f.align - 1) DIV f.align * f.align *)
  END CheckF;

PROCEDURE FindConvention (nm: TEXT): CallingConvention =
  VAR cc: CallingConvention;
  BEGIN
    FOR i := 0 TO LAST (CCs) DO
      cc := CCs[i];
      IF (cc # NIL) AND Text.Equal (nm, cc.name) THEN RETURN cc; END;
    END;
    RETURN NIL;
  END FindConvention;

PROCEDURE ConventionFromID (id: INTEGER): CallingConvention =
  BEGIN
    RETURN CCs[id];
  END ConventionFromID;

BEGIN
END Target.
