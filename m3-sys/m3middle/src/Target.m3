(* Copyright (C) 1993, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(*                                                              *)
(* File: Target.m3                                              *)
(* Last Modified On Tue Jan 24 09:58:43 PST 1995 By kalsow      *)
(*      Modified On Fri Dec  9 12:56:47 PST 1994 By Olaf Wagner *)

MODULE Target;

IMPORT Text, TargetMap, M3RT, TextUtils;

TYPE
  (* Just a few systems need special handling and are listed here.
   * Far more targets than this are supported.
   * See m3-sys/cminstall/src/config-no-install and scripts/python/targets.txt
   * for an idea as to the working targets.
   *
   * SystemNames and Systems need to roughly match.
   *)
  Systems = {
    Undefined,
    FreeBSD4,
    I386_CYGWIN,
    I386_INTERIX,
    I386_MINGW,
    I386_NT,
    LINUXLIBC6,
    NT386,
    SOLSUN, (* Old name for SPARC32_SOLARIS with Sun C compiler *)
    SOLGNU, (* Old name for SPARC32_SOLARIS with GNU C compiler *)
    SPARC32_SOLARIS,
    SPARC64_SOLARIS,
    I386_SOLARIS,
    AMD64_SOLARIS,
    Other
  };

CONST
  (* These are uppercase to facilitate case insensitivity. *)
  SystemNames = ARRAY OF TEXT {
    "",
    "FREEBSD4",
    "I386_CYGWIN",
    "I386_INTERIX",
    "I386_MINGW",
    "I386_NT",
    "LINUXLIBC6",
    "NT386",
    "SOLSUN",
    "SOLGNU",
    "SPARC32_SOLARIS",
    "SPARC64_SOLARIS",
    "I386_SOLARIS",
    "AMD64_SOLARIS"
  };

VAR (*CONST*)
  CCs : ARRAY [0..8] OF CallingConvention;
  System: Systems;

  (* The directory containing derived files. This is to aid the C
   * backend in converging target-dependent output to be target-independent.
   * It occurs unnecessarily in some places such as line directives.
   * m3front communicates to m3back via m3middle, a string that backend
   * might want to change.
   *)
  Build_dir: TEXT;
  Build_dir_length: INTEGER;

PROCEDURE Init64 () =
  BEGIN
    Integer := Int64;
    Word := Word64;
    Address := Word64;
    Address.cg_type := CGType.Addr;
  END Init64;

PROCEDURE Solaris(): BOOLEAN =
  BEGIN
    RETURN System IN SET OF Systems{Systems.SOLSUN,
                                    Systems.SOLGNU,
                                    Systems.SPARC32_SOLARIS,
                                    Systems.SPARC64_SOLARIS,
                                    Systems.I386_SOLARIS,
                                    Systems.AMD64_SOLARIS};
  END Solaris;

PROCEDURE IsX86orAmd64(System_name: TEXT): BOOLEAN =
(* System_name is uppercased to facilitate case insensitivity.
 * Is this too broad? *)
CONST start = ARRAY OF TEXT{"AMD64", "86",
                            "X86", "I386", "I486", "I586", "I686",
                            "I86",  "386",  "486",  "586",  "686"};
        end = ARRAY OF TEXT{"AMD64", "86"};
  BEGIN
    IF System IN SET OF Systems{Systems.FreeBSD4,
                                Systems.I386_CYGWIN,
                                Systems.I386_INTERIX,
                                Systems.I386_MINGW,
                                Systems.I386_NT,
                                Systems.LINUXLIBC6,
                                Systems.NT386} THEN
        RETURN TRUE;
    END;
    FOR i := FIRST(start) TO LAST(start) DO
      IF TextUtils.StartsWith(System_name, start[i]) THEN
        RETURN TRUE;
      END;
    END;
    FOR i := FIRST(end) TO LAST(end) DO
      IF TextUtils.EndsWith(System_name, end[i]) THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END IsX86orAmd64;

PROCEDURE Init (system: TEXT; in_OS_name: TEXT; backend_mode: M3BackendMode_t): BOOLEAN =
  VAR sys := 0;  max_align := 64;
      casePreservedSystem := system;
  BEGIN
    (* lookup the system -- linear search *)
    IF (system = NIL) THEN RETURN FALSE END;
    system := TextUtils.Upper(system); (* Uppercase for case insensitivity. *)
    WHILE NOT Text.Equal (system, SystemNames[sys]) DO
      INC (sys);  IF (sys >= NUMBER (SystemNames)) THEN EXIT END;
    END;
    System := VAL(sys, Systems);
    System_name := casePreservedSystem;
    OS_name := in_OS_name;

    (* common values *)

    Atomic_lock_free :=
        ARRAY [CGType.Word8..CGType.Addr] OF BOOLEAN { TRUE, .. };
    (* this is overly optimistic... *)

    Allow_packed_byte_aligned := FALSE;
    endian := Endian.Little;

    (* This is messy. See CsetjmpC.c. *)
    IF backend_mode = M3BackendMode_t.C THEN
      Setjmp := "m3_setjmp";
      Sigsetjmp := FALSE;
    ELSIF Solaris() THEN
      Setjmp := "sigsetjmp";
      Sigsetjmp := TRUE;
    ELSE
      Setjmp := "_setjmp";
      Sigsetjmp := FALSE;
    END;

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

    (* add the system-specific customization *)

    (* 64bit *)

    IF TextUtils.StartsWith(system, "ALPHA_")       (* But not ALPHA32_. *)
        OR TextUtils.Contains(system, "64") THEN
      Init64();
    ELSIF backend_mode # M3BackendMode_t.C THEN
      (* Change only alignment.  Size is always 64:
       * Aligning these types to 32 is incorrect on many but not all 32bit targets.
       * C backend cannot portably reduce alignment but it can portably increase
       * alignment. These types were 64-aligned for a very long time on
       * all 32bit targets.
       *)
      Longint.align := 32;
      Long.align := 32;
      Longreal.align := 32;
      Extended.align := 32;
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
      endian := Endian.Big;
    END;

    (* x86 and AMD64 allow unaligned loads/stores but converge C *)
    IF backend_mode # M3BackendMode_t.C THEN
      IF IsX86orAmd64(system) THEN
        Allow_packed_byte_aligned := TRUE;
      END;
    END;

    (* NT/x86 is the only system with multiple calling conventions,
     * ignoring little-used vectorcall of NT/amd64 and NT/x86.
     *)
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
  VAR integrated := backend_mode IN BackendIntegratedSet;
  VAR llvm := backend_mode IN BackendLlvmSet;

  PROCEDURE New(name: TEXT; id: [0..1]): CallingConvention =
    VAR cc := NEW(CallingConvention, name := name);
    BEGIN
      IF backend_mode = M3BackendMode_t.C OR target_has_calling_conventions THEN
        cc.m3cg_id            := id;
      ELSE
        cc.m3cg_id            := 0;
      END;
      IF backend_mode = M3BackendMode_t.C THEN
        cc.args_left_to_right := TRUE;
        cc.results_on_left    := TRUE;
        cc.standard_structs   := TRUE;
      ELSIF llvm THEN
        cc.args_left_to_right := TRUE;
        cc.results_on_left    := FALSE;
        cc.standard_structs   := TRUE;
(* CHECK: ^Are these right for llvm? They are same as gcc. *)
      ELSIF integrated THEN
        cc.args_left_to_right := FALSE;
        cc.results_on_left    := TRUE;
        cc.standard_structs   := FALSE;
      ELSE (* gcc-derived back end. *)
        cc.args_left_to_right := TRUE;
        cc.results_on_left    := FALSE;
        cc.standard_structs   := TRUE;
      END;
      RETURN cc;
    END New;
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

PROCEDURE SetBuild_dir(build_dir: TEXT) =
BEGIN
  build_dir := "../" & build_dir & "/";
  Build_dir := build_dir;
  Build_dir_length := Text.Length(build_dir);
END SetBuild_dir;

PROCEDURE PathCharEqual(ch1, ch2: CHAR): BOOLEAN =
BEGIN
  RETURN (ch1 = ch2) OR
     (ch1 = '\\' AND ch2 = '/') OR
     (ch2 = '\\' AND ch1 = '/');
END PathCharEqual;

PROCEDURE CleanupSourcePath(file: TEXT): TEXT =
VAR length: INTEGER;
    build_dir: TEXT;
    build_dir_length: INTEGER;
    pos: INTEGER;
BEGIN

  <* ASSERT BackendModeInitialized *>

  (* Review: Is this function too lossy?
   * Would we be better off with just substituting build_dir with "build_dir"?
   *)

  IF file # NIL AND BackendMode = M3BackendMode_t.C THEN
    file := TextUtils.SubstChar(file, '\\', '/');
    length := Text.Length(file);
    build_dir := Build_dir;
    build_dir_length := Build_dir_length;

    (* m3front does like:
     * set_source_file("../AMD64_DARWIN/WordMod.m3 => ../src/builtinWord/Mod.mg")
     * which damages debugging (this file does not exist) and is unnecessarily
     * target specific, damaging portable distribution format.
     *)
    IF length > 0 AND Text.GetChar(file, length - 1) = 'g' THEN
      pos := TextUtils.Pos(file, " => ");
      IF pos # -1 THEN
        file := Text.Sub(file, pos + 4, length - pos - 4);
        length := Text.Length(file);
      END
    END;

    (* Remove build_dir from start for portable distribution format.
     * This is typically generic instantations but could be anything.
     *)
    IF length > Build_dir_length THEN
      FOR i := 0 TO build_dir_length - 1 DO
        IF NOT PathCharEqual(Text.GetChar(file, i), Text.GetChar(build_dir, i)) THEN
          EXIT;
        END;
        IF i = build_dir_length - 1 THEN
          file := Text.Sub(file, build_dir_length, length - build_dir_length);
        END;
      END;
    END;
  END;

  RETURN file;
END CleanupSourcePath;

(*-------------------------- range of WIDECHAR ------------------------------*)

VAR wideCharSize        := 16;
VAR wideCharMax         := WideChar16Max;

PROCEDURE IsWideChar32 (): BOOLEAN =
BEGIN
  RETURN wideCharSize = 32;
END IsWideChar32;

PROCEDURE WideCharSize (): INTEGER =
BEGIN
  RETURN wideCharSize;
END WideCharSize;

PROCEDURE WideCharMax (): INTEGER =
BEGIN
  RETURN wideCharMax;
END WideCharMax;

PROCEDURE WideCharNumber (): INTEGER =
BEGIN
  RETURN wideCharMax + 1;
END WideCharNumber;

PROCEDURE SetWideChar16 () =
BEGIN
  wideCharSize := 16;
  wideCharMax := WideChar16Max;
END SetWideChar16;

PROCEDURE SetWideChar32 () =
BEGIN
  wideCharSize := 32;
  wideCharMax := WideChar32Max;
END SetWideChar32;

(*---------------------------------------------------------------------------*)

BEGIN
END Target.
