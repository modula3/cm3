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

<*UNUSED*>PROCEDURE IsSPARC(): BOOLEAN =
CONST startsWith = TextUtils.StartsWith;
  BEGIN
    RETURN startsWith(System_name, "SPARC") OR startsWith(System_name, "SOL");
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
    Little_endian             := TRUE;

    IF backend_mode = M3BackendMode_t.C THEN
      Setjmp := "m3_setjmp";
    ELSE
      (* Do not save/restore signal mask. Doing so is much more expensive. *)
      Setjmp := "_setjmp";
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

    (* x86 and AMD64 allow unaligned loads/stores *)

    IF IsX86() OR IsAMD64() THEN
      Allow_packed_byte_aligned := TRUE;
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
  VAR integrated := backend_mode IN BackendIntegratedSet;
  VAR llvm := backend_mode IN BackendLlvmSet;

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
    CCs := ARRAY OF CallingConvention{ New("C",          0),
                                       New("WINAPIV",    0),
                                       New("__cdecl",    0),
                                       New("WINAPI",     1),
                                       New("CALLBACK",   1),
                                       New("APIENTRY",   1),
                                       New("APIPRIVATE", 1),
                                       New("PASCAL",     1),
                                       New("__stdcall",  1) };
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
