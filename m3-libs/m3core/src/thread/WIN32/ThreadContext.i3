(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Last modified on Mon Nov 21 16:42:26 PST 1994 by kalsow         *)
(*      modified on Tue Oct  4 10:34:00 PDT 1994 by isard          *)

INTERFACE ThreadContext;

IMPORT WinDef, Word;

CONST SIZE_OF_80387_REGISTERS = 80;

CONST CONTEXT_i386 = 16_00010000;    (* this assumes that i386 and *)
CONST CONTEXT_i486 = 16_00010000;    (* i486 have identical context records *)

CONST CONTEXT_CONTROL = Word.Or(CONTEXT_i386, 16_0001);
                          (* SS:SP, CS:IP, FLAGS, BP *)
CONST CONTEXT_INTEGER = Word.Or(CONTEXT_i386, 16_0002);
                          (* AX, BX, CX, DX, SI, DI *)
CONST CONTEXT_SEGMENTS= Word.Or(CONTEXT_i386, 16_0004);
                          (* DS, ES, FS, GS *)
CONST CONTEXT_FLOATING_POINT = Word.Or(CONTEXT_i386, 16_0008);
                          (* 387 state *)
CONST CONTEXT_DEBUG_REGISTERS = Word.Or(CONTEXT_i386, 16_0010);
                          (* DB 0-3,6,7 *)

CONST CONTEXT_FULL = Word.Or(CONTEXT_CONTROL, Word.Or(CONTEXT_INTEGER,
                                                      CONTEXT_SEGMENTS));

TYPE
  FLOATING_SAVE_AREA = RECORD
    ControlWord:   WinDef.DWORD;
    StatusWord:    WinDef.DWORD;
    TagWord:       WinDef.DWORD;
    ErrorOffset:   WinDef.DWORD;
    ErrorSelector: WinDef.DWORD;
    DataOffset:    WinDef.DWORD;
    DataSelector:  WinDef.DWORD;
    RegisterArea:  ARRAY[0 .. SIZE_OF_80387_REGISTERS-1] OF WinDef.BYTE;
    Cr0NpxState:   WinDef.DWORD;
  END;

TYPE PFLOATING_SAVE_AREA = ADDRESS;
  (* ptr to FLOATING_SAVE_AREA *)

TYPE
  CONTEXT = RECORD
    ContextFlags: WinDef.DWORD;

    Dr0: WinDef.DWORD;
    Dr1: WinDef.DWORD;
    Dr2: WinDef.DWORD;
    Dr3: WinDef.DWORD;
    Dr6: WinDef.DWORD;
    Dr7: WinDef.DWORD;

    FloatSave: FLOATING_SAVE_AREA;

    SegGs: WinDef.DWORD;
    SegFs: WinDef.DWORD;
    SegEs: WinDef.DWORD;
    SegDs: WinDef.DWORD;

    Edi: WinDef.DWORD;
    Esi: WinDef.DWORD;
    Ebx: WinDef.DWORD;
    Edx: WinDef.DWORD;
    Ecx: WinDef.DWORD;
    Eax: WinDef.DWORD;

    Ebp:    WinDef.DWORD;
    Eip:    WinDef.DWORD;
    SegCs:  WinDef.DWORD;
    EFlags: WinDef.DWORD;
    Esp:    WinDef.DWORD;
    SegSs:  WinDef.DWORD;
  END;

TYPE PCONTEXT = UNTRACED REF CONTEXT;

END ThreadContext.
