(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Last modified on Mon Nov 21 16:42:26 PST 1994 by kalsow         *)
(*      modified on Tue Oct  4 10:34:00 PDT 1994 by isard          *)

INTERFACE ThreadContext;

FROM WinDef IMPORT BYTE, DWORD, SIZE_T;
FROM Word IMPORT Or;

CONST SIZE_OF_80387_REGISTERS = 80;

CONST CONTEXT_i386 = 16_00010000;    (* this assumes that i386 and *)
CONST CONTEXT_i486 = 16_00010000;    (* i486 have identical context records *)

CONST CONTEXT_CONTROL = Or(CONTEXT_i386, 16_0001);
                          (* SS:SP, CS:IP, FLAGS, BP *)
CONST CONTEXT_INTEGER = Or(CONTEXT_i386, 16_0002);
                          (* AX, BX, CX, DX, SI, DI *)
CONST CONTEXT_SEGMENTS= Or(CONTEXT_i386, 16_0004);
                          (* DS, ES, FS, GS *)
CONST CONTEXT_FLOATING_POINT = Or(CONTEXT_i386, 16_0008);
                          (* 387 state *)
CONST CONTEXT_DEBUG_REGISTERS = Or(CONTEXT_i386, 16_0010);
                          (* DB 0-3,6,7 *)

CONST CONTEXT_FULL = Or(CONTEXT_CONTROL, Or(CONTEXT_INTEGER,
                                            CONTEXT_SEGMENTS));
CONST MAXIMUM_SUPPORTED_EXTENSION = 512;

TYPE
  FLOATING_SAVE_AREA = RECORD
    ControlWord:   DWORD;
    StatusWord:    DWORD;
    TagWord:       DWORD;
    ErrorOffset:   DWORD;
    ErrorSelector: DWORD;
    DataOffset:    DWORD;
    DataSelector:  DWORD;
    RegisterArea:  ARRAY[0 .. SIZE_OF_80387_REGISTERS-1] OF BYTE;
    Cr0NpxState:   DWORD;
  END;

TYPE PFLOATING_SAVE_AREA = ADDRESS;
  (* ptr to FLOATING_SAVE_AREA *)

TYPE
  CONTEXT = RECORD
    ContextFlags: DWORD;

    Dr0: DWORD;
    Dr1: DWORD;
    Dr2: DWORD;
    Dr3: DWORD;
    Dr6: DWORD;
    Dr7: DWORD;

    FloatSave: FLOATING_SAVE_AREA;

    SegGs: DWORD;
    SegFs: DWORD;
    SegEs: DWORD;
    SegDs: DWORD;

    Edi: DWORD;
    Esi: DWORD;
    Ebx: DWORD;
    Edx: DWORD;
    Ecx: DWORD;
    Eax: DWORD;

    Ebp:    SIZE_T; (* temporary Win64 hack *)
    Eip:    DWORD;
    SegCs:  DWORD;
    EFlags: DWORD;
    Esp:    DWORD;
    SegSs:  DWORD;

    ExtendedRegisters:  ARRAY[0 .. MAXIMUM_SUPPORTED_EXTENSION-1] OF BYTE;
  END;

TYPE PCONTEXT = UNTRACED REF CONTEXT;

END ThreadContext.
