(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Last modified on Mon Nov 21 16:42:26 PST 1994 by kalsow         *)
(*      modified on Tue Oct  4 10:34:00 PDT 1994 by isard          *)

INTERFACE ThreadContext;

FROM WinDef IMPORT BYTE, UINT16, DWORD, SIZE_T;
FROM Word IMPORT Or;

CONST SIZE_OF_80387_REGISTERS = 80;

CONST CONTEXT_i386  = 16_00100000;    (* It is de-facto CONTEXT_AMD64  this assumes that i386 and *)
CONST CONTEXT_i486  = 16_00100000;    (* It is de-facto CONTEXT_AMD64  i486 have identical context records *)
CONST CONTEXT_AMD64 = 16_00100000;

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

CONST CONTEXT_XSTATE     = Or(CONTEXT_AMD64, 16_00000040);
CONST CONTEXT_KERNEL_CET = Or(CONTEXT_AMD64, 16_00000080);


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
  DWORD64 = SIZE_T ;   (* UINT64 *)

TYPE
  M128A   = RECORD
    Low : SIZE_T ;     (* ULONGLONG *)
    Hi  : SIZE_T ;     (* ToDo: LONGLONG *)
  END;

TYPE
  CONTEXT = RECORD

    P1Home : DWORD64 ;
    P2Home : DWORD64 ;
    P3Home : DWORD64 ;
    P4Home : DWORD64 ;
    P5Home : DWORD64 ;
    P6Home : DWORD64 ;

    ContextFlags : DWORD ;
    MxCsr        : DWORD ;

    SegCs  : UINT16 ;
    SegDs  : UINT16 ;
    SegEs  : UINT16 ;
    SegFs  : UINT16 ;
    SegGs  : UINT16 ;
    SegSs  : UINT16 ;
    EFlags : DWORD  ;

    Dr0 : DWORD64 ;
    Dr1 : DWORD64 ;
    Dr2 : DWORD64 ;
    Dr3 : DWORD64 ;
    Dr6 : DWORD64 ;
    Dr7 : DWORD64 ;
                  
    Rax : DWORD64 ;
    Rcx : DWORD64 ;
    Rdx : DWORD64 ;
    Rbx : DWORD64 ;
    Esp : DWORD64 ;   (* Rsp *)
    Ebp : DWORD64 ;   (* Rbp *)
    Rsi : DWORD64 ;
    Rdi : DWORD64 ;
    R8  : DWORD64 ;
    R9  : DWORD64 ;
    R10 : DWORD64 ;
    R11 : DWORD64 ;
    R12 : DWORD64 ;
    R13 : DWORD64 ;
    R14 : DWORD64 ;
    R15 : DWORD64 ;
    Eip : DWORD64 ;   (* Rip *)

(* union {                           *)
(*  XMM_SAVE_AREA32 FltSave;         *)
(*  struct {                         *)
    XMM_SAVE_AREA32 : RECORD
      Header    : ARRAY [0..2] OF M128A;     (*         Header[2] : M128A ;          *)
      Legacy    : ARRAY [0..8] OF M128A;     (*         Legacy[8] : M128A ;          *)
      Xmm0      : M128A ;         (* *)
      Xmm1      : M128A ;         (* *)
      Xmm2      : M128A ;         (* *)
      Xmm3      : M128A ;         (* *)
      Xmm4      : M128A ;         (* *)
      Xmm5      : M128A ;         (* *)
      Xmm6      : M128A ;         (* *)
      Xmm7      : M128A ;         (* *)
      Xmm8      : M128A ;         (* *)
      Xmm9      : M128A ;         (* *)
      Xmm10     : M128A ;         (* *)
      Xmm11     : M128A ;         (* *)
      Xmm12     : M128A ;         (* *)
      Xmm13     : M128A ;         (* *)
      Xmm14     : M128A ;         (* *)
      Xmm15     : M128A ;         (* *)
    END;  
(*  } DUMMYSTRUCTNAME;               *)
(* } DUMMYUNIONNAME;                 *)
(*                                   *)
    VectorRegister       : ARRAY [0..26] OF M128A; (* VectorRegister[26]   : M128A   ;     *)
    VectorControl        : DWORD64 ;    (* *)
                                        (* *)
    DebugControl         : DWORD64 ;    (* *)
    LastBranchToRip      : DWORD64 ;    (* *)
    LastBranchFromRip    : DWORD64 ;    (* *)
    LastExceptionToRip   : DWORD64 ;    (* *)
    LastExceptionFromRip : DWORD64 ;    (* *)

    (* ExtendedRegisters:  ARRAY[0 .. MAXIMUM_SUPPORTED_EXTENSION-1] OF BYTE; *)
  END;

TYPE PCONTEXT = UNTRACED REF CONTEXT;

END ThreadContext.
