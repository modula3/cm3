(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Last modified on Mon Nov 21 16:42:26 PST 1994 by kalsow         *)
(*      modified on Tue Oct  4 10:34:00 PDT 1994 by isard          *)

INTERFACE ThreadContext;

FROM Word IMPORT Or;
FROM Cstdint IMPORT uint8_t, uint16_t, uint32_t;

CONST CONTEXT_AMD64           = 16_00100000;
CONST CONTEXT_CONTROL         = Or(CONTEXT_AMD64, 16_00000001);
CONST CONTEXT_INTEGER         = Or(CONTEXT_AMD64, 16_00000002);
CONST CONTEXT_SEGMENTS        = Or(CONTEXT_AMD64, 16_00000004);
CONST CONTEXT_FLOATING_POINT  = Or(CONTEXT_AMD64, 16_00000008);
CONST CONTEXT_DEBUG_REGISTERS = Or(CONTEXT_AMD64, 16_00000010);

CONST CONTEXT_FULL = Or(CONTEXT_CONTROL, Or(CONTEXT_INTEGER, CONTEXT_FLOATING_POINT));

CONST CONTEXT_ALL = Or(CONTEXT_CONTROL, Or(CONTEXT_INTEGER,
                    Or(CONTEXT_SEGMENTS, Or(CONTEXT_FLOATING_POINT,
                       CONTEXT_DEBUG_REGISTERS))));
CONST CONTEXT_XSTATE     = Or(CONTEXT_AMD64, 16_00000040);
CONST CONTEXT_KERNEL_CET = Or(CONTEXT_AMD64, 16_00000080);


(* TODO: This should be 16 byte aligned. *)
TYPE M128A = RECORD
    Low: INTEGER;
    High: INTEGER;
  END;

(* TODO: This should be 16 byte aligned (at least via M128A). *)
TYPE
  CONTEXT = RECORD
    (* Register parameter home addresses.
     *
     * N.B. These fields are for convenience - they could be used to extend the
     *      context record in the future.
     *)
    P1Home: INTEGER;
    P2Home: INTEGER;
    P3Home: INTEGER;
    P4Home: INTEGER;
    P5Home: INTEGER;
    P6Home: INTEGER;

    (* Control flags. *)
    ContextFlags: uint32_t;
    MxCsr: uint32_t;

    (* Segment Registers and processor flags. *)
    SegCs: uint16_t;
    SegDs: uint16_t;
    SegEs: uint16_t;
    SegFs: uint16_t;
    SegGs: uint16_t;
    SegSs: uint16_t;
    EFlags: uint32_t;

    (* Debug registers *)
    Dr0: INTEGER;
    Dr1: INTEGER;
    Dr2: INTEGER;
    Dr3: INTEGER;
    Dr6: INTEGER;
    Dr7: INTEGER;

    (* Integer registers. *)
    Rax: INTEGER;
    Rcx: INTEGER;
    Rdx: INTEGER;
    Rbx: INTEGER;
    (* Hack for interop for 32bit RTSignal.m3.
    Rsp: INTEGER;
    Rbp: INTEGER;
    *)  Esp: INTEGER;
        Ebp: INTEGER;
    Rsi: INTEGER;
    Rdi: INTEGER;
    R8: INTEGER;
    R9: INTEGER;
    R10: INTEGER;
    R11: INTEGER;
    R12: INTEGER;
    R13: INTEGER;
    R14: INTEGER;
    R15: INTEGER;

    (* Program counter. *)
    (* Hack for interop for 32bit RTSignal.m3. *)
    Eip(*Rip*): INTEGER;

    (* Floating point state.
     *
     * union {
     *  XMM_SAVE_AREA32 FltSave;
     *  struct {
     *)
            Header: ARRAY [0..1] OF M128A;
            Legacy: ARRAY [0..7] OF M128A;
            Xmm0: M128A;
            Xmm1: M128A;
            Xmm2: M128A;
            Xmm3: M128A;
            Xmm4: M128A;
            Xmm5: M128A;
            Xmm6: M128A;
            Xmm7: M128A;
            Xmm8: M128A;
            Xmm9: M128A;
            Xmm10: M128A;
            Xmm11: M128A;
            Xmm12: M128A;
            Xmm13: M128A;
            Xmm14: M128A;
            Xmm15: M128A;
            Reserved: ARRAY [0..95] OF uint8_t;
    (*   } DUMMYSTRUCTNAME;
     * } DUMMYUNIONNAME;
     *
     * Vector registers.
     *)
    VectorRegister: ARRAY [0..25] OF M128A;
    VectorControl: INTEGER;

    (* Special debug control registers. *)
    DebugControl:         INTEGER;
    LastBranchToRip:      INTEGER;
    LastBranchFromRip:    INTEGER;
    LastExceptionToRip:   INTEGER;
    LastExceptionFromRip: INTEGER;
  END;

TYPE PCONTEXT = UNTRACED REF CONTEXT;

END ThreadContext.
