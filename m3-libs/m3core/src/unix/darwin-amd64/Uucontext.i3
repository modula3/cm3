(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)

INTERFACE Uucontext;

FROM Ctypes IMPORT int, char, unsigned_int, void_star;
FROM Utypes IMPORT u_int8_t, u_int16_t, u_int32_t, u_int64_t, size_t;

(*** <mach/i386/thread_state.h> ***)

CONST
  I386_THREAD_STATE_MAX = 144;    (* Size of biggest state possible *)

  THREAD_STATE_MAX = I386_THREAD_STATE_MAX;

(*** <mach/thread_status.h> ***)

(*
 *	Generic definition for machine-dependent thread status.
 *)
TYPE
  natural_t = unsigned_int;
  thread_state_t = UNTRACED REF natural_t; (* Variable-length array *)

(* THREAD_STATE_MAX is now defined in <mach/machine/thread_state.h> *)
TYPE
  thread_state_data_t = ARRAY [0..THREAD_STATE_MAX-1] OF natural_t;

CONST
  THREAD_STATE_FLAVOR_LIST = 0;       (* List of valid flavors *)
  THREAD_STATE_FLAVOR_LIST_NEW = 128;

TYPE
  thread_state_flavor_t = int;
  thread_state_flavor_array_t = UNTRACED REF thread_state_flavor_t;

(*** <mach/i386/thread_status.h> ***)

(*
 * THREAD_STATE_FLAVOR_LIST 0
 * 	these are the supported flavors
 *)
CONST
  x86_THREAD_STATE32    = 1;
  x86_FLOAT_STATE32     = 2;
  x86_EXCEPTION_STATE32 = 3;
  x86_THREAD_STATE64    = 4;
  x86_FLOAT_STATE64     = 5;
  x86_EXCEPTION_STATE64 = 6;
  x86_THREAD_STATE      = 7;
  x86_FLOAT_STATE       = 8;
  x86_EXCEPTION_STATE   = 9;
  x86_DEBUG_STATE32     = 10;
  x86_DEBUG_STATE64     = 11;
  x86_DEBUG_STATE       = 12;
  THREAD_STATE_NONE     = 13;

(*
 * Largest state on this machine:
 * (be sure mach/machine/thread_state.h matches!)
 *)
CONST
  THREAD_MACHINE_STATE_MAX = THREAD_STATE_MAX;

TYPE
  struct_x86_state_hdr = RECORD
    flavor: 	int;
    count:	int;
  END;
  x86_state_hdr_t = struct_x86_state_hdr;

(*
 * Main thread state consists of
 * general registers, segment registers,
 * eip and eflags.
 *)

TYPE
  struct_x86_thread_state32 = RECORD
    eax:    unsigned_int;
    ebx:    unsigned_int;
    ecx:    unsigned_int;
    edx:    unsigned_int;
    edi:    unsigned_int;
    esi:    unsigned_int;
    ebp:    unsigned_int;
    esp:    unsigned_int;
    ss:     unsigned_int;
    eflags: unsigned_int;
    eip:    unsigned_int;
    cs:     unsigned_int;
    ds:     unsigned_int;
    es:     unsigned_int;
    fs:     unsigned_int;
    gs:     unsigned_int;
  END;
  x86_thread_state32_t = struct_x86_thread_state32;

  struct_x86_thread_state64 = RECORD
    rax:	u_int64_t;
    rbx:	u_int64_t;
    rcx:	u_int64_t;
    rdx:	u_int64_t;
    rdi:	u_int64_t;
    rsi:	u_int64_t;
    rbp:	u_int64_t;
    rsp:	u_int64_t;
    r8:		u_int64_t;
    r9:		u_int64_t;
    r10:	u_int64_t;
    r11:	u_int64_t;
    r12:	u_int64_t;
    r13:	u_int64_t;
    r14:	u_int64_t;
    r15:	u_int64_t;
    rip:	u_int64_t;
    rflags:	u_int64_t;
    cs:		u_int64_t;
    fs:		u_int64_t;
    gs:		u_int64_t;
  END;
  x86_thread_state64_t = struct_x86_thread_state64;

(*
 * Default segment register values.
 *)
CONST
  USER_CODE_SELECTOR = 16_0017;
  USER_DATA_SELECTOR = 16_001f;
  KERN_CODE_SELECTOR = 16_0008;
  KERN_DATA_SELECTOR = 16_0010;

TYPE
  fp_control_t = RECORD
    invalid: BITS 1 FOR [0..1];
    denorm:  BITS 1 FOR [0..1];
    zdiv:    BITS 1 FOR [0..1];
    ovrfl:   BITS 1 FOR [0..1];
    undfl:   BITS 1 FOR [0..1];
    precis:  BITS 1 FOR [0..1];
    unused1: BITS 2 FOR [0..3];
    pc:      BITS 2 FOR [0..3];
    rc:      BITS 2 FOR [0..3];
    inf:     BITS 1 FOR [0..1];
    unused2: BITS 3 FOR [0..7];
  END;

CONST
  FP_PREC_24B = 0;
  FP_PREC_53B = 2;
  FP_PREC_64B = 3;

  FP_RND_NEAR = 0;
  FP_RND_DOWN = 1;
  FP_RND_UP   = 2;
  FP_CHOP     = 3;

(*
 * Status word.
 *)
TYPE
  fp_status_t = RECORD
    invalid: BITS 1 FOR [0..1];
    denorm:  BITS 1 FOR [0..1];
    zdiv:    BITS 1 FOR [0..1];
    ovrfl:   BITS 1 FOR [0..1];
    undfl:   BITS 1 FOR [0..1];
    precis:  BITS 1 FOR [0..1];
    stkflt:  BITS 1 FOR [0..1];
    errsumm: BITS 1 FOR [0..1];
    c0:      BITS 1 FOR [0..1];
    c1:      BITS 1 FOR [0..1];
    c2:      BITS 1 FOR [0..1];
    tos:     BITS 3 FOR [0..7];
    c3:      BITS 1 FOR [0..1];
    busy:    BITS 1 FOR [0..1];
  END;

(* defn of 80bit x87 FPU or MMX register  *)
TYPE
  struct_mmst_reg = RECORD
    mmst_reg:  ARRAY [0..9] OF char;
    mmst_rsrv: ARRAY [0..5] OF char;
  END;

(* defn of 128 bit XMM regs *)
TYPE
  struct_xmm_reg = RECORD
    xmm_reg: ARRAY [0..15] OF char;
  END;

(*
 * Floating point state.
 *)
CONST
  FP_STATE_BYTES = 512;	      (* number of chars worth of data from fpu_fcw *)

(* For legacy reasons we need to leave the hw_state as char bytes *)
TYPE
  struct_x86_float_state32 = RECORD
    fpu_reserved: ARRAY [0..1] OF int;
    fpu_fcw: fp_control_t;		 (* x87 FPU control word *)
    fpu_fsw: fp_status_t;		 (* x87 FPU status word *)
    fpu_ftw: u_int8_t;			 (* x87 FPU tag word *)
    fpu_rsrv1: u_int8_t;		 (* reserved *)
    fpu_fop: u_int16_t;			 (* x87 FPU Opcode *)
    fpu_ip: u_int32_t;		      (* x87 FPU Instruction Pointer offset *)
    fpu_cs: u_int16_t;		    (* x87 FPU Instruction Pointer Selector *)
    fpu_rsrv2: u_int16_t;		 (* reserved *)
    fpu_dp: u_int32_t;	(* x87 FPU Instruction Operand(Data) Pointer offset *)
    fpu_ds: u_int16_t; (* x87 FPU Instruction Operand(Data) Pointer Selector *)
    fpu_rsrv3: u_int16_t;		 (* reserved *)
    fpu_mxcsr: u_int32_t;		 (* MXCSR Register state *)
    fpu_mxcsrmask: u_int32_t;		 (* MXCSR mask *)
    fpu_stmm0: struct_mmst_reg;		 (* ST0/MM0 *)
    fpu_stmm1: struct_mmst_reg;		 (* ST1/MM1 *)
    fpu_stmm2: struct_mmst_reg;		 (* ST2/MM2 *)
    fpu_stmm3: struct_mmst_reg;		 (* ST3/MM3 *)
    fpu_stmm4: struct_mmst_reg;		 (* ST4/MM4 *)
    fpu_stmm5: struct_mmst_reg;		 (* ST5/MM5 *)
    fpu_stmm6: struct_mmst_reg;		 (* ST6/MM6 *)
    fpu_stmm7: struct_mmst_reg;		 (* ST7/MM7 *)
    fpu_xmm0: struct_mmst_reg;		 (* XMM 0 *)
    fpu_xmm1: struct_mmst_reg;		 (* XMM 1 *)
    fpu_xmm2: struct_mmst_reg;		 (* XMM 2 *)
    fpu_xmm3: struct_mmst_reg;		 (* XMM 3 *)
    fpu_xmm4: struct_mmst_reg;		 (* XMM 4 *)
    fpu_xmm5: struct_mmst_reg;		 (* XMM 5 *)
    fpu_xmm6: struct_mmst_reg;		 (* XMM 6 *)
    fpu_xmm7: struct_mmst_reg;		 (* XMM 7 *)
    fpu_rsrv4: ARRAY [0..14*16-1] OF char; (* reserved *)
    fpu_reserved1: int;
  END;
  x86_float_state32_t = struct_x86_float_state32;

  struct_x86_float_state64 = RECORD
    fpu_reserved: ARRAY [0..1] OF int;
    fpu_fcw: fp_control_t;                     (* x87 FPU control word *)
    fpu_fsw: fp_status_t;                      (* x87 FPU status word *)
    fpu_ftw: u_int8_t;                         (* x87 FPU tag word *)
    fpu_rsrv1: u_int8_t;                       (* reserved *) 
    fpu_fop: u_int16_t;                        (* x87 FPU Opcode *)
    fpu_ip: u_int32_t; (* x87 FPU Instruction Pointer offset *)
    fpu_cs: u_int16_t; (* x87 FPU Instruction Pointer Selector *)
    fpu_rsrv2: u_int16_t;                      (* reserved *)
    fpu_dp: u_int32_t; (* x87 FPU Instruction Operand(Data) Pointer offset *)
    fpu_ds: u_int16_t; (* x87 FPU Instruction Operand(Data) Pointer Selector *)
    fpu_rsrv3: u_int16_t;                      (* reserved *)
    fpu_mxcsr: u_int32_t;                      (* MXCSR Register state *)
    fpu_mxcsrmask: u_int32_t;                  (* MXCSR mask *)
    fpu_stmm0: struct_mmst_reg;                (* ST0/MM0   *)
    fpu_stmm1: struct_mmst_reg;                (* ST1/MM1  *)
    fpu_stmm2: struct_mmst_reg;                (* ST2/MM2  *)
    fpu_stmm3: struct_mmst_reg;                (* ST3/MM3  *)
    fpu_stmm4: struct_mmst_reg;                (* ST4/MM4  *)
    fpu_stmm5: struct_mmst_reg;                (* ST5/MM5  *)
    fpu_stmm6: struct_mmst_reg;                (* ST6/MM6  *)
    fpu_stmm7: struct_mmst_reg;                (* ST7/MM7  *)
    fpu_xmm0: struct_xmm_reg;                  (* XMM 0  *)
    fpu_xmm1: struct_xmm_reg;                  (* XMM 1  *)
    fpu_xmm2: struct_xmm_reg;                  (* XMM 2  *)
    fpu_xmm3: struct_xmm_reg;                  (* XMM 3  *)
    fpu_xmm4: struct_xmm_reg;                  (* XMM 4  *)
    fpu_xmm5: struct_xmm_reg;                  (* XMM 5  *)
    fpu_xmm6: struct_xmm_reg;                  (* XMM 6  *)
    fpu_xmm7: struct_xmm_reg;                  (* XMM 7  *)
    fpu_xmm8: struct_xmm_reg;                  (* XMM 8  *)
    fpu_xmm9: struct_xmm_reg;                  (* XMM 9  *)
    fpu_xmm10: struct_xmm_reg;                 (* XMM 10  *)
    fpu_xmm11: struct_xmm_reg;                 (* XMM 11 *)
    fpu_xmm12: struct_xmm_reg;                 (* XMM 12  *)
    fpu_xmm13: struct_xmm_reg;                 (* XMM 13  *)
    fpu_xmm14: struct_xmm_reg;                 (* XMM 14  *)
    fpu_xmm15: struct_xmm_reg;                 (* XMM 15  *)
    fpu_rsrv4: ARRAY [0..6*16-1] OF char;      (* reserved *)
    fpu_reserved1: int;
  END;
  x86_float_state64_t = struct_x86_float_state64;

(*
 * Extra state that may be
 * useful to exception handlers.
 *)
TYPE
  struct_x86_exception_state32 = RECORD
    trapno: unsigned_int;
    err: unsigned_int;
    faultvaddr: unsigned_int;
  END;
  x86_exception_state32_t = struct_x86_exception_state32;

  struct_x86_debug_state32 = RECORD
    dr0: unsigned_int;
    dr1: unsigned_int;
    dr2: unsigned_int;
    dr3: unsigned_int;
    dr4: unsigned_int;
    dr5: unsigned_int;
    dr6: unsigned_int;
    dr7: unsigned_int;
  END;
  x86_debug_state32_t = struct_x86_debug_state32;

  struct_x86_exception_state64 = RECORD
    trapno: unsigned_int;
    err: unsigned_int;
    faultvaddr: u_int64_t;
  END;
  x86_exception_state64_t = struct_x86_exception_state64;

  struct_x86_debug_state64 = RECORD
    dr0: u_int64_t;
    dr1: u_int64_t;
    dr2: u_int64_t;
    dr3: u_int64_t;
    dr4: u_int64_t;
    dr5: u_int64_t;
    dr6: u_int64_t;
    dr7: u_int64_t;
  END;
  x86_debug_state64_t = struct_x86_debug_state64;

(*** <i386/ucontext.h> ***)

TYPE
  struct_mcontext32 = RECORD
    es: x86_exception_state32_t;
    ss: x86_thread_state32_t;
    fs: x86_float_state32_t;
  END;
  struct_mcontext32_star = UNTRACED REF struct_mcontext32;

  struct_mcontext64 = RECORD
    es: x86_exception_state64_t;
    ss: x86_thread_state64_t;
    fs: x86_float_state64_t;
  END;
  struct_mcontext64_star = UNTRACED REF struct_mcontext64;

(*** <sys/signal.h> ***)

TYPE
  (* Structure used in sigaltstack call. *)
  struct_sigaltstack = RECORD
    ss_sp:    void_star;   (* signal stack base *)
    ss_size:  size_t;	   (* signal stack length *)
    ss_flags: int;	   (* SA_DISABLE and/or SA_ONSTACK *)
  END;
  stack_t = struct_sigaltstack;

(*** <sys/ucontext.h> ***)

TYPE
  sigset_t = u_int32_t;
  struct_ucontext32 = RECORD
    uc_onstack:  int;
    uc_sigmask:  sigset_t;       	 (* signal mask used by this context *)
    uc_stack:    stack_t;		 (* stack used by this context *)
    uc_link:     struct_ucontext32_star; (* pointer to resuming context *)
    uc_mcsize:   size_t;		 (* size of the machine context passed in *)
    uc_mcontext: struct_mcontext32_star; (* pointer to machine specific context *)
  END;
  struct_ucontext32_star = UNTRACED REF struct_ucontext32;

  struct_ucontext64 = RECORD
    uc_onstack:  int;
    uc_sigmask:  sigset_t;       	 (* signal mask used by this context *)
    uc_stack:    stack_t;		 (* stack used by this context *)
    uc_link:     struct_ucontext64_star; (* pointer to resuming context *)
    uc_mcsize:   size_t;		 (* size of the machine context passed in *)
    uc_mcontext: struct_mcontext64_star; (* pointer to machine specific context *)
  END;
  struct_ucontext64_star = UNTRACED REF struct_ucontext64;

  ucontext_t_star = struct_ucontext64_star;
END Uucontext.
