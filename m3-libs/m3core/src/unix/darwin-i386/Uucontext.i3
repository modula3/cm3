(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)

INTERFACE Uucontext;

FROM Ctypes IMPORT int, char, unsigned_int, void_star;
FROM Utypes IMPORT u_int8_t, u_int16_t, u_int32_t, size_t;

(*** <mach/i386/thread_status.h> ***)

(*
 *	i386_thread_state	this is the structure that is exported
 *				to user threads for use in status/mutate
 *				calls.  This structure should never
 *				change.
 *
 *	i386_float_state	exported to use threads for access to 
 *				floating point registers. Try not to 
 *				change this one, either.
 *
 *)

(*     THREAD_STATE_FLAVOR_LIST 0 *)
CONST
  i386_THREAD_STATE    = 1;
  i386_FLOAT_STATE     = 2;
  i386_EXCEPTION_STATE = 3;
  THREAD_STATE_NONE    = 4;

(*
 * VALID_THREAD_STATE_FLAVOR is a platform specific macro that when passed
 * an exception flavor will return if that is a defined flavor for that
 * platform. The macro must be manually updated to include all of the valid
 * exception flavors as defined above.
 *)
(*
#define VALID_THREAD_STATE_FLAVOR(x)        \
         ((x == i386_THREAD_STATE)           || \
         (x == i386_FLOAT_STATE)             || \
         (x == i386_EXCEPTION_STATE)             || \
         (x == THREAD_STATE_NONE))
*)

(*
 * Main thread state consists of
 * general registers, segment registers,
 * eip and eflags.
 *)
TYPE
  i386_thread_state_t = RECORD
    eax: unsigned_int;
    ebx: unsigned_int;
    ecx: unsigned_int;
    edx: unsigned_int;
    edi: unsigned_int;
    esi: unsigned_int;
    ebp: unsigned_int;
    esp: unsigned_int;
    ss: unsigned_int;
    eflags: unsigned_int;
    eip: unsigned_int;
    cs: unsigned_int;
    ds: unsigned_int;
    es: unsigned_int;
    fs: unsigned_int;
    gs: unsigned_int;
  END;

CONST
  i386_THREAD_STATE_COUNT = BYTESIZE(i386_thread_state_t) DIV BYTESIZE(int);

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
  i386_float_state_t = RECORD
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

CONST
  i386_FLOAT_STATE_COUNT =
    BYTESIZE(i386_float_state_t) DIV BYTESIZE(unsigned_int);

(*
 * Extra state that may be
 * useful to exception handlers.
 *)
TYPE
  i386_exception_state_t = RECORD
    trapno: unsigned_int;
    err: unsigned_int;
    faultvaddr: unsigned_int;
  END;

CONST
  i386_EXCEPTION_STATE_COUNT =
    BYTESIZE(i386_exception_state_t) DIV BYTESIZE(unsigned_int);

(*
 * Machine-independent way for servers and Mach's exception mechanism to
 * choose the most efficient state flavor for exception RPC's:
 *)
CONST
  MACHINE_THREAD_STATE = i386_THREAD_STATE;
  MACHINE_THREAD_STATE_COUNT = i386_THREAD_STATE_COUNT;

(*** <i386/signal.h> ***)

(*
 * Information pushed on stack when a signal is delivered.
 * This is used by the kernel to restore state following
 * execution of the signal handler.  It is also made available
 * to the handler to allow it to properly restore state if
 * a non-standard exit is performed.
 *)
TYPE
  struct_sigcontext = RECORD
    sc_onstack: int;			 (* sigstack state to restore *)
    sc_mask:    int;			 (* signal mask to restore *)
    sc_eax:     unsigned_int;
    sc_ebx:     unsigned_int;
    sc_ecx:     unsigned_int;
    sc_edx:     unsigned_int;
    sc_edi:     unsigned_int;
    sc_esi:     unsigned_int;
    sc_ebp:     unsigned_int;
    sc_esp:     unsigned_int;
    sc_ss:      unsigned_int;
    sc_eflags:  unsigned_int;
    sc_eip:     unsigned_int;
    sc_cs:      unsigned_int;
    sc_ds:      unsigned_int;
    sc_es:      unsigned_int;
    sc_fs:      unsigned_int;
    sc_gs:      unsigned_int;
  END;

(*** <i386/ucontext.h> ***)

TYPE
  struct_mcontext = RECORD
    es: i386_exception_state_t;
    ss: i386_thread_state_t;
    fs: i386_float_state_t;
  END;
  mcontext_t = UNTRACED REF struct_mcontext;

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
  struct_ucontext = RECORD
    sc_onstack:  int;
    uc_sigmask:  sigset_t;       	 (* signal mask used by this context *)
    uc_stack:    stack_t;		 (* stack used by this context *)
    uc_link:     struct_ucontext_star;	 (* pointer to resuming context *)
    uc_mcsize:   size_t;		 (* size of the machine context passed in *)
    uc_mcontext: mcontext_t;		 (* pointer to machine specific context *)
  END;
  struct_ucontext_star = UNTRACED REF struct_ucontext;
  ucontext_t = struct_ucontext;
  ucontext_t_star = UNTRACED REF ucontext_t;

END Uucontext.
