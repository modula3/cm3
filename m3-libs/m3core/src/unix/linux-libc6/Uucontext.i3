(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)

INTERFACE Uucontext;

FROM Ctypes
IMPORT int, unsigned_long, unsigned_long_int, unsigned_short, void_star,
       unsigned_short_int;
FROM Utypes IMPORT size_t;

(* <bits/sigset.h> *)

(* A `sigset_t' has a bit for each signal.  *)
CONST
  SIGSET_NWORDS = 1024 DIV (8 * BYTESIZE(unsigned_long_int));

TYPE
  sigset_t = RECORD
    val: ARRAY[0..SIGSET_NWORDS-1] OF unsigned_long_int;
  END;

(* <asm/sigcontext.h> *)

(* As documented in the iBCS2 standard..
 *
 * The first part of "struct _fpstate" is just the normal i387
 * hardware setup, the extra "status" word is used to save the
 * coprocessor status word before entering the handler.
 *
 * Pentium III FXSR, SSE support
 *      Gareth Hughes <gareth@valinux.com>, May 2000
 *
 * The FPU state data structure has had to grow to accomodate the
 * extended FPU state required by the Streaming SIMD Extensions.
 * There is no documented standard to accomplish this at the moment.
 *)
TYPE
  struct_fpreg = RECORD
    significand: ARRAY[0..3] OF unsigned_short;
    exponent: unsigned_short;
  END;

  struct_fpxreg = RECORD
    significand: ARRAY[0..4] OF unsigned_short;
    exponent: unsigned_short;
    padding: ARRAY[0..2] OF unsigned_short;
  END;

  struct_xmmreg = RECORD
    element: ARRAY[0..3] OF unsigned_long;
  END;

  struct_fpstate = RECORD
    (* Regular FPU environment *)
    cw: unsigned_long;
    sw: unsigned_long;
    tag: unsigned_long;
    ipoff: unsigned_long;
    cssel: unsigned_long;
    dataoff: unsigned_long;
    datasel: unsigned_long;
    st: ARRAY[0..7] OF struct_fpreg;
    status: unsigned_short;
    magic: unsigned_short;		 (* 0xffff = regular FPU data only *)

    (* FXSR FPU environment *)
    fxsr_env: ARRAY[0..5] OF unsigned_long; (* FXSR FPU env is ignored *)
    mxcsr: unsigned_long;
    reserved: unsigned_long;
    fxsr_st: ARRAY[0..7] OF struct_fpxreg; (* FXSR FPU reg data is ignored *)
    xmm: ARRAY[0..7] OF struct_xmmreg;
    padding: ARRAY[0..55] OF unsigned_long;
  END;

CONST X86_FXSR_MAGIC = 16_0000;

TYPE
  struct_sigcontext = RECORD
    gs, gsh: unsigned_short;
    fs, fsh: unsigned_short;
    es, esh: unsigned_short;
    ds, dsh: unsigned_short;
    edi: unsigned_long;
    esi: unsigned_long;
    ebp: unsigned_long;
    esp: unsigned_long;
    ebx: unsigned_long;
    edx: unsigned_long;
    ecx: unsigned_long;
    eax: unsigned_long;
    trapno: unsigned_long;
    err: unsigned_long;
    eip: unsigned_long;
    cs, csh: unsigned_short;
    eflags: unsigned_long;
    esp_at_signal: unsigned_long;
    ss, ssh: unsigned_short;
    fp_state: UNTRACED REF struct_fpstate;
    oldmask: unsigned_long;
    cr2: unsigned_long;
  END;

(* <bits/sigstack.h> *)
TYPE
  struct_sigaltstack = RECORD
    ss_sp: void_star;
    ss_flags: int;
    ss_size: size_t;
  END;
  stack_t = struct_sigaltstack;

(* <sys/ucontext.h> *)

(* Type for general register.  *)
TYPE greg_t = int;

(* Number of general registers.  *)
CONST NGREG = 19;

(* Container for all general registers.  *)
TYPE gregset_t = ARRAY[0..NGREG-1] OF greg_t;

(* Number of each register in the `gregset_t' array.  *)
CONST
  REG_GS     =  0;
  REG_FS     =  1;
  REG_ES     =  2;
  REG_DS     =  3;
  REG_EDI    =  4;
  REG_ESI    =  5;
  REG_EBP    =  6;
  REG_ESP    =  7;
  REG_EBX    =  8;
  REG_EDX    =  9;
  REG_ECX    = 10;
  REG_EAX    = 11;
  REG_TRAPNO = 12;
  REG_ERR    = 13;
  REG_EIP    = 14;
  REG_CS     = 15;
  REG_EFL    = 16;
  REG_UESP   = 17;
  REG_SS     = 18;

(* Definitions taken from the kernel headers.  *)
TYPE
  struct_libc_fpreg = RECORD
    significand: ARRAY[0..3] OF unsigned_short_int;
    exponent: unsigned_short_int;
  END;

  struct_libc_fpstate = RECORD
    cw: unsigned_long_int;
    sw: unsigned_long_int;
    tag: unsigned_long_int;
    ipoff: unsigned_long_int;
    cssel: unsigned_long_int;
    dataoff: unsigned_long_int;
    datasel: unsigned_long_int;
    st: ARRAY[0..7] OF struct_libc_fpreg;
    status: unsigned_long_int;
  END;

(* Structure to describe FPU registers.  *)
TYPE
  fpregset_t = UNTRACED REF struct_libc_fpstate;

(* Context to describe whole processor state.  *)
TYPE
  mcontext_t = RECORD
    gregs: gregset_t;
    (* Due to Linux's history we have to use a pointer here.  The SysV/i386
       ABI requires a struct with the values.  *)
    fpregs: fpregset_t;
    oldmask: unsigned_long_int;
    cr2: unsigned_long_int;
  END;

(* Userlevel context.  *)
TYPE
  struct_ucontext = RECORD
    uc_flags: unsigned_long_int;
    uc_link: UNTRACED REF struct_ucontext;
    uc_stack: stack_t;
    uc_mcontext: mcontext_t;
    uc_sigmask: sigset_t;
    fpregs_mem: struct_libc_fpstate;
  END;
  ucontext_t = struct_ucontext;
  ucontext_t_star = UNTRACED REF ucontext_t;

END Uucontext.
