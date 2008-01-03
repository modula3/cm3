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

(* <asm/ptrace.h> *)

(*                                                                                             
 * This struct defines the way the registers are stored on the                                 
 * kernel stack during a system call or other kernel entry.                                    
 *                                                                                             
 * this should only contain volatile regs                                                      
 * since we can keep non-volatile in the thread_struct                                         
 * should set this up when only volatiles are saved                                            
 * by intr code.                                                                               
 *                                                                                             
 * Since this is going on the stack, *CARE MUST BE TAKEN* to insure                            
 * that the overall structure is a multiple of 16 bytes in length.                             
 *                                                                                             
 * Note that the offsets of the fields in this struct correspond with                          
 * the PT_* values below.  This simplifies arch/ppc/kernel/ptrace.c.                           
 *)

TYPE
  struct_pt_regs = RECORD
    gpr: ARRAY[0..31] OF unsigned_long;
    nip: unsigned_long;
    msr: unsigned_long;
    orig_gpr3: unsigned_long;        (* Used for restarting system calls *)
    ctr: unsigned_long;
    link: unsigned_long;
    xer: unsigned_long;
    ccr: unsigned_long;
    mq: unsigned_long;               (* 601 only (not used at present) *)
                                     (* Used on APUS to hold IPL value. *)
    trap: unsigned_long;             (* Reason for being here *)
    dar: unsigned_long;              (* Fault registers *)
    dsisr: unsigned_long;            (* used for ESR on 4xx/Book-E *)
    result: unsigned_long;           (* Result of a system call *)
  END;

(* <asm/sigcontext.h> *)

TYPE
  struct_sigcontext = RECORD
    unused: ARRAY[0..3] OF unsigned_long;
    signal: int;
    handler: unsigned_long;
    oldmask: unsigned_long;
    regs: UNTRACED REF struct_pt_regs;
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

(* Number of general registers.  *)
CONST NGREG = 48;

(* Container for all general registers.  *)
TYPE gregset_t = ARRAY[0..NGREG-1] OF unsigned_long;

(* Container for floating-point registers and status *)
TYPE
  struct_libc_fpstate = RECORD
    fpregs: ARRAY[0..31] OF double;
    fpscr: double;
    pad: ARRAY[0..1] OF unsigned_int;
  END;
  fpregset_t = struct_libc_fpstate;

(* Container for Altivec/VMX registers and status.
   Needs to be aligned on a 16-byte boundary. *)
TYPE
  struct_libc_vrstate = RECORD
    vrregs: ARRAY[0..32,0..4] OF unsigned_int;
    vrsave: unsigned_int;
    pad: ARRAY[0..1] OF unsigned_int;
    vscr: unsigned_int;
  END;
  vrregset_t = struct_libc_vrstate;

(* Context to describe whole processor state.  *)
TYPE
  mcontext_t = RECORD
    gregs: gregset_t;
    fpregs: fpregset_t;
    vrregs: vrregset_t;
  END;

(* Userlevel context.  *)
TYPE
  struct_ucontext = RECORD
    uc_flags: unsigned_long_int;
    uc_link: UNTRACED REF struct_ucontext;
    uc_stack: stack_t;
    (*
     * These fields are set up this way to maximize source and
     * binary compatibility with code written for the old
     * ucontext_t definition, which didn't include space for the
     * registers.
     *
     * Different versions of the kernel have stored the registers on
     * signal delivery at different offsets from the ucontext struct.
     * Programs should thus use the uc_mcontext.uc_regs pointer to
     * find where the registers are actually stored.  The registers
     * will be stored within the ucontext_t struct but not necessarily
     * at a fixed address.  As a side-effect, this lets us achieve
     * 16-byte alignment for the register storage space if the
     * Altivec registers are to be saved, without requiring 16-byte
     * alignment on the whole ucontext_t.
     *
     * The uc_mcontext.regs field is included for source compatibility
     * with programs written against the older ucontext_t definition,
     * and its name should therefore not change.  The uc_pad field
     * is for binary compatibility with programs compiled against the
     * old ucontext_t; it ensures that uc_mcontext.regs and uc_sigmask
     * are at the same offset as previously.
     *)
    uc_pad: ARRAY[0..6] OF int;
    uc_mcontext: UNTRACED REF mcontext_t;
    uc_sigmask: sigset_t;
    uc_reg_space: ARRAY[0..BYTESIZE(mcontext_t) + 12 - 1] OF char;  (* last for extensibility *)
  END;
  ucontext_t = struct_ucontext;
  ucontext_t_star = UNTRACED REF ucontext_t;

END Uucontext.
