(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)

(* This file is not yet filled in. Let's factor commonality
and eliminate dead. Let's not add stuff for usermode threads. *)

INTERFACE Uucontext;

FROM Ctypes
IMPORT int, unsigned_long, unsigned_long_int;

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
    undone: int;
  END;

(* <asm/sigcontext.h> *)

TYPE
  struct_sigcontext = RECORD
     undone: int;
  END;

(* <bits/sigstack.h> *)
TYPE
  struct_sigaltstack = RECORD
     undone: int;
  END;
  stack_t = struct_sigaltstack;

(* <sys/ucontext.h> *)

(* Number of general registers.  *)
CONST NGREG = 9999;

(* Container for all general registers.  *)
TYPE gregset_t = ARRAY[0..NGREG-1] OF unsigned_long;

(* Container for floating-point registers and status *)
TYPE
  struct_libc_fpstate = RECORD
    undone: int;
  END;
  fpregset_t = struct_libc_fpstate;

(* Container for Altivec/VMX registers and status.
   Needs to be aligned on a 16-byte boundary. *)
TYPE
  struct_libc_vrstate = RECORD
    undone: int;
  END;
  vrregset_t = struct_libc_vrstate;

(* Context to describe whole processor state.  *)
TYPE
  mcontext_t = RECORD
    undone: int;
  END;

(* Userlevel context.  *)
TYPE
  struct_ucontext = RECORD
    undone: int;
  END;
  ucontext_t = struct_ucontext;
  ucontext_t_star = UNTRACED REF ucontext_t;

(* ptrace.h *)
CONST
    PT_NIP = 9999;

END Uucontext.
