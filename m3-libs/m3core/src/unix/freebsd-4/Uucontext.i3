(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)

INTERFACE Uucontext;

FROM Ctypes IMPORT int, char_star, unsigned_long, unsigned_char;

(* ucontext.h *)

TYPE
  sigset_t = RECORD 
    sigbits : ARRAY [0..3] OF unsigned_long;  
  END;

  struct_sigaltstack = RECORD
    ss_sp      : char_star;
    ss_size    : int;
    ss_flags   : int;
  END;
  stack_t = struct_sigaltstack;

  greg_t = int;
  gregset_t = RECORD 
    r_fs     : greg_t;
    r_es     : greg_t;
    r_ds     : greg_t;
    r_edi    : greg_t;
    r_esi    : greg_t;
    r_ebp    : greg_t;
    r_isp    : greg_t;
    r_ebx    : greg_t;
    r_edx    : greg_t;
    r_ecx    : greg_t;
    r_eax    : greg_t;
    r_trapno : greg_t;
    r_err    : greg_t;
    r_eip    : greg_t;
    r_cs     : greg_t;
    r_eflags : greg_t;
    r_esp    : greg_t;
    r_ss     : greg_t;
    r_gs     : greg_t;
  END;

  fpregset_t = RECORD
    fpr_env    : ARRAY [0..6] OF unsigned_long;
    fpr_acc    : ARRAY [0..7] OF ARRAY [0..9] OF unsigned_char;
    fpr_ex_sw  : unsigned_long;
    fpr_pad    : ARRAY [0..63] OF unsigned_char;
  END;

  mcontext_t = RECORD
    mc_onstack : int;
    mc_gs      : int;
    mc_fs      : int;
    mc_es      : int;
    mc_ds      : int;
    mc_edi     : int;
    mc_esi     : int;
    mc_ebp     : int;
    mc_isp     : int;
    mc_ebx     : int;
    mc_edx     : int;
    mc_ecx     : int;
    mc_eax     : int;
    mc_trapno  : int;
    mc_err     : int;
    mc_eip     : int;
    mc_cs      : int;
    mc_eflags  : int;
    mc_esp     : int;
    mc_ss      : int;
    mc_len     : int;
    mc_fpformat: int;
    mc_ownedfp : int;
    mc_spare1  : int;
    mc_fpstate : ARRAY [0..127] OF int;
    mc_spare2  : ARRAY [0..7] OF int;
  END;

  struct_ucontext = RECORD
    uc_sigmask : sigset_t;
    uc_mcontext: mcontext_t;
    uc_link    : UNTRACED REF struct_ucontext;
    uc_stack   : stack_t;
    uc_flags   : int;
    uc_filler  : ARRAY [0..3] OF int;
  END;
  ucontext_t = struct_ucontext;
  ucontext_t_star = UNTRACED REF ucontext_t;

<*EXTERNAL*>
PROCEDURE swapcontext(VAR oucp: ucontext_t; READONLY ucp: ucontext_t): int;

<*EXTERNAL*>
PROCEDURE getcontext(VAR ucp: ucontext_t): int;

<*EXTERNAL*>
PROCEDURE setcontext(READONLY ucp: ucontext_t): int;

END Uucontext.
