(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)

INTERFACE Uucontext;

FROM Ctypes IMPORT int, long, char_star, unsigned_int, unsigned_char, double;
FROM Utypes IMPORT u_long, caddr_t;

(* ucontext.h *)

TYPE
  sigset_t = RECORD 
    sigbits : ARRAY [0..3] OF u_long;  
  END;

  struct_sigaltstack = RECORD
    ss_sp      : char_star;
    ss_size    : int;
    ss_flags   : int;
  END;
  stack_t = struct_sigaltstack;

  greg_t = int;
  gregset_t = RECORD 
    psr        : greg_t;
    pc         : greg_t;
    npc        : greg_t;
    y          : greg_t;
    g1         : greg_t;
    g2         : greg_t;
    g3         : greg_t;
    g4         : greg_t;
    g5         : greg_t;
    g6         : greg_t;
    g7         : greg_t;
    o0         : greg_t;
    o1         : greg_t;
    o2         : greg_t;
    o3         : greg_t;
    o4         : greg_t;
    o5         : greg_t;
    (*o6*) sp  : greg_t;
    o7         : greg_t;
  END;

  fpregset_t = RECORD
    fpu_regs   : ARRAY[0..15] OF double; (* 16 doubles *)
    fpu_q      : ADDRESS;		 (* ptr to array of FQ entries *)
    fpu_fsr    : unsigned_int;		 (* FPU status register *)
    fpu_qcnt   : unsigned_char;		 (* # of entries in saved FQ *)
    fpu_q_entrysize: unsigned_char;	 (* # of bytes per FQ entry *)
    fpu_en     : unsigned_char;		 (* flag signifying FPU in use *)
  END;

  xrs_t = RECORD
    xrs_id     : unsigned_int;		 (* indicates xrs_ptr validity *)
    xrs_ptr    : caddr_t;		 (* ptr to extra reg state *)
  END;

  mcontext_t = RECORD
    gregs      : gregset_t;
    gwins      : ADDRESS;		 (* POSSIBLE ptr to reg windows *)
    fpregs     : fpregset_t;		 (* floating point register set *)
    xrs        : xrs_t;			 (* POSSIBLE extra reg state assoc *)
    filler     : ARRAY[1..19] OF long;
  END;

  struct_ucontext = RECORD
    uc_flags   : u_long;
    uc_link    : UNTRACED REF struct_ucontext;
    uc_sigmask : sigset_t;
    uc_stack   : stack_t;
    uc_mcontext: mcontext_t;
    uc_filler  : ARRAY [1..23] OF long;
  END;
  ucontext_t = struct_ucontext;
  ucontext_t_star = UNTRACED REF ucontext_t;

<*EXTERNAL*>
PROCEDURE swapcontext(VAR oucp, ucp: ucontext_t): int;

<*EXTERNAL*>
PROCEDURE getcontext(VAR ucp: ucontext_t): int;

<*EXTERNAL*>
PROCEDURE setcontext(VAR ucp: ucontext_t): int;

END Uucontext.
