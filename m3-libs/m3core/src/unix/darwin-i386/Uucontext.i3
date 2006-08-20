(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)

INTERFACE Uucontext;

FROM Ctypes IMPORT int, long, char_star, unsigned_int, unsigned_char, double;
FROM Utypes IMPORT u_long, caddr_t;
FROM Usigcontext IMPORT struct_sigcontext;

(*** <i386/signal.h> ***)

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
    sc: struct_sigcontext;
  END;
  mcontext_t = UNTRACED REF struct_mcontext;

(*** <sys/signal.h> ***)

TYPE
  (* Structure used in sigaltstack call. *)
  struct_sigaltstack = RECORD
    ss_sp:    char_star;   (* signal stack base *)
    ss_size:  int;	   (* signal stack length *)
    ss_flags: int;	   (* SA_DISABLE and/or SA_ONSTACK *)
  END;
  stack_t = struct_sigaltstack;

(*** <sys/ucontext.h> ***)

TYPE
  struct_ucontext = RECORD
    sc_onstack:  int;
    uc_sigmask:  sigset_t;       	 (* signal mask used by this context *)
    uc_stack:    stack_t;		 (* stack used by this context *)
    uc_link:     struct_ucontext_star;	 (* pointer to resuming context *)
    uc_mcsize:   size_t;		 (* size of the machine context passed in *)
    uc_mcontext: mcontext_t;		 (* machine specific context *)
  END;
  struct_ucontext_star = UNTRACED REF struct_ucontext;
  ucontext_t = struct_ucontext;


END Uucontext.
