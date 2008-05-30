(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* This file is not yet filled in. Let's factor commonality
and eliminate dead. Let's not add stuff for usermode threads. *)

INTERFACE Uucontext;

FROM Ctypes IMPORT int, unsigned_long;

CONST
  SIGSET_NWORDS = 1024 DIV (8 * BYTESIZE(unsigned_long));
  NGREG = 9999;
  PT_NIP = 9999;

TYPE
  sigset_t = RECORD
    val: ARRAY[0..SIGSET_NWORDS-1] OF unsigned_long;
  END;

  struct_pt_regs = RECORD
    undone: int;
  END;

  struct_sigcontext = RECORD
     undone: int;
  END;

  struct_sigaltstack = RECORD
     undone: int;
  END;
  stack_t = struct_sigaltstack;

  gregset_t = ARRAY[0..NGREG-1] OF unsigned_long;

  struct_libc_fpstate = RECORD
    undone: int;
  END;
  fpregset_t = struct_libc_fpstate;

  struct_libc_vrstate = RECORD
    undone: int;
  END;
  vrregset_t = struct_libc_vrstate;

  mcontext_t = RECORD
    undone: int;
  END;

  struct_ucontext = RECORD
    undone: int;
  END;
  ucontext_t = struct_ucontext;
  ucontext_t_star = UNTRACED REF ucontext_t;

END Uucontext.
