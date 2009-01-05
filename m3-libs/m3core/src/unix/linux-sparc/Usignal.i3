(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

<*EXTERNAL*> INTERFACE Usignal;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT pid_t;
IMPORT Uucontext;

CONST
  SIGINT = 2;
  SIGQUIT = 3;
  SIGABRT = 6;
  SIGKILL = 9;
  SIGTERM = 15;

  NSIG = 65; (* 129 on MIPS *)

  (* flags in struct_sigaction *)
  SA_RESTART = 2;      (* Restart syscall on signal return. *)
  SA_SIGINFO = 16_200; (* three parameter signal callback instead of one parameter *)

TYPE
  SignalActionHandler = PROCEDURE (sig: int; sip: siginfo_t_star; uap: Uucontext.ucontext_t_star);
  SignalHandler = PROCEDURE (sig: int);
  sigset_t = Uucontext.sigset_t;
  siginfo_t_star = ADDRESS;

  struct_sigaction = RECORD
    sa_sigaction: SignalActionHandler; (* union of two function pointers *)
    sa_mask     : sigset_t;
    sa_flags    : int;
    sa_restorer : ADDRESS;
  END;

PROCEDURE kill (pid: pid_t; sig: int): int;

PROCEDURE sigaction (sig: int; READONLY act: struct_sigaction; VAR oact: struct_sigaction): int;
PROCEDURE sigdelset (VAR set: sigset_t; signo: int): int;
PROCEDURE sigfillset (VAR set: sigset_t): int;
PROCEDURE sigpending (VAR set: sigset_t): int;
PROCEDURE sigprocmask (how: int; READONLY set: sigset_t; VAR oset: sigset_t): int;
PROCEDURE sigsuspend (READONLY set: sigset_t): int;
PROCEDURE sigwait (READONLY set: sigset_t; VAR sig: int): int;

END Usignal.
