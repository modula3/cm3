(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Usignal;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT pid_t;
IMPORT Uucontext;

CONST
  SIGHUP = 1;
  SIGINT = 2;
  SIGQUIT = 3;
  SIGABRT = 6;
  SIGKILL = 9;
  SIGSEGV = 11;
  SIGPIPE = 13;
  SIGTERM = 15;
  NSIG = 32;

  SA_RESTART = 2;
  SA_SIGINFO = 16_0040;

TYPE
  SignalActionHandler = PROCEDURE (sig: int; sip: siginfo_t_star; uap: Uucontext.ucontext_t_star);
  SignalHandler = PROCEDURE (sig: int);
  sigset_t = INTEGER;
  siginfo_t_star = ADDRESS;
  sa_sigaction = ADDRESS;

  struct_sigaction = RECORD
    sa_sigaction: SignalActionHandler; (* union of two function pointers *)
    sa_mask     : sigset_t;
    sa_flags    : int;
  END;

<*EXTERNAL*> PROCEDURE kill (pid: pid_t; sig: int): int;
<*EXTERNAL*> PROCEDURE sigprocmask (how: int; READONLY set: sigset_t; VAR oset: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigsuspend (READONLY set: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigaction (sig: int; READONLY act: struct_sigaction; VAR oact: struct_sigaction): int;
<*EXTERNAL*> PROCEDURE sigpending (VAR set: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigwait (READONLY set: sigset_t; VAR sig: int): int;
<*EXTERNAL*> PROCEDURE sigdelset (VAR set: sigset_t; signo: int): int;
<*EXTERNAL*> PROCEDURE sigemptyset (VAR set: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigfillset (VAR set: sigset_t): int;

END Usignal.
