(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Usignal;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT pid_t;
IMPORT Usysdep;

CONST
  SIGHUP = Usysdep.SIGHUP;
  SIGINT = Usysdep.SIGINT;
  SIGQUIT = Usysdep.SIGQUIT;
  SIGABRT = Usysdep.SIGABRT;
  SIGKILL = Usysdep.SIGKILL;
  SIGSEGV = Usysdep.SIGSEGV;
  SIGPIPE = Usysdep.SIGPIPE;
  SIGTERM = Usysdep.SIGTERM;
  NSIG = Usysdep.NSIG;
  SA_RESTART = Usysdep.SA_RESTART;
  SA_SIGINFO = Usysdep.SA_SIGINFO;

TYPE
  SignalActionHandler = Usysdep.SignalActionHandler;
  SignalHandler = Usysdep.SignalHandler;
  sigset_t = Usysdep.sigset_t;
  siginfo_t_star = Usysdep.siginfo_t_star;
  sa_sigaction = Usysdep.sa_sigaction;

  struct_sigaction = Usysdep.struct_sigaction;

<*EXTERNAL*> PROCEDURE kill (pid: pid_t; sig: int): int;
<*EXTERNAL*> PROCEDURE sigemptyset (VAR set: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigprocmask (how: int; READONLY set: sigset_t; VAR oset: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigsuspend (READONLY set: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigaction (sig: int; READONLY act: struct_sigaction; VAR oact: struct_sigaction): int;
<*EXTERNAL*> PROCEDURE sigpending (VAR set: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigwait (READONLY set: sigset_t; VAR sig: int): int;
<*EXTERNAL*> PROCEDURE sigdelset (VAR set: sigset_t; signo: int): int;
<*EXTERNAL*> PROCEDURE sigfillset (VAR set: sigset_t): int;

END Usignal.
