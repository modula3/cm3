(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Usignal;

FROM Ctypes IMPORT const_int, int;
FROM Utypes IMPORT pid_t;

<*EXTERNAL "Usignal__NSIG"*> VAR NSIG: const_int;
<*EXTERNAL "Usignal__SA_NOCLDSTOP"*> VAR SA_NOCLDSTOP: const_int;
<*EXTERNAL "Usignal__SA_RESETHAND"*> VAR SA_RESETHAND: const_int;
<*EXTERNAL "Usignal__SA_RESTART"*> VAR SA_RESTART: const_int;
<*EXTERNAL "Usignal__SIGABRT"*> VAR SIGABRT: const_int;
<*EXTERNAL "Usignal__SIGALRM"*> VAR SIGALRM: const_int;
<*EXTERNAL "Usignal__SIGBUS"*> VAR SIGBUS: const_int;
<*EXTERNAL "Usignal__SIGCANCEL"*> VAR SIGCANCEL: const_int;
<*EXTERNAL "Usignal__SIGCHLD"*> VAR SIGCHLD: const_int;
<*EXTERNAL "Usignal__SIGCLD"*> VAR SIGCLD: const_int;
<*EXTERNAL "Usignal__SIGCONT"*> VAR SIGCONT: const_int;
<*EXTERNAL "Usignal__SIGEXCEPT"*> VAR SIGEXCEPT: const_int;
<*EXTERNAL "Usignal__SIGFPE"*> VAR SIGFPE: const_int;
<*EXTERNAL "Usignal__SIGHUP"*>  VAR SIGHUP:  const_int;
<*EXTERNAL "Usignal__SIGILL"*> VAR SIGILL: const_int;
<*EXTERNAL "Usignal__SIGINT"*> VAR SIGINT: const_int;
<*EXTERNAL "Usignal__SIGIO"*> VAR SIGIO: const_int;
<*EXTERNAL "Usignal__SIGIOT"*> VAR SIGIOT: const_int;
<*EXTERNAL "Usignal__SIGKILL"*> VAR SIGKILL: const_int;
<*EXTERNAL "Usignal__SIGPIPE"*> VAR SIGPIPE: const_int;
<*EXTERNAL "Usignal__SIGPOLL"*> VAR SIGPOLL: const_int;
<*EXTERNAL "Usignal__SIGPROF"*> VAR SIGPROF: const_int;
<*EXTERNAL "Usignal__SIGQUIT"*> VAR SIGQUIT: const_int;
<*EXTERNAL "Usignal__SIGSEGV"*> VAR SIGSEGV: const_int;
<*EXTERNAL "Usignal__SIGSTOP"*> VAR SIGSTOP: const_int;
<*EXTERNAL "Usignal__SIGSYS"*> VAR SIGSYS: const_int;
<*EXTERNAL "Usignal__SIGTERM"*> VAR SIGTERM: const_int;
<*EXTERNAL "Usignal__SIGTRAP"*> VAR SIGTRAP: const_int;
<*EXTERNAL "Usignal__SIGTSTP"*> VAR SIGTSTP: const_int;
<*EXTERNAL "Usignal__SIGTTIN"*> VAR SIGTTIN: const_int;
<*EXTERNAL "Usignal__SIGTTOU"*> VAR SIGTTOU: const_int;
<*EXTERNAL "Usignal__SIGURG"*> VAR SIGURG: const_int;
<*EXTERNAL "Usignal__SIGUSR1"*> VAR SIGUSR1: const_int;
<*EXTERNAL "Usignal__SIGUSR2"*> VAR SIGUSR2: const_int;
<*EXTERNAL "Usignal__SIGVTALRM"*> VAR SIGVTALRM: const_int;
<*EXTERNAL "Usignal__SIGWINCH"*> VAR SIGWINCH: const_int;
<*EXTERNAL "Usignal__SIGXCPU"*> VAR SIGXCPU: const_int;
<*EXTERNAL "Usignal__SIGXFSZ"*> VAR SIGXFSZ: const_int;
<*EXTERNAL "Usignal__SIG_BLOCK"*> VAR SIG_BLOCK: const_int;
<*EXTERNAL "Usignal__SIG_SETMASK"*> VAR SIG_SETMASK: const_int;
<*EXTERNAL "Usignal__SIG_UNBLOCK"*> VAR SIG_UNBLOCK: const_int;

TYPE
  SignalHandler = PROCEDURE (sig: int);
  SignalActionHandler = PROCEDURE (sig: int; sip: siginfo_t_star; uap: ADDRESS (* Uucontext.ucontext_t_star *) );
  siginfo_t_star = ADDRESS;

<*EXTERNAL "Usignal__kill"*>PROCEDURE kill (pid: pid_t; sig: int): int;

END Usignal.
