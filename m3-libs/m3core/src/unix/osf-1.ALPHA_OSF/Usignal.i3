(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usignal;

FROM Ctypes IMPORT int, long, unsigned_long;
FROM Utypes IMPORT pid_t, size_t;

(* This first part is a direct copy of ../Common/Usignal.i3
 * It might be nice to not duplicate it.
 *)

(*CONST*)
<*EXTERNAL "Usignal__NSIG"*> VAR NSIG: int;
<*EXTERNAL "Usignal__SA_NOCLDSTOP"*> VAR SA_NOCLDSTOP: int;
<*EXTERNAL "Usignal__SA_RESETHAND"*> VAR SA_RESETHAND: int;
<*EXTERNAL "Usignal__SA_RESTART"*> VAR SA_RESTART: int;
<*EXTERNAL "Usignal__SIGABRT"*> VAR SIGABRT: int;
<*EXTERNAL "Usignal__SIGALRM"*> VAR SIGALRM: int;
<*EXTERNAL "Usignal__SIGBUS"*> VAR SIGBUS: int;
<*EXTERNAL "Usignal__SIGCANCEL"*> VAR SIGCANCEL: int;
<*EXTERNAL "Usignal__SIGCHLD"*> VAR SIGCHLD: int;
<*EXTERNAL "Usignal__SIGCLD"*> VAR SIGCLD: int;
<*EXTERNAL "Usignal__SIGCONT"*> VAR SIGCONT: int;
<*EXTERNAL "Usignal__SIGEXCEPT"*> VAR SIGEXCEPT: int;
<*EXTERNAL "Usignal__SIGFPE"*> VAR SIGFPE: int;
<*EXTERNAL "Usignal__SIGHUP"*>  VAR SIGHUP:  int;
<*EXTERNAL "Usignal__SIGILL"*> VAR SIGILL: int;
<*EXTERNAL "Usignal__SIGINT"*> VAR SIGINT: int;
<*EXTERNAL "Usignal__SIGIO"*> VAR SIGIO: int;
<*EXTERNAL "Usignal__SIGIOT"*> VAR SIGIOT: int;
<*EXTERNAL "Usignal__SIGKILL"*> VAR SIGKILL: int;
<*EXTERNAL "Usignal__SIGPIPE"*> VAR SIGPIPE: int;
<*EXTERNAL "Usignal__SIGPOLL"*> VAR SIGPOLL: int;
<*EXTERNAL "Usignal__SIGPROF"*> VAR SIGPROF: int;
<*EXTERNAL "Usignal__SIGQUIT"*> VAR SIGQUIT: int;
<*EXTERNAL "Usignal__SIGSEGV"*> VAR SIGSEGV: int;
<*EXTERNAL "Usignal__SIGSTOP"*> VAR SIGSTOP: int;
<*EXTERNAL "Usignal__SIGSYS"*> VAR SIGSYS: int;
<*EXTERNAL "Usignal__SIGTERM"*> VAR SIGTERM: int;
<*EXTERNAL "Usignal__SIGTRAP"*> VAR SIGTRAP: int;
<*EXTERNAL "Usignal__SIGTSTP"*> VAR SIGTSTP: int;
<*EXTERNAL "Usignal__SIGTTIN"*> VAR SIGTTIN: int;
<*EXTERNAL "Usignal__SIGTTOU"*> VAR SIGTTOU: int;
<*EXTERNAL "Usignal__SIGURG"*> VAR SIGURG: int;
<*EXTERNAL "Usignal__SIGUSR1"*> VAR SIGUSR1: int;
<*EXTERNAL "Usignal__SIGUSR2"*> VAR SIGUSR2: int;
<*EXTERNAL "Usignal__SIGVTALRM"*> VAR SIGVTALRM: int;
<*EXTERNAL "Usignal__SIGWINCH"*> VAR SIGWINCH: int;
<*EXTERNAL "Usignal__SIGXCPU"*> VAR SIGXCPU: int;
<*EXTERNAL "Usignal__SIGXFSZ"*> VAR SIGXFSZ: int;
<*EXTERNAL "Usignal__SIG_BLOCK"*> VAR SIG_BLOCK: int;
<*EXTERNAL "Usignal__SIG_SETMASK"*> VAR SIG_SETMASK: int;
<*EXTERNAL "Usignal__SIG_UNBLOCK"*> VAR SIG_UNBLOCK: int;

TYPE
  SignalHandler = PROCEDURE (sig: int);
  SignalActionHandler = PROCEDURE (sig: int; sip: siginfo_t_star; uap: ADDRESS (* Uucontext.ucontext_t_star *) );
  siginfo_t_star = ADDRESS;

<*EXTERNAL "Usignal__kill"*>PROCEDURE kill (pid: pid_t; sig: int): int;

 (* This part is unique to ALPHA_OSF.
  * Do we need it in cloned in Modula-3?
  *)
TYPE
  struct_sigcontext = RECORD
    sc_onstack   : long;		 (* sigstack state to restore *)
    sc_mask      : long;		 (* signal mask to restore *)
    sc_pc        : long;		 (* pc at time of signal *)
    sc_ps        : long;		 (* psl to restore *)
    sc_regs      : ARRAY [0..31] OF long; (* processor regs 0 to 31 *)
    sc_ownedfp   : long;		 (* fp has been used *)
    sc_fpregs    : ARRAY [0..31] OF long; (* fp regs 0 to 31 *)
    sc_fpcr      : unsigned_long;	 (* floating point control register *)
    sc_fp_control: unsigned_long;	 (* software fpcr *)
    sc_reserved1 : long;		 (* reserved for kernel *)
    sc_kreserved1: int;			 (* reserved for kernel *)
    sc_kreserved2: int;			 (* reserved for kernel *)
    sc_ssize     : size_t;		 (* stack size *)
    sc_sbase     : ADDRESS;		 (* stack start *)
    sc_traparg_a0: unsigned_long;	 (* a0 argument to trap on exception *)
    sc_traparg_a1: unsigned_long;	 (* a1 argument to trap on exception *)
    sc_traparg_a2: unsigned_long;	 (* a2 argument to trap on exception *)
    sc_fp_trap_pc: unsigned_long;	 (* imprecise pc *)
    sc_fp_trigger_sum: unsigned_long;	 (* Exception summary at trigger pc *)
    sc_fp_trigger_inst: unsigned_long;	 (* Instruction at trigger pc *)
  END;

END Usignal.
