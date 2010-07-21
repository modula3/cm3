(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Tue Nov  5 13:33:05 PST 1996 by heydon        *)
(*      modified on Thu Nov 11 15:53:59 PST 1993 by kalsow        *)
(*      modified on Mon Jun 29 18:20:32 PDT 1992 by muller        *)


INTERFACE Usignal;

FROM Ctypes IMPORT int, long, unsigned_long;
FROM Utypes IMPORT caddr_t, size_t;

(*** <signal.h> ***)

CONST
  SIGHUP    =  1;      (* hangup, generated when terminal disconnects *)
  SIGINT    =  2;      (* interrupt, generated from terminal special char *)
  SIGQUIT   =  3;      (* quit, generated from terminal special char *)
  SIGILL    =  4;      (* illegal instruction (not reset when caught) *)
      ILL_RESAD_FAULT	= 0;	(* reserved addressing fault *)
      ILL_PRIVIN_FAULT	= 1;	(* privileged instruction fault *)
      ILL_RESOP_FAULT	= 2;	(* reserved operand fault *)
      ILL_INST_FAULT	= 3;	(* Illegal instruction *)
      ILL_MODE_FAULT	= 4;	(* Illegal mode - VMSPAL only *)
  SIGTRAP   =  5;      (* trace trap (not reset when caught) *)
  SIGABRT   =  6;      (* abort process *)
  SIGEMT    =  7;      (* EMT instruction *)
  SIGFPE    =  8;      (* floating point exception *)
      FPE_INTOVF_TRAP   = 16_1; (* integer overflow *)
      FPE_INTDIV_TRAP   = 16_2; (* integer divide by zero *)
      FPE_FLTOVF_TRAP   = 16_3; (* floating overflow *)
      FPE_FLTDIV_TRAP   = 16_4; (* floating/decimal divide by zero *)
      FPE_FLTUND_TRAP   = 16_5; (* floating underflow *)
      FPE_DECOVF_TRAP   = 16_6; (* decimal overflow *)
      FPE_SUBRNG_TRAP   = 16_7; (* subscript out of range *)
      FPE_FLTOVF_FAULT  = 16_8; (* floating overflow fault *)
      FPE_FLTDIV_FAULT  = 16_9; (* divide by zero floating fault *)
      FPE_FLTUND_FAULT  = 16_a; (* floating underflow fault *)
      FPE_UNIMP_FAULT   = 16_b; (* Unimplemented FPU instruction *)
      FPE_INVALID_FAULT = 16_c; (* Invalid operation *)
      FPE_INEXACT_FAULT = 16_d; (* Inexact result *)
      FPE_HPARITH_TRAP  = 16_e; (* High performance trap *)
      FPE_INTOVF_FAULT  = 16_f; (* Integer Overflow fault *)
      FPE_ILLEGAL_SHADOW_TRAP = 16_10; (* Illegal Trap Shadow Trap *)
      FPE_GENTRAP       = 16_11; (* *)
  SIGKILL   =  9;      (* kill (cannot be caught or ignored) *)
  SIGBUS    =  10;     (* bus error (specification exception) *)
  SIGSEGV   =  11;     (* segmentation violation *)
  SIGSYS    =  12;     (* bad argument to system call *)
  SIGPIPE   =  13;     (* write on a pipe with no one to read it *)
  SIGALRM   =  14;     (* alarm clock timeout *)
  SIGTERM   =  15;     (* software termination signal *)
  SIGURG    =  16;     (* urgent condition on IO channel *)
  SIGSTOP   =  17;     (* stop (cannot be caught or ignored) *)
  SIGTSTP   =  18;     (* interactive stop *)
  SIGCONT   =  19;     (* continue if stopped *)
  SIGCHLD   =  20;     (* sent to parent on child stop or exit *)
  SIGTTIN   =  21;     (* background read attempted from control terminal *)
  SIGTTOU   =  22;     (* background write attempted to control terminal *)
  SIGPOLL   =  23;     (* I/O possible, or completed *)
  SIGXCPU   =  24;     (* cpu time limit exceeded (see setrlimit()) *)
  SIGXFSZ   =  25;     (* file size limit exceeded (see setrlimit()) *)
  SIGVTALRM =  26;     (* virtual time alarm (see setitimer) *)
  SIGPROF   =  27;     (* profiling time alarm (see setitimer) *)
  SIGWINCH  =  28;     (* window size changed *)
  SIGINFO   =  29;     (* information request *)
  SIGUSR1   =  30;     (* user defined signal 1 *)
  SIGUSR2   =  31;     (* user defined signal 2 *)
  SIGRESV   =  32;     (* reserved by Digital for future use *)

  (* additional signal names supplied for compatibility, only *)
  SIGIOINT  = SIGURG;  (* printer to backend error signal *)
  SIGIO     = SIGPOLL; (* STREAMS version of this signal *)
  SIGAIO    = SIGIO;   (* base lan i/o *)
  SIGPTY    = SIGIO;   (* pty i/o *)
  SIGIOT    = SIGABRT; (* abort (terminate) process *)
  SIGLOST   = SIGIOT;  (* old BSD signal ?? *)
  SIGPWR    = SIGINFO; (* Power Fail/Restart -- SVID3/SVR4 *)

  SIGCLD    = SIGCHLD;

  (* values of "how" argument to sigprocmask() call *)
  SIG_BLOCK    = 1;
  SIG_UNBLOCK  = 2;
  SIG_SETMASK  = 3;


(* Signal vector "template" used in sigvec call. *)
TYPE
  SignalHandler = PROCEDURE (sig: int;
                             sip: UNTRACED REF siginfo_t;
                             scp: UNTRACED REF struct_sigcontext);

  pid_t = int;

  sigset_t = unsigned_long;

  struct_sigaction = RECORD
    sa_handler : SignalHandler;    (* signal handler *)
    sa_mask    : sigset_t;         (* signals to block while in handler *)
    sa_flags   : int;              (* signal action flags *)
    sa_signo   : int;              (* signal number *)
  END;

CONST
  (* valid flags define for sa_flag field of sigaction structure *)
  SA_ONSTACK     = 16_0001;   (* run on special signal stack *)
  SA_RESTART     = 16_0002;   (* restart system calls on sigs *)
  SA_NOCLDSTOP   = 16_0004;   (* do not set SIGCHLD for child stops *)
  SA_NODEFER     = 16_0008;   (* don't block while handling *)
  SA_RESETHAND   = 16_0010;   (* old sys5 style behavior *)
  SA_NOCLDWAIT   = 16_0020;   (* no zombies *)
  SA_SIGINFO     = 16_0040;   (* deliver siginfo to handler *)
  SA_CLDNOTIFY   = 16_0080;   (* deliver siginfo when exiting *)


  (* Max siginfo size -- 128 bytes altogether (used to pad siginfo) *)
  SI_MAX_SIZE = 128;
  SI_PAD_SIZE = (SI_MAX_SIZE DIV BYTESIZE(int)) - 4;

TYPE
  siginfo_t = RECORD
    si_signo: int;			 (* signal number *)
    si_errno: int;			 (* error number *)
    si_code : int;			 (* signal code *)
    si_pad  : ARRAY [1..SI_PAD_SIZE] OF int; (* reserve space for fields *)
  END;

  (* SIGILL, SIGFPE, SIGSEGV, SIGBUS *)
  siginfo_t_fault = RECORD
    si_signo: int;			 (* signal number *)
    si_errno: int;			 (* error number *)
    si_code : int;			 (* signal code *)
    si_addr : caddr_t;			 (* faulting instruction/memory ref. *)
  END;

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
    sc_sbase     : caddr_t;		 (* stack start *)
    sc_traparg_a0: unsigned_long;	 (* a0 argument to trap on exception *)
    sc_traparg_a1: unsigned_long;	 (* a1 argument to trap on exception *)
    sc_traparg_a2: unsigned_long;	 (* a2 argument to trap on exception *)
    sc_fp_trap_pc: unsigned_long;	 (* imprecise pc *)
    sc_fp_trigger_sum: unsigned_long;	 (* Exception summary at trigger pc *)
    sc_fp_trigger_inst: unsigned_long;	 (* Instruction at trigger pc *)
  END;

  struct_sigstack = RECORD 
    ss_sp:      ADDRESS; (* signal stack pointer *)
    ss_onstack: int;     (* current status *)
  END;


(* Do not modifiy these variables *)

VAR (* CONST *)
  BADSIG, SIG_ERR, SIG_DFL, SIG_IGN, SIG_HOLD, SIG_CATCH: SignalHandler;


(*** kill(2) - send signal to a process ***)

<*EXTERNAL*>
PROCEDURE kill (pid: pid_t; sig: int): int;


(*** killpg(2) - send signal to a process or process group ***)

<*EXTERNAL*>
PROCEDURE killpg (pgrp: pid_t; sig: int): int;


(*** sigaction(2) - Specifies the action to take upon delivery of a signal ***)

<*EXTERNAL*>
PROCEDURE sigaction (sig: int;
                     READONLY new: struct_sigaction;
                     VAR      old: struct_sigaction): int;


(*** sigprocmask(2) - Sets the current signal mask ***)

<*EXTERNAL*>
PROCEDURE sigprocmask (how: int;
                       READONLY new: sigset_t;
                       old: UNTRACED REF sigset_t := NIL): int;


(*** sigsuspend(2) - Atomically changes the set of blocked signals and waits
  for a signal ***)

<*EXTERNAL*>
PROCEDURE sigsuspend (READONLY mask: sigset_t): int;


(*** sigpending(2) - Examines pending signals ***)

<*EXTERNAL*>
PROCEDURE sigpending (VAR set: sigset_t): int;


(*** sigemptyset, sigfillset, sigaddset, sigdelset, sigismember(3) - Creates
  and manipulates signal masks ***)

<*EXTERNAL*>
PROCEDURE sigemptyset (VAR set: sigset_t): int;

<*EXTERNAL*>
PROCEDURE sigfillset (VAR set: sigset_t): int;

<*EXTERNAL*>
PROCEDURE sigaddset (VAR set: sigset_t; signo: int): int;

<*EXTERNAL*>
PROCEDURE sigdelset (VAR set: sigset_t; signo: int): int;

<*EXTERNAL*>
PROCEDURE sigismember (VAR set: sigset_t; signo: int): int;

END Usignal.
