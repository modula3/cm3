(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking       *)
(*      modified on Mon Oct 31 14:56:36 PST 1994 by kalsow        *)
(*      modified on Thu Nov 12 11:38:20 PST 1992 by muller        *)

INTERFACE Usignal;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT caddr_t;
FROM Uucontext IMPORT ucontext_t, sigset_t;

(*** <sys/signal.h> ***)

CONST
  SIGHUP     =  1;  (* hangup *)
  SIGINT     =  2;  (* interrupt (rubout) *)
  SIGQUIT    =  3;  (* quit (ASCII FS) *)
  SIGILL     =  4;  (* illegal instruction (not reset when caught) *)
  SIGTRAP    =  5;  (* trace trap (not reset when caught) *)
  SIGIOT     =  6;  (* IOT instruction *)
  SIGABRT    =  6;  (* used by abort, replace SIGIOT in the future *)
  SIGEMT     =  7;  (* EMT instruction *)
  SIGFPE     =  8;  (* floating point exception *)
  SIGKILL    =  9;  (* kill (cannot be caught or ignored) *)
  SIGBUS     = 10;  (* bus error *)
  SIGSEGV    = 11;  (* segmentation violation *)
  SIGSYS     = 12;  (* bad argument to system call *)
  SIGPIPE    = 13;  (* write on a pipe with no one to read it *)
  SIGALRM    = 14;  (* alarm clock *)
  SIGTERM    = 15;  (* software termination signal from kill *)
  SIGUSR1    = 16;  (* user defined signal 1 *)
  SIGUSR2    = 17;  (* user defined signal 2 *)
  SIGCLD     = 18;  (* child status change *)
  SIGCHLD    = 18;  (* child status change alias (POSIX) *)
  SIGPWR     = 19;  (* power-fail restart *)
  SIGWINCH   = 20;  (* window size change *)
  SIGURG     = 21;  (* urgent socket condition *)
  SIGPOLL    = 22;  (* pollable event occured *)
  SIGIO      = 22;  (* input/output possible signal *)
  SIGSTOP    = 23;  (* stop (cannot be caught or ignored) *)
  SIGTSTP    = 24;  (* user stop requested from tty *)
  SIGCONT    = 25;  (* stopped process has been continued *)
  SIGTTIN    = 26;  (* background tty read attempted *)
  SIGTTOU    = 27;  (* background tty write attempted *)
  SIGVTALRM  = 28;  (* virtual timer expired *)
  SIGPROF    = 29;  (* profiling timer expired *)
  SIGXCPU    = 30;  (* exceeded cpu limit *)
  SIGXFSZ    = 31;  (* exceeded file size limit *)
  SIGWAITING = 32;  (* process's lwps are blocked *)
  SIGLWP     = 33;  (* special signal used by thread library *)
  SIGFREEZE  = 34;  (* special signal used by CPR *)
  SIGTHAW    = 35;  (* special signal used by CPR *)
  SIGCANCEL  = 36;  (* thread cancellation signal used by libthread *)
  (* signals 37-59 reserved *)

  (* Do not modify these variables *)
VAR (* READONLY *)
  SIG_ERR, SIG_DFL, SIG_IGN, SIG_HOLD: SignalHandler;

CONST
  SIG_BLOCK    = 1;    (* Add these signals to block mask *)
  SIG_UNBLOCK  = 2;    (* Remove these signals from block mask *)
  SIG_SETMASK  = 3;    (* Set block mask to this mask *)

  SIGNO_MASK   = 16_FF;
  SIGDEFER     = 16_0100;
  SIGHOLD      = 16_0200;
  SIGRELSE     = 16_0400;
  SIGIGNORE    = 16_0800;
  SIGPAUSE     = 16_1000;

TYPE
  (* SIGSEGV, SIGBUS, SIGILL, SIGFPE *)
  siginfo_t_fault = RECORD
    si_signo   : int;			 (* signal number *)
    si_code    : int;			 (* signal code *)
    si_errno   : int;			 (* error number *)
    si_addr    : caddr_t;		 (* faulting address *)
    si_trapno  : int;			 (* illegal trap number *)
  END;
  siginfo_t_fault_star = UNTRACED REF siginfo_t_fault;

  (* valid SIGFPE codes for si_code field of siginfo_t_fault structure above *)
CONST
  FPE_INTDIV = 1;			 (* integer divide by zero *)
  FPE_INTOVF = 2;			 (* integer overflow *)
  FPE_FLTDIV = 3;			 (* floating point divide by zero *)
  FPE_FLTOVF = 4;			 (* floating point overflow *)
  FPE_FLTUND = 5;			 (* floating point underflow *)
  FPE_FLTRES = 6;			 (* floating point inexact result *)
  FPE_FLTINV = 7;			 (* invalid floating point operation *)
  FPE_FLTSUB = 8;			 (* subscript out of range *)

TYPE
  SignalHandler = PROCEDURE (sig: int;
                             sip: UNTRACED REF siginfo_t_fault;
                             uap: UNTRACED REF ucontext_t);

  struct_sigaction = RECORD
    sa_flags   : int;            (* signal action flags *)
    sa_handler : SignalHandler;  (* signal handler *)
    sa_mask    : sigset_t;       (* signals to block while in handler *)
    sa_resv    : ARRAY [0..1] OF int;
  END;

 (* valid flags for sa_flag field of sigaction structure  *)
CONST
  SA_ONSTACK     = 16_0001;   (* run on special signal stack *)
  SA_OLDSTYLE    = 16_0002;   (* old "unreliable" UNIX semantics *)
  SA_RESTART     = 16_0004;   (* restart system calls on sigs *)
  SA_SIGINFO     = 16_0008;   (* provide signal information on sigs *)
  SA_NODEFER     = 16_0010;

  (* these are only valid for SIGCLD *)
  SA_NOCLDWAIT  = 16_10000;   (* don't save zombie children *)
  SA_NOCLDSTOP  = 16_20000;   (* don't send job control SIGCLD's *)

  (* this is only valid for SIGWAITING *)
  SA_WAITSIG    = 16_10000;   (* send SIGWAITING if all lwps block *)


(*** kill(2) - send signal to a process ***)

<*EXTERNAL*>
PROCEDURE kill (pid, sig: int): int;

(*** killpg(2) - send signal to a process or process group ***)

<*EXTERNAL*>
PROCEDURE killpg (pgrp, sig: int): int;

(*** sigpending(2) - examine pending signals ***)

<*EXTERNAL*>
PROCEDURE sigpending (VAR set: sigset_t): int;

(*** sigaction(2) - detailed signal management ***)

<*EXTERNAL*>
PROCEDURE sigaction (sig: int; VAR act, oact: struct_sigaction): int;

(*** sigprocmask(2) - change and/or examine calling process's signal mask ***)

<*EXTERNAL*>
PROCEDURE sigprocmask (how: int; READONLY set: sigset_t;
                       oset: UNTRACED REF sigset_t := NIL): int;

(*** sigsetops(3C) (sigemptyset,  sigfillset,  sigaddset,  sigdelset,
     sigismember) - manipulate sets of signals ***)

<*EXTERNAL*>
PROCEDURE sigemptyset(VAR set: sigset_t): int;

<*EXTERNAL*>
PROCEDURE sigfillset(VAR set: sigset_t): int;

<*EXTERNAL*>
PROCEDURE sigaddset(VAR set: sigset_t; signo: int): int;

<*EXTERNAL*>
PROCEDURE sigdelset(VAR set: sigset_t; signo: int): int;

<*EXTERNAL*>
PROCEDURE sigismember(VAR set: sigset_t; signo: int): int;

END Usignal.
