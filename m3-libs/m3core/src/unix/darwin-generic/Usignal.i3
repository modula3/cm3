(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Tue Mar  2 17:18:02 PST 1993 by muller    *)

INTERFACE Usignal;

IMPORT Uucontext;
FROM Ctypes IMPORT long, int, unsigned_int, void_star, char_star;
FROM Uucontext IMPORT ucontext_t_star;

(*** <sys/signal.h> ***)

CONST
  NSIG = 32;			  (* counting 0; could be 33 (mask is 1-32) *)

  SIGHUP    =  1;      (* hangup *)
  SIGINT    =  2;      (* interrupt *)
  SIGQUIT   =  3;      (* quit *)
  SIGILL    =  4;      (* illegal instruction (not reset when caught) *)
      ILL_NOOP 	    = 0;      (* if only I knew... *)
      ILL_ILLOPC    = 1;      (* illegal opcode *)
      ILL_ILLTRP    = 2;      (* illegal trap *)
      ILL_PRVOPC    = 3;      (* privileged opcode *)
  SIGTRAP   =  5;      (* trace trap (not reset when caught) *)
  SIGABRT   =  6;      (* abort() *)
   SIGIOT = SIGABRT;   (* IOT instruction *)
  SIGEMT    =  7;      (* EMT instruction *)
  SIGFPE    =  8;      (* floating point exception *)
      FPE_NOOP      = 0;      (* if only I knew... *)
      FPE_FLTDIV    = 1;      (* floating point divide by zero *)
      FPE_FLTOVF    = 2;      (* floating point overflow *)
      FPE_FLTUND    = 3;      (* floating point underflow *)
      FPE_FLTRES    = 4;      (* floating point inexact result *)
      FPE_FLTINV    = 5;      (* invalid floating point operation *)
  SIGKILL   =  9;      (* kill (cannot be caught or ignored) *)
  SIGBUS    =  10;     (* bus error *)
      BUS_NOOP      = 0;      (* if only I knew... *)
      BUS_ADRALN    = 1;      (* invalid address alignment *)
  SIGSEGV   =  11;     (* segmentation violation *)
      SEGV_NOOP     = 0;      (* if only I knew... *)
      SEGV_MAPERR   = 1;      (* address not mapped to object *)
      SEGV_ACCERR   = 2;      (* invalid permissions for mapped to object *)
  SIGSYS    =  12;     (* bad argument to system call *)
  SIGPIPE   =  13;     (* write on a pipe with no one to read it *)
  SIGALRM   =  14;     (* alarm clock *)
  SIGTERM   =  15;     (* software termination signal from kill *)
  SIGURG    =  16;     (* urgent condition on IO channel *)
  SIGSTOP   =  17;     (* sendable stop signal not from tty *)
  SIGTSTP   =  18;     (* stop signal from tty *)
  SIGCONT   =  19;     (* continue a stopped process *)
  SIGCHLD   =  20;     (* to parent on child stop or exit *)
      CLD_NOOP      = 0;      (* if only I knew... *)
      CLD_EXITED    = 1;      (* child has exited *)
      CLD_KILLED    = 2;
        (* child has terminated abnormally and did not create a core file *)
      CLD_DUMPED    = 3;
        (* child has terminated abnormally and create a core file *)
      CLD_TRAPPED   = 4;      (* traced child has trapped *)
      CLD_STOPPED   = 5;      (* child has stopped *)
      CLD_CONTINUED = 6;      (* stopped child has continued *)
  SIGTTIN   =  21;     (* to readers pgrp upon background tty read *)
  SIGTTOU   =  22;     (* like TTIN for output if (tp->t_local&LTOSTOP) *)
  SIGIO     =  23;     (* input/output possible signal *)
  SIGXCPU   =  24;     (* exceeded CPU time limit *)
  SIGXFSZ   =  25;     (* exceeded file size limit *)
  SIGVTALRM =  26;     (* virtual time alarm *)
  SIGPROF   =  27;     (* profiling time alarm *)
  SIGWINCH  =  28;     (* window size changes *)
  SIGINFO   =  29;     (* information request *)
  SIGUSR1   =  30;     (* user defined signal 1 *)
  SIGUSR2   =  31;     (* user defined signal 2 *)

(* Do not modifiy these variables *)
VAR (*CONST*)
  SIG_DFL, SIG_IGN, SIG_ERR, BADSIG : SignalHandler;

TYPE
  struct_siginfo = RECORD
    si_signo: int;		(* signal number *)
    si_errno: int;		(* errno association *)
    si_code: int;		(* signal code *)
    si_pid: int;		(* sending process *)
    si_uid: unsigned_int;	(* sender's ruid *)
    si_status: int;		(* exit value *)
    si_addr: void_star;		(* faulting instruction *)
    sigval_ptr: void_star;	(* signal value *)
    si_band: long;		(* band event for SIGPOLL *)
    pad: ARRAY[0..6] OF int;	(* RFU *)
  END;
  struct_siginfo_star = UNTRACED REF struct_siginfo;
  siginfo_t = struct_siginfo;
  siginfo_t_star = UNTRACED REF siginfo_t;

  SignalHandler = PROCEDURE (sig: int;
  		  	     sip: siginfo_t_star;
                             scp: ucontext_t_star);

  sigset_t = Uucontext.sigset_t;

  struct_sigaction = RECORD
    sa_sigaction: SignalHandler;        (* signal handler *)
    sa_mask     : sigset_t;             (* signals to block while in handler *)
    sa_flags    : int;                  (* signal action flags *)
  END;
  struct_sigaction_star = UNTRACED REF struct_sigaction;

CONST
  SA_ONSTACK     = 16_0001;   (* take signal on signal stack *)
  SA_RESTART     = 16_0002;   (* restart system on signal return *)
  SA_DISABLE	 = 16_0004;   (* disable taking signals on alternate stack *)
  SA_RESETHAND   = 16_0004;   (* reset to SIG_DFL when taking signal *)
  SA_NOCLDSTOP   = 16_0008;   (* do not generate SIGCHLD on child stop *)
  SA_NODEFER     = 16_0010;   (* don't mask the signal we're delivering *)
  SA_NOCLDWAIT   = 16_0020;   (* don't keep zombies around *)
  SA_SIGINFO     = 16_0040;   (* signal handler with SA_SIGINFO args *)
  SA_USERTRAMP	 = 16_0100;   (* do not bounce off kernel's sigtramp *)

CONST
  (* Flags for sigprocmask: *)
  SIG_BLOCK    = 1;    (* Add these signals to block mask *)
  SIG_UNBLOCK  = 2;    (* Remove these signals from block mask *)
  SIG_SETMASK  = 3;    (* Set block mask to this mask *)

CONST
  (* POSIX 1003.1b required values. *)
  SI_USER      = 16_10001;
  SI_QUEUE     = 16_10002;
  SI_TIMER     = 16_10003;
  SI_ASYNCIO   = 16_10004;
  SI_MESGQ     = 16_10005;

TYPE
  (* Structure used in sigaltstack call. *)
  struct_sigaltstack = RECORD
    ss_sp:    char_star;   (* signal stack base *)
    ss_size:  int;	   (* signal stack length *)
    ss_flags: int;	   (* SA_DISABLE and/or SA_ONSTACK *)
  END;
  stack_t = struct_sigaltstack;

CONST
  SS_ONSTACK   = 16_0001;  (* take signal on signal stack *)
  SS_DISABLE   = 16_0004;  (* disable taking signals on alternate stack *)
  MINSIGSTKSZ  = 8192;     (* minimum allowable stack *)
  SIGSTKSZ     = MINSIGSTKSZ + 32768; (* recommended stack size *)

(* 4.3 compatibility: Signal vector "template" used in sigvec call. *)
TYPE
  struct_sigvec = RECORD
    sv_handler: PROCEDURE (sig: int); (* signal handler *)
    sv_mask:    int;  	  	 (* signal mask to apply *)
    sv_flags:	int;		 (* see signal options below *)
  END;

CONST
  SV_ONSTACK   = SA_ONSTACK;
  SV_INTERRUPT = SA_RESTART;     (* same bit, opposite sense *)
  SV_RESETHAND = SA_RESETHAND;
  SV_NODEFER   = SA_NODEFER;
  SV_NOCLDSTOP = SA_NOCLDSTOP;
  SV_SIGINFO   = SA_SIGINFO;

TYPE
  (* Structure used in sigstack call. *)
  struct_sigstack = RECORD
    ss_sp:      char_star;	(* signal stack pointer *)
    ss_onstack: int;		(* current status *)
  END;

(* Convert a signal number to a mask suitable for sigblock(). *)
<*INLINE*> PROCEDURE sigmask (n: int): int;

(*** <signal.h> ***)

(*** raise(2) - send a signal to the current process ***)
<*EXTERNAL*> PROCEDURE raise (sig: int): int;

(*** kill(2) - send signal to a process ***)
<*EXTERNAL*> PROCEDURE kill (pid, sig: int): int;

(*** sigaction(2) - software signal facilities ***)
<*EXTERNAL "m3_sigaction"*>
PROCEDURE sigaction (sig: int;
                     READONLY new: struct_sigaction;
                     VAR old: struct_sigaction): int;

<*EXTERNAL*>
PROCEDURE sigaddset (VAR set: sigset_t; signo: int) : int;

<*EXTERNAL*>
PROCEDURE sigdelset (VAR set: sigset_t; signo: int) : int;

<*EXTERNAL*>
PROCEDURE sigemptyset (VAR set: sigset_t) : int;

<*EXTERNAL*>
PROCEDURE sigfillset (VAR set: sigset_t) : int;

<*EXTERNAL*>
PROCEDURE sigismember(VAR set: sigset_t; signo: int) : int;

(*** sigpending(2) - examine pending signals ***)
<*EXTERNAL*> PROCEDURE sigpending (VAR set: sigset_t): int;

(* FIXME - It is OK for vec and/or ovec to be NIL, so we shouldn't use VAR *)
<*EXTERNAL*>
PROCEDURE sigprocmask (how: int; READONLY new: sigset_t;
                       old: UNTRACED REF sigset_t := NIL) : int;

(*** sigsuspend(2) - release blocked signals and wait for interrupt ***)
<*EXTERNAL*>
PROCEDURE sigsuspend (VAR sigmask: sigset_t): int;

(*** killpg(2) - send signal to a process or process group ***)
<*EXTERNAL*> PROCEDURE killpg (pgrp, sig: int): int;

(*** sigblock(2) - block signals ***)
<*EXTERNAL*> PROCEDURE sigblock (mask: int): int;

(*** siginterrupt(3) - allow signals to interrupt system calls ***)
<*EXTERNAL*> PROCEDURE siginterrupt (sig, flag: int): int;

(*** sigpause(2) - atomically release blocked signals and wait for
                   interrupt ***)
<*EXTERNAL*> PROCEDURE sigpause (sigmask: int): int;

(*** sigsetmask(2) - set current signal mask ***)
<*EXTERNAL*> PROCEDURE sigsetmask (mask: int): unsigned_int;

(*** sigvec(2) - software signal facilities ***)
(* FIXME - It is OK for vec and/or ovec to be NIL, so we shouldn't use VAR *)
<*EXTERNAL*> PROCEDURE sigvec (sig: int; VAR vec, ovec: struct_sigvec): int;

END Usignal.
