(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Mon Mar 16 11:01:48 PST 1992 by muller           *)
(*      modified on Wed Nov 20 11:37:56 PST 1991 by muller@cs.ruu.nl *)
(*      modified on Fri Jun 29 09:24:15 1990 by piet@cs.ruu.nl *)


INTERFACE Usignal;

FROM Ctypes IMPORT int, char, short, long;

(*** <signal.h> ***)

CONST
  SIGHUP    =  1;      (* hangup *)
  SIGINT    =  2;      (* interrupt *)
  SIGQUIT   =  3;      (* quit *)
  SIGILL    =  4;      (* illegal instruction (not reset when caught) *)
(* Not on hp300
      ILL_RESAD_FAULT	= 0;	(* reserved addressing fault *)
      ILL_PRIVIN_FAULT	= 1;	(* privileged instruction fault *)
      ILL_RESOP_FAULT	= 2;	(* reserved operand fault *)
      (* CHME, CHMS, CHMU are not yet given back to users reasonably *)
*)
  SIGTRAP   =  5;      (* trace trap (not reset when caught) *)
  SIGIOT    =  6;      (* IOT instruction *)
  SIGEMT    =  7;      (* EMT instruction *)
  SIGFPE    =  8;      (* floating point exception *)
(* Not on hp300
      FPE_INTOVF_TRAP  = 1;    (* integer overflow *)
      FPE_INTDIV_TRAP  = 2;    (* integer divide by zero *)
      FPE_FLTOVF_TRAP  = 3;    (* floating overflow *)
      FPE_FLTDIV_TRAP  = 4;    (* floating/decimal divide by zero *)
      FPE_FLTUND_TRAP  = 5;    (* floating underflow *)
      FPE_DECOVF_TRAP  = 6;    (* decimal overflow *)
      FPE_SUBRNG_TRAP  = 7;    (* subscript out of range *)
      FPE_FLTOVF_FAULT = 8;    (* floating overflow fault *)
      FPE_FLTDIV_FAULT = 9;    (* divide by zero floating fault *)
      FPE_FLTUND_FAULT = 10;   (* floating underflow fault *)
*)
  SIGKILL   =  9;      (* kill (cannot be caught or ignored) *)
  SIGBUS    =  10;     (* bus error *)
  SIGSEGV   =  11;     (* segmentation violation *)
  SIGSYS    =  12;     (* bad argument to system call *)
  SIGPIPE   =  13;     (* write on a pipe with no one to read it *)
  SIGALRM   =  14;     (* alarm clock *)
  SIGTERM   =  15;     (* software termination signal from kill *)
  SIGUSR1   =  16;     (* user defined signal 1 *)
  SIGUSR2   =  17;     (* user defined signal 2 *)
  SIGCHLD   =  18;     (* Child process terminated or stopped *)
  SIGPWR    =  19;     (* power state indication *)
  SIGVTALRM =  20 ;     (* virtual timer alarm *)
  SIGPROF   =  21;     (* profiling timer alarm *)
  SIGIO     =  22;     (* asynchronous I/O *)
  SIGWINDOW =  23 ;     (* windowing signal *)
  SIGSTOP   =  24;     (* Stop signal (cannot be caught or ignored) *)
  SIGTSTP   =  25;     (* Interactive stop signal *)
  SIGCONT   =  26;     (* Continue if stopped *)
  SIGTTIN   =  27;     (* Read from control terminal attempted by a
            	  	   member of a background process group *)
  SIGTTOU   =  28;     (* Write to control terminal attempted by a 
				   member of a background process group *)
  SIGURG    =  29;     (* urgent condition on IO channel *)
  SIGLOST   =  30;     (* remote lock lost  (NFS)        *)
  		       (* Save for future use *)
  SIGDIL    =  32;     (* DIL signal *)

  NSIG      =  33;     (* Highest signal number + 1 *)

  (* System V definitions *)
  SIGCLD    = SIGCHLD;
  SIGABRT   = SIGIOT;


(* Signal vector "template" used in sigvec call. *)
TYPE
  SignalHandler = PROCEDURE (sig, code: int;
                             scp: UNTRACED REF struct_sigcontext);

  struct_sigset = RECORD
	sigset: ARRAY [1..8] OF long;
    END;
  sigset_t = struct_sigset;

  struct_sigaction = RECORD
    sa_handler: SignalHandler;     (* signal handler *)
    sa_mask:    sigset_t;          (* signal mask to apply *)
    sa_flags:   int;               (* see signal options below *) END;
	
  struct_sigvec  = RECORD
    sv_handler: SignalHandler;     (* signal handler *)
    sv_mask:    int;               (* signal mask to apply *)
    sv_flags:   int;               (* see signal options below *) END;

CONST
  empty_sigset_t = sigset_t {sigset := ARRAY [1..8] OF long {0, ..}};
  empty_sv_mask  : int = 0;

CONST
  SV_ONSTACK   = 16_0001;  (* take signal on signal stack *)
  SA_ONSTACK   = SV_ONSTACK;
  SV_INTERRUPT = 16_0002;  (* do not restart system on signal return *)
  SA_NOCLDSTOP = 16_0008;  (* Don't generate SIGCLD when children stop *)
  SV_OLDSIG    = 16_1000;  (* Emulate old signal() for POSIX *)
  SA_RESETHAND = 16_0004;  (* Reset after catching *)

  (* Defines for sigprocmask() call. POSIX. *)
  SIG_BLOCK    = 0;    (* Add these signals to block mask *)
  SIG_UNBLOCK  = 1;    (* Remove these signals from block mask *)
  SIG_SETMASK  = 2;    (* Set block mask to this mask *)

TYPE
  struct_sigstack = RECORD 
    ss_sp:      ADDRESS; (* signal stack pointer *)
    ss_onstack: int;     (* current status *) END;

(*
 * Information pushed on stack when a signal is delivered.
 * This is used by the kernel to restore state following
 * execution of the signal handler.  It is also made available
 * to the handler to allow it to properly restore state if
 * a non-standard exit is performed.
 *
 *	XXX - sigcontext needs updating per 4.3BSD - rr
 *
 *)
TYPE
  struct_sigcontext = RECORD
    sc_syscall: int;		(* interrupted system call if any *)
    sc_syscall_action: char ;	(* what to do after system call *)
    sc_pad1: char;
    sc_pad2: char;
    sc_onstack: char;   (* sigstack state to restore *)
    sc_mask: int;      (* signal mask to restore *)
    sc_sp: int;        (* sp to restore *)
    sc_ps: int;        (* ps to restore *)
    sc_pc: short;        (* pc to retore *)
 END;


(* Do not modifiy these variables *)

(* read-only *) VAR
  BADSIG, SIG_ERR, SIG_DFL, SIG_IGN: SignalHandler;


(* Convert a signal number to a mask suitable for sigblock(). *)
<*INLINE*>
PROCEDURE sigmask (n: int): int;


(*** kill(2) - send signal to a process ***)

<*EXTERNAL*>
PROCEDURE kill (pid, sig: int): int;


(*** killpg(2) - send signal to a process or process group ***)

<*EXTERNAL*>
PROCEDURE killpg (pgrp, sig: int): int;


(*** sigblock(2) - block signals ***)

<*EXTERNAL*>
PROCEDURE sigblock (mask: int): int;


(*** sigpause(2) - atomically release blocked signals and wait for
                   interrupt ***)

<*EXTERNAL*>
PROCEDURE sigpause (sigmask: int): int;


(*** sigpending(2) - examine pending signals ***)

<*EXTERNAL*>
PROCEDURE sigpending (VAR set: sigset_t): int;


(*** sigsetmask(2) - set current signal mask ***)

<*EXTERNAL*>
PROCEDURE sigsetmask (mask: int): int;


(*** sigstack(2) - set and/or get signal stack context ***)

<*EXTERNAL*>
PROCEDURE sigstack (VAR ss, oss: struct_sigstack): int;


(*** sigvec(2) - software signal facilities ***)

<*EXTERNAL*>
PROCEDURE sigvec (sig: int; VAR vec, ovec: struct_sigvec): int;

END Usignal.
