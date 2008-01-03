(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Mon Mar 16 17:21:02 PST 1992 by muller        *)

INTERFACE Usignal;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT u_long;

(*** <signal.h> ***)

CONST
  SIGHUP    =  1;      (* hangup *)
  SIGINT    =  2;      (* interrupt *)
  SIGQUIT   =  3;      (* quit *)
  SIGILL    =  4;      (* illegal instruction (not reset when caught) *)
      ILL_RESAD_FAULT	= 0;	(* reserved addressing fault *)
      ILL_PRIVIN_FAULT	= 1;	(* privileged instruction fault *)
      ILL_RESOP_FAULT	= 2;	(* reserved operand fault *)
      (* CHME, CHMS, CHMU are not yet given back to users reasonably *)
  SIGTRAP   =  5;      (* trace trap (not reset when caught) *)
  SIGIOT    =  6;      (* IOT instruction *)
  SIGEMT    =  7;      (* EMT instruction *)
  SIGFPE    =  8;      (* floating point exception *)
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
  SIGKILL   =  9;      (* kill (cannot be caught or ignored) *)
  SIGBUS    =  10;     (* bus error *)
  SIGSEGV   =  11;     (* segmentation violation *)
  SIGSYS    =  12;     (* bad argument to system call *)
  SIGPIPE   =  13;     (* write on a pipe with no one to read it *)
  SIGALRM   =  14;     (* alarm clock *)
  SIGTERM   =  15;     (* software termination signal from kill *)
  SIGURG    =  16;     (* urgent condition on IO channel *)
  SIGSTOP   =  17;     (* sendable stop signal not from tty *)
  SIGTSTP   =  18;     (* stop signal from tty *)
  SIGCONT   =  19;     (* continue a stopped process *)
  SIGCHLD   =  20;     (* to parent on child stop or exit *)
  SIGTTIN   =  21;     (* to readers pgrp upon background tty read *)
  SIGTTOU   =  22;     (* like TTIN for output if (tp->t_local&LTOSTOP) *)
  SIGIO     =  23;     (* input/output possible signal *)
  SIGXCPU   =  24;     (* exceeded CPU time limit *)
  SIGXFSZ   =  25;     (* exceeded file size limit *)
  (* signal 26 reserved on AIX PS/2 *)
  SIGMSG    =  27;     (* input data is in the HFT ring buffer *)
  SIGWINCH  =  28;     (* window size changes *)
  SIGPWR    =  29;     (* power failuer imminent (save your data) *)
  SIGUSR1   =  30;     (* User signal 1 (from SysV) *)
  SIGUSR2   =  31;     (* User signal 2 (from SysV) *)

  SIGPROF   =  32;	(* profiling time alarm *)
  SIGDANGER =  33;	(* system crash imminent *)
  SIGVTALRM =  34;      (* virtual time alarm *)
  SIGMIGRATE = 35;	(* migrate process to another CPU *)
  SIGPRE    =  36;	(* programming error *)

  (* signals 37-59 reserved *)

  SIGGRANT  =  60;	(* HFT monitor mode granted *)
  SIGRETRACT = 61; 	(* HFT monitor mode retracted *)
  SIGSOUND  =  62;	(* HFT sound sequence has completed *)

  (* signal 63 reserved *)

  (* System V definitions *)
  SIGCLD    = SIGCHLD;
  SIGABRT   = SIGIOT;


(* Signal vector "template" used in sigvec call. *)


(* The ANSI C standard specifies that a signal handler should return
   an integer.  AIX386 and IBMR2 follow the standard.  For IBMRT and other
   systems, there is no return value.  Left the declaration of
   SignalHandler alone to avoid problems elsewhere. *)
 
TYPE
  SignalHandler = PROCEDURE (sig, code: int;
                             scp: UNTRACED REF struct_sigcontext);

CONST
  NSIG = 63;
  SIGSETZ = NSIG+31 DIV 32;

TYPE	
  struct_sigset = RECORD
    setsize: int;
    sigs: ARRAY [1..SIGSETZ] OF u_long; END;
  sigset_t = struct_sigset;

  struct_sigvec  = RECORD
    sv_handler: SignalHandler;     (* signal handler *)
    sv_mask:    int;               (* signal mask to apply *)
    sv_flags:   int;               (* see signal options below *) END;

CONST
  empty_sigset_t = sigset_t {setsize := 0, 
                             sigs := ARRAY [1..SIGSETZ] OF u_long {0, ..}};
  empty_sv_mask  : int = 0;

CONST
  SV_ONSTACK   = 16_0001;  (* take signal on signal stack *)
  SV_INTERRUPT = 16_0002;  (* do not restart system on signal return *)
  SA_RESTART   = 16_0004;
  SA_NOCLDSTOP = 16_0008;
  SV_OLDSIG    = 16_1000;  (* Emulate old signal() for POSIX *)

  (* Defines for sigprocmask() call. POSIX. *)
  SIG_BLOCK    = 1;    (* Add these signals to block mask *)
  SIG_UNBLOCK  = 2;    (* Remove these signals from block mask *)
  SIG_SETMASK  = 3;    (* Set block mask to this mask *)

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
	sc_onstack: int;		(* $ sigstack state to restore *)
	sc_di: int;			(* $ edi *)
	sc_si: int;			(* $ esi *)
	sc_bp: int;			(* $ ebp *)
	sc_sp: int;			(*   esp *)
	sc_bx: int;			(* $ ebx *)
	sc_dx: int;			(* $ edx *)
	sc_cx: int;			(* $ ecx *)
	sc_ax: int;			(* $ eax *)
	sc_gs: int;			(* $ gs *)
	sc_fs: int;			(* $ fs *)
	sc_es: int;			(* $ es *)
	sc_uds: int;			(* $ ds *)
        sc_viol: int;                   (*   viol *)
	sc_err: int;			(*   err *)
	sc_ip: int;			(* $ eip *)
	sc_cs: int;			(* $ cs *)
	sc_fl: int;			(* $ fl *)
	sc_usp: int;			(* $ esp *)
	sc_uss: int;			(* $ ss *)
        sc_smask: sigset_t;             (* $ signal mask to restore *) END;


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
