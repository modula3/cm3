(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Tue Nov  5 13:33:05 PST 1996 by heydon        *)
(*      modified on Thu Nov 11 15:53:59 PST 1993 by kalsow        *)
(*      modified on Mon Jun 29 18:20:32 PDT 1992 by muller        *)


INTERFACE Usignal;

FROM Ctypes IMPORT int, long, unsigned_long;
IMPORT Utypes;

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
  SIGVTALRM =  26;     (* virtual time alarm *)
  SIGPROF   =  27;     (* profiling time alarm *)
  SIGWINCH  =  28;     (* window size changes *)
  SIGINFO   =  29;     (* information request *)
  SIGUSR1   =  30;     (* User signal 1 (from SysV) *)
  SIGUSR2   =  31;     (* User signal 2 (from SysV) *)

  (* System V definitions *)
  SIGCLD    = SIGCHLD;
  SIGABRT   = SIGIOT;


(* Signal vector "template" used in sigvec call. *)
TYPE
  SignalHandler =
    PROCEDURE (sig, code: int; scp: UNTRACED REF struct_sigcontext);

  sigset_t = int;

  struct_sigvec  = RECORD
    sv_handler: SignalHandler;     (* signal handler *)
    sv_mask:    sigset_t;          (* signal mask to apply *)
    sv_flags:   int;               (* see signal options below *)
  END;

CONST
  empty_sigset_t : sigset_t = 0;
  empty_sv_mask  : sigset_t = 0;

CONST
  SV_ONSTACK   = 16_0001;  (* take signal on signal stack *)
  SV_INTERRUPT = 16_0002;  (* do not restart system on signal return *)
  SA_NOCLDSTOP = 16_0004;  (* Don't generate SIGCLD when children stop *)
  SV_OLDSIG    = 16_1000;  (* Emulate old signal() for POSIX *)

  (* Defines for "how" argument of sigprocmask() call. POSIX. *)
  SIG_BLOCK    = 1;    (* Add these signals to block mask *)
  SIG_UNBLOCK  = 2;    (* Remove these signals from block mask *)
  SIG_SETMASK  = 3;    (* Set block mask to this mask *)

TYPE
  struct_sigstack = RECORD 
    ss_sp:      ADDRESS; (* signal stack pointer *)
    ss_onstack: int;     (* current status *)
  END;

(*
 * Information pushed on stack when a signal is delivered.
 * This is used by the kernel to restore state following
 * execution of the signal handler.  It is also made available
 * to the handler to allow it to properly restore state if
 * a non-standard exit is performed.
 *
 * WARNING: THE sigcontext MUST BE KEPT CONSISTENT WITH /usr/include/setjmp.h
 * AND THE LIBC ROUTINES setjmp() AND longjmp()
 *
 *)
TYPE
  longV = ARRAY [0..31] OF long; (* room for 32 registers *)

TYPE
  struct_sigcontext = RECORD
    (*-----------------------------------------------------------------------
     * BEGIN REGION THAT MUST AGREE WITH jmp_buf REGION IN setjmp.h
     *)
    sc_onstack : long;      (* sigstack state to restore *)
    sc_mask    : long;      (* signal mask to restore *)
    sc_pc      : long;      (* pc at time of signal *)
    sc_ps      : long;      (* psl to restore *)

    (*
     * General purpose registers
     *)
    sc_regs       : longV;    (* processor regs 0 to 31 *)
    sc_ownedfp    : long;     (* fp has been used *)
    sc_fpregs     : longV;    (* fp regs 0 to 31 *)
    sc_fpcr       : unsigned_long;  (* floating point control register *)
    sc_fp_control : unsigned_long;  (* software fpcr *)

    (* END OF REGION THAT MUST AGREE WITH jmp_buf REGION IN setjmp.h
     -----------------------------------------------------------------------*)

    sc_reserved1  : long;    (* reserved for kernel *)
    sc_kreserved1 : int;     (* reserved for kernel *)
    sc_kreserved2 : int;     (* reserved for kernel *)

    sc_ssize      : Utypes.size_t;  (* stack size *)
    sc_sbase      : Utypes.caddr_t; (* stack start *)

    sc_traparg_a0 : unsigned_long; (* a0 argument to trap on exception *)
    sc_traparg_a1 : unsigned_long; (* a1 argument to trap on exception *)
    sc_traparg_a2 : unsigned_long; (* a2 argument to trap on exception *)

    sc_fp_trap_pc      : unsigned_long; (* imprecise pc *)
    sc_fp_trigger_sum  : unsigned_long; (* exception summary at trigger pc *)
    sc_fp_trigger_inst : unsigned_long; (* instruction at trigger pc *)
  END;

CONST
  R_SP = 30; (* index of stack pointer in sc_regs above *)

(* Do not modifiy these variables *)

VAR (* CONST *)
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
