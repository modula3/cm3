(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Oct  7 09:51:29 PDT 1994 by ericv         *)
(*      modified on Thu Nov 12 11:38:20 PST 1992 by muller        *)

INTERFACE Usignal;

FROM Ctypes IMPORT int, char_star;
FROM Utypes IMPORT uint32_t, uint64_t, pid_t;

(*** <signal.h> ***)

CONST
  SIGHUP    =  1;      (* hangup *)
  SIGINT    =  2;      (* interrupt *)
  SIGQUIT   =  3;      (* quit *)
  SIGILL    =  4;      (* illegal instruction (not reset when caught) *)
  SIGTRAP   =  5;      (* trace trap (not reset when caught) *)
  SIGIOT    =  6;      (* IOT instruction *)
  SIGABRT   =  6;      (* IOT instruction *)
  SIGEMT    =  7;      (* EMT instruction *)
  SIGFPE    =  8;      (* floating point exception *)
  SIGKILL   =  9;      (* kill (cannot be caught or ignored) *)
  SIGBUS    =  10;     (* bus error *)
  SIGSEGV   =  11;     (* segmentation violation *)
  SIGSYS    =  12;     (* bad argument to system call *)
  SIGPIPE   =  13;     (* write on a pipe with no one to read it *)
  SIGALRM   =  14;     (* alarm clock *)
  SIGTERM   =  15;     (* software termination signal from kill *)
  SIGUSR1   =  16;     (* user defined signal 1 *)
  SIGUSR2   =  17;     (* user defined signal 2 *)
  SIGCLD    =  18;     (* child status change *)
  SIGCHLD   =  18;     (* child status change *)
  SIGPWR    =  19;     (* power-fail restart *)
  SIGWINCH  =  20;     (* window size change *)
  SIGURG    =  21;     (* urgent socket condition *)
  SIGPOLL   =  22;     (* pollable event occured *)
  SIGIO     =  22;     (* input/output possible signal *)
  SIGSTOP   =  23;     (* stop (cannot be caught or ignored) *)
  SIGTSTP   =  24;     (* user stop requested from tty *)
  SIGCONT   =  25;     (* stopped process has been continued *)
  SIGTTIN   =  26;     (* background tty read attempted *)
  SIGTTOU   =  27;     (* background tty write attempted *)
  SIGVTALRM =  28;     (* virtual timer expired *)

  SIGPROF   =  29;     (* profiling timer expired *)
  SIGXCPU   =  30;     (* exceeded cpu limit *)
  SIGXFSZ   =  31;     (* exceeded file size limit *)

    ILL_ILLOPC_FAULT = 1;	(* illegal opcode *)
    ILL_ILLOPN_FAULT = 2;	(* illegal operand *)
    ILL_ILLADR_FAULT = 3;	(* illegal addressing mode *)
    ILL_ILLTRP_FAULT = 4;	(* illegal trap *)
    ILL_PRVOPC_FAULT = 5;	(* privileged opcode *)
    ILL_PRVREG_FAULT = 6;	(* privileged register *)
    ILL_COPROC_FAULT = 7;	(* co-processor *)
    ILL_BADSTK_FAULT = 8;	(* bad stack *)

    FPE_INTDIV_TRAP = 1;	(* integer divide by zero *)
    FPE_INTOVF_TRAP = 2;	(* integer overflow *)
    FPE_FLTDIV_TRAP = 3;	(* floating point divide by zero *)
    FPE_FLTOVF_TRAP = 4;	(* floating point overflow *)
    FPE_FLTUND_TRAP = 5;	(* floating point underflow *)
    FPE_FLTRES_TRAP = 6;	(* floating point inexact result *)
    FPE_FLTINV_TRAP = 7;	(* invalid floating point operation *)
    FPE_FLTSUB_TRAP = 8;	(* subscript out of range *)

    SEGV_MAPERR_FAULT = 1;	(* address not mapped to object *)
    SEGV_ACCERR_FAULT = 2;	(* invalid permissions *)

    BUS_ADRALN_FAULT = 1;	(* invalid address alignment *)
    BUS_ADRERR_FAULT = 2;	(* non-existent physical address *)
    BUS_OBJERR_FAULT = 3;	(* object specific hardware error *)

    TRAP_BRKPT_FAULT = 1;	(* process breakpoint *)
    TRAP_TRACE_FAULT = 2;	(* process trace *)

    CLD_EXITED_FAULT = 1;	(* child has exited *)
    CLD_KILLED_FAULT = 2;	(* child was killed *)
    CLD_DUMPED_FAULT = 3;	(* child has coredumped *)
    CLD_TRAPPED_FAULT = 4;	(* traced child has stopped *)
    CLD_STOPPED_FAULT = 5;	(* child has stopped on signal *)
    CLD_CONTINUED_FAULT = 6;	(* stopped child has continued *)

    POLL_IN_FAULT = 1;          (* input available *)
    POLL_OUT_FAULT = 2;         (* output buffers available *)
    POLL_MSG_FAULT = 3;	        (* output buffers available *)
    POLL_ERR_FAULT = 4;	        (* I/O error *)
    POLL_PRI_FAULT = 5;	        (* high priority input available *)
    POLL_HUP_FAULT = 6;	        (* device disconnected *)


(* Signal vector "template" used in sigvec call. *)
TYPE

  (* The ANSI C standard specifies that a signal handler should return
     an integer.  AIX386 and IBMR2 follow the standard.  For IBRT and other
     systems, there is no return value.  Left the declaration of
     SignalHandler alone to avoid problems elsewhere. *)

  SignalHandler = PROCEDURE (sig, code: int;
                             scp: UNTRACED REF struct_sigcontext);

  struct_sigvec  = RECORD
    sv_handler: SignalHandler;     (* signal handler *)
    sv_mask:    int;               (* signal mask to apply *)
    sv_flags:   int;               (* see signal options below *) END;

CONST
  SV_ONSTACK	 = 16_0001;
  SV_INTERRUPT	 = 16_0002;		(* not supported *)
  NUMBSDSIGS	 = 32;  (* can't be expanded *)

(* read-only *) VAR
  SIG_ERR, SIG_DFL, SIG_IGN, SIG_HOLD: SignalHandler;

TYPE
  struct_sigset = RECORD 
    sigbits : ARRAY [1..4] OF uint32_t;  
    END;
  sigset_t = struct_sigset;

  struct_sigaction = RECORD
    sa_flags : int;     (* signal action flags *)
    sa_handler: SignalHandler;     (* signal handler *)
    sa_mask : sigset_t;  (* signals to block while in handler *)
    sa_resv : ARRAY [0..1] OF int
  END;
  sigaction_t = struct_sigaction;

CONST
  empty_sigset_t = sigset_t{ sigbits := ARRAY [1..4] OF uint32_t { 0, .. } };
  empty_sv_mask  : int = 0;

(*
 * Definitions for the "how" parameter to sigprocmask():
 *
 * The parameter specifies whether the bits in the incoming mask are to be
 * added to the presently-active set for the process, removed from the set,
 * or replace the active set.
 *)
CONST
  SIG_NOP	 = 0;	(* Not using 0 will catch some user errors. *)
  SIG_BLOCK	 = 1;
  SIG_UNBLOCK	 = 2;	
  SIG_SETMASK	 = 3;
  SIG_SETMASK32	 = 256;	(* SGI added so that BSD sigsetmask won't 
				   affect the upper 32 sigal set *)

(* definitions for the sa_flags field *)
(*
 * IRIX5/SVR4 ABI definitions
 *)
CONST
  SA_ONSTACK       = 16_00000001;	(* handle this signal on sigstack *)
  SA_RESETHAND     = 16_00000002;	(* reset handler *)
  SA_RESTART       = 16_00000004;	(* restart interrupted system call *)
  SA_SIGINFO       = 16_00000008;	(* provide siginfo to handler *)
  SA_NODEFER       = 16_00000010;	(* do not block current signal *)
(* The next 2 are only meaningful for SIGCHLD *)
  SA_NOCLDWAIT     = 16_00010000;	(* don't save zombie children *)
  SA_NOCLDSTOP     = 16_00020000;	(* if set don't send SIGCLD	*)
					(* to parent when child stop	*)
(* IRIX5 additions *)
  SA_BSDCALL	 = 16_10000000;	        (* don't scan for dead children when *)
					(* setting SIGCHLD *)
					(* SJCTRL bit in proc struct.	*)


(*
 * Information pushed on stack when a signal is delivered. This is used by
 * the kernel to restore state following execution of the signal handler.
 * It is also made available to the handler to allow it to properly restore
 * state if a non-standard exit is performed.
 *
 * sc_regmask is examined by the kernel when doing sigreturn()'s
 * and indicates which registers to restore from sc_regs
 * bit 0 == 1 indicates that all coprocessor state should be restored
 *	for each coprocessor that has been used
 * bits 1 - 31 == 1 indicate registers 1 to 31 should be restored by
 *	sigcleanup from sc_regs.
 *)

(*
 * The IRIX5 version
 * sigcontext is not part of the ABI - so this version is used to
 * handle 32 and 64 bit applications - it is a constant size regardless
 * of compilation mode, and always returns 64 bit register values
 *)
TYPE
  sigcontext_t = struct_sigcontext;
  struct_sigcontext = RECORD
	sc_regmask	: uint32_t;	(* regs to restore in sigcleanup *)
	sc_status	: uint32_t;	(* cp0 status register *)
        sc_pc		: uint64_t;   (* pc at time of signal *)
	(*
	 * General purpose registers
	 *)
        sc_regs	: ARRAY [0..31] OF uint64_t; (* processor regs 0 to 31 *)
	(*
	 * Floating point coprocessor state
	 *)
	sc_fpregs : ARRAY [0..31] OF uint64_t; (* fp regs 0 to 31 *)
	sc_ownedfp	: uint32_t;	(* fp has been used *)
	sc_fpc_csr	: uint32_t;	(* fpu control and status reg *)
	sc_fpc_eir	: uint32_t;	(* fpu exception instruction reg *)
					(* implementation/revision *)
	sc_ssflags	: uint32_t;	(* signal stack state to restore *)
	sc_mdhi		: uint64_t;	(* Multiplier hi and low regs *)
	sc_mdlo		: uint64_t;
	(*
	 * System coprocessor registers at time of signal
	 *)
	sc_cause	: uint64_t;	(* cp0 cause register *)
	sc_badvaddr	: uint64_t;	(* cp0 bad virtual address *)
	sc_triggersave	: uint64_t;	(* state of graphics trigger (SGI) *)
	sc_sigset	: sigset_t;	(* signal mask to restore *)
	sc_pad		: ARRAY [0..31] OF uint64_t;
      END;

  struct_sigstack = RECORD 
    ss_sp:      char_star; (* signal stack pointer *)
    ss_onstack: int;     (* current status *) END;


CONST
  NSIG           = 65;      (* valid signal numbers are from 1 to NSIG-1 *)
  MAXSIG	 = (NSIG-1);    (* actual # of signals *)
  NUMSIGS	 = (NSIG-1);    (* for POSIX array sizes, true # of sigs *)

  BRK_USERBP	 = 0;	(* user bp (used by debuggers) *)
  BRK_KERNELBP	 = 1;	(* kernel bp (used by prom) *)
  BRK_ABORT	 = 2;	(* abort(3) uses to cause SIGIOT *)
  BRK_BD_TAKEN	 = 3;	(* for taken bd emulation *)
  BRK_BD_NOTTAKEN= 4;	(* for not taken bd emulation *)
  BRK_SSTEPBP	 = 5;	(* user bp (used by debuggers) *)
  BRK_OVERFLOW	 = 6;	(* overflow check *)
  BRK_DIVZERO	 = 7;	(* divide by zero check *)
  BRK_RANGE	 = 8;	(* range error check *)
  BRK_MULOVF	 = 1023;(* multiply overflow detected *)


(**************** System V facilities ******************)

(*** sigset(2) - specify signal handling -- signal is blocked during
                 handler, and automatically restored on exit ***)

<*EXTERNAL*>
PROCEDURE sigset(sig: int; handler: SignalHandler );

(*** sighold(2) - add sig to the calling process's signal mask ***)

<*EXTERNAL*>
PROCEDURE sighold(sig: int ): int;

(*** sigrelse(2) - remove sig from the calling process's signal mask ***)

<*EXTERNAL*>
PROCEDURE sigrelse(sig: int ): int;

(*** sigignore(2) - ignore the given signal ***)

<*EXTERNAL*>
PROCEDURE sigignore(sig: int ): int;

(*** sigpause(2) - suspend process until a signal is received ***)

<*EXTERNAL*>
PROCEDURE sigpause(sig: int): int;

(*** signal(2) - specify signal handling -- signal is NOT blocked on
                 entry to the handler, handler must call signal() again ***)

<*EXTERNAL*>
PROCEDURE signal(sig: int; handler: SignalHandler ): SignalHandler;

(*** kill(2) - send signal to a process ***)

<*EXTERNAL*>
PROCEDURE kill (pid: pid_t; sig: int): int;

(*** raise(3) - send a signal to the current process ***)

<*EXTERNAL*>
PROCEDURE raise(sig: int ): int;


(*********************** POSIX facilities *********************)


(*** sigaction(2) - specify signal handling ***)

<*EXTERNAL*>
PROCEDURE sigaction (sig: int; VAR act, oact: struct_sigaction): int;

(*** sigpending(2) - examine pending signals ***)

<*EXTERNAL*>
PROCEDURE sigpending (VAR set: sigset_t): int;

(*** sigprocmask(2) - alter the set of blocked signals ***)

<*EXTERNAL*>
PROCEDURE sigprocmask (how: int; VAR set, oset: sigset_t): int;

(*** sigsuspend(2) - release blocked signals and wait for interrupt ***)

<*EXTERNAL*>
PROCEDURE sigsuspend (VAR set: sigset_t): int;

(*** signal set manipulation and examination ***)

<*EXTERNAL*> PROCEDURE sigaddset(VAR set: sigset_t; sig: int): int;
<*EXTERNAL*> PROCEDURE sigdelset(VAR set: sigset_t; sig: int): int;
<*EXTERNAL*> PROCEDURE sigemptyset(VAR set: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigfillset(VAR set: sigset_t): int;
<*EXTERNAL*> PROCEDURE sigismember(READONLY set: sigset_t; sig: int): int;


(************************* 4.3 BSD facilities *******************)

(*** sigvec(3B) - specify signal handling, with block and restore  ***)

<*EXTERNAL*>
PROCEDURE sigvec (sig: int; VAR vec, ovec: struct_sigvec): int;

(*** killpg(3B) - send signal to a process or process group ***)

<*EXTERNAL*>
PROCEDURE killpg (pgrp, sig: int): int;

(*** Convert a signal number to a mask suitable for sigblock(). ***)

<*INLINE*>
PROCEDURE sigmask (n: int): int;

(*** sigblock(3B) - block signals ***)

<*EXTERNAL*>
PROCEDURE sigblock (mask: int): int;

(*** BSDsigpause(3B) - release blocked signals and wait for interrupt ***)

<*EXTERNAL*>
PROCEDURE BSDsigpause (sigmask: int): int;

(*** sigsetmask(3B) - set current signal mask ***)

<*EXTERNAL*>
PROCEDURE sigsetmask (mask: int): int;

(*** sigstack(3B) - set and/or get signal stack context ***)

<*EXTERNAL*>
PROCEDURE sigstack (VAR ss, oss: struct_sigstack): int;

(*** BSDsignal(3B) - simplified interface to sigvec() ***)

<*EXTERNAL*>
PROCEDURE BSDsignal(sig: int; handler: SignalHandler ): SignalHandler;


END Usignal.
