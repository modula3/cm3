(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Tue Mar  2 17:18:02 PST 1993 by muller    *)
(* ow 03.10.1994 *)

INTERFACE Usignal;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT pid_t, u_int32_t;

(*** <signal.h> ***)

  (* I don't know about all the indented values below from the 
     Linux implementation *)
CONST
  SIGHUP    =  1;      (* hangup *)
  SIGINT    =  2;      (* interrupt *)
  SIGQUIT   =  3;      (* quit *)
  SIGILL    =  4;      (* illegal instruction (not reset when caught) *)
  SIGTRAP   =  5;      (* trace trap (not reset when caught) *)
  SIGIOT    =  6;      (* IOT instruction *)
  SIGEMT    =  7;      (* EMT instruction *)
  SIGFPE    =  8;      (* floating point exception *)
      FPE_INTDIV_TRAP      = 20;  (* integer divide by zero *)
      FPE_INTOVF_TRAP      = 21;  (* integer overflow *)
      FPE_FLTOPERR_TRAP    =  1;  (* [floating operand error] *)
      FPE_FLTDEN_TRAP      =  2;  (* [floating denormalized operand] *)
      FPE_FLTDIV_TRAP      =  3;  (* [floating divide by zero] *)
      FPE_FLTOVF_TRAP      =  4;  (* [floating overflow] *)
      FPE_FLTUND_TRAP      =  5;  (* [floating underflow] *)
      FPE_FLTINEX_TRAP     =  6;  (* [floating inexact result] *)
      FPE_UUOP_TRAP        =  7;  (* [floating undefined opcode] *)
      FPE_DATACH_TRAP      =  8;  (* [floating data chain exception] *)
      FPE_FLTSTK_TRAP      = 16;  (* [floating stack fault] *)
      FPE_FPA_ENABLE       = 17;  (* [FPA not enabled] *)
      FPE_FPA_ERROR        = 18;  (* [FPA arithmetic exception] *)
  SIGKILL   =  9;      (* kill (cannot be caught or ignored) *)
  SIGBUS    =  10;     (* bus error *)
      BUS_HWERR	  = 1;     (* misc hardware error (e.g. timeout) *)
      BUS_ALIGN	  = 2;     (* hardware alignment error *)
  SIGSEGV   =  11;     (* segmentation violation *)
      SEGV_NOMAP  = 3;     (* no mapping at the fault address *)
      SEGV_PROT   = 4;      (* access exceeded protections *)
      SEGV_OBJERR = 5;    (* object returned errno value *)
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
  SIGLOST   =  29;     (* Sys-V rec lock: notify user upon server crash *)
  SIGUSR1   =  30;     (* User signal 1 (from SysV) *)
  SIGUSR2   =  31;     (* User signal 2 (from SysV) *)

  (* System V definitions *)
  SIGCLD    = SIGCHLD;
  SIGABRT   = SIGIOT;


(* Signal vector "template" used in sigaction call. *)
TYPE
  SignalHandler = PROCEDURE (sig, code: int;
                             scp: UNTRACED REF struct_sigcontext);

  sigset_t = ARRAY [0..3] OF u_int32_t;
  sigset_t_star = UNTRACED REF sigset_t;
  const_sigset_t_star = sigset_t_star;

CONST
  empty_sigset_t = sigset_t{ 0, .. };

CONST
  (* Defines for sigprocmask() call. POSIX. *)
  SIG_BLOCK    = 1;    (* Add these signals to block mask *)
  SIG_UNBLOCK  = 2;    (* Remove these signals from block mask *)
  SIG_SETMASK  = 3;    (* Set block mask to this mask *)

TYPE
  struct_sigaction = RECORD
    sa_handler  : SignalHandler;        (* signal handler *)
    sa_mask     : sigset_t;             (* signals to block while in handler *)
    sa_flags    : int;                  (* signal action flags *)
  END;

  struct_sigaction_star = UNTRACED REF struct_sigaction;
  const_struct_sigaction_star = struct_sigaction_star;

CONST
 (* Valid flags defined for sa_flags field of sigaction structure. *)
  SA_ONSTACK     = 16_0001;   (* run on special signal stack *)
  SA_RESTART     = 16_0002;   (* restart system calls on sigs *)
  SA_RESETHAND   = 16_0004;   (* reset to SIG_DFL when taking signal *)
  SA_NOCLDSTOP   = 16_0008;   (* do not generate SIGCHLD on child stop *)
  SA_NODEFER     = 16_0010;   (* don't mask the signal we're delivering *)
  SA_NOCLDWAIT   = 16_0020;   (* do not generate zombies on unwaited child *)

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
 *  ???? (ow)
 *)

TYPE
  struct_sigcontext = RECORD
    sc_gs: int;
    sc_fs: int;
    sc_es: int;
    sc_ds: int;
    sc_edi: int;
    sc_esi: int;
    sc_ebp: int;
    sc_ebx: int;
    sc_edx: int;
    sc_ecx: int;
    sc_eax: int;

    sc_eip: int;
    sc_cs: int;
    sc_eflags: int;
    sc_esp: int;
    sc_ss: int;

    sc_onstack: int;
    sc_mask13: int;

    sc_trapno: int;
    sc_err: int;

    sc_mask: sigset_t;
  END;

(* Do not modifiy these variables *)
VAR (*CONST*)
  BADSIG, SIG_ERR, SIG_DFL, SIG_IGN, SIG_HOLD: SignalHandler;

<*EXTERNAL*>
PROCEDURE kill(pid: pid_t; sig: int): int;

<*EXTERNAL "__sigemptyset14"*>
PROCEDURE sigemptyset(VAR set: sigset_t): int;

<*EXTERNAL "__sigfillset14"*>
PROCEDURE sigfillset(VAR set: sigset_t): int;

<*EXTERNAL "__sigaddset14"*>
PROCEDURE sigaddset(VAR set: sigset_t; signo: int): int;

<*EXTERNAL "__sigdelset14"*>
PROCEDURE sigdelset(VAR set: sigset_t; signo: int): int;

<*EXTERNAL "__sigismember14"*>
PROCEDURE sigismember(READONLY set: sigset_t; signo: int): int;

<*EXTERNAL "__sigaction14"*>
PROCEDURE sigaction (sig: int;  act: const_struct_sigaction_star;
                     oact: struct_sigaction_star): int;

<*EXTERNAL "__sigprocmask14"*>
PROCEDURE sigprocmask (how: int; set: const_sigset_t_star;
                       oldset: sigset_t_star) : int;

<*EXTERNAL "__sigpending14"*>
PROCEDURE sigpending (VAR set: sigset_t) : int;

<*EXTERNAL "__sigsuspend14"*>
PROCEDURE sigsuspend (READONLY set: sigset_t) : int;

END Usignal.
