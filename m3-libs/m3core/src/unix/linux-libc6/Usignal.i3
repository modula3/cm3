(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Mon Jan  5 11:11:07 GMT 1998 by rrw               *)
(*      modified on Fri Feb 24 15:18:21 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:58:12 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Tue Mar  2 17:18:02 PST 1993 by muller            *)


INTERFACE Usignal;

FROM Ctypes IMPORT int, void_star, int_star;
FROM Utypes IMPORT pid_t;
IMPORT Uucontext;

TYPE
  sigset_t = Uucontext.sigset_t;
  sigset_t_star = UNTRACED REF sigset_t;
  const_sigset_t_star = sigset_t_star;

(* <bits/signum.h> *)

(* Fake signal functions.  *)
VAR (*CONST*) (* Do not modifiy these variables *)
  SIG_ERR, SIG_DFL, SIG_IGN: SignalActionHandler;

(* Signals.  *)
CONST
  SIGHUP    =  1;      (* Hangup (POSIX).  *)
  SIGINT    =  2;      (* Interrupt (ANSI).  *)
  SIGQUIT   =  3;      (* Quit (POSIX).  *)
  SIGILL    =  4;      (* Illegal instruction (ANSI).  *)
  SIGTRAP   =  5;      (* Trace trap (POSIX).  *)
  SIGABRT   =  6;      (* Abort (ANSI).  *)
  SIGIOT    =  6;      (* IOT trap (4.2 BSD).  *)
  SIGBUS    =  7;      (* BUS error (4.2 BSD).  *)
  SIGFPE    =  8;      (* Floating-point exception (ANSI).  *)
  SIGKILL   =  9;      (* Kill, unblockable (POSIX).  *)
  SIGUSR1   = 10;      (* User-defined signal 1 (POSIX).  *)
  SIGSEGV   = 11;      (* Segmentation violation (ANSI).  *)
  SIGUSR2   = 12;      (* User-defined signal 2 (POSIX).  *)
  SIGPIPE   = 13;      (* Broken pipe (POSIX).  *)
  SIGALRM   = 14;      (* Alarm clock (POSIX).  *)
  SIGTERM   = 15;      (* Termination (ANSI).  *)
  SIGSTKFLT = 16;      (* Stack fault.  *)
  SIGCLD    = SIGCHLD; (* Same as SIGCHLD (System V).  *)
  SIGCHLD   = 17;      (* Child status has changed (POSIX).  *)
  SIGCONT   = 18;      (* Continue (POSIX).  *)
  SIGSTOP   = 19;      (* Stop, unblockable (POSIX).  *)
  SIGTSTP   = 20;      (* Keyboard stop (POSIX).  *)
  SIGTTIN   = 21;      (* Background read from tty (POSIX).  *)
  SIGTTOU   = 22;      (* Background write to tty (POSIX).  *)
  SIGURG    = 23;      (* Urgent condition on socket (4.2 BSD).  *)
  SIGXCPU   = 24;      (* CPU limit exceeded (4.2 BSD).  *)
  SIGXFSZ   = 25;      (* File size limit exceeded (4.2 BSD).  *)
  SIGVTALRM = 26;      (* Virtual alarm clock (4.2 BSD).  *)
  SIGPROF   = 27;      (* Profiling alarm clock (4.2 BSD).  *)
  SIGWINCH  = 28;      (* Window size change (4.3 BSD, Sun).  *)
  SIGPOLL   = SIGIO;   (* Pollable event occurred (System V).  *)
  SIGIO     = 29;      (* I/O now possible (4.2 BSD).  *)
  SIGPWR    = 30;      (* Power failure restart (System V).  *)
  SIGSYS    = 31;      (* Bad system call.  *)
  SIGUNUSED = 31;

  NSIG = 65;  (* Biggest signal number + 1 (including real-time signals).  *)

VAR (*CONST*)
  SIGRTMIN, SIGRTMAX: int;

(* Type of a signal handler.  *)
TYPE
  SignalHandler = PROCEDURE (sig: int);

(* Set the handler for the signal SIG to HANDLER, returning the old
   handler, or SIG_ERR on error.
   By default `signal' has the BSD semantic.  *)
<*EXTERNAL*> PROCEDURE signal(sig: int; handler: SignalHandler): SignalHandler;

(* Send signal SIG to process number PID.  If PID is zero,
   send SIG to all processes in the current process's process group.
   If PID is < -1, send SIG to all processes in process group - PID.  *)
<*EXTERNAL*> PROCEDURE kill (pid: pid_t; sig: int): int;

(* Send SIG to all processes in process group PGRP.
   If PGRP is zero, send SIG to all processes in
   the current process's process group.  *)
<*EXTERNAL*> PROCEDURE killpg (pgrp: pid_t; sig: int): int;

(* Raise signal SIG, i.e., send SIG to yourself.  *)
<*EXTERNAL*> PROCEDURE raise (sig: int): int;

(* Set the mask of blocked signals to MASK,
   wait for a signal to arrive, and then restore the mask.  *)
<*EXTERNAL*> PROCEDURE sigpause (mask: int): int;

(* <bits/siginfo.h> *)

TYPE
  sigval_t = int;
  siginfo_t_star = void_star;		 (* it's a complicated union *)
  siginfo_t_fault = RECORD
    si_signo: int;              (* Signal number.  *)
    si_errno: int;              (* If non-zero, an errno value associated with
                                   this signal, as defined in <errno.h>.  *)
    si_code: int;               (* Signal code.  *)
    si_addr: void_star;		(* Faulting insn/memory ref. *)
  END;
  siginfo_t_fault_star = UNTRACED REF siginfo_t_fault;

(* Clear all signals from SET.  *)
<*EXTERNAL*> PROCEDURE sigemptyset (set: sigset_t_star): int;

(* Set all signals in SET.  *)
<*EXTERNAL*> PROCEDURE sigfillset (set: sigset_t_star): int;

(* Add SIGNO to SET.  *)
<*EXTERNAL*> PROCEDURE sigaddset (set: sigset_t_star; signo: int): int;

(* Remove SIGNO from SET.  *)
<*EXTERNAL*> PROCEDURE sigdelset (set: sigset_t_star; signo: int): int;

(* Return 1 if SIGNO is in SET, 0 if not.  *)
<*EXTERNAL*> PROCEDURE sigismember (set: const_sigset_t_star; signo: int): int;

(* <bits/sigaction.h> *)

(* Structure describing the action to be taken when a signal arrives.  *)
TYPE
  SignalActionHandler = PROCEDURE (sig: int;
                                   sip: siginfo_t_star;
                                   uap: Uucontext.ucontext_t_star);
  SignalRestoreHandler = PROCEDURE ();
  struct_sigaction = RECORD
    (* Signal handler.  *)
    sa_sigaction: SignalActionHandler;  (* signal handler *)
    sa_mask     : sigset_t;             (* signals to block while in handler *)
    sa_flags    : int;                  (* signal action flags *)
    sa_restorer : SignalRestoreHandler; (* restores interrupted state *)
  END;
  struct_sigaction_star = UNTRACED REF struct_sigaction;
  const_struct_sigaction_star = struct_sigaction_star;

(* Bits in `sa_flags'.  *)
CONST
  SA_NOCLDSTOP =  1;	      (* Don't send SIGCHLD when children stop.  *)
  SA_NOCLDWAIT =  2;	      (* Don't create zombie on child death.  *)
  SA_SIGINFO   =  4;	      (* Invoke signal-catching function with
				 three arguments instead of one.  *)

  SA_ONSTACK   = 16_08000000; (* Use signal stack by using `sa_restorer'. *)
  SA_RESTART   = 16_10000000; (* Restart syscall on signal return.  *)
  SA_NODEFER   = 16_40000000; (* Don't automatically block the signal when
				    its handler is being executed.  *)
  SA_RESETHAND = 16_80000000; (* Reset to SIG_DFL on entry to handler.  *)

  SA_INTERRUPT = 16_20000000; (* Historical no-op.  *)

(* Some aliases for the SA_ constants.  *)
CONST
  SA_NOMASK  = SA_NODEFER;
  SA_ONESHOT = SA_RESETHAND;
  SA_STACK   = SA_ONSTACK;

(* Values for the HOW argument to `sigprocmask'.  *)
CONST
  SIG_BLOCK   = 0;			(* Block signals.  *)
  SIG_UNBLOCK = 1;			(* Unblock signals.  *)
  SIG_SETMASK = 2;			(* Set the set of blocked signals.  *)

(* Get and/or change the set of blocked signals.  *)
<*EXTERNAL*> PROCEDURE sigprocmask (how: int; set: const_sigset_t_star;
                                    oset: sigset_t_star): int;

(* Change the set of blocked signals to SET,
   wait until a signal arrives, and restore the set of blocked signals. *)
<*EXTERNAL*> PROCEDURE sigsuspend (set: sigset_t_star): int;

(* Get and/or set the action for signal SIG.  *)
<*EXTERNAL*> PROCEDURE sigaction (sig: int; act: const_struct_sigaction_star;
                                  oact: struct_sigaction_star): int;

(* Put in SET all signals that are blocked and waiting to be delivered.  *)
<*EXTERNAL*> PROCEDURE sigpending (set: sigset_t_star): int;

(* Select any of pending signals from SET or wait for any to arrive.  *)
<*EXTERNAL*> PROCEDURE sigwait (set: const_sigset_t_star; sig: int_star): int;

(* The following functions are used internally in the C library and in              
   other code which need deep insights.  *)

(* Return number of available real-time signal with highest priority.  *)
<*EXTERNAL "__libc_current_sigrtmin" *> PROCEDURE libc_current_sigrtmin(): int;
(* Return number of available real-time signal with lowest priority.  *)
<*EXTERNAL "__libc_current_sigrtmax" *> PROCEDURE libc_current_sigrtmax(): int;

END Usignal.
