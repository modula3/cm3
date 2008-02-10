(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Mon Jan  5 11:11:07 GMT 1998 by rrw               *)
(*      modified on Fri Feb 24 15:18:21 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:58:12 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Tue Mar  2 17:18:02 PST 1993 by muller            *)

(* $Id: Usignal.i3,v 1.15 2008-02-10 04:36:18 jkrell Exp $ *)

(* This file was generated from Usignal.i3.c. Do not edit it. *)

INTERFACE Usignal;

FROM Ctypes IMPORT int, unsigned_int, unsigned_short_int, unsigned_long_int;
IMPORT Uucontext;

(*** <signal.h> ***)

CONST
  (* SIGHUP = 16_00000001; *) (* hangup *)
  SIGINT = 16_00000002; (* interrupt *)
  SIGQUIT = 16_00000003; (* quit *)
  (* SIGILL = 16_00000004; *) (* illegal instruction (not reset when caught) *)
  (* SIGTRAP = 16_00000005; *) (* trace trap (not reset when caught) *)
  (* SIGEMT = 16_00000007; *) (* EMT instruction *)
  (* SIGBUS = 16_0000000a; *) (* bus error *)
  (* BUS_HWERR	 = 16_00000001; *) (* misc hardware error (e.g. timeout) *)
  (* BUS_ALIGN	 = 16_00000002; *) (* hardware alignment error *)
  (* SIGFPE = 16_00000008; *) (* floating point exception *)
  (* FPE_INTDIV_TRAP = 16_00000014; *) (* integer divide by zero *)
  (* FPE_INTOVF_TRAP = 16_00000015; *) (* integer overflow *)
  (* FPE_FLTOPERR_TRAP = 16_00000001; *) (* [floating operand error] *)
  (* FPE_FLTDEN_TRAP = 16_00000002; *) (* [floating denormalized operand] *)
  (* FPE_FLTDIV_TRAP = 16_00000003; *) (* [floating divide by zero] *)
  (* FPE_FLTOVF_TRAP = 16_00000004; *) (* [floating overflow] *)
  (* FPE_FLTUND_TRAP = 16_00000005; *) (* [floating underflow] *)
  (* FPE_FLTINEX_TRAP = 16_00000006; *) (* [floating inexact result] *)
  (* FPE_UUOP_TRAP = 16_00000007; *) (* [floating undefined opcode] *)
  (* FPE_DATACH_TRAP = 16_00000008; *) (* [floating data chain exception] *)
  (* FPE_FLTSTK_TRAP = 16_00000010; *) (* [floating stack fault] *)
  (* FPE_FPA_ENABLE = 16_00000011; *) (* [FPA not enabled] *)
  (* FPE_FPA_ERROR = 16_00000012; *) (* [FPA arithmetic exception] *)
  SIGKILL = 16_00000009; (* kill (cannot be caught or ignored) *)
  (* SIGUSR1 = 16_0000000a; *) (* User signal 1 (from SysV) *)
  (* SIGSEGV = 16_0000000b; *) (* segmentation violation *)
  (* SEGV_NOMAP = 16_00000003; *) (* no mapping at the fault address *)
  (* SEGV_PROT = 16_00000004; *) (* access exceeded protections *)
  (* SEGV_OBJERR = 16_00000005; *) (* object returned errno value *)
  (* SIGSYS = 16_0000000c; *) (* bad argument to system call *)
  SIGUSR2 = 16_0000001f; (* User signal 2 (from SysV) *)
  (* SIGPIPE = 16_0000000d; *) (* write on a pipe with no one to read it *)
  (* SIGALRM = 16_0000000e; *) (* alarm clock *)
  SIGTERM = 16_0000000f; (* software termination signal from kill *)
  (* SIGSTKFLT = 16_00000010; *) 
  (* SIGCHLD = 16_00000014; *) (* to parent on child stop or exit *)
  (* SIGCONT = 16_00000013; *) (* continue a stopped process *)
  (* SIGSTOP = 16_00000011; *) (* sendable stop signal not from tty *)
  (* SIGTSTP = 16_00000012; *) (* stop signal from tty *)
  (* SIGTTIN = 16_00000015; *) (* to readers pgrp upon background tty read *)
  (* SIGTTOU = 16_00000016; *) (* like TTIN for output if (tp->t_local&LTOSTOP) *)
  (* SIGIO = 16_00000017; *) (* input/output possible signal *)
  (* SIGURG = SIGIO; *) (* urgent condition on IO channel *)
  (* SIGPOLL = SIGIO; *) 
  (* SIGXCPU = 16_00000018; *) (* exceeded CPU time limit *)
  (* SIGXFSZ = 16_00000019; *) (* exceeded file size limit *)
  SIGVTALRM = 16_0000001a; (* virtual time alarm *)
  (* SIGPROF = 16_0000001b; *) (* profiling time alarm *)
  (* SIGWINCH = 16_0000001c; *) (* window size changes *)
  (* SIGLOST = 16_0000001d; *) (* Sys-V rec lock: notify user upon server crash *)

  (* System V definitions *)
  (* SIGCLD = SIGCHLD; *)
  SIGABRT = 16_00000006;

CONST
  SIGSET_NWORDS = 16_00000001;

(* Signal vector "template" used in sigaction call. *)
TYPE
  SignalHandler = PROCEDURE (sig: int;
                             scp: struct_sigcontext;
                             code: int);

  sigset_t = RECORD
    val : ARRAY [0 .. SIGSET_NWORDS - 1] OF INTEGER;
  END;
  sigset_t_star = UNTRACED REF sigset_t;

  siginfo_t = RECORD
    opaque:  ARRAY [0..16_00000025] OF int;
  END;

  siginfo_t_star = UNTRACED REF siginfo_t;

CONST
  empty_sigset_t : sigset_t = sigset_t{ARRAY [0..SIGSET_NWORDS - 1] 
      OF INTEGER{0, ..}};
  empty_sv_mask  : sigset_t = sigset_t{ARRAY [0..SIGSET_NWORDS - 1] 
      OF INTEGER{0, ..}};

CONST
  (* SV_ONSTACK = 16_00000001 ;*)  (* take signal on signal stack *)
  (* SV_INTERRUPT = 16_00000002 ;*)  (* do not restart system on signal return *)
  (* SV_OLDSIG is not provided (explicitly, anyway) by glibc2 *)
  (* SV_OLDSIG = 16_00001000 ;*)  (* Emulate old signal() for POSIX *)
  (* SV_RESETHAND = 16_00000004 ;*)  (* Reset handler to SIG_DFL on receipt *)

  (* Defines for sigprocmask() call. POSIX. *)
  (* SIG_BLOCK = 16_00000001 ;*) (* Add these signals to block mask *)
  (* SIG_UNBLOCK = 16_00000002 ;*) (* Remove these signals from block mask *)
  (* SIG_SETMASK = 16_00000000 ;*) (* Set block mask to this mask *)

TYPE
  SignalActionHandler = PROCEDURE (sig: int;
                                   sip: siginfo_t_star;
                                   uap: Uucontext.ucontext_t_star);
  SignalRestoreHandler = PROCEDURE ();

  struct_sigaction = RECORD
    sa_sigaction: SignalActionHandler;  (* signal handler *)
    sa_mask     : sigset_t;             (* signals to block while in handler *)
    sa_flags    : int;                  (* signal action flags *)
    sa_restorer : SignalRestoreHandler; (* restores interrupted state *)
  END;

  struct_sigaction_star = UNTRACED REF struct_sigaction;

 (* valid flags define for sa_flag field of sigaction structure  *)
CONST
  SA_NOCLDSTOP = 16_00000001; (* Don't generate SIGCLD when children stop *)
  SA_RESTART = 16_10000000;
  SA_NOMASK = 16_40000000;
  SA_ONESHOT = 16_80000000;


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
  (* There seems to be no simple corresponding structure under Linux - 
      use the structure in Csetjmp.i3 instead *)
  struct_sigcontext = RECORD
      gs, gsh: unsigned_short_int;
      fs, fsh: unsigned_short_int;
      es, esh: unsigned_short_int;
      ds, dsh: unsigned_short_int;
      edi: unsigned_long_int;
      esi: unsigned_long_int;
      ebp: unsigned_long_int;
      esp: unsigned_long_int;
      ebx: unsigned_long_int;
      edx: unsigned_long_int;
      ecx: unsigned_long_int;
      eax: unsigned_long_int;
      trapno: unsigned_long_int;
      err: unsigned_long_int;
      eip: unsigned_long_int;
      cs, csh: unsigned_short_int;
      eflags: unsigned_long_int;
      esp_at_signal: unsigned_long_int;
      ss, ssh: unsigned_short_int;
      i387: unsigned_long_int; (* Actually a struct _fpstate * *)
      oldmask: unsigned_long_int;
      cr2: unsigned_long_int;
    END;
  
 struct_fpreg = RECORD
   significand : ARRAY [0..3] OF unsigned_short_int;
   exponent : unsigned_short_int;
 END;

 struct_fpstate = RECORD
   cw : unsigned_long_int;
   sw : unsigned_long_int;
   tag : unsigned_long_int;
   ipoff : unsigned_long_int;
   cssel : unsigned_long_int;
   dataoff: unsigned_long_int;
   datasel : unsigned_long_int;
   st : ARRAY [0..7] OF struct_fpreg;
   status : unsigned_long_int;
 END;  


(* Do not modifiy these variables *)
VAR (*CONST*)
  BADSIG, SIG_ERR, SIG_DFL, SIG_IGN, SIG_HOLD: SignalActionHandler;


(* Convert a signal number to a mask suitable for sigblock(). *)
<*INLINE*> PROCEDURE sigmask (n: int): int;


(*** kill(2) - send signal to a process ***)

<*EXTERNAL*> PROCEDURE kill (pid, sig: int): int;


(*** killpg(2) - send signal to a process or process group ***)

<*EXTERNAL*> PROCEDURE killpg (pgrp, sig: int): int;


(*** sigblock(2) - block signals ***)

<*EXTERNAL*> PROCEDURE sigblock (mask: int): int;


(*** sigpause(2) - atomically release blocked signals and wait for
                   interrupt ***)

<*EXTERNAL*> PROCEDURE sigpause (sigmask: int): int;


(*** sigpending(2) - examine pending signals ***)

<*EXTERNAL*> PROCEDURE sigpending (VAR set: sigset_t): int;


(*** sigsetmask(2) - set current signal mask ***)

<*EXTERNAL*> PROCEDURE sigsetmask (mask: int): unsigned_int;


(*** sigstack(2) - set and/or get signal stack context ***)

<*EXTERNAL*> PROCEDURE sigstack (VAR ss, oss: struct_sigstack): int;


(*** sigaction(2) - software signal facilities ***)

<*EXTERNAL*>
PROCEDURE sigaction (sig: int;
                     READONLY act: struct_sigaction;
                     VAR oact: struct_sigaction): int;

(*** sigprocmask(2) - set the blocked signals ***)

<*EXTERNAL*>
PROCEDURE sigprocmask(how: int; set, oldset: sigset_t_star): int;

(*
PROCEDURE SigWord(sig : INTEGER) : INTEGER;
PROCEDURE SigMask(sig : INTEGER) : Word.T;
PROCEDURE SigIsMember(set : sigset_t; sig : INTEGER) : BOOLEAN;
PROCEDURE SigAddSet(set : sigset_t; sig : INTEGER) : sigset_t;
PROCEDURE SigDelSet(set : sigset_t; sig : INTEGER) : sigset_t;
*)

(* Change the set of blocked signals to SET,
   wait until a signal arrives, and restore the set of blocked signals. *)
<*EXTERNAL*> PROCEDURE sigsuspend (READONLY set: sigset_t): int;

(* Select any of pending signals from SET or wait for any to arrive.  *)
<*EXTERNAL*> PROCEDURE sigwait (READONLY set: sigset_t; VAR sig: int): int;

(* Remove SIGNO from SET.  *)
<*EXTERNAL*> PROCEDURE sigdelset (VAR set: sigset_t; signo: int): int;

(* Set all signals in SET.  *)
<*EXTERNAL*> PROCEDURE sigfillset (VAR set: sigset_t): int;

(* Bits in `sa_flags'.  *)
CONST
  SA_SIGINFO = 16_00000002; (* Invoke signal-catching function with
                               three arguments instead of one. *)

END Usignal.
