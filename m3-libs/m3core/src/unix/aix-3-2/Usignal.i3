(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Nov 12 11:38:20 PST 1992 by muller        *)

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
  SIGABRT   =  6;      (* IOT instruction *)
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
  SIGMSG    =  27;     (* input data is in the HFT ring buffer *)
  SIGWINCH  =  28;     (* window size changes *)
  SIGPWR    =  29;     (* power failuer imminent (save your data) *)

  SIGLOST   =  29;     (* Sys-V rec lock: notify user upon server crash *)
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
  SIGSAK     = 63;	(* secure attention key *)

  (* System V definitions *)
  SIGCLD    = SIGCHLD;
  SIGIOT    = SIGABRT;



(* Signal vector "template" used in sigvec call. *)
TYPE

  (* The ANSI C standard specifies that a signal handler should return
     an integer.  AIX386 and IBMR2 follow the standard.  For IBRT and other
     systems, there is no return value.  Left the declaration of
     SignalHandler alone to avoid problems elsewhere. *)

  SignalHandler = PROCEDURE (sig, code: int;
                             scp: UNTRACED REF struct_sigcontext);

  struct_sigset = RECORD 
    losigs: u_long;
    hisigs: u_long; END;

  sigset_t = struct_sigset;

  struct_sigvec  = RECORD
    sv_handler: SignalHandler;     (* signal handler *)
    sv_mask:    int;               (* signal mask to apply *)
    sv_flags:   int;               (* see signal options below *) END;

CONST
  empty_sigset_t = sigset_t { losigs := 0, hisigs := 0};
  empty_sv_mask  : int = 0;

CONST
  SV_ONSTACK   = 16_0001;  (* take signal on signal stack *)
  SV_INTERRUPT = 16_0008;  (* do not restart system on signal return *)
  SA_RESTART   = 16_0008;
  SA_NOCLDSTOP = 16_0004;  (* Don't generate SIGCLD when children stop *)
  SV_OLDSIG    = 16_1000;  (* Emulate old signal() for POSIX *)

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
  struct_label_t = RECORD	(* kernel jump buffer *)
    prev: UNTRACED REF struct_label_t;	(* chain to previous *)
    iar: u_long;	(* resume address *)
    stack: u_long;	(* stack pointer *)
    toc: u_long;	(* toc pointer *)
    cr: u_long;  	(* non-volatile part of cr *)
    intpri: u_long;   	(* priority level of the process *)
    reg: ARRAY [1..19] OF u_long;  (* non-volative regs (13..31) *) END;

  struct_adspace_t = RECORD	(* address space mapping *)
    alloc: u_long;	(* allocation flags *)
    srval: ARRAY [0..15] OF u_long;	(* contents of all seg regs *) END;

  struct_mstsave = RECORD
    prev: UNTRACED REF struct_mstsave;	(* previous save area *)
    kjmpbuf: UNTRACED REF struct_label_t;	(* pointer to fixed context *)
    stackfix: ADDRESS;	(* stack fix *)
    intpri: CHAR;	(* interrupt priority *)
    backt: CHAR;	(* back-track flag *)
    rsvd: ARRAY [0..1] OF CHAR;	(* reserved *)
    curid: int;		(* copy of curid *)
    excp_type: int;	(* exception type for debugger *)
    iar: u_long;	(* instruction address register *)
    msr: u_long;	(* machine state register *)
    cr: u_long;		(* condition register *)
    lr: u_long;		(* link register *)
    ctr: u_long;	(* count register *)
    xer: u_long;	(* fixed pointer register *)
    mq: u_long;		(* multiply/quotient register *)
    tid: u_long;	(* tid register *)
    fpscr: u_long;	(* floating point status reg *)
    fpeu: CHAR;		(* floating point ever used *)
    fpinfo: CHAR;       (* floating point status flags *)
(* This padding may not satisfy padding requirements because of translation by Modula-3 - CEG *)
    pad: ARRAY [0..1] OF CHAR; (* res - pad to dword boundary *)  
    except: ARRAY [0..4] OF u_long; (* exception structure *)
    bus: ADDRESS;	(* I/O bus limit register, CSR *)
    o_iar: u_long;    (* old iar (for longjmp excpt) *)
    o_toc: u_long;    (* old toc (for longjmp excpt) *)
    o_arg1: u_long;   (* old arg1 (for longjmp excpt) *)
    excbranch: u_long;  (* if not NULL, address to branch *)
                      (* to on exception.  Used by *)
                      (* assembler routines for low *)
                      (* cost exception handling *)
    fpscrx: u_long;   (* software extension to fpscr *)
    cachealign: ARRAY [0..7] OF u_long; (* cache-align registers *)
    as: struct_adspace_t; 	(* segment registers *)
    gpr: ARRAY [0..31] OF u_long;	(* general purpose registers *)
    fpr: ARRAY [0..31] OF LONGREAL;	(* floating point registers *) END;    

  struct_jmpbuf = RECORD
    jmp_context: struct_mstsave; END;

  struct_sigcontext = RECORD
    sc_onstack: int;	(* sigstack state to restore *)
    sc_mask: sigset_t;	(* signal mask to restore *)
    u_error: int;	(* u_error to restore *)
    sc_jmpbuf: struct_jmpbuf; (* process context to restore *) END;

  struct_sigaction = RECORD
    sa_handler: SignalHandler;     (* signal handler *)
    sa_mask : sigset_t;  (* signals to block while in handler *)
    sa_flags : int;     (* signal action flags *)
  END;

 (* valid flags define for sa_flag field of sigaction structure  *)
CONST
  SA_ONSTACK     = 16_0001;   (* run on special signal stack *)
  SA_OLDSTYLE    = 16_0002;   (* old "unreliable" UNIX semantics *)
(*SA_RESTART     = 16_0008;*) (* restart system calls on sigs *)
  SA_NODUMP      = 16_0010;   (* termination by this sig does not use a 
                                 core file *)
  SA_PARTDUMP    = 16_0020;   (* create a partial dump for this signal *)
  SA_FULLDUMP    = 16_0040;   (* create a full dump (with data areas) *)
  SA_SIGSETSTYLE = 16_0080;   (* new system V sigset type semantics *)

(* Do not modifiy these variables *)

(* read-only *) VAR
  BADSIG, SIG_ERR, SIG_DFL, SIG_IGN: SignalHandler;

  (* BADSIG doesn't appear to exist on AIX/6000(IBMR2), defined to be
     SIG_ERR in <sys/signal.h> *)


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

<*EXTERNAL*>
PROCEDURE sigaction (sig: int; VAR act, oact: struct_sigaction): int;

<*EXTERNAL*>
PROCEDURE sigprocmask (how: int; VAR set, oset: sigset_t): int;

END Usignal.
