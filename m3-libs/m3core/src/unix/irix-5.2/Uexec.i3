(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Oct 12 11:55:45 PDT 1994 by ericv      *)
(*      modified on Tue Sep 21 16:04:20 PDT 1993 by kalsow     *)
(*      modified on Tue Mar 24 20:01:21 PST 1992 by muller     *)
(*      modified on Mon Jul  9 16:47:46 PDT 1990 by mjordan    *)

UNSAFE INTERFACE Uexec;

IMPORT Ctypes, Utypes, Uresource;

(* Some of the Unix library process control calls. This is not a complete
interface, and should be added to as needed *)

(* Remember that any of the calls which may return an error code in
'Uerror.errno' should be serialized by use of 'UnixMutex.errno'. *)

<*EXTERNAL*> 
PROCEDURE execv(
    name: Ctypes.char_star;
    argv: Ctypes.char_star_star)
    : Ctypes.int
    RAISES {};
    
<*EXTERNAL*> 
PROCEDURE execvp(
    name: Ctypes.char_star;
    argv: Ctypes.char_star_star)
    : Ctypes.int
    RAISES {};
    
<*EXTERNAL execve *> 
PROCEDURE exect(
    name: Ctypes.char_star;
    argv: Ctypes.char_star_star;
    envp: Ctypes.char_star_star)
    : Ctypes.int
    RAISES {};

(*
 Option bits for the second argument of wait3.  WNOHANG causes the
 wait to not hang if there are no stopped or terminated processes, rather
 returning an error indication in this case (pid==0).  WUNTRACED
 indicates that the caller should receive status about untraced children
 which stop due to signals.  If children are stopped and a wait without
 this option is done, it is as though they were still running... nothing
 about them is returned.
 *)
CONST
  WNOHANG	 = 8_100;	
  WUNTRACED	 = 8_004;	 (* for POSIX *)

  WEXITED          = 8_001;    
  WTRAPPED         = 8_002;   
  WSTOPPED         = 8_004;    (* wait for processes stopped by signals *)
  WCONTINUED       = 8_010;    (* wait for processes continued *)
  WNOWAIT          = 8_200;    (* non destructive form of wait *)

  WSTOPFLG                 = 8_177;
  WCONTFLG                 = 8_177777;
  WCOREFLAG                = 8_200;
  WSIGMASK                 = 8_177;


TYPE
  w_A = Ctypes.int;    (* w_status *)

  (* terminated process status *)
  w_T = RECORD
      w_Filler  : BITS 16 FOR [0..16_FFFF];
      w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
      w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
      w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
  END;

  (* stopped process status *)
  w_S = RECORD
      w_Filler  : BITS 16 FOR [0..16_FFFF];
      w_Stopsig : BITS  8 FOR [0..16_FF];  (* signal that stopped us *)
      w_Stopval : BITS  8 FOR [0..16_FF];  (* == W_STOPPED if stopped *)
  END;

  (* union wait is a union of the three types above.  We will use w_A
     in the declarations and do a LOOPHOLE when necessary *)
  w_A_star = UNTRACED REF w_A;

(*** wait, wait3, waitpid - wait for process to terminate ***)

<*EXTERNAL*> 
PROCEDURE wait (status: w_A_star): Utypes.pid_t;

<*EXTERNAL*>
PROCEDURE wait3 (status: w_A_star; options: Ctypes.int;
                 rusage: Uresource.struct_rusage_star): Utypes.pid_t;

<*EXTERNAL*>
PROCEDURE waitpid (pid: Utypes.pid_t; status: w_A_star; 
                   options: Ctypes.int): Utypes.pid_t;

END Uexec.
