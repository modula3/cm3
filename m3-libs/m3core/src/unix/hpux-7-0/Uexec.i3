(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Sep 21 16:01:37 PDT 1993 by kalsow     *)
(*      modified on Tue Mar 24 20:01:18 PST 1992 by muller     *)
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
    
<*EXTERNAL*> 
PROCEDURE exect(
    name: Ctypes.char_star;
    argv: Ctypes.char_star_star;
    envp: Ctypes.char_star_star)
    : Ctypes.int
    RAISES {};

(* options bits for the second argument of wait3. *)
CONST
  WNOHANG = 1;			 (* dont hang in wait *)
  WUNTRACED = 2;		 (* tell about stopped, untraced children *)

TYPE
  w_A = Ctypes.unsigned_int;

  (* terminated process status *)
  w_T = RECORD
      w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
      w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
      w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
      w_Filler  : BITS 16 FOR [0..16_FFFF]; END;

  (* stopped process status *)
  w_S = RECORD
      w_Stopval : BITS  8 FOR [0..16_FF];  (* == W_STOPPED if stopped *)
      w_Stopsig : BITS  8 FOR [0..16_FF];  (* signal that stopped us *)
      w_Filler  : BITS 16 FOR [0..16_FFFF]; END;

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
