(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk     *)
(*      modified on Tue Mar 24 20:01:29 PST 1992 by muller     *)
(*      modified on Mon Jul  9 16:47:46 PDT 1990 by mjordan    *)

UNSAFE INTERFACE Uexec;

FROM Utypes IMPORT pid_t;
FROM Ctypes IMPORT char_star, char_star_star, int, unsigned_int, void_star;
FROM Uresource IMPORT struct_rusage_star;

(* Some of the Unix library process control calls. This is not a complete
interface, and should be added to as needed *)

<*EXTERNAL*> PROCEDURE execv(name: char_star; argv: char_star_star): int;
    
<*EXTERNAL*> PROCEDURE execvp(name: char_star; argv: char_star_star): int;
    
<*EXTERNAL*> PROCEDURE exect(name: char_star; argv: char_star_star;
                             envp: char_star_star): int;

(* options bits for the second argument of wait3. *)
CONST
  WNOHANG = 1;			 (* dont hang in wait *)
  WUNTRACED = 2;		 (* tell about stopped, untraced children *)

TYPE
  w_A = unsigned_int;

  (* terminated process status *)
  w_T = RECORD
      w_Filler  : BITS 16 FOR [0..16_FFFF];
      w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
      w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
      w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
  END;

  (* M3 view of return code *)
  w_M3 = RECORD
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

  (* Wait queues (needed for Umsg.i3) *)
  wait_queue = RECORD
     task : void_star;			 (* : task_struct;  see below *)
     next : UNTRACED REF wait_queue;
   END;
   (* task_struct - use void_star because task_struct
     should never be touched and is very very hard to emulate in Modula-3
   *)

  wait_queue_star = UNTRACED REF wait_queue;

(*** wait, wait3, waitpid - wait for process to terminate ***)

<*EXTERNAL*> PROCEDURE wait (status: w_A_star): pid_t;

<*EXTERNAL*> PROCEDURE wait3 (status: w_A_star; options: int;
                              rusage: struct_rusage_star): pid_t;

<*EXTERNAL*> PROCEDURE wait4 (wpid: pid_t; status: w_A_star; options: int;
                              rusage: struct_rusage_star): pid_t;

<*EXTERNAL*> PROCEDURE waitpid (pid: pid_t; status: w_A_star;
                                options: int): pid_t;

END Uexec.
