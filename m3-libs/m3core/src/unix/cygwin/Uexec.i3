(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jan  5 09:41:49 GMT 1998 by rrw        *)
(*      modified on Fri Apr 29 14:19:35 PDT 1994 by kalsow     *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk     *)
(*      modified on Tue Mar 24 20:01:29 PST 1992 by muller     *)
(*      modified on Mon Jul  9 16:47:46 PDT 1990 by mjordan    *)

UNSAFE INTERFACE Uexec;

FROM Ctypes IMPORT int, const_char_star, char_star_star, unsigned_int;
FROM Utypes IMPORT pid_t;

<*EXTERNAL*> 
PROCEDURE execvp(
    name: const_char_star;
    argv: char_star_star)
    : int
    RAISES {};

<*EXTERNAL*>
PROCEDURE execve(
    name : const_char_star;
    arg : char_star_star;
    envp : char_star_star) : int;

(* options bits for the second argument of wait3. *)
CONST
  WNOHANG = 1;			 (* dont hang in wait *)

TYPE
  w_A = BITS 32 FOR unsigned_int;

  (* terminated process status *)
  w_T = BITS 32 FOR RECORD
      w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
      w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
      w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
      w_Filler  : BITS 16 FOR [0..16_FFFF];
  END;

  (* M3 view of return code *)
  w_M3 = BITS 32 FOR RECORD
      w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
      w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
      w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
      w_Filler  : BITS 16 FOR [0..16_FFFF];
  END;

  (* stopped process status *)
  (* w_S = BITS 32 FOR RECORD
      w_Stopval : BITS  8 FOR [0..16_FF];  (* == W_STOPPED if stopped *)
      w_Stopsig : BITS  8 FOR [0..16_FF];  (* signal that stopped us *)
      w_Filler  : BITS 16 FOR [0..16_FFFF];
     END;
  *)

  (* union wait is a union of the three types above. We could use w_A
     in the declarations and do a LOOPHOLE when necessary *)
  w_A_star = UNTRACED REF w_A;

(*** waitpid - wait for process to terminate ***)

<*EXTERNAL*>
PROCEDURE waitpid (pid: pid_t; status: UNTRACED REF w_T;
                   options: int := 0): pid_t;

END Uexec.
