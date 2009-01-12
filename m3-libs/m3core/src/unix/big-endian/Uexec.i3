(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

<*EXTERNAL*> INTERFACE Uexec;

FROM Ctypes IMPORT int, const_char_star, char_star_star, unsigned_int;
FROM Utypes IMPORT pid_t;

PROCEDURE execv (name: const_char_star; argv: char_star_star): int RAISES {};
PROCEDURE execvp(name: const_char_star; argv: char_star_star): int RAISES {};
PROCEDURE execve(name: const_char_star; argv: char_star_star; envp: char_star_star): int;

(* options bits for waitpid *)
CONST
  WNOHANG = 0; (* do not hang in wait -- but make it 0 -- kernel threads *)

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
      w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
      w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
      w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
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

PROCEDURE waitpid (pid: pid_t; status: w_A_star; options: int): pid_t;

END Uexec.
