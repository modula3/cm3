(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE INTERFACE Uexec;

FROM Ctypes IMPORT int, const_char_star, char_star_star, unsigned;
FROM Utypes IMPORT pid_t;

<*EXTERNAL*> PROCEDURE execvp(name: const_char_star; argv: char_star_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE execve(name : const_char_star; arg : char_star_star; envp : char_star_star) : int;

CONST
  (* options bits for waitpid. *)
  WNOHANG = 1;

TYPE
  w_A = BITS 32 FOR unsigned;

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

<*EXTERNAL*> PROCEDURE waitpid (pid: pid_t; status: UNTRACED REF w_T; options: int := 0): pid_t;

END Uexec.
