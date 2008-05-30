(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* This file can likely be made identical for all Posix systems. *)

INTERFACE Uexec;

FROM Ctypes IMPORT int, const_char_star, char_star_star, unsigned;
FROM Utypes IMPORT pid_t;

<*EXTERNAL*> PROCEDURE execv(name: const_char_star; argv: char_star_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE execvp(name: const_char_star; argv: char_star_star) : int RAISES {};

CONST
  WNOHANG = 1;

TYPE
  w_A = unsigned;

  (* various views of a union *)

  w_T = RECORD
      w_Termsig : BITS  7 FOR [0..16_7F];
      w_Coredump: BITS  1 FOR [0..16_01];
      w_Retcode : BITS  8 FOR [0..16_FF];
      w_Filler  : BITS 16 FOR [0..16_FFFF];
  END;

  w_M3 = RECORD
      w_Retcode : BITS  8 FOR [0..16_FF];
      w_Termsig : BITS  7 FOR [0..16_7F];
      w_Coredump: BITS  1 FOR [0..16_01];
      w_Filler  : BITS 16 FOR [0..16_FFFF];
  END;

  (* stopped process status *)
  (* w_S = RECORD
      w_Stopval : BITS  8 FOR [0..16_FF];
      w_Stopsig : BITS  8 FOR [0..16_FF];
      w_Filler  : BITS 16 FOR [0..16_FFFF];
  END; *)

  w_A_star = UNTRACED REF w_A;

  (* union wait is a union of the three types above.  We will use w_A
     in the declarations and do a LOOPHOLE when necessary *)

<*EXTERNAL*> PROCEDURE waitpid (pid: pid_t; status: w_A_star; options: int): pid_t;

END Uexec.
