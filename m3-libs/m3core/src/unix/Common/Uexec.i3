(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

<*EXTERNAL*> INTERFACE Uexec;

FROM Ctypes IMPORT int, const_char_star, char_star_star;
FROM Utypes IMPORT pid_t;

PROCEDURE execv (name: const_char_star; argv: char_star_star): int RAISES {};
PROCEDURE execvp(name: const_char_star; argv: char_star_star): int RAISES {};
PROCEDURE execve(name: const_char_star; argv: char_star_star; envp: char_star_star): int;

(* compat with Usem usage *)
TYPE wait_queue_star = ADDRESS;

<*EXTERNAL "Uexec_WNOHANG"*> VAR WNOHANG: int; (* do not hang in wait *)

PROCEDURE waitpid (pid: pid_t; status: UNTRACED REF int; options: int): pid_t;

<*EXTERNAL "Uexec_RepackStatus"*> PROCEDURE RepackStatus(VAR status: int);

END Uexec.
