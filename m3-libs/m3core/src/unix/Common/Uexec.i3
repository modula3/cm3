(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Uexec;

FROM Ctypes IMPORT const_int, int, const_char_star, char_star_star;
FROM Utypes IMPORT pid_t;

<*EXTERNAL "Uexec__execv"*>PROCEDURE execv (name: const_char_star; argv: char_star_star): int RAISES {};
<*EXTERNAL "Uexec__execvp"*>PROCEDURE execvp(name: const_char_star; argv: char_star_star): int RAISES {};
<*EXTERNAL "Uexec__execve"*>PROCEDURE execve(name: const_char_star; argv: char_star_star; envp: char_star_star): int;

(* compat with Usem usage *)
TYPE wait_queue_star = ADDRESS;

<*EXTERNAL "Uexec__WNOHANG"*> VAR WNOHANG: const_int; (* do not hang in wait *)

<*EXTERNAL "Uexec__waitpid"*>PROCEDURE waitpid (pid: pid_t; status: UNTRACED REF int; options: int): pid_t;

<*EXTERNAL "Uexec__RepackStatus"*>
PROCEDURE RepackStatus(VAR status: int);

END Uexec.
