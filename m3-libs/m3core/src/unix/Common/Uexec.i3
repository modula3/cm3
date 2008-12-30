(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Uexec;

FROM Ctypes IMPORT int, const_char_star, char_star_star;

<*EXTERNAL*> PROCEDURE execv (name: const_char_star; argv: char_star_star): int RAISES {};
<*EXTERNAL*> PROCEDURE execvp(name: const_char_star; argv: char_star_star): int RAISES {};
<*EXTERNAL*> PROCEDURE execve(name: const_char_star; argv: char_star_star; envp: char_star_star): int;

END Uexec.
