(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Uucontext;

FROM Ctypes IMPORT int;

(*CONST*)
VAR
    context_t_size: int; (* VAR context := NEW(UNTRACED REF? ARRAY OF CHAR, Uucontext.context_t_size); *)

TYPE
    ucontext_t_star = ADDRESS;

PROCEDURE set_stack(a: ucontext_t_star; Start: ADDRESS; Size: INTEGER);
<*EXTERNAL*>PROCEDURE getcontext(a: ucontext_t_star);
<*EXTERNAL*>PROCEDURE setcontext(a: ucontext_t_star): int;
<*EXTERNAL*>PROCEDURE makecontext(a: ucontext_t_star; b: PROCEDURE (); argc: int := 0);
<*EXTERNAL*>PROCEDURE swapcontext(a, b: ucontext_t_star): int;

END Uucontext.
