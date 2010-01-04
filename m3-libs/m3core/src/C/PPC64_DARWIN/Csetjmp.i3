(* Copyright according to COPYRIGHT-CMASS. *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

CONST JBLEN = 26*2 + 18*2 + 129 + 1;

TYPE jmp_buf = ARRAY [0..JBLEN - 1] OF int;

<*EXTERNAL "_longjmp"  *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
