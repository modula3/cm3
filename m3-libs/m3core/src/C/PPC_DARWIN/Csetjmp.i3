(* Copyright according to COPYRIGHT-CMASS. *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..(26 + 18*2 + 129 + 1) - 1] OF int;

<*EXTERNAL "_longjmp"  *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
