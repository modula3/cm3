INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

(* 10*4: 40 bytes with 4 align *)

TYPE jmp_buf = ARRAY [0..9] OF INTEGER;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
