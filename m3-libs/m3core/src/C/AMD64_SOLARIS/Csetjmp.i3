INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

(* 8*8: 64 bytes with 8 align *)
TYPE jmp_buf = ARRAY [0..7] OF INTEGER;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
