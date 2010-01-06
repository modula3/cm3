INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

(* 88 bytes with 8 byte alignment *)
TYPE jmp_buf = ARRAY [0..10] OF INTEGER;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
