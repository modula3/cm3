INTERFACE Csetjmp;

FROM Ctypes IMPORT int, double;

(* 200 bytes with 8 byte alignment *)
TYPE jmp_buf = ARRAY [0..24] OF double;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
