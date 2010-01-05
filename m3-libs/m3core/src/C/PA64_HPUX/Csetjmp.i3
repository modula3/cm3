INTERFACE Csetjmp;

FROM Ctypes IMPORT int, double;

(* 640 bytes with 16 byte alignment if we can get it, else 8 byte alignment,
 which is the best we can ask for (front end internally uses 16 byte alignment) *)
TYPE jmp_buf = ARRAY [0..79] OF double;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
