INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

 (* this is a sigsetjmp until further investigation; 16 for regular jmpbuf *)
TYPE jmp_buf = ARRAY [0..17] OF int;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
