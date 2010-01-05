INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..82] OF INTEGER;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
