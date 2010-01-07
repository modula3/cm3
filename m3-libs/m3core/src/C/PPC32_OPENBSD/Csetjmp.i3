INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

  (* 400 bytes with 4 byte alignment *)
TYPE jmp_buf = ARRAY [0..99] OF INTEGER;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
