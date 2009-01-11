INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE
  jmp_buf = RECORD
    opaque : ARRAY [1..25] OF INTEGER;
  END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
