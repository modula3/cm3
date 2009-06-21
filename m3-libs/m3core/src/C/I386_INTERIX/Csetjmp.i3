INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE
 (* this is a sigsetjmp until further investigation; 16 for regular jmpbuf *)
  jmp_buf = RECORD
    opaque : ARRAY [1..18] OF int;
  END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
