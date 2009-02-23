INTERFACE Csetjmp;

FROM Ctypes IMPORT int, double;

TYPE
  (* 200 bytes with 8 byte alignment *)
  jmp_buf = RECORD
    opaque : ARRAY [1..25] OF double;
  END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
