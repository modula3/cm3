INTERFACE Csetjmp;

FROM Ctypes IMPORT int, double;

TYPE
  (* 0x190 (400) bytes with 8 byte alignment *)
  jmp_buf = RECORD
    opaque : ARRAY [1..50] OF LONGINT;
  END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
