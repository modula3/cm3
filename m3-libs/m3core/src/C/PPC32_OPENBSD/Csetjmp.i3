INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE
  (* jmp_buf: 400 bytes with 4 byte alignment; should suffice
   * sigjmp_buf: 404 bytes with 4 byte alignment
   * what we do: 408 bytes with 8 byte alignment
   *)
  jmp_buf = RECORD
    opaque : ARRAY [0..50] OF LONGINT;
  END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
