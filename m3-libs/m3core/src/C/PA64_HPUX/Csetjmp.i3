INTERFACE Csetjmp;

FROM Ctypes IMPORT int, double;

TYPE
  (* 640 bytes with 16 byte alignment if we can get it, else 8 byte alignment,
   which is the best we can ask for (front end internally uses 16 byte alignment) *)
  jmp_buf = RECORD
    opaque : ARRAY [1..80] OF double;
  END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
