(* This file is the same or almost the same across all platforms, and
little used. Let's factor the commonality and eliminate the dead. *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE
  jmp_buf = RECORD
    opaque : ARRAY [1..25] OF INTEGER;
  END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
