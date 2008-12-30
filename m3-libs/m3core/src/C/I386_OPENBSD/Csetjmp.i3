INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE
  jmp_buf = RECORD
(* This information appears both here and in m3middle/Target.m3.
   Ideally it would only occur in one place. *)
    opaque : ARRAY [0..9] OF INTEGER;
  END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
