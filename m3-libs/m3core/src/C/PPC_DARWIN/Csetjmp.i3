(* Copyright according to COPYRIGHT-CMASS. *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

CONST
  JBLEN = 26 + 18*2 + 129 + 1;

TYPE 
  jmp_buf = ARRAY [0..JBLEN] OF int;

<*EXTERNAL "_longjmp"  *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
