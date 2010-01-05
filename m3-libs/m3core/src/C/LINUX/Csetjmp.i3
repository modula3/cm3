(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT long, int, void_star;

TYPE 
  ptr_t = void_star;

  jmp_buf = RECORD
        bx, si, di: long;
        bp, sp, pc: ptr_t; END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
