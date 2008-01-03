(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Apr 30 16:25:40 PDT 1993 by muller         *)

INTERFACE Csetjmp;		(* for LINUX *)

FROM Ctypes IMPORT long, int, void_star;


TYPE 
  ptr_t = void_star;

  jmp_buf = RECORD
        bx, si, di: long;
        bp, sp, pc: ptr_t; END;

<*EXTERNAL "__setjmp" *> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "__setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
