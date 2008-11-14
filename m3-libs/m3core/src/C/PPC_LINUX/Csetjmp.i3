(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;
IMPORT Usignal;

TYPE
  jmp_buf = BITS 16_1280 FOR RECORD
    regs: ARRAY [0..(64 + (12 * 4)) - 1] OF INTEGER;
    mask_was_saved: INTEGER;
    saved_mask: Usignal.sigset_t; 
  END;

<*EXTERNAL*> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
