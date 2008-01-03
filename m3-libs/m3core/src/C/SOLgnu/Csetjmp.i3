(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking    *)
(*      modified on Fri Apr 29 09:55:36 PDT 1994 by kalsow     *)
(*      modified on Fri Sep 10 14:34:19 PDT 1993 by muller     *)

INTERFACE Csetjmp;		(* for SOLgnu *)

FROM Ctypes IMPORT int;

CONST
  SIGJBLEN = 19;
  JB_FLAGS = 0;
  JB_SP = 1;
  JB_PC = 2;
  JB_FP = 3;
  JB_I7 = 4;

TYPE jmp_buf = ARRAY [0..SIGJBLEN - 1] OF int;

<*EXTERNAL "sigsetjmp" *>
PROCEDURE setjmp (VAR env: jmp_buf; savemask := 1): int;
<*EXTERNAL "siglongjmp" *>
PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "sigsetjmp" *>
PROCEDURE usetjmp (VAR env: jmp_buf; savemask := 0): int;
<*EXTERNAL "siglongjmp" *>
PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
