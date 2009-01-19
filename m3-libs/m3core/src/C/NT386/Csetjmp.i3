(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Feb  1 20:36:42 PST 1993 by mjordan    *)
(*      modified on Fri Oct  9 17:44:27 PDT 1992 by muller     *)
(*      modified on Sun Jul 12, 1992 by thomas@mw.lpc.ethz.ch  *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE
  jmp_buf = RECORD
  (* For systems without user threads, the size of this need not be declared correctly.
     It is at the end of a record allocated by the compiler that the runtime merely needs
     the address of. The runtime does need to know the size. The compiler does not know the
     size. In future, the compiler should feed that size to here. *)
    opaque : INTEGER;
  END;

<*EXTERNAL "longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
