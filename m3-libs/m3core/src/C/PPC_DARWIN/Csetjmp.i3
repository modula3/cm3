(* Copyright according to COPYRIGHT-CMASS. *)
(* FIXME: copied from FreeBSD3 target. Probably needs to be changed. *)

INTERFACE Csetjmp;		(* for I386_DARWIN *)

FROM Ctypes IMPORT int;

TYPE 
  jmp_buf = ARRAY [0..191] OF int; (* actually, this is a sigjmp_buf,
                                     just in case *)
  fpjmp_buf = ARRAY [0..191] OF int;

<*EXTERNAL "setjmp"   *> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL "longjmp"  *> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_fpsetjmp" *>  PROCEDURE fpsetjmp (VAR env: fpjmp_buf): int;
<*EXTERNAL "_fplongjmp" *> PROCEDURE fplongjmp (VAR env: fpjmp_buf; val: int);

END Csetjmp.

