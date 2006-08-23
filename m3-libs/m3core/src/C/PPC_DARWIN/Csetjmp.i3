(* Copyright according to COPYRIGHT-CMASS. *)
(* FIXME: copied from FreeBSD3 target. Probably needs to be changed. *)

INTERFACE Csetjmp;		(* for PPC_DARWIN *)

FROM Ctypes IMPORT int;

CONST
  JBLEN = 26 + 36 + 129 + 1;

TYPE 
  sigjmp_buf = ARRAY [0..JBLEN] OF int;
  jmp_buf = sigjmp_buf;			 (* just in case *)

<*EXTERNAL "setjmp"    *> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL "longjmp"   *> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp"   *> PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp"  *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "sigsetjmp" *> PROCEDURE sigsetjmp (VAR env: sigjmp_buf): int;
<*EXTERNAL "siglongjmp"*> PROCEDURE siglongjmp (VAR env: sigjmp_buf; val: int);

END Csetjmp.

