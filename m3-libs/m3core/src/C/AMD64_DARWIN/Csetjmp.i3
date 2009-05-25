(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Oct 18 08:40:23 PDT 1994 by kalsow     *)
(*      modified on Fri Apr 30 16:25:40 PDT 1993 by muller     *)
(*      Olaf Wagner 12.09.1994                                 *)

INTERFACE Csetjmp;		(* for AMD64_DARWIN *)

FROM Ctypes IMPORT int;

(*
 * _JBLEN is number of ints required to save the following:
 * rflags, rip, rbp, rsp, rbx, r12, r13, r14, r15... these are 8 bytes each
 * mxcsr, fp control word, sigmask... these are 4 bytes each
 * add 16 ints for future expansion needs...
 *)
CONST
  JBLEN = (9 * 2) + 3 + 16;

TYPE 
  sigjmp_buf = ARRAY [0..JBLEN] OF int;
  jmp_buf = sigjmp_buf;			 (* just in case *)

<*EXTERNAL "setjmp"   *> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL "longjmp"  *> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.

