(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed Oct  5 15:50:08 PDT 1994 by ericv          *)
(*      modified on Tue Feb  5 00:27:55 1991 by muller         *)

INTERFACE Csetjmp;		(* for IRIX5 *)

FROM Ctypes IMPORT int;

CONST
  JBLEN = 28;
  SIGJBLEN = 128;

(*** System V facilities ***)

TYPE jmp_buf = ARRAY [0..JBLEN-1] OF int;

<*EXTERNAL*> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

(* For src/runtime/ex_frame/RTException.m3 *)

<*EXTERNAL setjmp *> PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL longjmp *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

(*** POSIX facilities ***)

TYPE sigjmp_buf = ARRAY [0..SIGJBLEN-1] OF int;

<*EXTERNAL*> PROCEDURE sigsetjmp (VAR env: sigjmp_buf; savemask: int): int;
<*EXTERNAL*> PROCEDURE siglongjmp (VAR env: sigjmp_buf; val: int);

(*** 4.3 BSD facilities ***)

<*EXTERNAL *> PROCEDURE BSDsetjmp (VAR env: sigjmp_buf): int;
<*EXTERNAL *> PROCEDURE BSDlongjmp (VAR env: sigjmp_buf; val: int): int;

<*EXTERNAL "_setjmp" *> PROCEDURE BSD_setjmp (VAR env: sigjmp_buf): int;
<*EXTERNAL "_longjmp" *>
PROCEDURE BSD_longjmp (VAR env: sigjmp_buf; val: int): int;

END Csetjmp.
