(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Csetjmp;
FROM Ctypes IMPORT int;

(* jmp_buf is number of bytes with 8 byte alignment.
   Ideally it is target-dependent and 4, 8, or 16, however 16 cannot be
   expressed and 8 is preferred over 4 to remove target-dependence, even
   if it is a slight waste on some platforms.
   See Jumpbuf_align in m3-sys/m3middle/Target.m3.
   This should never be instantiated. To help catch that, we make it large.
   2 * BYTESIZE(INTEGER) would be nice, if we could express that.
   Specifically, as far as we know, correctness requires 16 on PA64_HPUX
     and SPARC64_LINUX. 4 is adequate for correctness on PPC_LINUX, but
     16 is ideal there (according to comments in setjmp.h).

   "u" in "ulongjmp" is probably for "underscore".
   This variant of longjmp never restores the signal mask.
*)
TYPE jmp_buf = ARRAY [0..16_FFFFFF] OF LONGREAL;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
