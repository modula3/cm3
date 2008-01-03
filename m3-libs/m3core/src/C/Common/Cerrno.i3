(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Feb 24 13:40:14 PST 1992 by kalsow     *)
(*      modified on Mon Aug 13 23:56:36 1990 by muller         *)

INTERFACE Cerrno;

FROM Ctypes IMPORT int;

(* We cannot access "errno" directly as a variable, because on some systems
   it is a C macro that expands to something more complicated.

   The "errno" value is preserved across thread switches. *)

<*EXTERNAL "m3_Cerrno_GetErrno"*>
PROCEDURE GetErrno(): int;

<*EXTERNAL "m3_Cerrno_SetErrno"*>
PROCEDURE SetErrno(e: int);

END Cerrno.
