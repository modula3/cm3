(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Feb 24 13:40:14 PST 1992 by kalsow     *)
(*      modified on Mon Aug 13 23:56:36 1990 by muller         *)

INTERFACE Cerrno;

FROM Ctypes IMPORT int;

<*EXTERNAL*> VAR errno: int;
(* The value of errno is preserved across thread switches. *)

END Cerrno.
