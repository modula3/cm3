(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May 13 18:16:21 PDT 1993 by msm     *)
(*      modified on Fri Apr 16 09:28:14 PDT 1993 by steveg  *)
<*PRAGMA LL*>

(* Yet another level of indirection to limit OS dependencies from
   trestle *)

INTERFACE TrestleOS;


PROCEDURE Init();

PROCEDURE UserName(): TEXT;

END TrestleOS.


