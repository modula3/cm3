(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Tue Mar 29 21:54:11 PST 1994 by wobber        *)

INTERFACE Herrno;

FROM Ctypes IMPORT int;

(* We cannot access "h_errno" directly as a variable, because on some systems
   it is a C macro that expands to something more complicated. *)

<*EXTERNAL "m3_Herrno_Get_h_errno"*>
PROCEDURE Get_h_errno(): int;

END Herrno.
