(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Tue Mar 29 21:54:11 PST 1994 by wobber        *)

INTERFACE Herrno;

FROM Ctypes IMPORT int;

<*EXTERNAL*> VAR h_errno: int;

END Herrno.
