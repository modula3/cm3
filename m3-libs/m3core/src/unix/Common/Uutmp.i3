(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uutmp;

FROM Ctypes IMPORT char_star;

<*EXTERNAL "Uutmp__getlogin"*> PROCEDURE getlogin (): char_star;

END Uutmp.
