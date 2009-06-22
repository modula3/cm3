(* Copyright (C) 1992, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

UNSAFE INTERFACE FSPosixC;

FROM Ctypes IMPORT const_char_star;
FROM Udir IMPORT DIR_star;

<*EXTERNAL "FSPosixC__readdir_name"*> PROCEDURE readdir_name(iter: DIR_star): const_char_star;

END FSPosixC.
