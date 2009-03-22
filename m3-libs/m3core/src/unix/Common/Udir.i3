(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Udir;

FROM Ctypes IMPORT const_char_star, int;

TYPE
  (* DIR is opaque *)
  DIR_star = ADDRESS;

PROCEDURE opendir (filename: const_char_star): DIR_star;
<*EXTERNAL Udir__opendir*> PROCEDURE closedir(dirp: DIR_star): int;

END Udir.
