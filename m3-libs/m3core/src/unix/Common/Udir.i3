(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Udir;

FROM Ctypes IMPORT const_char_star, int;

TYPE
  DIR_star = ADDRESS; (* opaque *)
  struct_dirent_star = ADDRESS;

<*EXTERNAL Udir__opendir*> PROCEDURE opendir (filename: const_char_star): DIR_star;
<*EXTERNAL Udir__readdir*> PROCEDURE readdir(dirp: DIR_star): struct_dirent_star;
<*EXTERNAL Udir__closedir*> PROCEDURE closedir(dirp: DIR_star): int;

END Udir.
