(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Udir;

FROM Ctypes IMPORT int, char_star;
IMPORT Usysdep;

CONST
  MAXNAMLEN = Usysdep.MAXNAMLEN;
  MAXPATHLEN = Usysdep.MAXPATHLEN;

TYPE
  dirent = Usysdep.dirent;
  dirent_star = UNTRACED REF dirent;

  (* DIR is opaque *)
  DIR_star = ADDRESS;

<*EXTERNAL*> PROCEDURE opendir (filename: char_star): DIR_star;
<*EXTERNAL*> PROCEDURE readdir (dirp: DIR_star): dirent_star;
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): int;

END Udir.
