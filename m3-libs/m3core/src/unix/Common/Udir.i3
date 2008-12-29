(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Udir;

FROM Ctypes IMPORT char, const_char_star, int;

TYPE
  dirent = RECORD
    d_ino: LONGINT;
    pad: ARRAY [0..2] OF char; (* not portable *)
    d_name: ARRAY [0..255] OF char;
  END;
  dirent_star = UNTRACED REF dirent;

  (* DIR is opaque *)
  DIR_star = ADDRESS;

<*EXTERNAL*> PROCEDURE opendir (filename: const_char_star): DIR_star;
<*EXTERNAL "m3_readdir"*> PROCEDURE readdir (dirp: DIR_star): dirent_star;
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): int;

END Udir.
