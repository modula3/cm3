(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Udir;

FROM Ctypes IMPORT char, int, long, char_star;

CONST
  MAXPATHLEN = 260;

TYPE

  dirent = BITS 16_114 * 8 FOR RECORD
    pad : BITS 16_14 * 8 FOR ARRAY [0..0] OF [0..0];
    d_name: ARRAY [0..255] OF char;  
  END;

  (* DIR is opaque *)
  DIR_star = ADDRESS;

  dirent_star = UNTRACED REF dirent;

<*EXTERNAL*> PROCEDURE opendir (filename: char_star): DIR_star;
<*EXTERNAL*> PROCEDURE readdir (dirp: DIR_star): dirent_star;
<*EXTERNAL*> PROCEDURE telldir (dirp: DIR_star): long;
<*EXTERNAL*> PROCEDURE seekdir (dirp: DIR_star; loc: long);
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): int;

END Udir.
