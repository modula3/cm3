(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Udir;

(* This entire file needs checking still; see Cygwin. *)

FROM Ctypes IMPORT char, int, long, char_star;
FROM Utypes IMPORT uint8_t, uint16_t, uint32_t;

CONST
  MAXNAMLEN = 255;
  MAXPATHLEN = 1024;

TYPE
  dirent = RECORD
    d_fileno:   uint32_t;
    d_reclen:   uint16_t;
    d_type:     uint8_t;
    d_namelen:  uint8_t;
    d_name:     ARRAY [0..MAXNAMLEN] OF char;
  END;
  dirent_star = UNTRACED REF dirent;

  (* DIR is opaque *)
  DIR_star = ADDRESS;

<*EXTERNAL*> PROCEDURE opendir (filename: char_star): DIR_star;
<*EXTERNAL*> PROCEDURE readdir (dirp: DIR_star): dirent_star;
<*EXTERNAL*> PROCEDURE telldir (dirp: DIR_star): long;
<*EXTERNAL*> PROCEDURE seekdir (dirp: DIR_star; loc: long);
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): int;

END Udir.
