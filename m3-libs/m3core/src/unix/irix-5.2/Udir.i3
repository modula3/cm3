(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Oct  5 15:39:00 PDT 1994 by ericv      *)
(*      modified on Fri Sep 24 14:34:13 PDT 1993 by kalsow     *)
(*      modified on Sat Jun 27 15:27:28 PDT 1992 by muller     *)

INTERFACE Udir;

IMPORT Utypes;
FROM Ctypes IMPORT char_star, int, long, unsigned_short;

(*** <dirent.h>, <sys/dirent.h> ***)

CONST
  MAXNAMELEN = 255;     (* Maximum length of component of file path name. *)
  MAXPATHLEN = 4096;    (* Maximum length of file path name. *)

TYPE
  D_name = ARRAY [0..MAXNAMELEN] OF CHAR;
  struct_dirent = RECORD
    d_ino: Utypes.ino_t;
    d_off: Utypes.off_t;
    d_reclen: unsigned_short;
    d_name: D_name;
  END;
  dirent_t = struct_dirent;
  struct_dirent_star = UNTRACED REF struct_dirent;

  DIR = RECORD
    dd_fd: int;
    dd_loc: int;
    dd_size: int;
    dd_buf: char_star;
  END;
  DIR_star = UNTRACED REF DIR;

<* EXTERNAL *> PROCEDURE opendir(dir: char_star): DIR_star;
<* EXTERNAL *> PROCEDURE readdir(dirPtr: DIR_star): struct_dirent_star;
<* EXTERNAL *> PROCEDURE telldir(dirPtr: DIR_star): long;
<* EXTERNAL *> PROCEDURE seekdir(dirPtr: DIR_star; location: long);
<* EXTERNAL *> PROCEDURE rewinddir(dirPtr: DIR_star);
<* EXTERNAL *> PROCEDURE closedir(dirPtr: DIR_star): int;

(*** re-entrant version ***)
<* EXTERNAL *> PROCEDURE readdir_r(dirPtr: DIR_star; res: struct_dirent_star);

END Udir.
