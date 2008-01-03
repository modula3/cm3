(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking    *)
(*      modified on Mon Oct 31 15:00:39 PST 1994 by kalsow     *)
(*      modified on Sat Jun 27 15:27:28 PDT 1992 by muller     *)

INTERFACE Udir;

IMPORT Utypes;
FROM Ctypes IMPORT char_star, int, long, unsigned_short;

CONST
  MAXNAMELEN = 512;     (* Maximum length of component of file path name. *)
  MAXPATHLEN = 1024;    (* Maximum length of file path name. *)

TYPE
  struct_dirent_star_star = UNTRACED REF struct_dirent_star;
  struct_dirent_star = UNTRACED REF struct_dirent;
  struct_dirent = RECORD
    d_ino        : Utypes.ino_t;
    d_offset_XXX : Utypes.off_t;
    d_reclen     : unsigned_short;
    d_name       : D_name;
  END;

  D_name = ARRAY [0..MAXNAMELEN] OF CHAR;

  DIR_star = UNTRACED REF DIR;
  DIR = RECORD
    dd_fd   : int;
    dd_loc  : int;
    dd_size : int;
    dd_buf  : char_star;
    nlist   : struct_dirent_star_star;
  END;

<* EXTERNAL *> PROCEDURE opendir(dir: char_star): DIR_star;
<* EXTERNAL *> PROCEDURE readdir(dirPtr: DIR_star): struct_dirent_star;
<* EXTERNAL *> PROCEDURE telldir(dirPtr: DIR_star): long;
<* EXTERNAL *> PROCEDURE seekdir(dirPtr: DIR_star; location: long);
<* EXTERNAL *> PROCEDURE rewinddir(dirPtr: DIR_star);
<* EXTERNAL *> PROCEDURE closedir(dirPtr: DIR_star): int;

END Udir.
