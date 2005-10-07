(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Udir;

(*** <dirent.h> ***)

IMPORT Ctypes, Utypes;

CONST
  MAXNAMLEN = 255;   (* maximum length of component of file path name *)
  MAXPATHLEN = 1024; (* maximum length of file path name *)

CONST
 (* Types for d_type field in gen_dir *)
  DT_UNKNOWN = 0;
  DT_FIFO = 1;
  DT_CHR = 2;
  DT_DIR = 4;
  DT_BLK = 6;
  DT_REG = 8;
  DT_LNK = 10;
  DT_SOCK = 12;
  DT_WHT = 14;

TYPE
  dirent = RECORD
    d_ino: Utypes.ino_t;
    d_off: Utypes.off_t;
    d_reclen: Ctypes.unsigned_short_int;
    d_type: Ctypes.unsigned_char;
    d_name: ARRAY [0..255] OF Ctypes.char;
  END;

  (* DIR is an opaque type in glibc2 *)
  DIR_star = ADDRESS;

  dirent_star = UNTRACED REF dirent;

<*EXTERNAL*> PROCEDURE opendir (filename: Ctypes.const_char_star): DIR_star;
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): Ctypes.int;
<*EXTERNAL*> PROCEDURE readdir (dirp: DIR_star): dirent_star;
<*EXTERNAL*> PROCEDURE rewinddir (dirp: DIR_star);
<*EXTERNAL*> PROCEDURE seekdir (dirp: DIR_star; loc: Ctypes.long_int);
<*EXTERNAL*> PROCEDURE telldir (dirp: DIR_star): Ctypes.long_int;

END Udir.
