(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Mon Jan  5 00:45:57 GMT 1998 by rrw       *)
(*      modified on Fri Feb 24 14:51:00 PST 1995 by kalsow    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Apr 13 10:04:46 PDT 1992 by muller    *)

(*      modified on Mon Jul  2 13:25:12 PDT 1990 by mjordan   *)

UNSAFE INTERFACE Udir;

(*** <dir.h> ***)

IMPORT Ctypes;

CONST
  MAXPATHLEN = 260; (* maximum length of file path name *)

TYPE

  dirent = BITS 16_114 * 8 FOR RECORD (* describes directory entry *)
    pad : BITS 16_14 * 8 FOR ARRAY [0..0] OF [0..0];
    d_name: ARRAY [0..255] OF Ctypes.char;  
  END;

  (* DIR is opaque *)
  DIR_star = ADDRESS;

  dirent_star = UNTRACED REF dirent;

<*EXTERNAL*> PROCEDURE opendir (filename: Ctypes.char_star): DIR_star;
<*EXTERNAL*> PROCEDURE readdir (dirp: DIR_star): dirent_star;
<*EXTERNAL*> PROCEDURE telldir (dirp: DIR_star): Ctypes.long;
<*EXTERNAL*> PROCEDURE seekdir (dirp: DIR_star; loc: Ctypes.long);
(* 'rewinddir' is usually a macro for 'seekdir(dirp, 0)' *)
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): Ctypes.int;

END Udir.
