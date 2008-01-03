(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Feb 23 13:39:23 PST 1995 by kalsow    *)
(*      modified on Sat Feb 18 23:43:23 MET 1995 by ow        *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Apr 13 10:04:46 PDT 1992 by muller    *)
(*      modified on Mon Jul  2 13:25:12 PDT 1990 by mjordan   *)

UNSAFE INTERFACE Udir;

FROM Utypes IMPORT u_int32_t, u_int16_t, u_int8_t;
FROM Ctypes IMPORT char, char_star, int, void_star;

(*** <sys/dirent.h> ***)

(*
 * The dirent structure defines the format of directory entries returned by 
 * the getdirentries(2) system call.
 *
 * A directory entry has a struct dirent at the front of it, containing its
 * inode number, the length of the entry, and the length of the name
 * contained in the entry.  These are followed by the name padded to a 4
 * byte boundary with null bytes.  All names are guaranteed null terminated.
 * The maximum length of a name in a directory is MAXNAMLEN.
 *)

CONST
  MAXNAMLEN = 255;

TYPE
  struct_dirent = RECORD
    d_fileno:   u_int32_t;		 (* file number of entry *)
    d_reclen:   u_int16_t;		 (* length of this record *)
    d_type:     u_int8_t;		 (* file type, see below *)
    d_namelen:  u_int8_t;		 (* length of string in d_name *)
    d_name:     ARRAY [0..MAXNAMLEN] OF char;
					 (* name must be no longer than his *)
  END;
  struct_dirent_star = UNTRACED REF struct_dirent;

(*
 * File types
 *)
CONST
  DT_UNKNOWN =      0;
  DT_FIFO    =      1;
  DT_CHR     =      2;
  DT_DIR     =      4;
  DT_BLK     =      6;
  DT_REG     =      8;
  DT_LNK     =     10;
  DT_SOCK    =     12;

(*** <dirent.h> ***)

CONST
  DIRBLKSIZ = 1024;

TYPE
  DIR = void_star;
  DIR_star = UNTRACED REF DIR;

<*EXTERNAL*> PROCEDURE opendir (filename: char_star): DIR_star;
<*EXTERNAL*> PROCEDURE readdir (dirp: DIR_star): struct_dirent_star;
<*EXTERNAL*> PROCEDURE rewinddir (dirp: DIR_star);
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): int;

END Udir.
