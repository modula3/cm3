(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Feb 23 13:39:23 PST 1995 by kalsow    *)
(*      modified on Sat Feb 18 23:43:23 MET 1995 by ow        *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Apr 13 10:04:46 PDT 1992 by muller    *)
(*      modified on Mon Jul  2 13:25:12 PDT 1990 by mjordan   *)

UNSAFE INTERFACE Udir;

(*** <dir.h> ***)

IMPORT Ctypes;

CONST
  MAXNAMLEN = 255;   (* maximum length of component of file path name *)
  MAXPATHLEN = 1024; (* maximum length of file path name *)

  (* file types *)
  DT_UNKNOWN =      0;
  DT_FIFO    =      1;
  DT_CHR     =      2;
  DT_DIR     =      4;
  DT_BLK     =      6;
  DT_REG     =      8;
  DT_LNK     =     10;
  DT_SOCK    =     12;

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
TYPE
  dirent = RECORD                    (* describes directory entry *)
    d_fileno:   Ctypes.long;           (* inode number of entry *)
    d_reclen:   Ctypes.unsigned_char;  (* record length in bytes *)
    d_type:     Ctypes.unsigned_char;  (* file types, see above *)
    d_namelen:  Ctypes.unsigned_short; (* name length in bytes *)
    d_name:     ARRAY [0..MAXNAMLEN] OF Ctypes.char;  (* name *)
  END;

  direct = dirent;                    (* backwards compatibility *)

  DIR = RECORD
    dd_fd:    Ctypes.int; (* file descriptor associated with directory *)
    dd_loc:   Ctypes.long; (* offset in current buffer *)
    dd_size:  Ctypes.long; (* amount of data returned by getdirentries *)
    dd_buf:   UNTRACED REF Ctypes.char; (* data buffer *)
    dd_len:   Ctypes.int; (* size of data buffer *)
    dd_seek:  Ctypes.long (* magic cookie returned by getdirentries *);
    dd_rewind: Ctypes.long; (* magic cookie for rewinding *)
  END;

  DIR_star = UNTRACED REF DIR;

  direct_star = UNTRACED REF direct;

<*EXTERNAL*> PROCEDURE opendir (filename: Ctypes.char_star): DIR_star;
<*EXTERNAL*> PROCEDURE readdir (dirp: DIR_star): direct_star;
<*EXTERNAL*> PROCEDURE telldir (dirp: DIR_star): Ctypes.long;
<*EXTERNAL*> PROCEDURE seekdir (dirp: DIR_star; loc: Ctypes.long);
<*EXTERNAL*> PROCEDURE rewinddir (dirp: DIR_star);
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): Ctypes.int;
<*EXTERNAL*> PROCEDURE getdirentries(fd  : Ctypes.int;
                                     buf : UNTRACED REF Ctypes.char;
                                     nbytes : Ctypes.int;
                                     basep  : UNTRACED REF Ctypes.long): Ctypes.int;

END Udir.
