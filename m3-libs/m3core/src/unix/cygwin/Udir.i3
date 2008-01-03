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

TYPE

  gen_dir = RECORD                    (* describes directory entry *)
    d_ino: Ctypes.long;                           (* inode number of entry *)
    d_off: Ctypes.long;
    d_reclen: Ctypes.unsigned_short;              (* record length in bytes *)
    (* Unsupported in Linux 1.0 :
    d_namelen: Ctypes.unsigned_short;             (* name length in bytes *)
    ****************)
    d_type : Ctypes.unsigned_char; (* Wierd .. *)
    d_name: ARRAY [0..255] OF Ctypes.char;  
    (* name - fixed at 256 chars by glibc2 *)
  END;

  dirent = gen_dir;
  direct = gen_dir;                    (* backwards compatibility *)
  
  (* DIR = RECORD
    dd_fd:    Ctypes.int;
    dd_loc:   Ctypes.long;
    dd_size:  Ctypes.long;
    (* Unsupported in Linux 1.0 :
    dd_bbase: Ctypes.long;
    dd_entno: Ctypes.long;
    dd_bsize: Ctypes.long;
    *******************************)
    dd_buf:   UNTRACED REF Ctypes.char;
  END;
  *)

  (* DIR is an opaque type in glibc2 *)
  DIR_star = ADDRESS;

  direct_star = UNTRACED REF direct;
  dirent_star = UNTRACED REF dirent;

<*EXTERNAL*> PROCEDURE opendir (filename: Ctypes.char_star): DIR_star;
<*EXTERNAL*> PROCEDURE readdir (dirp: DIR_star): dirent_star;
<*EXTERNAL*> PROCEDURE telldir (dirp: DIR_star): Ctypes.long;
<*EXTERNAL*> PROCEDURE seekdir (dirp: DIR_star; loc: Ctypes.long);
(* 'rewinddir' is usually a macro for 'seekdir(dirp, 0)' *)
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): Ctypes.int;

END Udir.
