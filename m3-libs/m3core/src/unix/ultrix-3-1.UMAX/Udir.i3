(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Tue Nov 19 10:57:44 PST 1991 by muller    *)
(*      modified on Wed Oct 30 10:14:03 PST 1991 by kalsow    *)

(*      modified on Mon Jul  2 13:25:12 PDT 1990 by mjordan   *)

UNSAFE INTERFACE Udir;

(*** <dir.h> ***)

IMPORT Ctypes;

CONST
  MAXNAMLEN = 255;   (* maximum length of component of file path name *)
  MAXPATHLEN = 1024; (* maximum length of file path name *)

TYPE
  gen_dir = RECORD                    (* describes directory entry *)
    (* SunOS has another field here. *)
    d_ino: Ctypes.unsigned_long;                  (* inode number of entry *)
    d_reclen: Ctypes.unsigned_short;              (* record length in bytes *)
    d_namelen: Ctypes.unsigned_short;             (* name length in bytes *)
    d_name: ARRAY [0..MAXNAMLEN] OF Ctypes.char;  (* name *)
  END;

  direct = gen_dir;                    (* backwards compatibility *)
  
  DIR = RECORD
    dd_fd:    Ctypes.int;
    dd_loc:   Ctypes.long;
    dd_size:  Ctypes.long;
    dd_bbase: Ctypes.long;
    dd_entno: Ctypes.long;
    dd_bsize: Ctypes.long;
    dd_buf:   UNTRACED REF Ctypes.char;
  END;

  DIR_star = UNTRACED REF DIR;

  direct_star = UNTRACED REF direct;

<*EXTERNAL*> PROCEDURE opendir (filename: Ctypes.char_star): DIR_star;
<*EXTERNAL*> PROCEDURE readdir (dirp: DIR_star): direct_star;
<*EXTERNAL*> PROCEDURE telldir (dirp: DIR_star): Ctypes.long;
<*EXTERNAL*> PROCEDURE seekdir (dirp: DIR_star; loc: Ctypes.long);
(* 'rewinddir' is usually a macro for 'seekdir(dirp, 0)' *)
<*EXTERNAL*> PROCEDURE closedir(dirp: DIR_star): Ctypes.int;

END Udir.
