(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Nov 11 14:03:31 PST 1993 by kalsow    *)
(*      modified on Tue Nov 19 10:57:44 PST 1991 by muller    *)

(*      modified on Mon Jul  2 13:25:12 PDT 1990 by mjordan   *)

UNSAFE INTERFACE Udir;

(*** <dir.h> ***)

IMPORT Ctypes, Utypes;

CONST
  MAXNAMLEN = 255;   (* maximum length of component of file path name *)
  MAXPATHLEN = 1024; (* maximum length of file path name *)

TYPE
  gen_dir = RECORD        (* describes directory entry *)
    d_ino     : Utypes.ino_t;     (* inode number of entry *)
    d_reclen  : Utypes.ushort_t;  (* record length in bytes *)
    d_namelen : Utypes.ushort_t;  (* name length in bytes *)
    d_name    : ARRAY [0..MAXNAMLEN] OF Ctypes.char;  (* name *)
  END;

  direct = gen_dir;                    (* backwards compatibility *)
  
  DIR = RECORD
    dd_fd:      Ctypes.int;     (* file descriptor associated with directory *)
    dd_loc:     Ctypes.long;    (* offset in current buffer *)
    dd_size:    Ctypes.long;    (* amount of data returned by getdirentries *)
    dd_buf:     Ctypes.char_star; (* data buffer *)
    dd_bufsize: Ctypes.int;
    dd_len:     Ctypes.int;     (* size of data buffer *)
    dd_seek:    Ctypes.long;    (* magic cookie returned by getdirentries *)
    dd_lock:    Ctypes.void_star; (* for inter-thread locking *)
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
