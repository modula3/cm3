(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Oct 31 15:00:39 PST 1994 by kalsow     *)
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
    d_offset_XXX : Utypes.off_t; (* not set until libucb is no *)
                                 (* longer linked into M3 programs. *)
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
(* Make readdir external again when libucb is no longer linked into M3
   programs. *)
               PROCEDURE readdir(dirPtr: DIR_star): struct_dirent_star;
<* EXTERNAL *> PROCEDURE telldir(dirPtr: DIR_star): long;
<* EXTERNAL *> PROCEDURE seekdir(dirPtr: DIR_star; location: long);
<* EXTERNAL *> PROCEDURE rewinddir(dirPtr: DIR_star);
<* EXTERNAL *> PROCEDURE closedir(dirPtr: DIR_star): int;


(*----------------------------------------------- UCB compatibility hack ---*)
(* This is an interface to the Berkeley-style readdir routine.  The rest
   of Udir is identical between Berkeley and System V.  This should go
   away when libucb is no longer linked into M3 programs. *)

TYPE
  UCB_struct_direct_star = UNTRACED REF UCB_struct_direct;
  UCB_struct_direct = RECORD
    d_ino     : Utypes.ino_t;
    d_reclen  : unsigned_short;
    d_namlen  : unsigned_short;
    d_name    : D_name;
  END;

<* EXTERNAL "readdir"*>
PROCEDURE UCB_readdir(dirPtr: DIR_star): UCB_struct_direct_star;

END Udir.
