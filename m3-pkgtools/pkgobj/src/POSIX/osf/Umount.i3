(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Mar  4 17:09:50 PST 1994 by wobber        *)
(*      modified on Fri Oct  2 14:03:26 PDT 1992 by birrell       *)

INTERFACE Umount;

IMPORT Unix, Utypes;

FROM Ctypes IMPORT char_star, int, short;

TYPE
  mount_name = ARRAY [0..Unix.MaxPathLen-1] OF CHAR;

  fsid_t = Utypes.quad;

  struct_statfs_star = UNTRACED REF struct_statfs;
  struct_statfs = RECORD
    f_type: short;                 (* type of filesystem (see below) *)
    f_flags: short;                (* copy of mount flags *)
    f_fsize: int;                  (* fundamental filesystem block size *)
    f_bsize: int;                  (* optimal transfer block size *)
    f_blocks: int;                 (* total data blocks in file system *)
    f_bfree: int;                  (* free blocks in fs *)
    f_bavail: int;                 (* free blocks avail to non-su *)
    f_files: int;                  (* total file nodes in file system *)
    f_ffree: int;                  (* free file nodes in fs *)
    f_fsid: fsid_t;                (* file system id *)
    f_spare: ARRAY [0..8] OF int;  (* spare for later *)
    f_mntonname: mount_name;       (* directory on which mounted *)
    f_mntfromname: mount_name;     (* mounted filesystem *)
    mount_info: ARRAY [0..10] OF int;
  END;

CONST    (* flags in struct_statfs.flags *)
  M_RONLY =        16_0001;          (* read only filesystem *)
  M_SYNCHRONOUS =  16_0002;          (* file system written synchronously *)
  M_NOEXEC =       16_0004;          (* can't exec from filesystem *)
  M_NOSUID =       16_0008;          (* don't honor setuid bits on fs *)
  M_NODEV =        16_0010;          (* don't interpret special files *)

  M_EXPORTED =     16_0100;          (* export flag *)
  M_EXRDONLY =     16_0200;          (* export read-only *)
  M_EXRDMOSTLY =   16_0400;          (* exported ro to most *)

  M_LOCAL =        16_1000;          (* filesystem is stored locally *)
  M_QUOTA =        16_2000;          (* quotas are enabled on filesystem *)

<*EXTERNAL*> PROCEDURE statfs (
         path: char_star;
         statfs: struct_statfs_star;
         length: int) : int;

END Umount.
