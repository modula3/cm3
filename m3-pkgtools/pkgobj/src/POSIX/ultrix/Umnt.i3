(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jan  6 18:13:59 PST 1993 by wobber        *)
(*      modified on Fri Oct  2 14:03:26 PDT 1992 by birrell       *)

INTERFACE Umnt;

FROM Unix IMPORT MaxPathLen;
FROM Ctypes IMPORT char_star, int, short, unsigned_int, unsigned_int_star;

TYPE
  fs_data = RECORD
          fd_req:     fs_data_req;   (* required data *)
          fd_spare:   ARRAY [0..111] OF unsigned_int;
  END;
  
  dev_t = short;

  fs_data_req = RECORD
          flags: unsigned_int;        (* how mounted *)
          mtsize: unsigned_int;       (* max transfer size in bytes *)
          otsize: unsigned_int;       (* optimal transfer size in bytes *)
          bsize: unsigned_int;        (* fs block size in bytes for vm code *)
          fstype: unsigned_int;       (* see ../h/fs_types.h  *)
          gtot: unsigned_int;         (* total number of gnodes *)
          gfree: unsigned_int;        (* # of free gnodes *)
          btot: unsigned_int;         (* total number of 1K blocks *)
          bfree: unsigned_int;        (* # of free 1K blocks *)
          bfreen: unsigned_int;       (* user consumable 1K blocks *)
          pgthresh: unsigned_int;     (* min size in bytes before paging *)
          uid: int;                   (* uid that mounted me *)
          dev: dev_t;                 (* major/minor of fs *)
          exroot: short;              (* root mapping from exports *)
          devname: ARRAY [0..MaxPathLen+4] OF CHAR; (* name of dev *)
          path: ARRAY [0..MaxPathLen+4] OF CHAR;  (* name of mount point *)
          updates: unsigned_int;      (* number of writes *)
  END;

  fs_data_star = UNTRACED REF fs_data;

CONST    (* values for mode argument *)
  NOSTAT_MANY = 1;
  STAT_MANY = 2;
  STAT_ONE = 3;
  NOSTAT_ONE = 4;
  STAT_FD = 5;
  NOSTAT_FD = 6;

CONST    (* flags in fd_data_req.flags *)
  M_RONLY =        16_0001;
  M_MOD =          16_0002;
  M_QUOTA =        16_0004;
  M_LOCAL =        16_0008;
  M_NOEXEC =       16_0010;
  M_NOSUID =       16_0020;
  M_NODEV =        16_0040;
  M_FORCE =        16_0080;
  M_SYNC =         16_0100;
  M_DONE =         16_0200;
  M_NOCACHE =      16_0400;
  M_EXPORTED =     16_0800;          (* export flag *)
  M_NOFH =         16_1000;          (* no fhandle flag *)
  M_EXRONLY =      16_2000;          (* export read-only *)

<*EXTERNAL*> PROCEDURE getmnt (
         start: unsigned_int_star;
         buffer: fs_data_star;
         nbytes, mode: int;
         path: char_star) : int;


END Umnt.
