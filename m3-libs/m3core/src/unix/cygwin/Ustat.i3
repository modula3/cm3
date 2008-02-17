(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Mar 15 16:49:26 PST 1995 by kalsow     *)
(*      modified on Tue Mar 24 20:42:39 PST 1992 by muller     *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, long, char_star;
FROM Utypes IMPORT u_short, dev_t, ino_t, off_t;
FROM Utypes IMPORT mode_t, nlink_t, uid_t, gid_t;
FROM Utypes IMPORT timestruc_t, blkcnt_t, blksize_t;

CONST
  S_IFMT  : u_short = 8_0170000;
  S_IFSOCK: u_short = 8_0140000;
  S_IFLNK : u_short = 8_0120000;
  S_IFREG : u_short = 8_0100000;
  S_IFPIPE: u_short = 8_0000000;
  S_IFBLK : u_short = 8_0060000;
  S_IFDIR : u_short = 8_0040000;
  S_IFCHR : u_short = 8_0020000;
  S_IFIFO : u_short = 8_0010000;
  S_IFPORT          = S_IFIFO;
(*
  S_ISUID : u_short = 8_0004000;
  S_ISGID : u_short = 8_0002000;
  S_ISVTX : u_short = 8_0001000;
*)
  S_IREAD : u_short = 8_0400;
  S_IWRITE: u_short = 8_0200;
  S_IEXEC : u_short = 8_0100;
  S_GREAD : u_short = 0;
  S_GWRITE: u_short = 0;
  S_GEXEC : u_short = 0;
  S_OREAD : u_short = 0;
  S_OWRITE: u_short = 0;
  S_OEXEC : u_short = 0;

TYPE
  pad2_t = BITS 16 FOR [0..0];
  pad4_t = BITS 32 FOR [0..0];
  struct_stat = BITS 16_60 * 8 FOR RECORD
    st_dev          : dev_t;    (* 0123 *)
    unused_pad1     : pad4_t;   (* 4567 *)
    unused_st_ino   : ino_t;    (* 89ab cdef *)
    st_mode   : mode_t;         (* 10 11 12 13 *)
    unused_st_nlink  : nlink_t; (* 14 15 *)
    unused_pad2 : pad2_t; (* 16 17 *)
    st_uid    : uid_t; (* 18 19 1a 1b *)
    st_gid    : gid_t; (* 1c 1d 1e 1f *)
    st_rdev   : dev_t; (* 20 21 22 23 *)
    unused_pad3 : pad4_t; (* 24 25 26 27 *)
    st_size   : off_t; (* 28 29 2a 2b 2c 2d 2e 2f *)
    st_atim   : timestruc_t; (* 30 31 32 33 34 35 36 37 *)
(*  st_mtim   : timestruc_t;               hack *)
    st_mtime  : long;                   (* hack *) (* 38 39 3a 3b *)
    unused_st_mtime_nanoseconds : long; (* hack *) (* 3c 3d 3e 3f *)
    unused_st_ctim   : timestruc_t; (* 40 41 42 43 44 45 46 47 *)
    unused_st_blksize: blksize_t; (* 48 49 4a 4b *)
    unused_pad4 : pad4_t; (* 4c 4d 4e 4f *)
    unused_st_blocks : blkcnt_t; (* 50 51 52 53 54 55 56 57 *)
    unused_st_spare4 : ARRAY [0..1] OF long; (* 58 59 5a 5b 5c 5d 5e 5f *)
  END;

  struct_stat_star = UNTRACED REF struct_stat;

<*EXTERNAL*> PROCEDURE stat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

END Ustat.
