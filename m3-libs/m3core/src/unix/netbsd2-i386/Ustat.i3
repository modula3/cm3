(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Mar 15 16:47:47 PST 1995 by kalsow     *)
(*      modified on Sat Feb 18 23:43:23 MET 1995 by ow         *)
(*      modified on Tue Mar 24 20:42:39 PST 1992 by muller     *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, char_star, long;

FROM Utypes IMPORT u_short, dev_t, ino_t, off_t,
  mode_t, nlink_t, uid_t, gid_t, time_t, u_int32_t, int64_t,
  blkcnt_t, blksize_t;


CONST
  S_IFMT  : u_short = 8_0170000;
  S_IFSOCK: u_short = 8_0140000;
  S_IFLNK : u_short = 8_0120000;
  S_IFREG : u_short = 8_0100000;
  S_IFPIPE: u_short = 8_0000000; (* no such constant in stat.h!*)
  S_IFBLK : u_short = 8_0060000;
  S_IFDIR : u_short = 8_0040000;
  S_IFCHR : u_short = 8_0020000;
  S_IFIFO : u_short = 8_0010000;
  S_IFPORT          = S_IFIFO;
  S_ISUID : u_short = 8_0004000;
  S_ISGID : u_short = 8_0002000;
  S_ISVTX : u_short = 8_0001000;
  S_IREAD : u_short = 8_0000400;
  S_IWRITE: u_short = 8_0000200;
  S_IEXEC : u_short = 8_0000100;
  S_GREAD : u_short = 8_0000040;
  S_GWRITE: u_short = 8_0000020;
  S_GEXEC : u_short = 8_0000010;
  S_OREAD : u_short = 8_0000004;
  S_OWRITE: u_short = 8_0000002;
  S_OEXEC : u_short = 8_0000001;

TYPE
  struct_stat = RECORD
    st_dev    : dev_t;
    st_ino    : ino_t;
    st_mode   : mode_t;
    st_nlink  : nlink_t;
    st_uid    : uid_t;
    st_gid    : gid_t;
    st_rdev   : dev_t;
    st_atime  : time_t;
    st_atimensec : long;
    st_mtime  : time_t;
    st_mtimensec : long;
    st_ctime  : time_t;
    st_ctimensec : long;
    st_size   : off_t;
    st_pad1   : int;
    st_blocks : blkcnt_t;
    st_pad2   : int;
    st_blksize: blksize_t;
    st_flags  : u_int32_t;
    st_gen    : u_int32_t;
    st_qspare : ARRAY [0..1] OF int64_t;
  END;

  struct_stat_star = UNTRACED REF struct_stat;

<*EXTERNAL "__stat13"*>
PROCEDURE stat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL "__lstat13"*>
PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL "__fstat13"*>
PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

END Ustat.
