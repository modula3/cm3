(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Thu Apr 09 13:52:08 PDT 1992 by muller                   *)

INTERFACE Ustat;

FROM Ctypes IMPORT short, int, long, char_star;
FROM Utypes IMPORT dev_t, ino_t, off_t, time_t,
                   u_long_t, mode_t, nlink_t, u_short_t, uid_t, gid_t,
                   fstore_t, siteno_t, u_short;

(*** stat, lstat, fstat - get file status ***)

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
	       st_spare0 : u_short_t;
               st_uid    : uid_t;
               st_gid    : gid_t;
               st_rdev   : dev_t;
               st_size   : off_t;
               st_atime  : time_t;
               st_spare1 : u_long_t;
               st_mtime  : time_t;
               st_spare2 : u_long_t;
               st_ctime  : time_t;
               st_spare3 : u_long_t;
               st_blksize: u_long_t;
               st_blocks : u_long_t;
               st_gen    : u_long_t;
               st_type   : u_long_t;
               st_vfs    : u_long_t;
               st_flag   : u_long_t;
               st_cmtcnt : u_long_t;
               st_fstore : fstore_t;
               st_version: u_long_t;
               st_css    : siteno_t;
               st_ss     : siteno_t;
               st_rdevsite: siteno_t;
               st_spare4 : short;
               st_nid    : long;
               st_uid_raw: uid_t;
               st_gid_raw: gid_t;
               st_uid_rev_tag: u_long_t;
               st_gid_rev_tag: u_long_t;  END;

  struct_stat_star = UNTRACED REF struct_stat;

<*EXTERNAL*> PROCEDURE stat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE fstat (fd: int; buf: struct_stat_star): int;

END Ustat.
