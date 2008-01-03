(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sat Jun 27 15:27:28 PDT 1992 by muller         *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, long, char_star;
FROM Utypes IMPORT u_short, dev_t, ino_t, off_t, time_t,
                   mode_t, nlink_t, uid_t, gid_t, ulong_t;


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
               st_uid    : uid_t;
               st_gid    : gid_t;
               st_rdev   : dev_t;
               st_size   : off_t;
               st_atime  : time_t;
               st_spare1 : int;
               st_mtime  : time_t;
               st_spare2 : int;
               st_ctime  : time_t;
               st_spare3 : int;
               st_blksize: ulong_t;
               st_blocks : long;
               st_spare41: ulong_t;
               st_spare42: ulong_t;   END;

  struct_stat_star = UNTRACED REF struct_stat;

<*EXTERNAL*> PROCEDURE stat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

END Ustat.
