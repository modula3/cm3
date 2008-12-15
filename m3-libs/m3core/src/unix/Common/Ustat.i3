(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, char_star;
FROM Utypes IMPORT u_short;

CONST
  S_IFMT   : u_short = 8_0170000;
  S_IFSOCK : u_short = 8_0140000;
  S_IFLNK  : u_short = 8_0120000;
  S_IFREG  : u_short = 8_0100000;
  S_IFBLK  : u_short = 8_0060000;
  S_IFDIR  : u_short = 8_0040000;
  S_IFCHR  : u_short = 8_0020000;
  S_IFIFO  : u_short = 8_0010000;
  S_ISUID  : u_short = 8_0004000;
  S_ISGID  : u_short = 8_0002000;
  S_ISVTX  : u_short = 8_0001000;
  S_IREAD  : u_short = 8_0000400;
  S_IWRITE : u_short = 8_0000200;
  S_IEXEC  : u_short = 8_0000100;
  S_GREAD  : u_short = 8_0000040;
  S_GWRITE : u_short = 8_0000020;
  S_GEXEC  : u_short = 8_0000010;
  S_OREAD  : u_short = 8_0000004;
  S_OWRITE : u_short = 8_0000002;
  S_OEXEC  : u_short = 8_0000001;

TYPE
  struct_stat = RECORD
(* Sorted by size, then by name; make everything LONGINT if possible, else INTEGER;
Limit on LONGINT is compatibility with existing Modula-3 code. Blowing up the sizes
larger than necessary is a slight deoptimization for the sake of simplicity and
commonality. *)
    st_mtime : LONGINT;
    st_rdev  : LONGINT;
    st_size  : LONGINT;
    st_gid   : INTEGER;
    st_mode  : INTEGER;
    st_uid   : INTEGER;
  END;
  struct_stat_star = UNTRACED REF struct_stat;

<*EXTERNAL "m3_stat"*> PROCEDURE stat (path: char_star; buf: struct_stat_star): int;
<*EXTERNAL "m3_lstat"*> PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;
<*EXTERNAL "m3_fstat"*> PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

END Ustat.
