(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, char_star;

CONST
  S_IFMT   = 8_0170000;
  S_IFSOCK = 8_0140000;
  S_IFLNK  = 8_0120000;
  S_IFREG  = 8_0100000;
  S_IFBLK  = 8_0060000;
  S_IFDIR  = 8_0040000;
  S_IFCHR  = 8_0020000;
  S_IFIFO  = 8_0010000;
  S_ISUID  = 8_0004000;
  S_ISGID  = 8_0002000;
  S_ISVTX  = 8_0001000;
  S_IREAD  = 8_0000400;
  S_IWRITE = 8_0000200;
  S_IEXEC  = 8_0000100;
  S_GREAD  = 8_0000040;
  S_GWRITE = 8_0000020;
  S_GEXEC  = 8_0000010;
  S_OREAD  = 8_0000004;
  S_OWRITE = 8_0000002;
  S_OEXEC  = 8_0000001;

TYPE
  struct_stat = RECORD
    st_mode  : LONGINT;
    st_mtime : LONGINT;
    st_size  : LONGINT;
  END;
  struct_stat_star = UNTRACED REF struct_stat;

<*EXTERNAL "m3_stat"*> PROCEDURE stat (path: char_star; buf: struct_stat_star): int;
<*EXTERNAL "m3_lstat"*> PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;
<*EXTERNAL "m3_fstat"*> PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

END Ustat.
