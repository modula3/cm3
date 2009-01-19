(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

<*EXTERNAL*> INTERFACE Ustat;

FROM Ctypes IMPORT int, char_star;

(*CONST*)
<*EXTERNAL "Ustat_S_IFMT"*> VAR S_IFMT: int;
<*EXTERNAL "Ustat_S_IFSOCK"*> VAR S_IFSOCK: int;
<*EXTERNAL "Ustat_S_IFLNK"*> VAR S_IFLNK: int;
<*EXTERNAL "Ustat_S_IFREG"*> VAR S_IFREG: int;
<*EXTERNAL "Ustat_S_IFBLK"*> VAR S_IFBLK: int;
<*EXTERNAL "Ustat_S_IFDIR"*> VAR S_IFDIR: int;
<*EXTERNAL "Ustat_S_IFCHR"*> VAR S_IFCHR: int;
<*EXTERNAL "Ustat_S_IFIFO"*> VAR S_IFIFO: int;
<*EXTERNAL "Ustat_S_ISUID"*> VAR S_ISUID: int;
<*EXTERNAL "Ustat_S_ISGID"*> VAR S_ISGID: int;
<*EXTERNAL "Ustat_S_ISVTX"*> VAR S_ISVTX: int;
<*EXTERNAL "Ustat_S_IREAD"*> VAR S_IREAD: int;
<*EXTERNAL "Ustat_S_IWRITE"*> VAR S_IWRITE: int;
<*EXTERNAL "Ustat_S_IEXEC"*> VAR S_IEXEC: int;
<*EXTERNAL "Ustat_S_GREAD"*> VAR S_GREAD: int;
<*EXTERNAL "Ustat_S_GWRITE"*> VAR S_GWRITE: int;
<*EXTERNAL "Ustat_S_GEXEC"*> VAR S_GEXEC: int;
<*EXTERNAL "Ustat_S_OREAD"*> VAR S_OREAD: int;
<*EXTERNAL "Ustat_S_OWRITE"*> VAR S_OWRITE: int;
<*EXTERNAL "Ustat_S_OEXEC"*> VAR S_OEXEC: int;

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
