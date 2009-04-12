(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, char_star;

(*CONST*)
<*EXTERNAL "Ustat__S_IFMT"*>   VAR S_IFMT: int;
<*EXTERNAL "Ustat__S_IFSOCK"*> VAR S_IFSOCK: int;
<*EXTERNAL "Ustat__S_IFLNK"*>  VAR S_IFLNK: int;
<*EXTERNAL "Ustat__S_IFREG"*>  VAR S_IFREG: int;
<*EXTERNAL "Ustat__S_IFBLK"*>  VAR S_IFBLK: int;
<*EXTERNAL "Ustat__S_IFDIR"*>  VAR S_IFDIR: int;
<*EXTERNAL "Ustat__S_IFCHR"*>  VAR S_IFCHR: int;
<*EXTERNAL "Ustat__S_IFIFO"*>  VAR S_IFIFO: int;
<*EXTERNAL "Ustat__S_ISUID"*>  VAR S_ISUID: int;
<*EXTERNAL "Ustat__S_ISGID"*>  VAR S_ISGID: int;
<*EXTERNAL "Ustat__S_ISVTX"*>  VAR S_ISVTX: int;
<*EXTERNAL "Ustat__S_IREAD"*>  VAR S_IREAD: int;
<*EXTERNAL "Ustat__S_IWRITE"*> VAR S_IWRITE: int;
<*EXTERNAL "Ustat__S_IEXEC"*>  VAR S_IEXEC: int;
<*EXTERNAL "Ustat__S_GREAD"*>  VAR S_GREAD: int;
<*EXTERNAL "Ustat__S_GWRITE"*> VAR S_GWRITE: int;
<*EXTERNAL "Ustat__S_GEXEC"*>  VAR S_GEXEC: int;
<*EXTERNAL "Ustat__S_OREAD"*>  VAR S_OREAD: int;
<*EXTERNAL "Ustat__S_OWRITE"*> VAR S_OWRITE: int;
<*EXTERNAL "Ustat__S_OEXEC"*>  VAR S_OEXEC: int;

TYPE
  struct_stat = RECORD
(*
This MUST match UstatC.c.

Sorted by size, then by name; make everything LONGINT if possible, else INTEGER;
Limit on LONGINT is compatibility with existing Modula-3 code. Blowing up the sizes
larger than necessary is a slight deoptimization for the sake of simplicity and
commonality. *)
    st_dev   : LONGINT; (* Utypes.dev_t   *)
    st_ino   : LONGINT; (* Utypes.ino_t   *)
    st_mtime : LONGINT; (* not time_t     *)
    st_nlink : LONGINT; (* Utypes.nlink_t *)
    st_rdev  : LONGINT; (* Utypes.dev_t   *)
    st_size  : LONGINT; (* Utypes.off_t   *)
    st_gid   : INTEGER; (* Utypes.gid_t   *)
    st_mode  : INTEGER; (* Utypes.mode_t  *)
    st_uid   : INTEGER; (* Utypes.uid_t   *)
  END;
  struct_stat_star = UNTRACED REF struct_stat;

<*EXTERNAL "Ustat__stat"*>
PROCEDURE stat (path: char_star; buf: struct_stat_star): int;
<*EXTERNAL "Ustat__lstat"*>
PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;
<*EXTERNAL "Ustat__fstat"*>
PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

END Ustat.
