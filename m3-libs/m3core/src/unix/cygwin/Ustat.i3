(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, char_star;
IMPORT Usysdep;

CONST
  (* most of these are NOT sysdep, and the ones that aren't,
  are common except for Cygwin; fix later *)
  S_IFMT   = Usysdep.S_IFMT;
  S_IFSOCK = Usysdep.S_IFSOCK;
  S_IFLNK  = Usysdep.S_IFLNK;
  S_IFREG  = Usysdep.S_IFREG;
  S_IFBLK  = Usysdep.S_IFBLK;
  S_IFDIR  = Usysdep.S_IFDIR;
  S_IFCHR  = Usysdep.S_IFCHR;
  S_IFIFO  = Usysdep.S_IFIFO;
  S_IREAD  = Usysdep.S_IREAD;
  S_IWRITE = Usysdep.S_IWRITE;
  S_IEXEC  = Usysdep.S_IEXEC;
  S_GREAD  = Usysdep.S_GREAD;
  S_GWRITE = Usysdep.S_GWRITE;
  S_GEXEC  = Usysdep.S_GEXEC;
  S_OREAD  = Usysdep.S_OREAD;
  S_OWRITE = Usysdep.S_OWRITE;
  S_OEXEC  = Usysdep.S_OEXEC;

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
