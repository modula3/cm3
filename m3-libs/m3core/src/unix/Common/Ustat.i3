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

(* st_flags, only on some systems, else 0 *)
  (* Super-user and owner changeable flags. *)
<*EXTERNAL "Ustat__UF_SETTABLE"*>   VAR UF_SETTABLE: int;   (* mask of owner changeable flags *)
<*EXTERNAL "Ustat__UF_NODUMP"*>     VAR UF_NODUMP: int;     (* Do not dump the file. *)
<*EXTERNAL "Ustat__UF_IMMUTABLE"*>  VAR UF_IMMUTABLE: int;  (* The file may not be changed. *)
<*EXTERNAL "Ustat__UF_APPEND"*>     VAR UF_APPEND: int;     (* The file may only be appended to. *)
<*EXTERNAL "Ustat__UF_NOUNLINK"*>   VAR UF_NOUNLINK: int;   (* The file may not be renamed or deleted. *)
<*EXTERNAL "Ustat__UF_OPAQUE"*>     VAR UF_OPAQUE: int;     (* The directory is opaque when viewed through a union stack. *)

  (* Super-user changeable flags. *)
<*EXTERNAL "Ustat__SF_SETTABLE"*>   VAR SF_SETTABLE: int;   (* mask of superuser changeable flags *)
<*EXTERNAL "Ustat__SF_ARCHIVED"*>   VAR SF_ARCHIVED: int;   (* The file may be archived. *)
<*EXTERNAL "Ustat__SF_IMMUTABLE"*>  VAR SF_IMMUTABLE: int;  (* The file may not be changed. *)
<*EXTERNAL "Ustat__SF_APPEND"*>     VAR SF_APPEND: int;     (* The file may only be appended to. *)
<*EXTERNAL "Ustat__SF_NOUNLINK"*>   VAR SF_NOUNLINK: int;   (* The file may not be renamed or deleted. *)
<*EXTERNAL "Ustat__SF_SNAPSHOT"*>   VAR SF_SNAPSHOT: int;   (* The file is a snapshot file. *)

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
    st_flags : INTEGER; (* only on some platforms: Darwin, FreeBSD, OpenBSD, NetBSD, else 0 *)
    st_gid   : INTEGER; (* Utypes.gid_t   *)
    st_mode  : INTEGER; (* Utypes.mode_t  *)
    st_uid   : INTEGER; (* Utypes.uid_t   *)
  END;
  struct_stat_star = UNTRACED REF struct_stat;

<*EXTERNAL "Ustat__stat"*>
PROCEDURE stat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL "Ustat__fstat"*>
PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

<*EXTERNAL "Ustat__lstat"*>
PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL "Ustat__chflags"*> (* only on some platforms *)
PROCEDURE chflags (path: char_star; flags: INTEGER): int;

<*EXTERNAL "Ustat__fchflags"*> (* only on some platforms *)
PROCEDURE fchflags (fd: int; flags: INTEGER): int;

END Ustat.
