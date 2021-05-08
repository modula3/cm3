(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Ustat;

FROM Ctypes IMPORT const_int, int, const_char_star;

<*EXTERNAL "Ustat__S_IFMT"*>   VAR S_IFMT: const_int;
<*EXTERNAL "Ustat__S_IFSOCK"*> VAR S_IFSOCK: const_int;
<*EXTERNAL "Ustat__S_IFLNK"*>  VAR S_IFLNK: const_int;
<*EXTERNAL "Ustat__S_IFREG"*>  VAR S_IFREG: const_int;
<*EXTERNAL "Ustat__S_IFBLK"*>  VAR S_IFBLK: const_int;
<*EXTERNAL "Ustat__S_IFDIR"*>  VAR S_IFDIR: const_int;
<*EXTERNAL "Ustat__S_IFCHR"*>  VAR S_IFCHR: const_int;
<*EXTERNAL "Ustat__S_IFIFO"*>  VAR S_IFIFO: const_int;
<*EXTERNAL "Ustat__S_ISUID"*>  VAR S_ISUID: const_int;
<*EXTERNAL "Ustat__S_ISGID"*>  VAR S_ISGID: const_int;
<*EXTERNAL "Ustat__S_ISVTX"*>  VAR S_ISVTX: const_int;
<*EXTERNAL "Ustat__S_IREAD"*>  VAR S_IREAD: const_int;
<*EXTERNAL "Ustat__S_IWRITE"*> VAR S_IWRITE: const_int;
<*EXTERNAL "Ustat__S_IEXEC"*>  VAR S_IEXEC: const_int;
<*EXTERNAL "Ustat__S_GREAD"*>  VAR S_GREAD: const_int;
<*EXTERNAL "Ustat__S_GWRITE"*> VAR S_GWRITE: const_int;
<*EXTERNAL "Ustat__S_GEXEC"*>  VAR S_GEXEC: const_int;
<*EXTERNAL "Ustat__S_OREAD"*>  VAR S_OREAD: const_int;
<*EXTERNAL "Ustat__S_OWRITE"*> VAR S_OWRITE: const_int;
<*EXTERNAL "Ustat__S_OEXEC"*>  VAR S_OEXEC: const_int;

(* st_flags, only on some systems, else 0 *)
  (* Super-user and owner changeable flags. *)
<*EXTERNAL "Ustat__UF_SETTABLE"*>   VAR UF_SETTABLE: const_int;   (* mask of owner changeable flags *)
<*EXTERNAL "Ustat__UF_NODUMP"*>     VAR UF_NODUMP: const_int;     (* Do not dump the file. *)
<*EXTERNAL "Ustat__UF_IMMUTABLE"*>  VAR UF_IMMUTABLE: const_int;  (* The file may not be changed. *)
<*EXTERNAL "Ustat__UF_APPEND"*>     VAR UF_APPEND: const_int;     (* The file may only be appended to. *)
<*EXTERNAL "Ustat__UF_NOUNLINK"*>   VAR UF_NOUNLINK: const_int;   (* The file may not be renamed or deleted. *)
<*EXTERNAL "Ustat__UF_OPAQUE"*>     VAR UF_OPAQUE: const_int;     (* The directory is opaque when viewed through a union stack. *)

  (* Super-user changeable flags. *)
<*EXTERNAL "Ustat__SF_SETTABLE"*>   VAR SF_SETTABLE: const_int;   (* mask of superuser changeable flags *)
<*EXTERNAL "Ustat__SF_ARCHIVED"*>   VAR SF_ARCHIVED: const_int;   (* The file may be archived. *)
<*EXTERNAL "Ustat__SF_IMMUTABLE"*>  VAR SF_IMMUTABLE: const_int;  (* The file may not be changed. *)
<*EXTERNAL "Ustat__SF_APPEND"*>     VAR SF_APPEND: const_int;     (* The file may only be appended to. *)
<*EXTERNAL "Ustat__SF_NOUNLINK"*>   VAR SF_NOUNLINK: const_int;   (* The file may not be renamed or deleted. *)
<*EXTERNAL "Ustat__SF_SNAPSHOT"*>   VAR SF_SNAPSHOT: const_int;   (* The file is a snapshot file. *)

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
PROCEDURE stat (path: const_char_star; buf: struct_stat_star): int;

<*EXTERNAL "Ustat__fstat"*>
PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

<*EXTERNAL "Ustat__lstat"*>
PROCEDURE lstat (path: const_char_star; buf: struct_stat_star): int;

<*EXTERNAL "Ustat__chflags"*> (* only on some platforms *)
PROCEDURE chflags (path: const_char_star; flags: INTEGER): int;

<*EXTERNAL "Ustat__fchflags"*> (* only on some platforms *)
PROCEDURE fchflags (fd: int; flags: INTEGER): int;

END Ustat.
