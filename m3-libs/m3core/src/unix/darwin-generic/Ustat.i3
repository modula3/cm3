(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Mar 15 16:47:47 PST 1995 by kalsow     *)
(*      modified on Sat Feb 18 23:43:23 MET 1995 by ow         *)
(*      modified on Tue Mar 24 20:42:39 PST 1992 by muller     *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, long, const_char_star;
FROM Utypes IMPORT u_short, u_long, dev_t, ino_t, off_t;
FROM Utypes IMPORT mode_t, nlink_t, uid_t, gid_t, time_t, int32_t,
  u_int32_t, int64_t;

TYPE
  struct_stat = RECORD
    st_dev       : dev_t;		 (* inode's device *)
    st_ino       : ino_t;		 (* inode's number *)
    st_mode      : mode_t;		 (* inode protection mode *)
    st_nlink     : nlink_t;		 (* number of hard links *)
    st_uid       : uid_t;		 (* user ID of the file's owner *)
    st_gid       : gid_t;		 (* group ID of the file's group *)
    st_rdev      : dev_t;		 (* device type *)
    st_atime     : time_t;		 (* time of last access *)
    st_atimensec : long;		 (* nsec of last access *)
    st_mtime     : time_t;		 (* time of last data modification *)
    st_mtimensec : long;		 (* nsec of last data modification *)
    st_ctime     : time_t;		 (* time of last file status change *)
    st_ctimensec : long;		 (* nsec of last file status change *)
    st_size      : off_t;		 (* file size, in bytes *)
    st_blocks    : int64_t;		 (* blocks allocated for file *)
    st_blksize   : u_int32_t;		 (* optimal blocksize for I/O *)
    st_flags     : u_int32_t;		 (* user defined flags for file *)
    st_gen       : u_int32_t;		 (* file generation number *)
    st_lspare    : int32_t;
    st_qspare    : ARRAY [0..1] OF int64_t;
  END;
  struct_stat_star = UNTRACED REF struct_stat;

CONST
  S_ISUID : u_short = 8_0004000;	 (* set user id on execution *)
  S_ISGID : u_short = 8_0002000;	 (* set group id on execution *)
  S_ISTXT : u_short = 8_0001000;	 (* sticky bit *)

  S_IRWXU : u_short = 8_0000700;	 (* RWX mask for owner *)
  S_IRUSR : u_short = 8_0000400;	 (* R for owner *)
  S_IWUSR : u_short = 8_0000200;	 (* W for owner *)
  S_IXUSR : u_short = 8_0000100;	 (* X for owner *)

  S_IREAD  = S_IRUSR;
  S_IWRITE = S_IWUSR;
  S_IEXEC  = S_IXUSR;

  S_IRWXG : u_short = 8_0000070;	 (* RWX mask for group *)
  S_IRGRP : u_short = 8_0000040;	 (* R for group *)
  S_IWGRP : u_short = 8_0000020;	 (* W for group *)
  S_IXGRP : u_short = 8_0000010;	 (* X for group *)

  S_GREAD  = S_IRGRP;
  S_GWRITE = S_IWGRP;
  S_GEXEC  = S_IXGRP;

  S_IRWXO : u_short = 8_0000007;	 (* RWX mask for other *)
  S_IROTH : u_short = 8_0000004;	 (* R for other *)
  S_IWOTH : u_short = 8_0000002;	 (* W for other *)
  S_IXOTH : u_short = 8_0000001;	 (* X for other *)

  S_OREAD  = S_IROTH;
  S_OWRITE = S_IWOTH;
  S_OEXEC  = S_IXOTH;

  S_IFMT  : u_short = 8_0170000;	 (* type of file mask *)
  S_IFIFO : u_short = 8_0010000;	 (* named pipe (fifo) *)
  S_IFCHR : u_short = 8_0020000;	 (* character special *)
  S_IFDIR : u_short = 8_0040000;	 (* directory *)
  S_IFBLK : u_short = 8_0060000;	 (* block special *)
  S_IFREG : u_short = 8_0100000;	 (* regular *)
  S_IFLNK : u_short = 8_0120000;	 (* symbolic link *)
  S_IFSOCK: u_short = 8_0140000;	 (* socket *)
  S_IFWHT : u_short = 8_0160000;	 (* whiteout *)
  S_ISVTX : u_short = 8_0001000;	 (* save swapped text even after use *)

  S_IFPIPE: u_short = 8_0000000;	 (* no such constant in stat.h! *)
  S_IFPORT = S_IFIFO;

(*
 * Definitions of flags stored in file flags word.
 *)
CONST
  (* Super-user and owner changeable flags. *)
  UF_SETTABLE  = 16_0000ffff;	      (* mask of owner changeable flags *)
  UF_NODUMP    = 16_00000001;	      (* do not dump file *)
  UF_IMMUTABLE = 16_00000002;	      (* file may not be changed *)
  UF_APPEND    = 16_00000004;	      (* writes to file may only append *)
  UF_OPAQUE    = 16_00000008;	      (* directory is opaque wrt. union *)

  (* Super-user changeable flags. *)
  SF_SETTABLE  = 16_ffff0000;	      (* mask of superuser changeable flags *)
  SF_ARCHIVED  = 16_00010000;	      (* file is archived *)
  SF_IMMUTABLE = 16_00020000;	      (* file may not be changed *)
  SF_APPEND    = 16_00040000;	      (* writes to file may only append *)

<*EXTERNAL*> PROCEDURE chmod(path: const_char_star; mode: mode_t): int;
<*EXTERNAL*> PROCEDURE fstat(fd: int;  buf: struct_stat_star): int;
<*EXTERNAL*> PROCEDURE mkdir(path: const_char_star; mode: mode_t): int;
<*EXTERNAL*> PROCEDURE mkfifo(path: const_char_star; mode: mode_t): int;
<*EXTERNAL*> PROCEDURE stat(path: const_char_star; buf: struct_stat_star): int;
<*EXTERNAL*> PROCEDURE umask(mode: mode_t): mode_t;
<*EXTERNAL*> PROCEDURE chflags(path: const_char_star; flags: u_long): int;
<*EXTERNAL*> PROCEDURE fchflags(fd: int; flags: u_long): int;
<*EXTERNAL*> PROCEDURE fchmod(fd: int; mode: mode_t): int;
<*EXTERNAL*> PROCEDURE lstat(path: const_char_star; buf: struct_stat_star):int;

END Ustat.
