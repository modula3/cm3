(* Copyright (C) 1989, 1992, Digital Equipment Corporation                   *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Sep 24 14:26:49 PDT 1993 by kalsow                   *)
(*      modified on Tue Mar 24 20:35:14 PST 1992 by muller                   *)

INTERFACE Ustat;

FROM Ctypes IMPORT short, int, char_star, unsigned_long, unsigned_short;
FROM Utypes IMPORT u_short, dev_t, ino_t, off_t, time_t, uid_t, gid_t;

(*** sys/stat.h ***)

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
                 st_dev         : dev_t;
                 st_ino         : ino_t;
                 st_mode_ext    : unsigned_short;
                 st_mode        : unsigned_short;
                 st_nlink       : short;
                 st_pad_to_word : short;
                 st_uid_ext     : short;
                 st_uid         : unsigned_short;
                 st_gid_ext     : unsigned_short;
                 st_gid         : unsigned_short;
                 st_rdev        : dev_t;
                 st_size        : off_t;
                 st_atime       : time_t;
                 st_spare1      : int;
                 st_mtime       : time_t;
                 st_spare2      : int;
                 st_ctime       : time_t;
                 st_spare3      : int;
                 st_blksize     : unsigned_long;
                 st_blocks      : unsigned_long;
    	         st_vfstype     : unsigned_long;     (* Type of fs *)
                 st_vfs	        : unsigned_long;  (* Vfs number *)
    	         st_type        : unsigned_long;  (* Vnode type *)
     	         st_gen         : unsigned_long;  (* Inode generation number *)
  	         st_flag        : unsigned_long;  (* Flag word *)
	         Reserved1      : uid_t;  (* Reserved *)
	         Reserved2      : gid_t;  (* Reserved *)
	         st_access      : unsigned_short; (* Process' access to file *)
	         st_spare4      : ARRAY [0..4] OF unsigned_long; (*Reserved *)
                END;

  struct_stat_star = UNTRACED REF struct_stat;

(*** stat, lstat, fstat - get file status ***)

<*EXTERNAL*> PROCEDURE stat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE fstat (fd: int; buf: struct_stat_star): int;


END Ustat.
