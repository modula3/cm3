(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Mar 15 16:48:53 PST 1995 by kalsow     *)
(*      modified on Wed Oct 12 11:55:40 PDT 1994 by ericv      *)
(*      modified on Sat Jun 27 15:27:28 PDT 1992 by muller     *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, long, char_star,char;
FROM Utypes IMPORT dev_t, ino_t, off_t,
                   mode_t, nlink_t, uid_t, gid_t, time_t;


(* MODE MASKS *)

(* de facto standard definitions *)
CONST
  S_IFMT	 = 16_F000;	(* type of file *)
  S_IAMB	 = 16_1FF;	(* access mode bits *)
  S_IFIFO	 = 16_1000;	(* fifo *)
  S_IFCHR	 = 16_2000;	(* character special *)
  S_IFDIR	 = 16_4000;	(* directory *)
  S_IFNAM	 = 16_5000;  (* XENIX special named file *)
  S_INSEM  = 16_1;	(* XENIX semaphore subtype of IFNAM *)
  S_INSHD  = 16_2;	(* XENIX shared data subtype of IFNAM *)

  S_IFBLK	 = 16_6000;	(* block special *)
  S_IFREG	 = 16_8000;	(* regular *)
  S_IFLNK	 = 16_A000;	(* symbolic link *)
  S_IFSOCK	 = 16_C000;	(* socket *)

(*** PIPE and PORT don't exist, they're for src/os/POSIX/FilePosix.m3 ***)
  S_IFPIPE       = 16_1000;     (* fifo *)
  S_IFPORT       = 16_0000;     (* pipe *)

  S_ISUID	 = 16_800;	(* set user id on execution *)
  S_ISGID	 = 16_400;	(* set group id on execution *)

  S_ISVTX	 = 16_200;	(* save swapped text even after use *)

  S_ENFMT	 = S_ISGID;	(* record locking enforcement flag *)

(* the following macros are for POSIX conformance *)

  S_IRWXU	 = 8_0700;		(* read, write, execute: owner *)
  S_IRUSR	 = 8_0400;		(* read permission: owner *)
  S_IWUSR	 = 8_0200;		(* write permission: owner *)
  S_IXUSR	 = 8_0100;		(* execute permission: owner *)
  S_IRWXG	 = 8_0070;		(* read, write, execute: group *)
  S_IRGRP	 = 8_0040;		(* read permission: group *)
  S_IWGRP	 = 8_0020;		(* write permission: group *)
  S_IXGRP	 = 8_0010;		(* execute permission: group *)
  S_IRWXO	 = 8_0007;		(* read, write, execute: other *)
  S_IROTH	 = 8_0004;		(* read permission: other *)
  S_IWOTH	 = 8_0002;		(* write permission: other *)
  S_IXOTH	 = 8_0001;		(* execute permission: other *)

  (* deprecated: use S_IRUSR, ... defined above *)
  S_IREAD  = S_IRUSR;
  S_IWRITE = S_IWUSR;
  S_IEXEC  = S_IXUSR;
  S_GREAD  = S_IRGRP;
  S_GWRITE = S_IWGRP;
  S_GEXEC  = S_IXGRP;
  S_OREAD  = S_IROTH;
  S_OWRITE = S_IWOTH;
  S_OEXEC  = S_IXOTH;


TYPE
  struct_stat = RECORD
               st_dev    : dev_t;
	       st_pad1	 : ARRAY [0..2] OF long;
               st_ino    : ino_t;
               st_mode   : mode_t;
               st_nlink  : nlink_t;
               st_uid    : uid_t;
               st_gid    : gid_t;
               st_rdev   : dev_t;
	       st_pad2 	 : ARRAY [0..1] OF long;
               st_size   : off_t;
	       st_pad3	 : long;

            (* st_atime  : timestruc_t;  *** KLUDGE for FSPosix and FilePosix*)
               st_atime  : time_t;
               st_spare1 : long;

            (* st_mtime  : timestruc_t;  *** KLUDGE for FSPosix and FilePosix*)
               st_mtime  : time_t;
               st_spare2 : long;

            (* st_ctime  : timestruc_t;  *** KLUDGE for FSPosix and FilePosix*)
               st_ctime  : time_t;
               st_spare3 : long;

               st_blksize: long;
               st_blocks : long;
	       st_fstype : ARRAY  [0..15] OF char ;
	       st_pad4 	 : ARRAY  [0..7] OF long; END;

  struct_stat_star = UNTRACED REF struct_stat;

<*EXTERNAL*> PROCEDURE stat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE lstat (path: char_star; buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

END Ustat.
