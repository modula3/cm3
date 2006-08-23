(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Utypes;

FROM Ctypes IMPORT 
	long, unsigned_long, int, unsigned_int, short, unsigned_short,
        char, unsigned_char, long_long, unsigned_long_long;

(*** <i386/types.h> ***)

TYPE
  int8_t    = char;
  u_int8_t  = unsigned_char;
  int16_t   = short;
  u_int16_t = unsigned_short;
  int32_t   = int;
  u_int32_t = unsigned_int;
  int64_t   = long_long;
  u_int64_t = unsigned_long_long;

(*** <sys/types.h> ***)

(*
 * Basic system types and major/minor device constructing/busting macros.
 *)

(* major part of a device *)
PROCEDURE major (x: int): int;

(* minor part of a device *)
PROCEDURE minor (x: int): int;

(* make a device number *)
PROCEDURE makedev (x, y: int): dev_t;

TYPE
  u_char  = unsigned_char;
  u_short = unsigned_short;
  u_int   = unsigned_int;
  u_long  = unsigned_long;
  ushort  = unsigned_short;             (* Sys V compatibility *)
  uint    = unsigned_int;               (* Sys V compatibility *)

TYPE
  u_quad_t     = u_int64_t;		 (* quads *)
  quad_t       = int64_t;
  qaddr_t      = UNTRACED REF quad_t;

  caddr_t      = UNTRACED REF char;	 (* core address *)
  daddr_t      = int32_t;		 (* disk address *)
  dev_t        = int32_t;		 (* device number *)
  fixpt_t      = u_int32_t;		 (* fixed point number *)
  blkcnt_t     = int64_t;		 (* total blocks *)
  blksize_t    = int32_t;		 (* preferred block size *)
  gid_t        = u_int32_t;		 (* group id *)
  in_addr_t    = u_int32_t;		 (* base type for internet address *)
  in_port_t    = u_int16_t;
  ino_t        = u_int32_t;		 (* inode number *)
  key_t        = int32_t;		 (* IPC key (for Sys V IPC) *)
  mode_t       = u_int16_t;		 (* permissions *)
  nlink_t      = u_int16_t;		 (* link count *)
  id_t         = u_int32_t;
  pid_t        = int32_t;		 (* process id *)
  off_t        = int64_t;           	 (* file offset *)
  segsz_t      = int32_t;		 (* segment size *)
  swblk_t      = int32_t;		 (* swap offset *)
  uid_t        = u_int32_t;		 (* user id *)

  clock_t      = unsigned_long;		 (* clock() *)
  size_t       = unsigned_long;
  ssize_t      = long;
  time_t       = long;

  useconds_t   = u_int32_t;		 (* microseconds (unsigned) *)
  suseconds_t  = int32_t;		 (* microseconds (signed) *)

(*
 * This code is present here in order to maintain historical backward
 * compatability, and is intended to be removed at some point in the
 * future; please include <sys/select.h> instead.
 *)
CONST
  NBBY = 8;				 (* bits in a byte *) 
  NFDBITS = BYTESIZE (int32_t) * NBBY;	 (* bits per mask *)

PROCEDURE howmany (x, y: int): int;

TYPE
  fd_mask = int32_t;

(*
 * Select uses bit masks of file descriptors in longs.  These macros
 * manipulate such bit fields (the filesystem macros use chars).  The
 * extra protection here is to permit application redefinition above
 * the default size.
 *)
CONST
  FD_SETSIZE = 1024;

TYPE
  struct_fd_set = RECORD
    fds_bits: ARRAY [0..(FD_SETSIZE + NFDBITS - 1) DIV NFDBITS - 1] OF int32_t;
  END;
  fd_set = struct_fd_set;

PROCEDURE FD_SET   (n: int; VAR p: fd_set): int;
PROCEDURE FD_CLR (n: int; VAR p: fd_set): int;
PROCEDURE FD_ISSET (n: int; READONLY p: fd_set): int;
PROCEDURE FD_ZERO  (VAR p: fd_set);

<*EXTERNAL "m3_asLong"*>
PROCEDURE asLong(val: off_t): long;

<*EXTERNAL "m3_assignOffT"*>
PROCEDURE assignOffT (VAR dest: off_t; src: long);

END Utypes.
