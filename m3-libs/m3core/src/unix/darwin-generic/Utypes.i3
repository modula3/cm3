(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Sat Jan  7 14:47:05 PST 1995 by kalsow    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Jan 11 14:34:58 PST 1993 by muller    *)
(* ow Sun Nov  6 17:12:47 MET 1994                            *)

INTERFACE Utypes;

FROM Ctypes IMPORT 
	long, unsigned_long, int, unsigned_int, short, unsigned_short,
        char, unsigned_char;

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
  int8_t    = char;
  u_int8_t  = unsigned_char;
  int16_t   = short;
  u_int16_t = unsigned_short;
  int32_t   = int;
  u_int32_t = unsigned_int;
  int64_t   = RECORD val := ARRAY [0..1] OF int32_t {0,0}; END;
  u_int64_t = int64_t;

  register_t = int32_t;

  intptr_t   = long;
  uintptr_t  = unsigned_long;

  u_char  = unsigned_char;
  u_short = unsigned_short;
  u_int   = unsigned_int;
  u_long  = unsigned_long;
  ushort  = unsigned_short;             (* Sys V compatibility *)
  uint    = unsigned_int;               (* Sys V compatibility *)

  u_quad_t     = u_int64_t;		 (* quads *)
  quad_t       = int64_t;
  qaddr_t      = UNTRACED REF quad_t;

  caddr_t      = UNTRACED REF char;	 (* core address *)
  daddr_t      = int32_t;		 (* disk address *)
  dev_t        = int32_t;		 (* device number *)
  fixpt_t      = u_int32_t;		 (* fixed point number *)
  gid_t        = u_int32_t;		 (* group id *)
  in_addr_t    = u_int32_t;		 (* base type for internet address *)
  in_port_t    = u_int16_t;
  ino_t        = u_int32_t;		 (* inode number *)
  key_t        = long;			 (* IPC key (for Sys V IPC) *)
  mode_t       = u_int16_t;		 (* permissions *)
  nlink_t      = u_int16_t;		 (* link count *)
  off_t        = int64_t;           	 (* file offset *)
  pid_t        = int;			 (* process id *)
  rlim_t       = quad_t;		 (* resource limit *)
  segsz_t      = int32_t;		 (* segment size *)
  swblk_t      = int32_t;		 (* swap offset *)
  uid_t        = u_int32_t;		 (* user id *)
  useconds_t   = u_int32_t;		 (* microseconds (unsigned) *)

  clock_t      = u_long;
  size_t       = unsigned_int;
  ssize_t      = unsigned_int;
  time_t       = long;

CONST
  NBBY = 8;				 (* number of bits in a byte *)

  (*
   * Select uses bit masks of file descriptors in longs.
   * These macros manipulate such bit fields (the filesystem macros use chars).
   * FD_SETSIZE may be defined by the user, but the default here
   * should be >= NOFILE (param.h).
   *)
  FD_SETSIZE = 1024;

TYPE
  fd_mask        = int32_t;

CONST
  NFDBITS = BYTESIZE (fd_mask) * NBBY;      (* bits per mask *)
  NFDSHIFT = 5;                             (* Shift based on above *)

PROCEDURE howmany (x, y: int): int;

TYPE
  struct_fd_set = RECORD
    fds_bits: ARRAY [0..(FD_SETSIZE + NFDBITS - 1) DIV NFDBITS -1] OF fd_mask;
  END;
  fd_set = struct_fd_set;

PROCEDURE FD_SET   (n: int; p: UNTRACED REF fd_set): int;
PROCEDURE FD_CLEAR (n: int; p: UNTRACED REF fd_set): int;
PROCEDURE FD_ISSET (n: int; p: UNTRACED REF fd_set): int;
PROCEDURE FD_ZERO  (p: UNTRACED REF fd_set);

<*EXTERNAL "m3_asLong"*>
PROCEDURE asLong(val: off_t): long;

END Utypes.
