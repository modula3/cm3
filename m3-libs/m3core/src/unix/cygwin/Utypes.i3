(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Apr 29 15:38:49 PDT 1994 by kalsow    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Jan 11 14:34:58 PST 1993 by muller    *)

(* $Id: Utypes.i3,v 1.9 2008-02-11 05:38:24 jkrell Exp $ *)

(* This file was generated from Utypes.i3.cpp. Do not edit it. *)

INTERFACE Utypes;

FROM Ctypes IMPORT 
	long, unsigned_long, int, unsigned_int, unsigned_short,
        unsigned_char, long_long, unsigned_long_long;

(*** <sys/types.h> ***)

(*
 * Basic system types and major/minor device constructing/busting macros.
 *)

(* major part of a device *)
PROCEDURE major (x: dev_t): int;

(* minor part of a device *)
PROCEDURE minor (x: dev_t): int;

(* make a device number *)
PROCEDURE makedev (x, y: int): dev_t;

TYPE
  u_char  = unsigned_char;
  u_short = unsigned_short;
  u_int   = unsigned_int;
  uint    = unsigned_int; (* sys V compatibility *)
  u_long  = unsigned_long;
  ushort  = unsigned_short; (* sys III compat *)

  quad         = long_long;
  daddr_t      = long; 
  caddr_t      = ADDRESS;
  ino_t        = unsigned_long_long;

  size_t       = u_int;
  time_t       = long;
  dev_t        = u_long;
  off_t        = long_long;

  key_t        = long_long; (* sys V compatibility *)
  clock_t      = u_long; (* POSIX compliance *)
  mode_t       = u_int; (* POSIX compliance *)
  nlink_t      = u_short; (* POSIX compliance *)
  uid_t        = u_long; (* POSIX compliance *)
  pid_t        = int; (* POSIX compliance *)
  gid_t        = u_long; (* POSIX compliance *)

CONST
  NBBY = 8;                           (* number of bits in a byte *)

  (*
   * Select uses bit masks of file descriptors in longs.
   * These macros manipulate such bit fields (the filesystem macros use chars).
   * FD_SETSIZE may be defined by the user, but the default here
   * should be >= NOFILE (param.h).
   *)
  FD_SETSIZE = 64;

  (* How many things we'll allow select to use. 0 if unlimited *)
  MAXSELFD = 256;

TYPE
  fd_mask        = long;
 
CONST
  NFDBITS = BYTESIZE (fd_mask) * NBBY;      (* bits per mask (power of 2!)*)
  NFDSHIFT = 5;                             (* Shift based on above *)

PROCEDURE howmany (x, y: int): int;

TYPE
  struct_fd_set = RECORD
       fds_bits: ARRAY [0 .. 
                        (FD_SETSIZE + NFDBITS - 1) DIV NFDBITS -1] OF fd_mask;
    END;
  fd_set = struct_fd_set;

PROCEDURE FD_SET   (n: int; p: UNTRACED REF fd_set): int;
PROCEDURE FD_CLEAR (n: int; p: UNTRACED REF fd_set): int;
PROCEDURE FD_ISSET (n: int; p: UNTRACED REF fd_set): int;
PROCEDURE FD_ZERO  (p: UNTRACED REF fd_set);

END Utypes.
