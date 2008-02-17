(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Apr 29 15:38:49 PDT 1994 by kalsow    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Jan 11 14:34:58 PST 1993 by muller    *)

(* $Id$ *)

(* This file was generated from Utypes.i3.cpp. Do not edit it. *)

INTERFACE Utypes;

FROM Ctypes IMPORT 
	long, unsigned_long, int, unsigned_int, unsigned_short,
        unsigned_char, long_long, unsigned_long_long;

(*** <sys/types.h> ***)

(*
 * Basic system types.
 *)

TYPE

  u_char = unsigned_char;
  u_short = unsigned_short;
  u_int = unsigned_int;
  u_long = unsigned_long;
  ino_t        = unsigned_long_long; (* inode -- the same for multiple hard links to the same file *)
  size_t       = unsigned_int;
  time_t       = long;
  dev_t        = unsigned_long; (* device *) 
  off_t        = long_long; (* file size or offset *)
  clock_t      = unsigned_long;
  mode_t       = unsigned_int; (* mode of a file *)
  nlink_t      = unsigned_short; (* number of links to a file *)
  uid_t        = unsigned_long; (* user id *)
  pid_t        = int; (* process id *)
  gid_t        = unsigned_long; (* group id *)

  (* for struct stat *)
  blkcnt_t     = long_long;
  blksize_t     = long;

  struct_timespec = RECORD
    tv_sec  : long; (* Seconds *)
    tv_nsec : long; (* Nanoseconds *)
  END;

  timestruc_t = struct_timespec;

END Utypes.
