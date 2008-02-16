(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Apr 29 15:38:49 PDT 1994 by kalsow    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Jan 11 14:34:58 PST 1993 by muller    *)

(* $Id: Utypes.i3,v 1.11 2008-02-16 20:44:40 jkrell Exp $ *)

(* This file was generated from Utypes.i3.cpp. Do not edit it. *)

INTERFACE Utypes;

FROM Ctypes IMPORT 
	long, unsigned_long, int, unsigned_int, unsigned_short,
        unsigned_char, long_long, unsigned_long_long;

(*** <sys/types.h> ***)

(*
 * Basic system types and major/minor device constructing/busting macros.
 *)

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

END Utypes.
