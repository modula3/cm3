(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Mar  4 11:52:37 PST 1992 by muller        *)

INTERFACE Utypes;

FROM Ctypes IMPORT 
	long, unsigned_long, int, unsigned_int, short, unsigned_short,
        unsigned_char, signed_char;

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
  s_char  = signed_char;
  u_char  = unsigned_char;
  u_short = unsigned_short;
  u_int   = unsigned_int;
  uint    = unsigned_int;               (* sys V compatibility *)
  u_long  = unsigned_long;
  ushort  = unsigned_short;             (* sys III compat *)

(* #ifdef vax *)
  struct__physadr = RECORD r: ARRAY [0..0] OF int; END;
  physadr         = UNTRACED REF struct__physadr;

  struct_label_t = RECORD val: ARRAY [0..13] OF int; END;
  label_t        = struct_label_t;
(*#endif*)

  struct__quad = RECORD val: ARRAY [0..1] OF long; END;
  quad         = struct__quad;
  daddr_t      = long; 
  caddr_t      = ADDRESS;
  ino_t        = u_long;
  gno_t        = u_long;
  cnt_t        = short;               (* sys V compatibility *)
  swblk_t      = long;
  size_t       = int;
  time_t       = int;
  dev_t        = short;
  off_t        = int;
  paddr_t      = long;                (* sys V compatibility *)
  key_t        = long;                (* sys V compatibility *)
  clock_t      = int;                 (* POSIX compliance    *)
  mode_t       = u_short;             (* POSIX compliance    *)
  nlink_t      = short;               (* POSIX compliance    *)
  uid_t        = short;               (* POSIX compliance    *)
  pid_t        = int;                 (* POSIX compliance    *)
  gid_t        = short;               (* POSIX compliance    *)

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
  MAXSELFD = 64;

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
