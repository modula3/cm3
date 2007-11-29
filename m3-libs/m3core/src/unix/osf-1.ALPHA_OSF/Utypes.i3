(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Aug 20 09:49:52 PDT 1993 by kalsow    *)
(*      modified on Wed Apr  7 16:01:28 PDT 1993 by muller    *)

INTERFACE Utypes;

FROM Ctypes IMPORT 
	long, unsigned_long, int, unsigned_int, short, unsigned_short,
        unsigned_char, char_star, void_star, char;

(*** <sys/types.h> ***)


TYPE
  ptrdiff_t  = long;
  wchar_t    = unsigned_char;
  wctype_t   = unsigned_int;
  fpos_t     = long;
  time_t     = int;
  clock_t    = int;
  size_t     = unsigned_long;
  ssize_t    = long;


(*
 * shorthand type definitions for unsigned storage classes
 *)
TYPE
  uchar     = unsigned_char;
  u_char    = unsigned_char;
  uchar_t   = unsigned_char;
  vuchar_t  = (* volatile *) unsigned_char;
  vu_char   = (* volatile *) unsigned_char;

  ushort    = unsigned_short;
  u_short   = unsigned_short;
  ushort_t  = unsigned_short;
  vushort_t = (* volatile *) unsigned_short;
  vu_short  = (* volatile *) unsigned_short;

  uint      = unsigned_int;
  u_int     = unsigned_int;
  uint_t    = unsigned_int;
  vuint_t   = (* volatile *) unsigned_int;
  vu_int    = (* volatile *) unsigned_int;

  ulong     = unsigned_long;
  u_long    = unsigned_long;
  ulong_t   = unsigned_long;
  vulong_t  = (* volatile *) unsigned_long;
  vu_long   = (* volatile *) unsigned_long;

TYPE
  struct__physadr = RECORD r: ARRAY [0..0] OF long; END;
  physadr_t       = UNTRACED REF struct__physadr;
  physadr         = physadr_t;

TYPE
  struct_label_t  = RECORD val: ARRAY [0..9] OF long; END;
  label_t         = struct_label_t;

TYPE
  level_t      = int;

  daddr_t      = int;            (* disk address *)
  caddr_t      = char_star;      (* "core" (i.e. memory) address *)
  qaddr_t      = UNTRACED REF quad;
  addr_t       = char_star;

  ino_t        = uint_t;         (* inode number (filesystem) *)
  cnt_t        = short;
  dev_t        = int;            (* device number (major+minor) *)
  chan_t       = int;            (* channel number (minor's minor) *)
  off_t        = long;           (* file offset *)

  rlim_t       = unsigned_long;  (* resource limit *)
  paddr_t      = int;
  nlink_t      = ushort_t;

  key_t        = int;            (* ipc key type *)
  mode_t       = uint_t;         (* file mode *)
  uid_t        = uint_t;         (* user ID *)
  gid_t        = uint_t;         (* group ID *)
  mid_t        = void_star;      (* module ID *)
  pid_t        = int;            (* process ID *)

  slab_t       = ARRAY [0..11] OF char;  (* security label *)

(*
 * The following type is for various kinds of identifiers.  The
 * actual type must be the same for all since some system calls
 * (such as sigsend) take arguments that may be any of these
 * types.  The enumeration type idtype_t defined in sys/procset.h
 * is used to indicate what type of id is being specified.
 *)
TYPE
  id_t = pid_t; (* A process, process group, session, scheduling class,
                   user or group id. *)

CONST
  P_MYID = -1;  (* a usually illegal value for IDs, but specifying
                   whatever the value is for my process *)


TYPE
  shmatt_t  = ulong_t;  (* for shmid_ds.shm_nattach *)
  msgqnum_t = ulong_t;  (* for msqid_ds.msg_qnum *)
  msglen_t  = ulong_t;  (* for msqid_ds.msg_qbytes *)


TYPE
  wint_t   = unsigned_int;      (* wide character *)
  sigset_t = unsigned_long;   (* signal mask *)
  timer_t  = long;            (* timer id *)

TYPE
  sig_t  = PROCEDURE ();


(*
 * Types used by dev_t annotation procedures
 *)
TYPE
  major_t = uint_t;      (* major device number   *)
  minor_t = uint_t;      (* minor device number   *)
  devs_t  = uint_t;      (* device-specific info  *)
  unit_t  = uint_t;      (* unit number of device *)


(*
 * Basic system types and major/minor device constructing/busting macros.
 *)

(* major part of a device *)
PROCEDURE major (x: dev_t): major_t;

(* minor part of a device *)
PROCEDURE minor (x: dev_t): minor_t;

(* make a device number *)
PROCEDURE makedev (x: major_t;  y: minor_t): dev_t;

TYPE
  struct__quad = RECORD val: ARRAY [0..1] OF int; END;
  quad         = struct__quad;

TYPE
  swblk_t      = long;
  fixpt_t      = u_long;

CONST
  NBBY = 8; (* number of bits in a byte *)

  (*
   * Select uses bit masks of file descriptors in longs.
   * These macros manipulate such bit fields (the filesystem macros use chars).
   * FD_SETSIZE may be defined by the user, but the default here
   * should be equal to OPEN_MAX_SYSTEM (param.h).
   *)
  MAX_NOFILE = 4096;
  FD_SETSIZE = MAX_NOFILE;

  (* How many things we'll allow select to use. 0 if unlimited *)
  MAXSELFD = MAX_NOFILE;

TYPE
  fd_mask = int;
 
CONST
  NFDBITS = BYTESIZE (fd_mask) * NBBY;      (* bits per mask (power of 2!)*)
  NFDSHIFT = 5;                             (* Shift based on above *)

PROCEDURE howmany (x, y: INTEGER): INTEGER;

CONST
  n_masks = (FD_SETSIZE + NFDBITS - 1) DIV NFDBITS;
            (* == howmany (FD_SETSIZE, NFDBITS) *)

TYPE
  struct_fd_set = RECORD
       fds_bits: ARRAY [0 .. n_masks - 1] OF fd_mask;
  END;
  fd_set = struct_fd_set;

PROCEDURE FD_SET   (n: INTEGER; p: UNTRACED REF fd_set): INTEGER;
PROCEDURE FD_CLEAR (n: INTEGER; p: UNTRACED REF fd_set): INTEGER;
PROCEDURE FD_ISSET (n: INTEGER; p: UNTRACED REF fd_set): INTEGER;
PROCEDURE FD_ZERO  (p: UNTRACED REF fd_set);

END Utypes.
