(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

(* INTERFACE Unix; *)

IMPORT Ctypes;
FROM Ctypes IMPORT long;
FROM Cstdint IMPORT uint16_t, uint32_t, int16_t, int32_t, int64_t;

(* Unix.i3 *)

CONST
  MaxPathLen = 1024;
  MSETUID = 0;
  MSETGID = 0;
  MSTICKY = 0;
  MROWNER = 8_0400;
  MWOWNER = 8_0200;
  MXOWNER = 8_0100;
  MRGROUP = 8_0040;
  MWGROUP = 8_0020;
  MXGROUP = 8_0010;
  MROTHER = 8_0004;
  MWOTHER = 8_0002;
  MXOTHER = 8_0001;
  F_OK = 16_0;
  X_OK = 16_1;
  W_OK = 16_2;
  R_OK = 16_4;
  P_NOWAIT = 16_2;
  F_SETFD = 16_2;
  F_GETFL = 16_3;
  F_SETFL = 16_4;
  FIONREAD = 16_4004667f;
  O_RDONLY = 16_0;
  O_RDWR = 16_2;
  O_CREAT = 16_200;
  O_EXCL = 16_800;
  O_TRUNC = 16_400;
  O_NDELAY = 16_4000;
  M3_NONBLOCK = 16_4000;

CONST
  MAX_FDSET = 1024;

(* INTERFACE Usignal; *)

CONST
  SIGINT = 16_00000002;
  SIGKILL = 16_00000009;

TYPE
  SignalHandler = ADDRESS;
  SignalActionHandler = ADDRESS;

(* INTERFACE Usocket; *)

CONST
  SOCK_STREAM = 1;
  SOCK_DGRAM = 2;

  SO_REUSEADDR = 16_0004;
  SO_KEEPALIVE = 16_0008;
  SO_LINGER = 16_0080;

  SOL_SOCKET = 16_FFFF;
  AF_INET = 2;
  MSG_PEEK = 2;

TYPE
  struct_linger = RECORD
    l_onoff: uint16_t;
    l_linger: uint16_t;
  END;

(* INTERFACE Ustat; *)

CONST
  S_IFMT  : uint16_t = 8_0170000;
  S_IFSOCK: uint16_t = 8_0140000;
  S_IFLNK : uint16_t = 8_0120000;
  S_IFREG : uint16_t = 8_0100000;
  S_IFBLK : uint16_t = 8_0060000;
  S_IFDIR : uint16_t = 8_0040000;
  S_IFCHR : uint16_t = 8_0020000;
  S_IFIFO : uint16_t = 8_0010000;
  S_IREAD : uint16_t = 8_0400;
  S_IWRITE: uint16_t = 8_0200;
  S_IEXEC : uint16_t = 8_0100;
  S_GREAD : uint16_t = 0;
  S_GWRITE: uint16_t = 0;
  S_GEXEC : uint16_t = 0;
  S_OREAD : uint16_t = 0;
  S_OWRITE: uint16_t = 0;
  S_OEXEC : uint16_t = 0;

(* INTERFACE Utime; *)

CONST
  ITIMER_REAL = 0;
  ITIMER_VIRTUAL = 1;

TYPE
  struct_timeval = RECORD
    tv_sec: long;
    tv_usec: long;
  END;

  struct_timezone = RECORD
    tz_minuteswest:  int32_t;
    tz_dsttime:      int32_t;
  END;

  struct_timespec = RECORD
    tv_sec: time_t;
    tv_nsec: long;
  END;

  struct_itimerval = RECORD
    it_interval: struct_timeval;
    it_value: struct_timeval;
  END;

  struct_tm_star = UNTRACED REF struct_tm;
  struct_tm = RECORD
    tm_sec:   int32_t;
    tm_min:   int32_t;
    tm_hour:  int32_t;
    tm_mday:  int32_t;
    tm_mon:   int32_t;
    tm_year:  int32_t;
    tm_wday:  int32_t;
    tm_yday:  int32_t;
    tm_isdst: int32_t;
  END;

(* INTERFACE Utypes; *)

TYPE
  (* avoid ever using u_long; use Cstdint.uint32_t or Cstddef.size_t instead *)
  u_long = uint32_t;

  (* use Cstddef.size_t instead *)
  size_t = uint32_t;

  (* ideally uint64_t or uint64_t, on all platforms, deal with it later *)
  time_t = int32_t;

  (* ideally uint64_t or int64_t, on all platforms, deal with it later *)
  off_t = int64_t;

  (* ideally uint64_t or int64_t, on all platforms, deal with it later *)
  clock_t = uint32_t;

  uid_t = uint32_t;
  pid_t = int32_t;
  gid_t = uint32_t;

  socklen_t = int32_t;
  hostent_addrtype_t = int16_t;
  hostent_length_t = int16_t;

(*
  struct_timespec = RECORD
    tv_sec  : int32_t;
    tv_nsec : int32_t;
  END;
*)

(* INTERFACE Uutsname; *)

CONST
  SYS_NMLN = 20;

END Usysdep.
