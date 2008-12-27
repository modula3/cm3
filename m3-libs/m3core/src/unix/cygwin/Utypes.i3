(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* This file was generated from Utypes.i3.cpp. Do not edit it. *)

INTERFACE Utypes;

IMPORT Ctypes;

TYPE
  uint8_t = Ctypes.unsigned_char;
  uint16_t = Ctypes.unsigned_short;
  uint32_t = Ctypes.unsigned_int;
  uint64_t = Ctypes.unsigned_long_long;
  int8_t = Ctypes.char;
  int16_t = Ctypes.short;
  int32_t = Ctypes.int;
  int64_t = Ctypes.long_long;
  u_char = uint8_t;
  u_short = uint16_t;
  u_int = uint32_t;
  u_long = uint32_t;
  size_t = uint32_t;
  time_t = int32_t;
  off_t = int64_t;
  clock_t = uint32_t;
  uid_t = uint32_t;
  pid_t = int32_t;
  gid_t = uint32_t;

  socklen_t = int32_t;
  hostent_addrtype_t = int16_t;
  hostent_length_t = int16_t;

  struct_timespec = RECORD
    tv_sec  : int32_t;
    tv_nsec : int32_t;
  END;
  timestruc_t = struct_timespec;

END Utypes.
