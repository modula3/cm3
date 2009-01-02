(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* This file was generated from Utypes.i3.cpp. Do not edit it. *)

INTERFACE Utypes;

IMPORT Ctypes;
IMPORT Usysdep;

TYPE

  (* fixed size types; favor Cstdint instead *)
  uint8_t = Ctypes.unsigned_char;
  uint16_t = Ctypes.unsigned_short;
  uint32_t = Ctypes.unsigned_int;
  uint64_t = Ctypes.unsigned_long_long;
  int8_t = Ctypes.char;
  int16_t = Ctypes.short;
  int32_t = Ctypes.int;
  int64_t = Ctypes.long_long;

  (* funny names; avoid using *)
  u_char = uint8_t;
  u_short = uint16_t;
  u_int = uint32_t;

  (* avoid using u_long; use uint32_t or size_t instead, whichever is correct *)
  u_long = Usysdep.u_long;

  size_t = Usysdep.size_t;

  (* ideally all file sizes, file offsets, times are 64 bit, but deal with that later *)
  time_t = Usysdep.time_t;
  off_t = Usysdep.off_t;
  clock_t = Usysdep.clock_t;

  uid_t = Usysdep.uid_t;
  pid_t = Usysdep.pid_t;
  gid_t = Usysdep.gid_t;

  socklen_t = Usysdep.socklen_t;
  hostent_addrtype_t = Usysdep.hostent_addrtype_t;
  hostent_length_t = Usysdep.hostent_length_t;

  struct_timespec = Usysdep.struct_timespec;

  timestruc_t = struct_timespec;

END Utypes.
