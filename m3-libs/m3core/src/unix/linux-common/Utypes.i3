(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Utypes;

IMPORT Cstddef;
IMPORT Cstdint;
IMPORT Usysdep;

TYPE
  uint8_t = Cstdint.uint8_t;
  uint16_t = Cstdint.uint16_t;
  uint32_t = Cstdint.uint32_t;
  uint64_t = Cstdint.uint64_t;

  int8_t = Cstdint.int8_t;
  int16_t = Cstdint.int16_t;
  int32_t = Cstdint.int32_t;
  int64_t = Cstdint.int64_t;

  u_int8_t = uint8_t;
  u_int16_t = uint16_t;
  u_int32_t = uint32_t;
  u_int64_t = uint64_t;

  u_short = uint16_t;
  u_int   = uint32_t;

  clock_t = Usysdep.clock_t; (* ideally always 64 bits *)
  gid_t = Usysdep.gid_t;
  off_t = Usysdep.off_t; (* ideally always 64 bits *)
  pid_t = Usysdep.pid_t;
  size_t = Cstddef.size_t;
  ssize_t = INTEGER;
  time_t = Usysdep.time_t; (* ideally always 64 bits *)
  uid_t = Usysdep.uid_t;

  socklen_t = Usysdep.socklen_t;
  hostent_addrtype_t = Usysdep.hostent_addrtype_t;
  hostent_length_t = Usysdep.hostent_length_t;

END Utypes.
