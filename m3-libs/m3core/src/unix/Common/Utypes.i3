(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Utypes;

IMPORT Cstddef;
IMPORT Cstdint;
IMPORT Usysdep;

TYPE

  (* useless forwarding of unsigned fixed sized types; favor Cstdint instead *)
  uint8_t = Cstdint.uint8_t;
  uint16_t = Cstdint.uint16_t;
  uint32_t = Cstdint.uint32_t;
  uint64_t = Cstdint.uint64_t;

  (* useless forwarding of signed fixed sized types; favor Cstdint instead *)
  int8_t = Cstdint.int8_t;
  int16_t = Cstdint.int16_t;
  int32_t = Cstdint.int32_t;
  int64_t = Cstdint.int64_t;

  (* useless funny synonyms; favor explicitly sized types *)
  u_short = uint16_t;
  u_int = uint32_t;

 (* Exactly pointer-sized integers, unsigned and signed *)
  size_t = Cstddef.size_t;
  ssize_t = Cstddef.ptrdiff_t;

  (* This might not be right for all systems, but let's hope. We
  can make it so with wrapper functions, if function parameters are the
  only occurence. If it occurs in structs, then no. *)
  off_t = int64_t;

  (* Ideally this is always 64 bits, else time runs out in 2038. *)
  time_t = Usysdep.time_t;

  gid_t = Usysdep.gid_t;
  pid_t = Usysdep.pid_t;
  uid_t = Usysdep.uid_t;

  socklen_t = Cstdint.uint32_t; (* int32_t on Cygwin, size_t on HP-UX, but ok via wrappers (see Usocket.i3) *)

  mode_t = INTEGER;
  dev_t = LONGINT;
  ino_t = LONGINT;

END Utypes.
