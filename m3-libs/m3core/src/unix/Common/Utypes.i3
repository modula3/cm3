(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Utypes;

IMPORT Cstddef, Cstdint;

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

  (* synonyms for compatibility *)
  u_int8_t = uint8_t;
  u_int16_t = uint16_t;
  u_int32_t = uint32_t;
  u_int64_t = uint64_t;

 (* Exactly pointer-sized integers, unsigned and signed *)
  size_t = Cstddef.size_t;
  ssize_t = Cstddef.ptrdiff_t;

  (* This might not be right for all systems, but let's hope. We
   * can make it so with wrapper functions, if function parameters are the
   * only occurence. If it occurs in structs, then no.
   *)
  off_t = int64_t;

  time_t = LONGINT;

  (* NOTE: Signedness of gid_t and uid_t can be important.
   * see how cvsup sets NoOwner and NoGroup.
   *)
  gid_t = INTEGER; (* signed on some systems, unsigned on some systems, portable code cannot depend on either *)
  pid_t = INTEGER; (* generally only 32 bits but ok *)
  uid_t = INTEGER; (* signed on some systems, unsigned on some systems, portable code cannot depend on either *)

  socklen_t = size_t; (* underlying platform varies *)

  mode_t  = INTEGER; (* often only 16 bits but ok *)
  dev_t   = LONGINT; (* sometimes 32 bits, sometimes 64 bits *)
  ino_t   = LONGINT; (* sometimes 32 bits, sometimes 64 bits *)
  nlink_t = LONGINT; (* sometimes 16 bits, sometimes 32, unknown if ever 64, ok *)

END Utypes.
