(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

INTERFACE Uin;

FROM Utypes IMPORT uint16_t, uint32_t;
FROM Ctypes IMPORT char;

CONST
  IPPROTO_TCP = 6;

TYPE

  in_port_t = uint16_t;
  sa_family_t = uint16_t;
  in_addr_t = uint32_t;

  struct_in_addr = RECORD
    s_addr: in_addr_t;
  END;

  struct_sockaddr_in = BITS 16 * 8 FOR RECORD
    sin_family: sa_family_t;
    sin_port: in_port_t;
    sin_addr: struct_in_addr;
    (* Pad to size of `struct sockaddr'. *)
    sin_zero : ARRAY [0..7] OF char;
  END;
  struct_sockaddr_in_star = UNTRACED REF struct_sockaddr_in;

PROCEDURE ntohl(x: uint32_t): uint32_t;
PROCEDURE ntohs(x: uint16_t): uint16_t;
PROCEDURE htonl(x: uint32_t): uint32_t;
PROCEDURE htons(x: uint16_t): uint16_t;

END Uin.
