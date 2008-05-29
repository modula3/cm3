(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

INTERFACE Uin;

FROM Ctypes IMPORT char;
FROM Utypes IMPORT uint8_t, uint16_t, uint32_t;

CONST
  IPPROTO_TCP = 6;

TYPE
  in_port_t = uint16_t;
  sa_family_t = uint16_t;
  in_addr_t = uint32_t;

  struct_in_addr =   RECORD
    s_addr: uint32_t;
  END;

  struct_sockaddr_in = RECORD
    sin_len: uint8_t;
    sin_family: uint8_t;
    sin_port: uint16_t;
    sin_addr: struct_in_addr;
    sin_zero: ARRAY [0..7] OF char;
  END;
  struct_sockaddr_in_star = UNTRACED REF struct_sockaddr_in;

PROCEDURE ntohl(x: uint32_t): uint32_t;
PROCEDURE ntohs(x: uint16_t): uint16_t;
PROCEDURE htonl(x: uint32_t): uint32_t;
PROCEDURE htons(x: uint16_t): uint16_t;

END Uin.
