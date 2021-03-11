(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

INTERFACE Uin;

FROM Ctypes IMPORT char, unsigned_short, unsigned;

CONST
  IPPROTO_TCP = 6;

TYPE
  struct_in_addr = RECORD
    s_addr: unsigned;
  END;

  struct_sockaddr_in = RECORD
    sin_family: unsigned_short; (* this is signed on some platforms; it does not matter *)
    sin_port: unsigned_short;
    sin_addr: struct_in_addr;
    sin_zero: ARRAY [0..7] OF char;
  END;

  struct_in6_addr = RECORD
    u6_addr8 : ARRAY[0..15] OF char;
  END;

  struct_sockaddr_in6 = RECORD
    sin6_family : unsigned_short;   (* AF_INET6 *)
    sin6_port   : unsigned_short;   (* port number *)
    sin6_flowinfo : unsigned;       (* IPv6 flow information *)
    sin6_addr : struct_in6_addr;    (* IPv6 address *)
    sin6_scope_id : unsigned;       (* Scope ID (new in 2.4) *)
  END;

  (* generic struct just to get the family before casting *)
  struct_sockaddr = RECORD
    sin_family : unsigned_short;   (* AF_INET or AF_INET6 *)
    data       : ARRAY [0..13] OF CHAR;
  END;

  struct_sockaddr_un = RECORD
    sin_family: unsigned_short; (* this is signed on some platforms; it does not matter *)
    sun_path: ARRAY [0..103] OF char;
  END;

<*EXTERNAL "Uin__ntohl"*> PROCEDURE ntohl(x: unsigned): unsigned;
<*EXTERNAL "Uin__ntohs"*> PROCEDURE ntohs(x: unsigned_short): unsigned_short;
<*EXTERNAL "Uin__htonl"*> PROCEDURE htonl(x: unsigned): unsigned;
<*EXTERNAL "Uin__htons"*> PROCEDURE htons(x: unsigned_short): unsigned_short;

END Uin.
