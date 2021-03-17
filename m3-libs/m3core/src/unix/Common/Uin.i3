(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

INTERFACE Uin;

FROM Ctypes IMPORT char, unsigned_short, unsigned, int;

<*EXTERNAL "Usocket__IPPROTO_TCP"*> VAR IPPROTO_TCP: int; (* portable *)

TYPE
  struct_in_addr = RECORD
    s_addr: unsigned;
  END;

 (* On some platforms family is 8 bits and there is another 8 bits next to it
  * but you do not have read or write it.
  *)
  struct_sockaddr_in = RECORD
    sin_family: unsigned_short; (* This is 16 bits on most platforms. *)
    sin_port: unsigned_short;
    sin_addr: struct_in_addr;
    sin_zero: ARRAY [0..7] OF char;
  END;

  struct_in6_addr = RECORD
    u6_addr8 : ARRAY[0..15] OF char;
  END;

 (* On some platforms family is 8 bits and there is another 8 bits next to it
  * but you do not have read or write it.
  *)
  struct_sockaddr_in6 = RECORD
    sin6_family : unsigned_short;   (* AF_INET6 *)
    sin6_port   : unsigned_short;   (* port number *)
    sin6_flowinfo : unsigned;       (* IPv6 flow information *)
    sin6_addr : struct_in6_addr;    (* IPv6 address *)
    sin6_scope_id : unsigned;       (* Scope ID (new in 2.4) *)
  END;

 (* The size of sun_path here is made up.
  * On some platforms family is 8 bits and there is another 8 bits next to it
  * but you do not have read or write it.
  *)
  struct_sockaddr_un = RECORD
    sun_family: unsigned_short; (* this is signed on some platforms; it does not matter *)
    sun_path: ARRAY [0..125] OF char;
  END;

<*EXTERNAL "Uin__ntohl"*> PROCEDURE ntohl(x: unsigned): unsigned;
<*EXTERNAL "Uin__ntohs"*> PROCEDURE ntohs(x: unsigned_short): unsigned_short;
<*EXTERNAL "Uin__htonl"*> PROCEDURE htonl(x: unsigned): unsigned;
<*EXTERNAL "Uin__htons"*> PROCEDURE htons(x: unsigned_short): unsigned_short;

END Uin.
