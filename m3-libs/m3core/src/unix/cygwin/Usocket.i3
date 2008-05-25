(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Usocket;

FROM Ctypes IMPORT int, int_star, char_star;
FROM Utypes IMPORT uint16_t;
FROM Uin IMPORT struct_sockaddr_in;

CONST
  SOCK_STREAM = 1;
  SOCK_DGRAM = 2;
  SO_REUSEADDR = 4;
  SO_KEEPALIVE = 8;
  SO_LINGER = 16_0080;
  MSG_PEEK = 16_2;
  SOL_SOCKET = 1;
  AF_INET = 2;

TYPE
  struct_linger = RECORD
    l_onoff: uint16_t;
    l_linger: uint16_t;
  END;

<*EXTERNAL*> PROCEDURE accept(s: int; addr: UNTRACED REF struct_sockaddr_in; addrlen: int_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE bind(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: int) : int RAISES {};
<*EXTERNAL*> PROCEDURE connect(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: int) : int RAISES {};
<*EXTERNAL*> PROCEDURE getpeername(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: int_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE getsockname(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: int_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE getsockopt(s, level, optname: int; optval: char_star; optlen: int_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE listen(s, backlog: int): int RAISES {};
<*EXTERNAL*> PROCEDURE recvfrom(s: int; buf: char_star; len, flags: int; from: UNTRACED REF struct_sockaddr_in; fromlen: int_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE sendto(s: int; msg: char_star; len, flags: int; to: UNTRACED REF struct_sockaddr_in; tolen: int) : int RAISES {};
<*EXTERNAL*> PROCEDURE setsockopt(s, level, optname: int; optval: char_star; optlen: int) : int RAISES {};
<*EXTERNAL*> PROCEDURE shutdown(s, how: int): int RAISES {};
<*EXTERNAL*> PROCEDURE socket(af, type, protocol: int): int RAISES {};

END Usocket.
