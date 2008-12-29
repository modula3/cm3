(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Usocket;

FROM Ctypes IMPORT int, void_star, const_void_star;
FROM Uin IMPORT struct_sockaddr_in;
FROM Utypes IMPORT socklen_t, size_t;

CONST
  SOCK_STREAM = 1;
  SOCK_DGRAM = 2;

  SO_REUSEADDR = 16_0004;
  SO_KEEPALIVE = 16_0008;
  SO_LINGER = 16_0080;

  SOL_SOCKET = 16_FFFF;
  AF_INET = 2;
  MSG_PEEK = 2;

TYPE
  struct_linger = RECORD
    l_onoff: int;
    l_linger: int;
  END;

  socklen_t_star = UNTRACED REF socklen_t;

<*EXTERNAL*> PROCEDURE accept(s: int; addr: UNTRACED REF struct_sockaddr_in; addrlen: socklen_t_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE bind(s: int; name: (*const*) UNTRACED REF struct_sockaddr_in; namelen: socklen_t) : int RAISES {};
<*EXTERNAL*> PROCEDURE connect(s: int; name: (*const*) UNTRACED REF struct_sockaddr_in; namelen: socklen_t) : int RAISES {};
<*EXTERNAL*> PROCEDURE getpeername(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: socklen_t_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE getsockname( s: int; name: UNTRACED REF struct_sockaddr_in; namelen: socklen_t_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE getsockopt(s, level, optname: int; optval: void_star; optlen: socklen_t_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE listen(s, backlog: int): int RAISES {};
<*EXTERNAL*> PROCEDURE recvfrom(s: int; buf: void_star; len: size_t; flags: int; from: UNTRACED REF struct_sockaddr_in; fromlen: socklen_t_star) : int RAISES {};
<*EXTERNAL*> PROCEDURE sendto(s: int; msg: const_void_star; len: size_t; flags: int; to: UNTRACED REF struct_sockaddr_in; tolen: socklen_t) : int RAISES {};
<*EXTERNAL*> PROCEDURE setsockopt(s, level, optname: int; optval: const_void_star; optlen: socklen_t) : int RAISES {};
<*EXTERNAL*> PROCEDURE shutdown(s, how: int): int RAISES {};
<*EXTERNAL*> PROCEDURE socket(af, type, protocol: int): int RAISES {};

END Usocket.
