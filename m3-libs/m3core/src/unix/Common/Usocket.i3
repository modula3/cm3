(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

<*EXTERNAL*> INTERFACE Usocket;

FROM Ctypes IMPORT int, void_star, const_void_star;
FROM Cstddef IMPORT size_t;
FROM Uin IMPORT struct_sockaddr_in;
FROM Utypes IMPORT socklen_t;
IMPORT Usysdep;

CONST
  SOCK_STREAM = Usysdep.SOCK_STREAM;
  SOCK_DGRAM = Usysdep.SOCK_DGRAM;

  SO_REUSEADDR = Usysdep.SO_REUSEADDR;
  SO_KEEPALIVE = Usysdep.SO_KEEPALIVE;
  SO_LINGER = Usysdep.SO_LINGER;

  SOL_SOCKET = Usysdep.SOL_SOCKET;

  AF_INET = Usysdep.AF_INET;
  MSG_PEEK = Usysdep.MSG_PEEK;

TYPE
  struct_linger = Usysdep.struct_linger;

  socklen_t_star = UNTRACED REF socklen_t;

PROCEDURE accept(s: int; addr: UNTRACED REF struct_sockaddr_in; addrlen: socklen_t_star) : int RAISES {};
PROCEDURE bind(s: int; name: (*const*) UNTRACED REF struct_sockaddr_in; namelen: socklen_t) : int RAISES {};
PROCEDURE connect(s: int; name: (*const*) UNTRACED REF struct_sockaddr_in; namelen: socklen_t) : int RAISES {};
PROCEDURE getpeername(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: socklen_t_star) : int RAISES {};
PROCEDURE getsockname(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: socklen_t_star) : int RAISES {};
PROCEDURE getsockopt(s, level, optname: int; optval: void_star; optlen: socklen_t_star) : int RAISES {};
PROCEDURE listen(s, backlog: int): int RAISES {};
PROCEDURE recvfrom(s: int; buf: void_star; len: size_t; flags: int; from: UNTRACED REF struct_sockaddr_in; fromlen: socklen_t_star) : int RAISES {};
PROCEDURE sendto(s: int; msg: const_void_star; len: size_t; flags: int; to: UNTRACED REF struct_sockaddr_in; tolen: socklen_t) : int RAISES {};
PROCEDURE setsockopt(s, level, optname: int; optval: const_void_star; optlen: socklen_t) : int RAISES {};
PROCEDURE shutdown(s, how: int): int RAISES {};
PROCEDURE socket(af, type, protocol: int): int RAISES {};

END Usocket.
