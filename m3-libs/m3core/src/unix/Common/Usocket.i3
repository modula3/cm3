(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

<*EXTERNAL*> INTERFACE Usocket;

FROM Ctypes IMPORT int, void_star, const_void_star;
FROM Cstddef IMPORT size_t;
FROM Uin IMPORT struct_sockaddr_in;
FROM Utypes IMPORT socklen_t;
IMPORT Usysdep;

(* CONST *)
<*EXTERNAL "Usocket__SOCK_STREAM"*>  VAR SOCK_STREAM: int;
<*EXTERNAL "Usocket__SOCK_DGRAM"*>   VAR SOCK_DGRAM: int;

<*EXTERNAL "Usocket__SO_REUSEADDR"*> VAR SO_REUSEADDR: int;
<*EXTERNAL "Usocket__SO_KEEPALIVE"*> VAR SO_KEEPALIVE: int;
<*EXTERNAL "Usocket__SO_LINGER"*>    VAR SO_LINGER: int;

<*EXTERNAL "Usocket__SOL_SOCKET"*>   VAR SOL_SOCKET: int;

<*EXTERNAL "Usocket__SOL_SOCKET"*>   VAR AF_INET: int;
<*EXTERNAL "Usocket__MSG_PEEK"*>     VAR MSG_PEEK: int;

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
