(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Usocket;

FROM Ctypes IMPORT int, void_star, const_void_star;
FROM Cstdint IMPORT uint32_t;
FROM Cstddef IMPORT size_t;
FROM Uin IMPORT struct_sockaddr_in;
IMPORT Usysdep;
IMPORT Utypes;

(* CONST *)
<*EXTERNAL "Usocket__SOCK_STREAM"*>  VAR SOCK_STREAM: int;
<*EXTERNAL "Usocket__SOCK_DGRAM"*>   VAR SOCK_DGRAM: int;

<*EXTERNAL "Usocket__SO_REUSEADDR"*> VAR SO_REUSEADDR: int;
<*EXTERNAL "Usocket__SO_KEEPALIVE"*> VAR SO_KEEPALIVE: int;
<*EXTERNAL "Usocket__SO_LINGER"*>    VAR SO_LINGER: int;

<*EXTERNAL "Usocket__SOL_SOCKET"*>   VAR SOL_SOCKET: int;

<*EXTERNAL "Usocket__AF_INET"*>      VAR AF_INET: int;
<*EXTERNAL "Usocket__MSG_PEEK"*>     VAR MSG_PEEK: int;

TYPE
  struct_linger = Usysdep.struct_linger;
  socklen_t = Utypes.socklen_t;
  socklen_t_star = UNTRACED REF socklen_t;

<*EXTERNAL Usocket__accept*>
PROCEDURE accept(s: int; addr: UNTRACED REF struct_sockaddr_in; addrlen: socklen_t_star) : int RAISES {};

<*EXTERNAL Usocket__bind*>
PROCEDURE bind(s: int; name: (*const*) UNTRACED REF struct_sockaddr_in; namelen: socklen_t) : int RAISES {};

<*EXTERNAL Usocket__connect*>
PROCEDURE connect(s: int; name: (*const*) UNTRACED REF struct_sockaddr_in; namelen: socklen_t) : int RAISES {};

<*EXTERNAL Usocket__getpeername*>
PROCEDURE getpeername(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: socklen_t_star) : int RAISES {};

<*EXTERNAL Usocket__getsockname*>
PROCEDURE getsockname(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: socklen_t_star) : int RAISES {};

<*EXTERNAL Usocket__getsockopt*>
PROCEDURE getsockopt(s, level, optname: int; optval: void_star; optlen: socklen_t_star) : int RAISES {};

<*EXTERNAL Usocket__listen*>
PROCEDURE listen(s, backlog: int): int RAISES {};

<*EXTERNAL Usocket__recvfrom*>
PROCEDURE recvfrom(s: int; buf: void_star; len: size_t; flags: int; from: UNTRACED REF struct_sockaddr_in; fromlen: socklen_t_star) : int RAISES {};

<*EXTERNAL Usocket__sendto*>
PROCEDURE sendto(s: int; msg: const_void_star; len: size_t; flags: int; to: UNTRACED REF struct_sockaddr_in; tolen: socklen_t) : int RAISES {};

<*EXTERNAL Usocket__setsockopt*>
PROCEDURE setsockopt(s, level, optname: int; optval: const_void_star; optlen: socklen_t) : int RAISES {};

<*EXTERNAL Usocket__shutdown*>
PROCEDURE shutdown(s, how: int): int RAISES {};

<*EXTERNAL Usocket__socket*>
PROCEDURE socket(af, type, protocol: int): int RAISES {};

END Usocket.
