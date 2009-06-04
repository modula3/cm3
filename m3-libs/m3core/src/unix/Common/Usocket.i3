(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Usocket;

FROM Ctypes IMPORT int, void_star, const_void_star, const_char_star, char_star;
FROM Cstddef IMPORT size_t;
FROM Uin IMPORT struct_sockaddr_in;
IMPORT Usysdep;
IMPORT Utypes;

(* CONST *)

(* not all constants are necessarily available on all platforms *)

(* ai_protocol *)
<*EXTERNAL "Usocket__IPPROTO_TCP"*>  VAR IPPROTO_TCP: int;
<*EXTERNAL "Usocket__IPPROTO_UDP"*>  VAR IPPROTO_UDP: int;
<*EXTERNAL "Usocket__IPPROTO_RM"*>   VAR IPPROTO_RM: int;
<*EXTERNAL "Usocket__IPPROTO_PGM"*>  VAR IPPROTO_PGM: int; (* synonym for previous? *)
<*EXTERNAL "Usocket__IPPROTO_IGMP"*> VAR IPPROTO_IGMP: int;

(* ai_socktype *)
<*EXTERNAL "Usocket__SOCK_STREAM"*>  VAR SOCK_STREAM: int;
<*EXTERNAL "Usocket__SOCK_DGRAM"*>   VAR SOCK_DGRAM: int;
<*EXTERNAL "Usocket__SOCK_RAW"*>     VAR SOCK_RAW: int;
<*EXTERNAL "Usocket__SOCK_RDM"*>     VAR SOCK_RDM: int; (* reliable message datagram *)
<*EXTERNAL "Usocket__SOCK_SEQPACKET"*> VAR SOCK_SEQPACKET: int; (* reliable message datagram *)

(* ai_family *)
<*EXTERNAL "Usocket__AF_UNSPEC"*>    VAR AF_UNSPEC: int; (* unspecified *)
<*EXTERNAL "Usocket__AF_INET"*>      VAR AF_INET: int; (* IPv4 *)
<*EXTERNAL "Usocket__AF_INET6"*>     VAR AF_INET6: int; (* IPv6 *)
<*EXTERNAL "Usocket__AF_NETBIOS"*>   VAR AF_NETBIOS: int;
<*EXTERNAL "Usocket__AF_IRDA"*>      VAR AF_IRDA: int; (* infrared data association *)
<*EXTERNAL "Usocket__AF_BTH"*>       VAR AF_BTH: int; (* Bluetooth *)
<*EXTERNAL "Usocket__AF_UNIX"*>      VAR AF_UNIX: int;
<*EXTERNAL "Usocket__AF_LOCAL"*>     VAR AF_LOCAL: int;
<*EXTERNAL "Usocket__AF_IPX"*>       VAR AF_IPX: int;

(* ai_flags *)
<*EXTERNAL "Usocket__AI_PASSIVE"*>           VAR AI_PASSIVE: int;
<*EXTERNAL "Usocket__AI_CANONNAME"*>         VAR AI_CANONNAME: int;
<*EXTERNAL "Usocket__AI_NUMERIC_HOST"*>      VAR AI_NUMERIC_HOST: int;
<*EXTERNAL "Usocket__AI_ADDRCONFIG"*>        VAR AI_ADDRCONFIG: int;
<*EXTERNAL "Usocket__AI_NON_AUTHORITATIVE"*> VAR AI_NON_AUTHORITATIVE: int;
<*EXTERNAL "Usocket__AI_SECURE"*>            VAR AI_SECURE: int;
<*EXTERNAL "Usocket__AI_RETURN_PREFERRED_NAMES"*> VAR AI_RETURN_PREFERRED_NAMES: int;

<*EXTERNAL "Usocket__SO_REUSEADDR"*> VAR SO_REUSEADDR: int;
<*EXTERNAL "Usocket__SO_KEEPALIVE"*> VAR SO_KEEPALIVE: int;
<*EXTERNAL "Usocket__SO_LINGER"*>    VAR SO_LINGER: int;

<*EXTERNAL "Usocket__SO_TYPE"*>      VAR SO_TYPE: int;
<*EXTERNAL "Usocket__SO_ERROR"*>     VAR SO_ERROR: int;
<*EXTERNAL "Usocket__SO_DONTROUTE"*> VAR SO_DONTROUTE: int;
<*EXTERNAL "Usocket__SO_BROADCAST"*> VAR SO_BROADCAST: int;
<*EXTERNAL "Usocket__SO_SNDBUF"*>    VAR SO_SNDBUF: int;
<*EXTERNAL "Usocket__SO_RCVBUF"*>    VAR SO_RCVBUF: int;
<*EXTERNAL "Usocket__SO_OOBINLINE"*> VAR SO_OOBINLINE: int;
<*EXTERNAL "Usocket__SO_NO_CHECK"*>  VAR SO_NO_CHECK: int;
<*EXTERNAL "Usocket__SO_PRIORITY"*>  VAR SO_PRIORITY: int;
<*EXTERNAL "Usocket__SO_BSDCOMPAT"*> VAR SO_BSDCOMPAT: int;
<*EXTERNAL "Usocket__SO_ACCEPTCON"*> VAR SO_ACCEPTCON: int;
<*EXTERNAL "Usocket__SO_CONDITIONAL_ACCEPT"*> VAR SO_CONDITIONAL_ACCEPT: int;
<*EXTERNAL "Usocket__SO_EXCLUSIVEADDRUSE"*> VAR SO_EXCLUSIVEADDRUSE: int;
<*EXTERNAL "Usocket__SO_PORT_SCALABILITY"*> VAR SO_PORT_SCALABILITY: int;

<*EXTERNAL "Usocket__SOL_SOCKET"*>   VAR SOL_SOCKET: int;

<*EXTERNAL "Usocket__MSG_PEEK"*>     VAR MSG_PEEK: int;

TYPE
  struct_linger = Usysdep.struct_linger;
  socklen_t = Utypes.socklen_t;
  socklen_t_star = UNTRACED REF socklen_t;

<*EXTERNAL "Usocket__accept"*>
PROCEDURE accept(s: int; addr: UNTRACED REF struct_sockaddr_in; addrlen: socklen_t_star) : int RAISES {};

<*EXTERNAL "Usocket__bind"*>
PROCEDURE bind(s: int; name: (*const*) UNTRACED REF struct_sockaddr_in; namelen: socklen_t) : int RAISES {};

<*EXTERNAL "Usocket__connect"*>
PROCEDURE connect(s: int; name: (*const*) UNTRACED REF struct_sockaddr_in; namelen: socklen_t) : int RAISES {};

<*EXTERNAL "Usocket__getpeername"*>
PROCEDURE getpeername(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: socklen_t_star) : int RAISES {};

<*EXTERNAL "Usocket__getsockname"*>
PROCEDURE getsockname(s: int; name: UNTRACED REF struct_sockaddr_in; namelen: socklen_t_star) : int RAISES {};

<*EXTERNAL "Usocket__getsockopt"*>
PROCEDURE getsockopt(s, level, optname: int; optval: void_star; optlen: socklen_t_star) : int RAISES {};

<*EXTERNAL "Usocket__listen"*>
PROCEDURE listen(s, backlog: int): int RAISES {};

<*EXTERNAL "Usocket__recvfrom"*>
PROCEDURE recvfrom(s: int; buf: void_star; len: size_t; flags: int; from: UNTRACED REF struct_sockaddr_in; fromlen: socklen_t_star) : int RAISES {};

<*EXTERNAL "Usocket__sendto"*>
PROCEDURE sendto(s: int; msg: const_void_star; len: size_t; flags: int; to: UNTRACED REF struct_sockaddr_in; tolen: socklen_t) : int RAISES {};

<*EXTERNAL "Usocket__setsockopt"*>
PROCEDURE setsockopt(s, level, optname: int; optval: const_void_star; optlen: socklen_t) : int RAISES {};

<*EXTERNAL "Usocket__shutdown"*>
PROCEDURE shutdown(s, how: int): int RAISES {};

<*EXTERNAL "Usocket__socket"*>
PROCEDURE socket(af, type, protocol: int): int RAISES {};

TYPE
  addrinfo_t = RECORD
    ai_flags: int;
    ai_family: int;
    ai_socktype: int;
    ai_protocol: int;
    ai_addrlen: size_t;
    ai_canonname: char_star;
    ai_addr: UNTRACED REF struct_sockaddr_in;
    ai_next: UNTRACED REF addrinfo_t;
  END;

<*EXTERNAL "Usocket__getaddrinfo"*>
PROCEDURE getaddrinfo(nodename: const_char_star; servname: const_char_star; hints: UNTRACED REF addrinfo_t; res: UNTRACED REF addrinfo_t): int;

<*EXTERNAL "Usocket__freeaddrinfo"*>
PROCEDURE freeaddrinfo(addrinfo: UNTRACED REF addrinfo_t);

END Usocket.
