(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Usocket;

FROM Ctypes IMPORT const_int, int, void_star, const_void_star, const_char_star;
FROM Cstddef IMPORT size_t;
FROM Uin IMPORT struct_sockaddr_in;
IMPORT Utypes, Uin;

(* not all constants are necessarily available on all platforms
 ones marked * portable * are perhaps more portable
 ones not marked * portable * are perhaps less portable, but
 really, the marking means used by m3core/libm3 and/or defined in
 Uconstants.c without an #ifdef, or portable so far.
 Lack of a marker really means not used by m3core/libm3, but present
 here for compatibility with older files or for courtesy in case
 anyone uses them, it fairly cheap to provide "all constants"
*)

(* ai_protocol *)
<*EXTERNAL "Usocket__IPPROTO_TCP"*> VAR IPPROTO_TCP: const_int; (* portable *)
<*EXTERNAL "Usocket__IPPROTO_UDP"*> VAR IPPROTO_UDP: const_int;
<*EXTERNAL "Usocket__IPPROTO_RM"*>  VAR IPPROTO_RM: const_int;
<*EXTERNAL "Usocket__IPPROTO_PGM"*> VAR IPPROTO_PGM: const_int; (* synonym for previous? *)
<*EXTERNAL "Usocket__IPPROTO_IGMP"*>VAR IPPROTO_IGMP: const_int;

(* ai_socktype *)
<*EXTERNAL "Usocket__SOCK_STREAM"*> VAR SOCK_STREAM: const_int; (* portable *)
<*EXTERNAL "Usocket__SOCK_DGRAM"*>  VAR SOCK_DGRAM: const_int;
<*EXTERNAL "Usocket__SOCK_RAW"*>    VAR SOCK_RAW: const_int;
<*EXTERNAL "Usocket__SOCK_RDM"*>    VAR SOCK_RDM: const_int; (* reliable message datagram *)
<*EXTERNAL "Usocket__SOCK_SEQPACKET"*> VAR SOCK_SEQPACKET: const_int; (* reliable message datagram *)

(* ai_family *)
<*EXTERNAL "Usocket__AF_UNSPEC"*>   VAR AF_UNSPEC: const_int; (* unspecified *)
<*EXTERNAL "Usocket__AF_INET"*>     VAR AF_INET: const_int; (* IPv4 *) (* portable *) (* internetwork: UDP, TCP, etc. *)
<*EXTERNAL "Usocket__AF_INET6"*>    VAR AF_INET6: const_int; (* IPv6 *)
<*EXTERNAL "Usocket__AF_NETBIOS"*>  VAR AF_NETBIOS: const_int;
<*EXTERNAL "Usocket__AF_IRDA"*>     VAR AF_IRDA: const_int; (* infrared data association *)
<*EXTERNAL "Usocket__AF_BTH"*>      VAR AF_BTH: const_int; (* Bluetooth *)
<*EXTERNAL "Usocket__AF_UNIX"*>     VAR AF_UNIX: const_int;(* local to host (pipes, portals) *)
<*EXTERNAL "Usocket__AF_LOCAL"*>    VAR AF_LOCAL: const_int;
<*EXTERNAL "Usocket__AF_IPX"*>      VAR AF_IPX: const_int; (* Novell IPX *)
<*EXTERNAL "Usocket__AF_AX25"*>     VAR AF_AX25: const_int; (* Amateur Radio AX.25 *)
<*EXTERNAL "Usocket__AF_NETROM"*>   VAR AF_NETROM: const_int; (* Amateur radio NetROM *)
<*EXTERNAL "Usocket__AF_BRIDGE"*>   VAR AF_BRIDGE: const_int; (* Multiprotocol bridge *)
<*EXTERNAL "Usocket__AF_AAL5"*>     VAR AF_AAL5: const_int; (* Reserved for Werner's ATM *)
<*EXTERNAL "Usocket__AF_X25"*>      VAR AF_X25: const_int; (* X.25 *)
<*EXTERNAL "Usocket__AF_IMPLINK"*>  VAR AF_IMPLINK: const_int; (* arpanet imp addresses *)
<*EXTERNAL "Usocket__AF_PUP"*>      VAR AF_PUP: const_int; (* pup protocols: e.g. BSP *)
<*EXTERNAL "Usocket__AF_CHAOS"*>    VAR AF_CHAOS: const_int; (* mit CHAOS protocols *)
<*EXTERNAL "Usocket__AF_NS"*>       VAR AF_NS: const_int; (* XEROX NS protocols *)
<*EXTERNAL "Usocket__AF_NBS"*>      VAR AF_NBS: const_int; (* nbs protocols *)
<*EXTERNAL "Usocket__AF_ECMA"*>     VAR AF_ECMA: const_int; (* european computer manufacturers *)
<*EXTERNAL "Usocket__AF_DATAKIT"*>  VAR AF_DATAKIT: const_int; (* datakit protocols *)
<*EXTERNAL "Usocket__AF_CCITT"*>    VAR AF_CCITT: const_int; (* CCITT protocols, X.25 etc *)
<*EXTERNAL "Usocket__AF_SNA"*>      VAR AF_SNA: const_int; (* IBM SNA *)
<*EXTERNAL "Usocket__AF_DECnet"*>   VAR AF_DECnet: const_int; (* DECnet *)
<*EXTERNAL "Usocket__AF_DLI"*>      VAR AF_DLI: const_int; (* Direct data link interface *)
<*EXTERNAL "Usocket__AF_LAT"*>      VAR AF_LAT: const_int; (* LAT *)
<*EXTERNAL "Usocket__AF_HYLINK"*>   VAR AF_HYLINK: const_int; (* NSC Hyperchannel *)
<*EXTERNAL "Usocket__AF_APPLETALK"*>VAR AF_APPLETALK: const_int; (* Apple talk *)
<*EXTERNAL "Usocket__AF_BSC"*>      VAR AF_BSC: const_int; (* BISYNC 2780/3780 *)
<*EXTERNAL "Usocket__AF_DSS"*>      VAR AF_DSS: const_int; (* Distributed system services *)


(* ai_flags *)
<*EXTERNAL "Usocket__AI_PASSIVE"*>           VAR AI_PASSIVE: const_int;
<*EXTERNAL "Usocket__AI_CANONNAME"*>         VAR AI_CANONNAME: const_int;
<*EXTERNAL "Usocket__AI_NUMERIC_HOST"*>      VAR AI_NUMERIC_HOST: const_int;
<*EXTERNAL "Usocket__AI_ADDRCONFIG"*>        VAR AI_ADDRCONFIG: const_int;
<*EXTERNAL "Usocket__AI_NON_AUTHORITATIVE"*> VAR AI_NON_AUTHORITATIVE: const_int;
<*EXTERNAL "Usocket__AI_SECURE"*>            VAR AI_SECURE: const_int;
<*EXTERNAL "Usocket__AI_RETURN_PREFERRED_NAMES"*> VAR AI_RETURN_PREFERRED_NAMES: const_int;

<*EXTERNAL "Usocket__SO_REUSEADDR"*> VAR SO_REUSEADDR: const_int; (* portable *)
<*EXTERNAL "Usocket__SO_KEEPALIVE"*> VAR SO_KEEPALIVE: const_int; (* portable *)
<*EXTERNAL "Usocket__SO_LINGER"*>    VAR SO_LINGER: const_int; (* portable *)

<*EXTERNAL "Usocket__SO_TYPE"*>      VAR SO_TYPE: const_int;
<*EXTERNAL "Usocket__SO_ERROR"*>     VAR SO_ERROR: const_int;
<*EXTERNAL "Usocket__SO_DONTROUTE"*> VAR SO_DONTROUTE: const_int;
<*EXTERNAL "Usocket__SO_BROADCAST"*> VAR SO_BROADCAST: const_int;
<*EXTERNAL "Usocket__SO_SNDBUF"*>    VAR SO_SNDBUF: const_int;
<*EXTERNAL "Usocket__SO_RCVBUF"*>    VAR SO_RCVBUF: const_int;
<*EXTERNAL "Usocket__SO_OOBINLINE"*> VAR SO_OOBINLINE: const_int;
<*EXTERNAL "Usocket__SO_NO_CHECK"*>  VAR SO_NO_CHECK: const_int;
<*EXTERNAL "Usocket__SO_PRIORITY"*>  VAR SO_PRIORITY: const_int;
<*EXTERNAL "Usocket__SO_BSDCOMPAT"*> VAR SO_BSDCOMPAT: const_int;
<*EXTERNAL "Usocket__SO_ACCEPTCON"*> VAR SO_ACCEPTCON: const_int;
<*EXTERNAL "Usocket__SO_CONDITIONAL_ACCEPT"*> VAR SO_CONDITIONAL_ACCEPT: const_int;
<*EXTERNAL "Usocket__SO_EXCLUSIVEADDRUSE"*> VAR SO_EXCLUSIVEADDRUSE: const_int;
<*EXTERNAL "Usocket__SO_PORT_SCALABILITY"*> VAR SO_PORT_SCALABILITY: const_int;
<*EXTERNAL "Usocket__SO_PASSCRED"*> VAR SO_PASSCRED: const_int;
<*EXTERNAL "Usocket__SO_PEERCRED"*> VAR SO_PEERCRED: const_int;
<*EXTERNAL "Usocket__SO_RCVLOWAT"*> VAR SO_RCVLOWAT: const_int;
<*EXTERNAL "Usocket__SO_SNDLOWAT"*> VAR SO_SNDLOWAT: const_int;
<*EXTERNAL "Usocket__SO_REUSEPORT"*> VAR SO_REUSEPORT: const_int;
<*EXTERNAL "Usocket__SO_RCVTIMEO"*> VAR SO_RCVTIMEO: const_int;
<*EXTERNAL "Usocket__SO_SNDTIMEO"*> VAR SO_SNDTIMEO: const_int;
<*EXTERNAL "Usocket__SO_SECURITY_AUTHENTICATION"*> VAR SO_SECURITY_AUTHENTICATION: const_int;
<*EXTERNAL "Usocket__SO_SECURITY_ENCRYPTION_TRANSPORT"*> VAR SO_SECURITY_ENCRYPTION_TRANSPORT: const_int;
<*EXTERNAL "Usocket__SO_SECURITY_ENCRYPTION_NETWORK"*> VAR SO_SECURITY_ENCRYPTION_NETWORK: const_int;

<*EXTERNAL "Usocket__SOL_SOCKET"*>   VAR SOL_SOCKET: const_int; (* portable *)

<*EXTERNAL "Usocket__MSG_OOB"*>     VAR MSG_OOB: const_int;    (* process out-of-band data *)
<*EXTERNAL "Usocket__MSG_PEEK"*>    VAR MSG_PEEK: const_int;   (* portable *) (* peek at incoming message *)
<*EXTERNAL "Usocket__MSG_DONTROUTE"*> VAR MSG_DONTROUTE : const_int;(* send without using routing tables *)
<*EXTERNAL "Usocket__MSG_CTRUNC"*>  VAR MSG_CTRUNC: const_int; (* Control data lost before delivery *)
<*EXTERNAL "Usocket__MSG_PROXY"*>   VAR MSG_PROXY: const_int;

<*EXTERNAL "Usocket__SOMAXCONN"*>   VAR SOMAXCONN: const_int; (* Maximum queue length specifiable by listen. *)

TYPE
  socklen_t = Utypes.socklen_t; (* size_t *)
  socklen_t_star = UNTRACED REF socklen_t;

        struct_sockaddr_in_star = UNTRACED REF struct_sockaddr_in;
  const_struct_sockaddr_in_star =              struct_sockaddr_in_star;

  struct_linger = RECORD
(* Structure used for manipulating linger option.
   This is the same on all platforms, except Cygwin (and NT).
   We use a copying wrapper for Cygwin.
 *)
    l_onoff: int;		(* option on/off *)
    l_linger: int;		(* linger time *)
  END;

<*EXTERNAL "Usocket__accept"*>
PROCEDURE accept(s: int; addr: struct_sockaddr_in_star; addrlen: socklen_t_star) : int RAISES {};

<*EXTERNAL "Usocket__bind"*>
PROCEDURE bind(s: int; name: const_struct_sockaddr_in_star; namelen: socklen_t) : int RAISES {};

<*EXTERNAL "Usocket__connect"*>
PROCEDURE connect(s: int; name: const_struct_sockaddr_in_star; namelen: socklen_t) : int RAISES {};

<*EXTERNAL "Usocket__getpeername"*>
PROCEDURE getpeername(s: int; name: struct_sockaddr_in_star; namelen: socklen_t_star) : int RAISES {};

<*EXTERNAL "Usocket__getsockname"*>
PROCEDURE getsockname(s: int; name: struct_sockaddr_in_star; namelen: socklen_t_star) : int RAISES {};

<*EXTERNAL "Usocket__getsockopt"*>
PROCEDURE getsockopt(s, level, optname: int; optval: void_star; optlen: socklen_t_star) : int RAISES {};

<*EXTERNAL "Usocket__listen"*>
PROCEDURE listen(s, backlog: int): int RAISES {};

<*EXTERNAL "Usocket__recvfrom"*>
PROCEDURE recvfrom(s: int; buf: void_star; len: size_t; flags: int; from: struct_sockaddr_in_star; fromlen: socklen_t_star) : INTEGER RAISES {};

<*EXTERNAL "Usocket__sendto"*>
PROCEDURE sendto(s: int; msg: const_void_star; len: size_t; flags: int; to: const_struct_sockaddr_in_star; tolen: socklen_t) : INTEGER RAISES {};

<*EXTERNAL "Usocket__send"*>
PROCEDURE send(s: int; buf: const_void_star; len: size_t; flags: int): INTEGER;

<*EXTERNAL "Usocket__recv"*>
PROCEDURE recv(s: int; buf: void_star; len: size_t; flags: int): INTEGER;

<*EXTERNAL "Usocket__setsockopt"*>
PROCEDURE setsockopt(s, level, optname: int; optval: const_void_star; optlen: socklen_t) : int RAISES {};

<*EXTERNAL "Usocket__shutdown"*>
PROCEDURE shutdown(s, how: int): int RAISES {};

<*EXTERNAL "Usocket__socket"*>
PROCEDURE socket(af, type, protocol: int): int RAISES {};

(* Copy path into a struct sockaddr_un.sun_path
 * set sun_family = AF_UNIX
 * call bind or connect.
 * For failure return -1.
 * If path does not fit in sockaddr, errno = ENAMETOOLONG
 *
 * This hides the layout of sockaddr_un from Modula-3.
 *)
<*EXTERNAL "Usocket__connect_un"*>
PROCEDURE connect_un(fd: int; path: const_char_star): int;

<*EXTERNAL "Usocket__bind_un"*>
PROCEDURE bind_un(fd: int; path: const_char_star): int;

<*EXTERNAL "Usocket__accept_un"*>
PROCEDURE accept_un(fd: int): int;

END Usocket.
