(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jan 21 08:19:43 PST 1993 by kalsow     *)
(*      modified on Wed Mar 25 16:45:48 PST 1992 by muller     *)
(*      modified on Wed Jul 11  9:34:54 PDT 1990 by mjordan    *)

UNSAFE INTERFACE Usocket;


IMPORT Ctypes, Utypes, Uuio, Utime;

(*** sys/socket.h ***)

(*
 * Definitions related to sockets: types, address families, options.
 *)

(*
 * Types
 *)
CONST
  SOCK_STREAM    = 1;            (* stream socket *)
  SOCK_DGRAM     = 2;            (* datagram socket *)
  SOCK_RAW       = 3;            (* raw-protocol interface *)
  SOCK_RD        = 4;            (* reliably-delivered message *)
  SOCK_SEQPACKET = 5;            (* sequenced packet stream *)

(*
 * Option flags per-socket.
 *)
  SO_DEBUG       = 16_01;        (* turn on debugging info recording *)
  SO_ACCEPTCONN  = 16_02;        (* socket has had listen() *)
  SO_REUSEADDR   = 16_04;        (* allow local address reuse *)
  SO_KEEPALIVE   = 16_08;        (* keep connections alive *)
  SO_DONTROUTE   = 16_10;        (* just use interface addresses *)
  SO_BROADCAST   = 16_20;        (* permit sending of broadcast msgs *)
  SO_USELOOPBACK = 16_40;        (* bypass hardware when possible *)
  SO_LINGER      = 16_80;        (* linger on close if data present *)
  SO_OOBINLINE   = 16_100;       (* leave received OOB data in line *)

(*
 * Additional options, not kept in so_options.
 *)
  SO_SNDBUF      = 16_1001;       (* send buffer size *)
  SO_RCVBUF      = 16_1002;       (* receive buffer size *)
  SO_SNDLOWAT    = 16_1003;       (* send low-water mark *)
  SO_RCVLOWAT    = 16_1004;       (* receive low-water mark *)
  SO_SNDTIMEO    = 16_1005;       (* send timeout *)
  SO_RCVTIMEO    = 16_1006;       (* receive timeout *)
  SO_ERROR       = 16_1007;       (* get error status and clear *)
  SO_TYPE        = 16_1008;         (* get socket type *)
  
(*
 * Structure used for manipulating linger option.
 *)
TYPE
  struct_linger = RECORD
    l_onoff: Ctypes.int;		(* option on/off *)
    l_linger: Ctypes.int;		(* linger time *)
  END;


(*
 * Level number for (get/set)sockopt() to apply to socket itself.
 *)
CONST
  SOL_SOCKET     = 16_ffff;      (* options for socket level *)


(*
 * Address families.
 *)
  AF_UNSPEC      = 0;            (* unspecified *)
  AF_UNIX        = 1;            (* local to host (pipes, portals) *)
  AF_INET        = 2;            (* internetwork: UDP, TCP, etc. *)
  AF_IMPLINK     = 3;            (* arpanet imp addresses *)
  AF_PUP         = 4;            (* pup protocols: e.g. BSP *)
  AF_CHAOS       = 5;            (* mit CHAOS protocols *)
  AF_NS          = 6;            (* XEROX NS protocols *)
  AF_NBS         = 7;            (* nbs protocols *)
  AF_ECMA        = 8;            (* european computer manufacturers *)
  AF_DATAKIT     = 9;            (* datakit protocols *)
  AF_CCITT       = 10;           (* CCITT protocols, X.25 etc *)
  AF_SNA         = 11;           (* IBM SNA *)
  AF_DECnet	 = 12;           (* DECnet *)
  AF_DLI	 = 13;           (* Direct data link interface *)
  AF_LAT         = 14;           (* LAT *)
  AF_HYLINK      = 15;           (* NSC Hyperchannel *)
  AF_APPLETALK   = 16;           (* Apple talk *)
  AF_BSC         = 17;           (* BISYNC 2780/3780 *)
  AF_DSS         = 18;           (* Distributed system services *)

  AF_MAX         = 19;

(*
 * Structure used by kernel to store most
 * addresses.
 *)
TYPE
  struct_sockaddr = RECORD
    sa_family: Ctypes.unsigned_short;        (* address family *)
    sa_data: ARRAY [0..13] OF Ctypes.char;
                                 (* up to 14 bytes of direct address *)
  END;


(*
 * Structure used by kernel to pass protocol
 * information in raw sockets.
 *)
  struct_sockproto = RECORD
    sp_family: Ctypes.unsigned_short;        (* address family *)
    sp_protocol: Ctypes.unsigned_short;      (* protocol *)
  END;

(*
 * Protocol families, same as address families for now.
 *)
CONST
  PF_UNSPEC      = AF_UNSPEC;
  PF_UNIX        = AF_UNIX;
  PF_INET        = AF_INET;
  PF_IMPLINK     = AF_IMPLINK;
  PF_PUP         = AF_PUP;
  PF_CHAOS       = AF_CHAOS;
  PF_NS          = AF_NS;
  PF_NBS         = AF_NBS;
  PF_ECMA        = AF_ECMA;
  PF_DATAKIT     = AF_DATAKIT;
  PF_CCITT       = AF_CCITT;
  PF_SNA         = AF_SNA;
  PF_DECnet      = AF_DECnet;
  PF_DLI         = AF_DLI;
  PF_LAT         = AF_LAT;
  PF_HYLINK      = AF_HYLINK;
  PF_APPLETALK   = AF_APPLETALK;
  PF_BSC         = AF_BSC;
  PF_DSS         = AF_DSS;

  PF_MAX	 = AF_MAX;

(*
 * Maximum queue length specifiable by listen.
 *)
  SOMAXCONN      = 5;

(*
 * Message header for recvmsg and sendmsg calls.
 *)
TYPE
  struct_msghdr = RECORD
    msg_name: Utypes.caddr_t;         (* optional address *)
    msg_namelen: Ctypes.int;          (* size of address *)
    msg_iov: Uuio.struct_iovec_star;  (* scatter/gather array *)
    msg_iovlen: Ctypes.int;           (* # elements in msg_iov *)
    msg_accrights: Utypes.caddr_t;    (* access rights sent/received *)
    msg_accrightslen: Ctypes.int;
  END;


CONST
  MSG_OOB        = 16_1;         (* process out-of-band data *)
  MSG_PEEK       = 16_2;         (* peek at incoming message *)
  MSG_DONTROUTE  = 16_4;         (* send without using routing tables *)

  MSG_MAXIOVLEN  = 16;

(*
 * Definitions for UNIX IPC domain.
 *)
TYPE
  struct_sockaddr_un = RECORD
    sun_family: Ctypes.short;         (* AF_UNIX *)
    sun_path: ARRAY [0..107-4] OF Ctypes.char;
                                 (* path name (gag) *)
  END;

<*EXTERNAL*>
PROCEDURE accept(
    s: Ctypes.int;
    addr: UNTRACED REF struct_sockaddr;
    addrlen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE bind(
    s: Ctypes.int;
    name: UNTRACED REF struct_sockaddr;
    namelen: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE connect(
    s: Ctypes.int;
    name: UNTRACED REF struct_sockaddr;
    namelen: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE getpeername(
    s: Ctypes.int;
    name: UNTRACED REF struct_sockaddr;
    namelen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE getsockname(
    s: Ctypes.int;
    name: UNTRACED REF struct_sockaddr;
    namelen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE getsockopt(
    s, level, optname: Ctypes.int;
    optval: Ctypes.char_star;
    optlen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE listen(s, backlog: Ctypes.int): Ctypes.int RAISES {};

<*EXTERNAL*>
PROCEDURE recv(s: Ctypes.int; buf: Ctypes.char_star; len, flags: Ctypes.int): Ctypes.int RAISES {};

<*EXTERNAL*>
PROCEDURE recvfrom(
    s: Ctypes.int;
    buf: Ctypes.char_star;
    len, flags: Ctypes.int;
    from: UNTRACED REF struct_sockaddr;
    fromlen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE recvmsg(
    s: Ctypes.int;
    msg: UNTRACED REF struct_msghdr;
    flags: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE select(
    width: Ctypes.int;
    readfds, writefds, exceptfgs: UNTRACED REF Ctypes.int;
    timeout: Utime.struct_timeval)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE send(s: Ctypes.int; msg: Ctypes.char_star; len, flags: Ctypes.int): Ctypes.int RAISES {};

<*EXTERNAL*>
PROCEDURE sendto(
    s: Ctypes.int;
    msg: Ctypes.char_star;
    len, flags: Ctypes.int;
    to: UNTRACED REF struct_sockaddr;
    tolen: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE sendmsg(
    s: Ctypes.int;
    msg: UNTRACED REF struct_msghdr;
    flags: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE setsockopt(
    s, level, optname: Ctypes.int;
    optval: Ctypes.char_star;
    optlen: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE shutdown(s, how: Ctypes.int): Ctypes.int RAISES {};

<*EXTERNAL*>
PROCEDURE socket(af, type, protocol: Ctypes.int): Ctypes.int RAISES {};

<*EXTERNAL*>
PROCEDURE socketpair(
    d, type, protocol: Ctypes.int;
    sv: UNTRACED REF ARRAY [0..1] OF Ctypes.int)
    : Ctypes.int
    RAISES {};

END Usocket.
