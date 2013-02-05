(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Ted Wobber                                             *)
(*                                                           *)
(* Last modified on Thu Mar 23 13:33:28 PST 1995 by kalsow   *)
(*      modified on Mon Feb  6 16:35:23 PST 1995 by wobber   *)

INTERFACE WinSock;

       (* Taken from WINSOCK.H from WINSOCK version 1.1 *)

FROM Ctypes IMPORT char_star, char_star_star, char, int;
FROM WinBaseTypes IMPORT UINT8, UINT16, UINT32, INT16, INT32, PINT32, SIZE_T;
FROM Word IMPORT Or, And, Shift, Not;


(* Basic system type definitions, taken from the BSD file sys/types.h. *)

TYPE
  u_char  = UINT8; (* compat *)
  u_short = UINT16; (* compat *)
  u_int   = UINT32; (* compat *)
  u_long  = UINT32; (* compat *)
  socklen_t = int; (* to be like Posix Usocket.i3 *)

CONST
  FD_SETSIZE = 64;

TYPE
  SOCKET = SIZE_T;
  struct_fd_set = RECORD
    fd_count: UINT32;
    fd_array: ARRAY [0..FD_SETSIZE-1] OF SOCKET;
  END;

(* procedures to mimic C macros *)
PROCEDURE FD_CLR(s: SOCKET; VAR set: struct_fd_set);
PROCEDURE FD_SET(s: SOCKET; VAR set: struct_fd_set);
PROCEDURE FD_ZERO(VAR set: struct_fd_set);
PROCEDURE FD_ISSET(s: SOCKET; VAR set: struct_fd_set): BOOLEAN;

(*
 * Structure used in select() call, taken from the BSD file sys/time.h.
 *)

TYPE
  struct_timeval = RECORD
    tv_sec: INT32;
    tv_usec: INT32;
  END;


(*
 * Commands for ioctlsocket(),  taken from the BSD file fcntl.h.
 *
 *
 * Ioctl's have the command encoded in the lower word,
 * and the size of any in or out parameters in the upper
 * word.  The high 2 bits of the upper word are used
 * to encode the in/out status of the parameter; for now
 * we restrict parameters to at most 128 bytes.
 *)

CONST
  IOCPARM_MASK = 16_7f;               (* parameters must be < 128 bytes *)
  IOC_VOID     = 16_20000000;         (* no parameters *)
  IOC_OUT      = 16_40000000;         (* copy out parameters *)
  IOC_IN       = 16_80000000;         (* copy in parameters *)
  IOC_INOUT    = IOC_IN + IOC_OUT;

       (* 16_20000000 distinguishes new & old ioctl's *)

  FC  = Shift (ORD ('f'), 8);
  SC  = Shift (ORD ('s'), 8);
  INT = Shift (And (BYTESIZE (INTEGER), IOCPARM_MASK), 16);
  R   = IOC_OUT;
  W   = IOC_IN;

  (* file i/o controls *)
  FIONREAD =  Or (Or (R, INT), Or (FC, 127)); (* Get # bytes to read *)
  FIONBIO =   Or (Or (W, INT), Or (FC, 126)); (* Set/clear non-bl.i/o *)
  FIOASYNC =  Or (Or (W, INT), Or (FC, 125)); (* Set/clear async i/o    *)

  (* Socket i/o controls *)
  SIOCSHIWAT =     Or (Or (W, INT),  Or (SC,  0));  (* Set high watermark *)
  SIOCGHIWAT =     Or (Or (R, INT),  Or (SC,  1));  (* Get high watermark *)
  SIOCSLOWAT =     Or (Or (W, INT),  Or (SC,  2));  (* Set low watermark  *)
  SIOCGLOWAT =     Or (Or (R, INT),  Or (SC,  3));  (* Get low watermark  *)
  SIOCATMARK =     Or (Or (R, INT),  Or (SC,  7));  (* At oob mark?       *)

(*
 * Structures returned by network data base library, taken from the
 * BSD file netdb.h.  All addresses are supplied in host order, and
 * returned in network order (suitable for use in system calls).
 *)

TYPE
  struct_hostent  = RECORD
    h_name:       char_star;        (* official name of host *)
    h_aliases:    char_star_star;   (* alias list *)
    h_addrtype:   INT16;            (* host address type *)
    h_length:     INT16;            (* length of address *)
    h_addr_list:  char_star_star;   (* list of addresses from name server *)
    END;
  struct_hostent_star = UNTRACED REF struct_hostent;

(*
 * Assumption here is that a network number
 * fits in 32 bits -- probably a poor one.
 *)

  struct_netent = RECORD
    n_name:     char_star;      (* official name of net *)
    n_aliases:  char_star_star; (* alias list *)
    n_addrtype: INT16;          (* net address type *)
    padding:    UINT16;
    n_net:      UINT32;          (* network # *)
  END;
  struct_netent_star = UNTRACED REF struct_netent;

  struct_servent = RECORD
    s_name:    char_star;       (* official service name *)
    s_aliases: char_star_star;  (* alias list *)
    s_port:    INT16;           (* port # *)
    s_proto:   char_star;       (* protocol to use *)
  END;
  struct_servent_star = UNTRACED REF struct_servent;

  struct_protoent = RECORD
    p_name:    char_star;       (* official protocol name *)
    p_aliases: char_star_star;  (* alias list *)
    p_proto:   INT16;           (* protocol # *)
  END;
  struct_protoent_star = UNTRACED REF struct_protoent;

(*
 * Constants and structures defined by the internet system,
 * Per RFC 790, September 1981, taken from the BSD file netinet/in.h.
 *)

(*
 * Protocols
 *)

CONST
  IPPROTO_IP         = 0;              (* dummy for IP *)
  IPPROTO_ICMP       = 1;              (* control message protocol *)
  IPPROTO_GGP        = 2;              (* gateway^2 (deprecated) *)
  IPPROTO_TCP        = 6;              (* tcp *)
  IPPROTO_PUP        = 12;             (* pup *)
  IPPROTO_UDP        = 17;             (* user datagram protocol *)
  IPPROTO_IDP        = 22;             (* xns idp *)
  IPPROTO_ND         = 77;             (* UNOFFICIAL net disk proto *)

  IPPROTO_RAW        = 255;            (* raw IP packet *)
  IPPROTO_MAX        = 256;

(*
 * Port/socket numbers: network standard functions
 *)
  IPPORT_ECHO        = 7;
  IPPORT_DISCARD     = 9;
  IPPORT_SYSTAT      = 11;
  IPPORT_DAYTIME     = 13;
  IPPORT_NETSTAT     = 15;
  IPPORT_FTP         = 21;
  IPPORT_TELNET      = 23;
  IPPORT_SMTP        = 25;
  IPPORT_TIMESERVER  = 37;
  IPPORT_NAMESERVER  = 42;
  IPPORT_WHOIS       = 43;
  IPPORT_MTP         = 57;

(*
 * Port/socket numbers: host specific functions
 *)
  IPPORT_TFTP        = 69;
  IPPORT_RJE         = 77;
  IPPORT_FINGER      = 79;
  IPPORT_TTYLINK     = 87;
  IPPORT_SUPDUP      = 95;

(*
 * UNIX TCP sockets
 *)
  IPPORT_EXECSERVER  = 512;
  IPPORT_LOGINSERVER = 513;
  IPPORT_CMDSERVER   = 514;
  IPPORT_EFSSERVER   = 520;

(*
 * UNIX UDP sockets
 *)
  IPPORT_BIFFUDP     = 512;
  IPPORT_WHOSERVER   = 513;
  IPPORT_ROUTESERVER = 520;
                                        (* 520+1 also used *)

(*
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 *)
  IPPORT_RESERVED    = 1024;

(*
 * Link numbers
 *)
  IMPLINK_IP         = 155;
  IMPLINK_LOWEXPER   = 156;
  IMPLINK_HIGHEXPER  = 158;

(*
 * Internet address (old style... should be updated)
 *)
TYPE
  struct_in_addr = RECORD
    s_addr: UINT32;
  END;
  struct_in_addr_b = RECORD
    b1, b2, b3, b4: UINT8;   (* alias for in_addr *)
  END;
  struct_int_addr_w = RECORD
    w1, w2: UINT16;          (* alias for in_addr *)
  END;

(*
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
 *)
CONST
  IN_CLASSA_NET         =  16_ff000000;
  IN_CLASSA_NSHIFT      =  24;
  IN_CLASSA_HOST        =  16_00ffffff;
  IN_CLASSA_MAX         =  128;

  IN_CLASSB_NET         =  16_ffff0000;
  IN_CLASSB_NSHIFT      =  16;
  IN_CLASSB_HOST        =  16_0000ffff;
  IN_CLASSB_MAX         =  65536;

  IN_CLASSC_NET         =  16_ffffff00;
  IN_CLASSC_NSHIFT      =  8;
  IN_CLASSC_HOST        =  16_000000ff;

  INADDR_ANY            =  0;
  INADDR_LOOPBACK       =  16_7f000001;
  INADDR_BROADCAST      =  16_ffffffff;   
  INADDR_NONE           =  16_ffffffff;

(* procedures to mimic C macros *)
PROCEDURE IN_CLASSA(in: struct_in_addr): BOOLEAN;
PROCEDURE IN_CLASSB(in: struct_in_addr): BOOLEAN;
PROCEDURE IN_CLASSC(in: struct_in_addr): BOOLEAN;

(*
 * interpretations of various address fields:

     struct_in_addr_b.b2  --  host on imp
     struct_in_addr_b.b1  --  network
     struct_in_addr_w.w2  --  imp
     struct_in_addr_b.b4  --  imp #
     struct_in_addr_b.b3  --  logical host
 *)

(*
 * Socket address, internet style.
 *)
TYPE
  struct_sockaddr_in = RECORD
    sin_family: INT16;
    sin_port:   UINT16;
    sin_addr:   struct_in_addr;
    sin_zero:   ARRAY [0..7] OF char;
  END;

(*
 * Options for use with [gs]etsockopt at the IP level.
 *)
CONST IP_OPTIONS = 1;            (* set/get IP per-packet options *)

(*
 * Definitions related to sockets: types, address families, options,
 * taken from the BSD file sys/socket.h.
 *)

(*
 * This is used instead of -1, since the
 * SOCKET type is unsigned.
 *)
  INVALID_SOCKET = Not(0);
  SOCKET_ERROR   = -1;

(*
 * Types
 *)
  SOCK_STREAM    = 1;            (* stream socket *)
  SOCK_DGRAM     = 2;            (* datagram socket *)
  SOCK_RAW       = 3;            (* raw-protocol interface *)
  SOCK_RDM       = 4;            (* reliably-delivered message *)
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
 * Additional options.
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
 * Options for connect and disconnect data and options.  Used only by
 * non-TCP/IP transports such as DECNet, OSI TP4, etc.
 *)
  SO_CONNDATA    = 16_7000;
  SO_CONNOPT     = 16_7001;
  SO_DISCDATA    = 16_7002;
  SO_DISCOPT     = 16_7003;
  SO_CONNDATALEN = 16_7004;
  SO_CONNOPTLEN  = 16_7005;
  SO_DISCDATALEN = 16_7006;
  SO_DISCOPTLEN  = 16_7007;

(*
 * TCP options.
 *)
  TCP_NODELAY    = 16_1;

(*
 * Address families.
 *)
  AF_UNSPEC      = 0;            (* unspecified *)
  AF_UNIX        = 1;            (* local to host (pipes, portals) *)
  AF_INET        = 2;            (* internetwork: UDP, TCP, etc. *)
  AF_IMPLINK     = 3;            (* arpanet imp addresses *)
  AF_PUP         = 4;            (* pup protocols: e.g. BSP *)
  AF_CHAOS       = 5;            (* mit CHAOS protocols *)
  AF_IPX         = 6;            (* IPX and SPX *)
  AF_NS          = 6;            (* XEROX NS protocols *)
  AF_ISO         = 7;            (* ISO protocols *)
  AF_OSI         = 7;            (* OSI is ISO *)
  AF_ECMA        = 8;            (* european computer manufacturers *)
  AF_DATAKIT     = 9;            (* datakit protocols *)
  AF_CCITT       = 10;           (* CCITT protocols, X.25 etc *)
  AF_SNA         = 11;           (* IBM SNA *)
  AF_DECnet	 = 12;           (* DECnet *)
  AF_DLI	 = 13;           (* Direct data link interface *)
  AF_LAT         = 14;           (* LAT *)
  AF_HYLINK      = 15;           (* NSC Hyperchannel *)
  AF_APPLETALK   = 16;           (* Apple talk *)
  AF_NETBIOS     = 17;           (* NetBios-style addresses *)

  AF_MAX         = 18;

(*
 * Structure used by kernel to store most
 * addresses.
 *)
TYPE
  struct_sockaddr_star = UNTRACED REF struct_sockaddr;
  struct_sockaddr = RECORD
    sa_family: UINT16;                 (* address family *)
    sa_data: ARRAY [0..13] OF char;     (* up to 14 bytes of direct address *)
  END;


(*
 * Structure used by kernel to pass protocol
 * information in raw sockets.
 *)
  struct_sockproto = RECORD
    sp_family: UINT16;                 (* address family *)
    sp_protocol: UINT16;               (* protocol *)
  END;

(*
 * Structure used for manipulating linger option.
 *)
  struct_linger = RECORD
    l_onoff: UINT16;		        (* option on/off *)
    l_linger: UINT16;		        (* linger time *)
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
  PF_ISO         = AF_ISO;
  PF_OSI         = AF_ISO;
  PF_ECMA        = AF_ECMA;
  PF_DATAKIT     = AF_DATAKIT;
  PF_CCITT       = AF_CCITT;
  PF_SNA         = AF_SNA;
  PF_DECnet      = AF_DECnet;
  PF_DLI         = AF_DLI;
  PF_LAT         = AF_LAT;
  PF_HYLINK      = AF_HYLINK;
  PF_APPLETALK   = AF_APPLETALK;
  PF_NETBIOS     = AF_NETBIOS;

  PF_MAX	 = AF_MAX;

(*
 * Level number for (get/set)sockopt() to apply to socket itself.
 *)
  SOL_SOCKET     = 16_ffff;      (* options for socket level *)

(*
 * Maximum queue length specifiable by listen.
 *)
  SOMAXCONN      = 5;

  MSG_OOB        = 16_1;         (* process out-of-band data *)
  MSG_PEEK       = 16_2;         (* peek at incoming message *)
  MSG_DONTROUTE  = 16_4;         (* send without using routing tables *)

  MSG_MAXIOVLEN  = 16;

  MSG_PARTIAL    = 16_8000;      (* partial send or recv for message xport *)

(*
 * Define constant based on rfc883, used by gethostbyxxxx() calls.
 *)
CONST MAXGETHOSTSTRUCT = 1024;

(*
 * Define flags to be used with the WSAAsyncSelect() call.
 *)
  FD_READ         = 16_1;
  FD_WRITE        = 16_2;
  FD_OOB          = 16_4;
  FD_ACCEPT       = 16_8;
  FD_CONNECT      = 16_10;
  FD_CLOSE        = 16_20;

(*
 * All Windows Sockets error constants are biased by WSABASEERR from
 * the "normal"
 *)
CONST  WSABASEERR  = 10000;

(*
 * Windows Sockets definitions of regular Microsoft C error constants
 *)
  WSAEINTR              =  (WSABASEERR+4);
  WSAEBADF              =  (WSABASEERR+9);
  WSAEACCES             =  (WSABASEERR+13);
  WSAEFAULT             =  (WSABASEERR+14);
  WSAEINVAL             =  (WSABASEERR+22);
  WSAEMFILE             =  (WSABASEERR+24);

(*
 * Windows Sockets definitions of regular Berkeley error constants
 *)
  WSAEWOULDBLOCK        =  (WSABASEERR+35);
  WSAEINPROGRESS        =  (WSABASEERR+36);
  WSAEALREADY           =  (WSABASEERR+37);
  WSAENOTSOCK           =  (WSABASEERR+38);
  WSAEDESTADDRREQ       =  (WSABASEERR+39);
  WSAEMSGSIZE           =  (WSABASEERR+40);
  WSAEPROTOTYPE         =  (WSABASEERR+41);
  WSAENOPROTOOPT        =  (WSABASEERR+42);
  WSAEPROTONOSUPPORT    =  (WSABASEERR+43);
  WSAESOCKTNOSUPPORT    =  (WSABASEERR+44);
  WSAEOPNOTSUPP         =  (WSABASEERR+45);
  WSAEPFNOSUPPORT       =  (WSABASEERR+46);
  WSAEAFNOSUPPORT       =  (WSABASEERR+47);
  WSAEADDRINUSE         =  (WSABASEERR+48);
  WSAEADDRNOTAVAIL      =  (WSABASEERR+49);
  WSAENETDOWN           =  (WSABASEERR+50);
  WSAENETUNREACH        =  (WSABASEERR+51);
  WSAENETRESET          =  (WSABASEERR+52);
  WSAECONNABORTED       =  (WSABASEERR+53);
  WSAECONNRESET         =  (WSABASEERR+54);
  WSAENOBUFS            =  (WSABASEERR+55);
  WSAEISCONN            =  (WSABASEERR+56);
  WSAENOTCONN           =  (WSABASEERR+57);
  WSAESHUTDOWN          =  (WSABASEERR+58);
  WSAETOOMANYREFS       =  (WSABASEERR+59);
  WSAETIMEDOUT          =  (WSABASEERR+60);
  WSAECONNREFUSED       =  (WSABASEERR+61);
  WSAELOOP              =  (WSABASEERR+62);
  WSAENAMETOOLONG       =  (WSABASEERR+63);
  WSAEHOSTDOWN          =  (WSABASEERR+64);
  WSAEHOSTUNREACH       =  (WSABASEERR+65);
  WSAENOTEMPTY          =  (WSABASEERR+66);
  WSAEPROCLIM           =  (WSABASEERR+67);
  WSAEUSERS             =  (WSABASEERR+68);
  WSAEDQUOT             =  (WSABASEERR+69);
  WSAESTALE             =  (WSABASEERR+70);
  WSAEREMOTE            =  (WSABASEERR+71);

  WSAEDISCON            =  (WSABASEERR+101);

(*
 * Extended Windows Sockets error constant definitions
 *)
  WSASYSNOTREADY        =  (WSABASEERR+91);
  WSAVERNOTSUPPORTED    =  (WSABASEERR+92);
  WSANOTINITIALISED     =  (WSABASEERR+93);

(*
 * Error return codes from gethostbyname() and gethostbyaddr()
 * (when using the resolver). Note that these errors are
 * retrieved via WSAGetLastError() and must therefore follow
 * the rules for avoiding clashes with error numbers from
 * specific implementations or language run-time systems.
 * For this reason the codes are based at WSABASEERR+1001.
 * Note also that [WSA]NO_ADDRESS is defined only for
 * compatibility purposes.
 *)

(* Authoritative Answer: Host not found *)
  WSAHOST_NOT_FOUND     =  (WSABASEERR+1001);
  HOST_NOT_FOUND        =  WSAHOST_NOT_FOUND;

(* Non-Authoritative: Host not found, or SERVERFAIL *)
  WSATRY_AGAIN          =  (WSABASEERR+1002);
  TRY_AGAIN             =  WSATRY_AGAIN;

(* Non recoverable errors, FORMERR, REFUSED, NOTIMP *)
  WSANO_RECOVERY        =  (WSABASEERR+1003);
  NO_RECOVERY           =  WSANO_RECOVERY;

(* Valid name, no data record of requested type *)
  WSANO_DATA            =  (WSABASEERR+1004);
  NO_DATA               =  WSANO_DATA;

(* no address, look for MX record *)
  WSANO_ADDRESS         =  WSANO_DATA;
  NO_ADDRESS            =  WSANO_ADDRESS;



<* EXTERNAL accept:PASCAL *>
PROCEDURE accept(
    s: SOCKET; addr: struct_sockaddr_star; addrlen: PINT32): SOCKET;

<* EXTERNAL bind:PASCAL *>
PROCEDURE bind (
    s: SOCKET; name: struct_sockaddr_star; namelen: INT32): INT32;

<* EXTERNAL closesocket:PASCAL *>
PROCEDURE closesocket (s: SOCKET): INT32;

<* EXTERNAL connect:PASCAL *>
PROCEDURE connect (
    s: SOCKET; addr: struct_sockaddr_star; namelen: INT32): INT32;

<* EXTERNAL ioctlsocket:PASCAL *>
PROCEDURE ioctlsocket (s: SOCKET; cmd: INT32; argp: UNTRACED REF UINT32): INT32;

<* EXTERNAL getpeername:PASCAL *>
PROCEDURE getpeername (
    s: SOCKET; name: struct_sockaddr_star; namelen: PINT32): INT32;

<* EXTERNAL getsockname:PASCAL *>
PROCEDURE getsockname (
    s: SOCKET; name: struct_sockaddr_star; namelen: PINT32): INT32;

<* EXTERNAL getsockopt:PASCAL *>
PROCEDURE getsockopt (
    s: SOCKET; level: INT32; optname: INT32;
    optval: char_star;  optlen: PINT32): INT32;

<* EXTERNAL htonl:PASCAL *>
PROCEDURE htonl(hostlong: UINT32): UINT32;

<* EXTERNAL htons:PASCAL *>
PROCEDURE htons (hostshort: UINT16): UINT16;

<* EXTERNAL inet_addr:PASCAL *>
PROCEDURE inet_addr (cp: char_star): UINT32 (*struct_in_addr*);

<* EXTERNAL inet_ntoa:PASCAL *>
PROCEDURE inet_ntoa (in: struct_in_addr): char_star;

<* EXTERNAL listen:PASCAL *>
PROCEDURE listen(s: SOCKET; backlog: INT32): INT32;

<* EXTERNAL ntohl:PASCAL *>
PROCEDURE ntohl (netlong: UINT32): UINT32;

<* EXTERNAL ntohs:PASCAL *>
PROCEDURE ntohs (netshort: UINT16): UINT16;

<* EXTERNAL recv:PASCAL *>
PROCEDURE recv(s: SOCKET; buf: char_star; len, flags: INT32): INT32;

<* EXTERNAL recvfrom:PASCAL *>
PROCEDURE recvfrom(
    s: SOCKET; buf: char_star; len, flags: INT32;
    from: struct_sockaddr_star; fromlen: PINT32): INT32;

<* EXTERNAL select:PASCAL *>
PROCEDURE select (
    nfds: INT32; readfds, writefds, exceptfds: UNTRACED REF struct_fd_set;
    timeout: UNTRACED REF struct_timeval): INT32;

<* EXTERNAL send:PASCAL *>
PROCEDURE send(s: SOCKET; msg: char_star; len, flags: INT32): INT32;

<* EXTERNAL sendto:PASCAL *>
PROCEDURE sendto (
    s: SOCKET; buf: char_star; len, flags: INT32;
    to: struct_sockaddr_star; tolen: INT32): INT32;

<* EXTERNAL setsockopt:PASCAL *>
PROCEDURE setsockopt(
    s: SOCKET; level, optname: INT32; optval: char_star; optlen: INT32): INT32;

<* EXTERNAL shutdown:PASCAL *>
PROCEDURE shutdown(s: SOCKET; how: INT32): INT32;

<* EXTERNAL socket:PASCAL *>
PROCEDURE socket(af, type, protocol: INT32): SOCKET;


(* Database function prototypes *)

<* EXTERNAL gethostbyaddr:PASCAL *>
PROCEDURE gethostbyaddr (addr: char_star; len, type: INT32): struct_hostent_star;

<* EXTERNAL gethostbyname:PASCAL *>
PROCEDURE gethostbyname (name: char_star): struct_hostent_star;

<* EXTERNAL gethostname:PASCAL *>
PROCEDURE gethostname (name: char_star; namelen: INT32): INT32;

<* EXTERNAL getservbyport:PASCAL *>
PROCEDURE getservbyport (port: INT32; proto: char_star): struct_servent_star;

<* EXTERNAL getservbyname:PASCAL *>
PROCEDURE getservbyname (name, proto: char_star): struct_servent_star;

<* EXTERNAL getprotobynumber:PASCAL *>
PROCEDURE getprotobynumber (proto: INT32): struct_protoent_star;

<* EXTERNAL getprotobyname:PASCAL *>
PROCEDURE getprotobyname (name: char_star): struct_protoent_star;


(* Microsoft Windows Extension function prototypes *)

CONST
  WSADESCRIPTION_LEN    =  256;
  WSASYS_STATUS_LEN     =  128;

TYPE
  WSAData = RECORD
    wVersion:       UINT16;
    wHighVersion:   UINT16;
    szDescription:  ARRAY [0..WSADESCRIPTION_LEN] OF char;
    szSystemStatus: ARRAY [0..WSASYS_STATUS_LEN] OF char;
    iMaxSockets:    UINT16;
    iMaxUdpDg:      UINT16;
    lpVendorInfo:   char_star;
  END;
  LPWSADATA = UNTRACED REF WSAData;


<* EXTERNAL WSAStartup:PASCAL *>
PROCEDURE WSAStartup(wVersionRequired: UINT16; lpWSAData: LPWSADATA): INT32;

<* EXTERNAL WSACleanup:PASCAL *>
PROCEDURE WSACleanup(): INT32;

<* EXTERNAL WSAGetLastError:PASCAL *>
PROCEDURE WSAGetLastError(): INT32;

<* EXTERNAL WSASendDisconnect:PASCAL *>
PROCEDURE WSASendDisconnect(s: SOCKET; disconnectData: ADDRESS := NIL): INT32;

(* Rest of Microsoft extensions omitted ... assume multithreaded env. *)


END WinSock.
