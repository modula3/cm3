(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jan  5 11:25:53 GMT 1998 by rrw        *)
(*      modified on Fri Apr 29 15:16:48 PDT 1994 by kalsow     *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk     *)
(*      modified on Fri Apr 30 14:46:21 PDT 1993 by muller     *)
(*      modified on Wed Jul 11  9:34:54 PDT 1990 by mjordan    *)

INTERFACE Usocket;


IMPORT Ctypes, Uuio;

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
  SOCK_RDM       = 4;            (* reliably-delivered message *)
  SOCK_SEQPACKET = 5;            (* sequenced packet stream *)

(*
 * Option flags per-socket.
 *)
  SO_DEBUG       = 1;        (* turn on debugging info recording *)
  SO_REUSEADDR   = 2;        (* allow local address reuse *)
  SO_TYPE        = 3;        (* get socket type *)
  SO_ERROR       = 4;        (* get error status and clear *)
  SO_DONTROUTE   = 5;        (* just use interface addresses *)
  SO_BROADCAST   = 6;        (* permit sending of broadcast msgs *)
  SO_SNDBUF      = 7;        (* send buffer size *)
  SO_RCVBUF      = 8;        (* receive buffer size *)
  SO_KEEPALIVE   = 9;        (* keep connections alive *)
  SO_OOBINLINE   = 10;       (* leave received OOB data in line *)
  SO_NO_CHECK    = 11;
  SO_PRIORITY    = 12;
  SO_LINGER      = 13;        (* linger on close if data present *)

  SO_BSDCOMPAT   = 14;
(* SO_REUSEPORT = 15 *)
  SO_PASSCRED    = 16;
  SO_PEERCRED    = 17;
  SO_RCVLOWAT    = 18;
  SO_SNDLOWAT    = 19;
  SO_RCVTIMEO    = 20;
  SO_SNDTIMEO    = 21;

  (* Security levels - as per NRL IPv6 - don't actually do anything *)
  SO_SECURITY_AUTHENTICATION = 22;
  SO_SECURITY_ENCRYPTION_TRANSPORT = 23;
  SO_SECURITY_ENCRYPTION_NETWORK = 24;
(*
(*
 * Additional options, not kept in so_options.
 *)
  (* these constants may not be implemented - be careful *)
  SO_SNDLOWAT    = 16_1003;       (* send low-water mark *)
  SO_RCVLOWAT    = 16_1004;       (* receive low-water mark *)
  SO_SNDTIMEO    = 16_1005;       (* send timeout *)
  SO_RCVTIMEO    = 16_1006;       (* receive timeout *)
  *)

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
  SOL_SOCKET     = 1;      (* options for socket level *)


(*
 * Address families.
 *)
  AF_UNSPEC      = 0;            (* unspecified *)
  AF_UNIX        = 1;            (* local to host (pipes, portals) *)
  AF_INET        = 2;            (* internetwork: UDP, TCP, etc. *)

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
  (* Can't find this one either .. be careful *)
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

(*
 * Message header for recvmsg and sendmsg calls.
 *)
TYPE
  struct_msghdr = RECORD
    msg_name: Ctypes.void_star;       (* optional address *)
    msg_namelen: Ctypes.int;          (* size of address *)
    msg_iov: Uuio.struct_iovec_star;  (* scatter/gather array *)
    msg_iovlen: Ctypes.int;           (* # elements in msg_iov *)
    msg_control: Ctypes.void_star;     (* Ancilliary data *)
    msg_controllen : Ctypes.int;      (* Length of control *)
    msg_flags : Ctypes.int;           (* Flags on recieved message *)
  END;

(* Used for storage of ancilliary object info *)
  struct_cmsghdr = RECORD
     cmsg_len : Ctypes.int;  (* Length of data in cmsg_data + length of
                                cmsghdr structure *)
     cmsg_level : Ctypes.int;
     cmsg_type : Ctypes.int;
     (* I think the data is supposed to follow here, or something .. *)
   END;

CONST
  MSG_OOB        = 16_1;         (* process out-of-band data *)
  MSG_PEEK       = 16_2;         (* peek at incoming message *)
  MSG_DONTROUTE  = 16_4;         (* send without using routing tables *)
  MSG_CTRUNC     = 16_0200;      (* Control data lost before delivery *)

(*
 * Definitions for UNIX IPC domain.
 *)
TYPE
  struct_sockaddr_un = RECORD
    sun_family: Ctypes.unsigned_short;         (* AF_UNIX *)
    sun_path: ARRAY [0..107] OF Ctypes.char;   (* path name (gag) *)
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
