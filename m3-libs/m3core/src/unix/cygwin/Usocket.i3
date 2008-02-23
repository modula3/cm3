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

IMPORT Ctypes, Uin;

(*** sys/socket.h ***)

(*
 * Definitions related to sockets: types, address families, options.
 *)

CONST
  SOCK_STREAM    = 1;            (* stream socket *)
  SOCK_DGRAM     = 2;            (* datagram socket *)
  SO_REUSEADDR   = 4;       (* allow local address reuse *)
  SO_KEEPALIVE   = 8;       (* keep connections alive *)
  SO_LINGER      = 16_0080; (* linger on close if data present *)
  MSG_PEEK       = 16_2;         (* peek at incoming message *)

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
  AF_INET        = 2;            (* internetwork: UDP, TCP, etc. *)


<*EXTERNAL*>
PROCEDURE accept(
    s: Ctypes.int;
    addr: UNTRACED REF Uin.struct_sockaddr_in;
    addrlen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE bind(
    s: Ctypes.int;
    name: UNTRACED REF Uin.struct_sockaddr_in;
    namelen: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE connect(
    s: Ctypes.int;
    name: UNTRACED REF Uin.struct_sockaddr_in;
    namelen: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE getpeername(
    s: Ctypes.int;
    name: UNTRACED REF Uin.struct_sockaddr_in;
    namelen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE getsockname(
    s: Ctypes.int;
    name: UNTRACED REF Uin.struct_sockaddr_in;
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
    from: UNTRACED REF Uin.struct_sockaddr_in;
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
    to: UNTRACED REF Uin.struct_sockaddr_in;
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
