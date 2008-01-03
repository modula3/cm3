(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Jul 11 18:04:22 PDT 1992 by muller                   *)
(*      modified on Tue Feb 11 22:04:30 PST 1992 by nichols@parc.xerox.com   *)


INTERFACE Uin;

FROM Ctypes IMPORT char;
FROM Utypes IMPORT u_int8_t, u_int16_t, in_addr_t, in_port_t;
IMPORT Word;

(* Constants and structures defined by the internet system,
   Per RFC 790, September 1981. *)

CONST
  IPPROTO_IP = 0;		(* dummy for IP *)
  IPPROTO_ICMP = 1;		(* control message protocol *)
  IPPROTO_IGMP = 2;		(* group control protocol *)
  IPPROTO_GGP = 3;		(* gateway^2 (deprecated) *)
  IPPROTO_TCP = 6;		(* tcp *)
  IPPROTO_EGP = 8;		(* exterior gateway protocol *)
  IPPROTO_PUP = 12;		(* pup *)
  IPPROTO_UDP = 17;		(* user datagram protocol *)
  IPPROTO_IDP = 22;		(* xns idp *)
  IPPROTO_TP = 29;              (* tp-4 w/ class negotiation *)
  IPPROTO_RSVP = 46;            (* resource reservation *)
  IPPROTO_EON = 80;             (* ISO cnlp *)
  IPPROTO_ENCAP = 98;           (* encapsulation header *)

  IPPROTO_DIVERT = 254;         (* divert pseudo-protocol *)
  IPPROTO_RAW = 255;		(* raw IP packet *)
  IPPROTO_MAX = 256;

(* Port/socket numbers: network standard functions *)
  IPPORT_ECHO = 7;
  IPPORT_DISCARD = 9;
  IPPORT_SYSTAT = 11;
  IPPORT_DAYTIME = 13;
  IPPORT_NETSTAT = 15;
  IPPORT_FTP = 21;
  IPPORT_TELNET = 23;
  IPPORT_SMTP = 25;
  IPPORT_TIMESERVER = 37;
  IPPORT_NAMESERVER = 42;
  IPPORT_WHOIS = 43;
  IPPORT_MTP = 57;

(* Port/socket numbers: host specific functions *)
  IPPORT_TFTP = 69;
  IPPORT_RJE = 77;
  IPPORT_FINGER = 79;
  IPPORT_TTYLINK = 87;
  IPPORT_SUPDUP = 95;

(* UNIX TCP sockets *)
  IPPORT_EXECSERVER = 512;
  IPPORT_LOGINSERVER = 513;
  IPPORT_CMDSERVER = 514;
  IPPORT_EFSSERVER = 520;

(* UNIX UDP sockets *)
  IPPORT_BIFFUDP = 512;
  IPPORT_WHOSERVER = 513;
  IPPORT_ROUTESERVER = 520;	(* 520+1 also used *)

(* Ports < IPPORT_RESERVED are reserved for privileged processes (e.g. root).
   Ports > IPPORT_USERRESERVED are reserved for servers, not necessarily
   privileged. *)
  IPPORT_RESERVED = 1024;
  IPPORT_USERRESERVED = 5000;

(* Link numbers *)
  IMPLINK_IP = 155;
  IMPLINK_LOWEXPER = 156;
  IMPLINK_HIGHEXPER = 158;


TYPE
  struct_in_addr =   RECORD
                       s_addr: in_addr_t; END;
  struct_in_addr_b = RECORD
                       s_b1, s_b2, s_b3, s_b4: u_int8_t; END;
  struct_in_addr_w = RECORD
                       s_w1, s_w2: u_int16_t; END;


(* Definitions of bits in internet address integers.
   On subnets, the decomposition of addresses to host and net parts
   is done according to subnet mask, not the masks here. *)
PROCEDURE IN_CLASSA(i: INTEGER): BOOLEAN;
CONST
  (* Compiler bug/feature forces us to use Word.Shift *)
  IN_CLASSA_NET: in_addr_t = Word.Shift(16_ff0000, 8);
  IN_CLASSA_NSHIFT = 24;
  IN_CLASSA_HOST = 16_00ffffff;
  IN_CLASSA_MAX = 128;

PROCEDURE IN_CLASSB(i: INTEGER): BOOLEAN;
CONST
  IN_CLASSB_NET: in_addr_t = Word.Shift(16_ffff00, 8);
  IN_CLASSB_NSHIFT = 16;
  IN_CLASSB_HOST = 16_0000ffff;
  IN_CLASSB_MAX = 65536;

PROCEDURE IN_CLASSC(i: INTEGER): BOOLEAN;
CONST
  IN_CLASSC_NET: in_addr_t = Word.Shift(16_ffffff, 8);
  IN_CLASSC_NSHIFT = 8;
  IN_CLASSC_HOST = 16_000000ff;

PROCEDURE IN_CLASSD(i: INTEGER): BOOLEAN;
PROCEDURE IN_MULTICAST(i: INTEGER): BOOLEAN;

PROCEDURE IN_EXPERIMENTAL(i: INTEGER): BOOLEAN;
PROCEDURE IN_BADCLASS(i: INTEGER): BOOLEAN;

CONST
  INADDR_ANY: in_addr_t = 16_00000000;
  INADDR_LOOPBACK: in_addr_t = 16_7F000001;
  INADDR_BROADCAST: in_addr_t = Word.Shift(16_ffffff, 8) + 16_ff;
				(* must be masked *)

  IN_LOOPBACKNET = 127;		(* official! *)

(* Stuff the loopback address into an Internet address. *)
PROCEDURE IN_SET_LOOPBACK_ADDR(a: struct_sockaddr_in_star);

(* Socket address, internet style. *)
TYPE
  struct_sockaddr_in = RECORD
    sin_len: u_int8_t;
    sin_family: u_int8_t;
    sin_port: in_port_t;
    sin_addr: struct_in_addr;
    sin_zero: ARRAY [0..7] OF char;
  END;
  struct_sockaddr_in_star = UNTRACED REF struct_sockaddr_in;

(* Options for use with [gs]etsockopt at the IP level. *)
CONST
  IP_OPTIONS = 1;		(* set/get IP per-packet options *)

(* Procedures for number representation conversion. *)
PROCEDURE ntohl(x: in_addr_t): in_addr_t;
PROCEDURE ntohs(x: in_port_t): in_port_t;
PROCEDURE htonl(x: in_addr_t): in_addr_t;
PROCEDURE htons(x: in_port_t): in_port_t;

END Uin.
