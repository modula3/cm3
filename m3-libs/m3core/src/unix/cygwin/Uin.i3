(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 24 14:55:36 PST 1995 by kalsow                   *)
(*      modified on Tue Feb 14 20:15:00 GMT 1995 by rrw1000@cam.ac.uk        *)
(*      modified on Sat Jul 11 18:04:22 PDT 1992 by muller                   *)
(*      modified on Tue Feb 11 22:04:30 PST 1992 by nichols@parc.xerox.com   *)


INTERFACE Uin;

FROM Ctypes IMPORT unsigned_short, unsigned_int, char;

(* Constants and structures defined by the internet system,
   Per RFC 790, September 1981. *)

CONST
  IPPROTO_TCP = 6;		(* tcp *)

TYPE

  in_port_t = unsigned_short;
  sa_family_t = unsigned_short;
  in_addr_t = unsigned_int;

  struct_in_addr = RECORD
    s_addr: in_addr_t;
  END;

(* Socket address, internet style. *)
TYPE
  struct_sockaddr_in = BITS 16 * 8 FOR RECORD
    sin_family: sa_family_t;
    sin_port: in_port_t;
    sin_addr: struct_in_addr;
    (* Pad to size of `struct sockaddr'. *)
    sin_zero : ARRAY [0..7] OF char;
  END;
  struct_sockaddr_in_star = UNTRACED REF struct_sockaddr_in;

(* Procedures for number representation conversion. *)
PROCEDURE ntohl(x: unsigned_int): unsigned_int;
PROCEDURE ntohs(x: unsigned_short): unsigned_short;
PROCEDURE htonl(x: unsigned_int): unsigned_int;
PROCEDURE htons(x: unsigned_short): unsigned_short;

END Uin.
