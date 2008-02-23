(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 24 14:55:36 PST 1995 by kalsow                   *)
(*      modified on Tue Feb 14 20:15:00 GMT 1995 by rrw1000@cam.ac.uk        *)
(*      modified on Sat Jul 11 18:04:22 PDT 1992 by muller                   *)
(*      modified on Tue Feb 11 22:04:30 PST 1992 by nichols@parc.xerox.com   *)


INTERFACE Uin;

FROM Ctypes IMPORT short, char;
FROM Utypes IMPORT u_short, u_long;

(* Constants and structures defined by the internet system,
   Per RFC 790, September 1981. *)

CONST
  IPPROTO_TCP = 6;		(* tcp *)

TYPE
  struct_in_addr = RECORD
    s_addr: u_long;
  END;

(* Socket address, internet style. *)
TYPE
  struct_sockaddr_in = RECORD
    sin_family: short;
    sin_port: u_short;
    sin_addr: struct_in_addr;
    sin_zero: ARRAY [0..7] OF char;
  END;
  struct_sockaddr_in_star = UNTRACED REF struct_sockaddr_in;

(* Procedures for number representation conversion. *)
PROCEDURE ntohl(x: u_long): u_long;
PROCEDURE ntohs(x: u_short): u_short;
PROCEDURE htonl(x: u_long): u_long;
PROCEDURE htons(x: u_short): u_short;

END Uin.
