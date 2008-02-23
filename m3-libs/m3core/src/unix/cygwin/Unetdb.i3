(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Feb 24 15:02:49 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:28:40 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Fri Apr 30 14:42:11 PDT 1993 by muller            *)

INTERFACE Unetdb;

FROM Ctypes IMPORT short, int, char_star, char_star_star, const_char_star;

(*** <netdb.h> ***)

(*
 * Structures returned by network
 * data base library.  All addresses
 * are supplied in host order, and
 * returned in network order (suitable
 * for use in system calls).
 *)

TYPE
  struct_hostent = RECORD
    h_name:       char_star;        (* official name of host *)
    h_aliases:    char_star_star;   (* alias list *)
    h_addrtype:   short;            (* host address type *)
    h_length:     short;            (* length of address *)
    h_addr_list:  char_star_star;   (* list of addresses from name server *)
  END;
  struct_hostent_star = UNTRACED REF struct_hostent;

CONST
  TRY_AGAIN      = 2; (* Non-Authoritive Host not found, or SERVERFAIL *)
  NO_RECOVERY    = 3; (* Non recoverable errors, FORMERR, REFUSED, NOTIMP *)
  NO_ADDRESS     = 4; (* Valid host name, no address, look for MX record *)

(*** gethostbyaddr(3n), gethostbyname(3n) - get network host entry ***)

<*EXTERNAL*>
PROCEDURE gethostbyname (name: const_char_star): struct_hostent_star;

<*EXTERNAL*>
PROCEDURE gethostbyaddr (addr: const_char_star; len, type: int): struct_hostent_star;

END Unetdb.
