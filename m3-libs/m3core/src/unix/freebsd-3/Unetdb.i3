(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Apr 30 14:42:11 PDT 1993 by muller        *)

INTERFACE Unetdb;

FROM Ctypes IMPORT int, char_star, char_star_star, unsigned_long;

(*** <netdb.h> ***)

(*
 * Structures returned by network
 * data base library.  All addresses
 * are supplied in host order, and
 * returned in network order (suitable
 * for use in system calls).
 *)

TYPE
  struct_hostent  = RECORD
    h_name:       char_star;        (* official name of host *)
    h_aliases:    char_star_star;   (* alias list *)
    h_addrtype:   int;              (* host address type *)
    h_length:     int;              (* length of address *)
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
    n_addrtype: int;            (* net address type *)
    n_net:      unsigned_long;  (* network # *)
    END;
  struct_netent_star = UNTRACED REF struct_netent;

  struct_servent = RECORD
    s_name:    char_star;       (* official service name *)
    s_aliases: char_star_star;  (* alias list *)
    s_port:    int;             (* port # *)
    s_proto:   char_star;       (* protocol to use *)
    END;
  struct_servent_star = UNTRACED REF struct_servent;

  struct_protoent = RECORD
    p_name:    char_star;       (* official protocol name *)
    p_aliases: char_star_star;  (* alias list *)
    p_proto:   int;             (* protocol # *)
    END;
  struct_protoent_star = UNTRACED REF struct_protoent;

  struct_rpcent = RECORD
    r_name:    char_star;        (* name of server for this rpc program *)
    r_aliases: char_star_star;   (* alias list *)
    r_number:  int;              (* rpc program number *)
    END;


(*
 * Error return codes from gethostbyname() and gethostbyaddr()
 *)

CONST
  HOST_NOT_FOUND = 1; (* Authoritive Answer Host not found *)
  TRY_AGAIN      = 2; (* Non-Authoritive Host not found, or SERVERFAIL *)
  NO_RECOVERY    = 3; (* Non recoverable errors, FORMERR, REFUSED, NOTIMP *)
  NO_ADDRESS     = 4; (* Valid host name, no address, look for MX record *)

(* The services accessible via the V2.4 gethostent interface *)

  SVC_LOCAL = 1;    (* /etc/hosts *)
  SVC_BIND  = 2;    (* Internet domain service *)
  SVC_YP    = 3;    (* Yellow Pages service *)

(* Number of services *)

  NSVCS = 4;        (* Number of supported services *)

PROCEDURE VALID_SVC (a: int): BOOLEAN;

(*** gethostent(3n), gethostbyaddr(3n), gethostbyname(3n), 
     sethostent(3n), endhostent(3n) - get network host entry ***)

<*EXTERNAL*>
PROCEDURE gethostbyname (name: char_star): struct_hostent_star;

<*EXTERNAL*>
PROCEDURE gethostbyaddr (addr: char_star; len, type: int): struct_hostent_star;

<*EXTERNAL*>
PROCEDURE sethostent (stayopen: int);

<*EXTERNAL*>
PROCEDURE endhostent ();

(*** getnetent(3n), getnetbyaddr(3n), getnetbyname(3n), 
     setnetent(3n), endnetent(3n) - get network entry ***)

<*EXTERNAL*>
PROCEDURE getnetent (): struct_netent_star;

<*EXTERNAL*>
PROCEDURE getnetbyname (name: char_star): struct_netent_star;

<*EXTERNAL*>
PROCEDURE getnetbyaddr (addr: char_star; type: int): struct_netent_star;

<*EXTERNAL*>
PROCEDURE setnetent (stayopen: int);

<*EXTERNAL*>
PROCEDURE endnetent ();


(*** getprotoent(3n), getprotobynumber(3n), getprotobyname(3n), 
     setprotoent(3n), endprotoent(3n) - get protocol entry ***)

<*EXTERNAL*>
PROCEDURE getprotoent (): struct_protoent_star;

<*EXTERNAL*>
PROCEDURE getprotobynumber (proto: int): struct_protoent_star;

<*EXTERNAL*>
PROCEDURE getprotobyname (name: char_star): struct_protoent_star;

<*EXTERNAL*>
PROCEDURE setprotoent (stayopen: int);

<*EXTERNAL*>
PROCEDURE endprotoent ();


(*** getservent(3n), getservbynumber(3n), getservbyname(3n), 
     setservent(3n), endservent(3n) - get service entry ***)

<*EXTERNAL*>
PROCEDURE getservent (): struct_servent_star;

<*EXTERNAL*>
PROCEDURE getservbyport (port: int; proto: char_star): struct_servent_star;

<*EXTERNAL*>
PROCEDURE getservbyname (name, proto: char_star): struct_servent_star;

<*EXTERNAL*>
PROCEDURE setservent (stayopen: int);

<*EXTERNAL*>
PROCEDURE endservent ();

END Unetdb.
