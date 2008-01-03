(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Jun 27 13:59:11 PDT 1995 by steveg *)

INTERFACE TCPExtras;

IMPORT Ctypes, IP, TCP;

PROCEDURE LocalEndpoint(conn: TCP.T): IP.Endpoint RAISES {IP.Error};
(* return the local endpoint of a TCP connection *)

PROCEDURE htons(s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int;
PROCEDURE ntohs(s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int;
(* convert between host (h) and network (n) byte orders. *)

END TCPExtras.
