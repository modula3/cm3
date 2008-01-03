(* Copyright (C) 1995, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Tue Mar  7 12:11:55 PST 1995 by kalsow       *)
(*                                                               *)
(* Contributed by Peter Klein (pk@i3.informatik.rwth-aachen.de)  *)
(*    - Mar 7, 1995                                              *)

INTERFACE TCPPeer;

IMPORT TCP, IP;

PROCEDURE Get (channel: TCP.T): IP.Endpoint RAISES {IP.Error};
(* Return peer endpoint for TCP connection. *)

PROCEDURE GetName (channel: TCP.T): TEXT RAISES {IP.Error};
(* Return peer name for TCP connection. *)

PROCEDURE Match (channel: TCP.T;  address: IP.Address;  maskBits: [0 .. 32]):
  BOOLEAN RAISES {IP.Error};
(* Returns TRUE if the first maskBits bits of peer's endpoint adress
   match the given address. *)

END TCPPeer.
