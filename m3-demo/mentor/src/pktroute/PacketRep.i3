(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sun Aug  8 15:15:20 PDT 1993 by heydon                   *)

INTERFACE PacketRep;

IMPORT Packet;

TYPE
  T = Packet.TPub BRANDED OBJECT
    source, current, finDest: CARDINAL
  END;
  (* If "p" is a "PacketRep.T", then "p.source" is "p"'s source node,
     "p.current" is its current node, and "p.finDest" is its final
     destination. *)

REVEAL
  Packet.T <: T;

END PacketRep.
