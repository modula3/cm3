(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sun Aug  8 15:38:35 PDT 1993 by heydon                   *)

MODULE Packet;

IMPORT PacketRep;

REVEAL
  T = PacketRep.T BRANDED OBJECT OVERRIDES
    init := Init;
    curr := Curr;
    dest := Dest;
    done := Done;
    moveTo := MoveTo
  END;

PROCEDURE Init(pkt: T; src, finDest: CARDINAL): T =
  BEGIN
    pkt.source := src;
    pkt.current := src;
    pkt.finDest := finDest;
    RETURN pkt
  END Init;

PROCEDURE Curr(pkt: T): CARDINAL =
  BEGIN RETURN pkt.current END Curr;

PROCEDURE Dest(pkt: T): CARDINAL =
  BEGIN RETURN pkt.finDest END Dest;

PROCEDURE Done(pkt: T): BOOLEAN =
  BEGIN RETURN pkt.current = pkt.finDest END Done;

PROCEDURE MoveTo(pkt: T; to: CARDINAL) =
  BEGIN pkt.current := to END MoveTo;

BEGIN
END Packet.

