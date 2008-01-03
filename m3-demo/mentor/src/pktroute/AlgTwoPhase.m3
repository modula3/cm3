(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Wed Aug 11 00:00:07 PDT 1993 by heydon                   *)

MODULE AlgTwoPhase;

IMPORT AlgGreedy, Packet, PacketRep;
IMPORT Algorithm, ZeusPanel;

TYPE
  T = AlgGreedy.T BRANDED OBJECT OVERRIDES
    newPkt := NewPkt
  END;

  Pkt = Packet.T BRANDED OBJECT
    mid: INTEGER;			 (* -1 => in phase II *)
  OVERRIDES
    init := Init;
    dest := Dest;
    moveTo := MoveTo
  END;

PROCEDURE New(): Algorithm.T =
  BEGIN
    RETURN NEW(T, data := ZeusPanel.NewForm("AlgGreedy.fv")).init()
  END New;

PROCEDURE NewPkt(alg: T): Packet.T =
  VAR res := NEW(Pkt); num := alg.graph.numNodes(); BEGIN
    res.mid := alg.random.integer(min := 0, max := num - 1);
    RETURN res
  END NewPkt;

PROCEDURE Init(pkt: Pkt; src, finDest: CARDINAL): Packet.T =
  BEGIN
    EVAL Packet.T.init(pkt, src, finDest);
    IF pkt.mid = pkt.source THEN pkt.mid := -1 END;
    RETURN pkt
  END Init;

PROCEDURE Dest(pkt: Pkt): CARDINAL =
  BEGIN
    IF pkt.mid = -1 OR (pkt.current = pkt.finDest)
      THEN RETURN pkt.finDest
      ELSE RETURN pkt.mid
    END
  END Dest;

PROCEDURE MoveTo(pkt: Pkt; to: CARDINAL) =
  BEGIN
    pkt.current := to;
    IF to = pkt.mid THEN pkt.mid := -1 END
  END MoveTo;

BEGIN
  ZeusPanel.RegisterAlg(New, "TwoPhaseGreedy", "PktRoute")
END AlgTwoPhase.
