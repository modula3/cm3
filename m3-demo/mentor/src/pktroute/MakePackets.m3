(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Tue Jan 31 15:40:31 PST 1995 by kalsow                   *)
(*      modified on Sun Aug  8 15:56:33 PDT 1993 by heydon                   *)

MODULE MakePackets;

IMPORT AlgGreedy, Packet;
IMPORT PktRouteIE;
IMPORT Rd, Thread, Text, Sx, RefList;

PROCEDURE SourceFromName(nm: TEXT): Source =
  BEGIN
    IF    Text.Equal(nm, "randomSrc")    THEN RETURN Source.Random
    ELSIF Text.Equal(nm, "constantSrc")  THEN RETURN Source.Constant
    ELSIF Text.Equal(nm, "pktsFromFile") THEN RETURN Source.FromFile
    ELSE <* ASSERT FALSE *>
    END
  END SourceFromName;

PROCEDURE RandomPkts(alg: AlgGreedy.T; total: CARDINAL): Packet.Array
    RAISES {Thread.Alerted} =
  VAR res: Packet.Array; numNodes := alg.graph.numNodes(); BEGIN
    PktRouteIE.StartPackets(alg, total);
    res := NEW(Packet.Array, total);
    VAR src, dest, hops: INTEGER; BEGIN
      FOR i := 0 TO LAST(res^) DO
        REPEAT
          REPEAT src := alg.random.integer(min := 0, max := numNodes - 1)
          UNTIL alg.maxQSz = -1 OR alg.qSz[src] < alg.maxQSz;
          dest := alg.random.integer(min := 0, max := numNodes - 1);
          hops := alg.unweighted.dist(src, dest)
        UNTIL hops > 0 OR numNodes = 1;
        res[i] := alg.newPkt().init(src, dest);
        INC(alg.qSz[src]);
        res[i].id := i;
        PktRouteIE.NewPacket(alg, i, src, dest, hops,
          alg.graph.nodeName(dest));
      END
    END;
    PktRouteIE.EndPackets(alg);
    RETURN res
  END RandomPkts;

PROCEDURE ConstantPkts(alg: AlgGreedy.T; num: CARDINAL): Packet.Array
    RAISES {Thread.Alerted} =
  VAR
    numNodes := alg.graph.numNodes();
    tot := num * numNodes;
    res := NEW(Packet.Array, tot);
    pkt, dest, hops: INTEGER;
  BEGIN
    PktRouteIE.StartPackets(alg, tot);
    FOR i := 0 TO numNodes - 1 DO
      FOR j := 0 TO num - 1 DO
        pkt := (i * num) + j;
        REPEAT
          dest := alg.random.integer(min := 0, max := numNodes - 1);
          hops := alg.unweighted.dist(i, dest)
        UNTIL hops > 0 OR numNodes = 1;
        res[pkt] := alg.newPkt().init(i, dest);
        res[pkt].id := pkt;
        INC(alg.qSz[i]);
        PktRouteIE.NewPacket(alg, pkt, i, dest, hops,
          alg.graph.nodeName(dest));
      END
    END;
    PktRouteIE.EndPackets(alg);
    RETURN res
  END ConstantPkts;

PROCEDURE FromFilePkts(alg: AlgGreedy.T; rd: Rd.T): Packet.Array
    RAISES {Packet.BadPkts, Thread.Alerted} =
  VAR res: Packet.Array; n := alg.graph.numNodes(); pktId := 0;
  PROCEDURE ReadPacket(l: RefList.T) RAISES {Packet.BadPkts, Thread.Alerted} =
    VAR from, to: INTEGER; BEGIN
      TYPECASE l.head OF REF INTEGER (i) => from := i^ ELSE
        RAISE Packet.BadPkts("Packet 'from' value not an integer")
      END;
      l := l.tail;
      TYPECASE l.head OF REF INTEGER (i) => to := i^ ELSE
        RAISE Packet.BadPkts("Packet 'to' value not an integer")
      END;
      IF NOT (0 <= from AND from < n AND 0 <= to AND to < n) THEN
        RAISE Packet.BadPkts("Node id out of range in packet specification")
      END;
      res[pktId] := alg.newPkt().init(from, to);
      res[pktId].id := pktId;
      INC(alg.qSz[from]);
      PktRouteIE.NewPacket(alg, pktId, from, to,
        alg.unweighted.dist(from, to), alg.graph.nodeName(to))
    END ReadPacket;
  VAR sx: Sx.T; BEGIN
    TRY sx := Sx.Read(rd) EXCEPT
      Rd.EndOfFile => RAISE Packet.BadPkts("Encountered end-of-file")
    | Sx.ReadError (msg) => RAISE Packet.BadPkts(msg)
    END;
    VAR num := RefList.Length(sx); BEGIN
      PktRouteIE.StartPackets(alg, num);
      res := NEW(Packet.Array, num)
    END;
    TYPECASE sx OF RefList.T (curr) =>
      WHILE curr # NIL DO
        TYPECASE curr.head OF RefList.T (r) => ReadPacket(r) ELSE
          RAISE Packet.BadPkts("Bad packet specification")
        END;
        INC(pktId);
        curr := curr.tail
      END
    ELSE RAISE Packet.BadPkts("Top-level packet specification is not a list")
    END;
    PktRouteIE.EndPackets(alg);
    RETURN res
  END FromFilePkts;

BEGIN
END MakePackets.
