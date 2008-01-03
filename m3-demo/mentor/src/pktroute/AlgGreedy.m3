(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Tue Jan 31 14:47:28 PST 1995 by kalsow                   *)
(*      modified on Wed Aug 11 01:43:37 PDT 1993 by heydon                   *)

MODULE AlgGreedy;

IMPORT Graph, Topology, ASP, Packet, MakePackets;
IMPORT ZeusPanel, Algorithm, PktRouteIE;
IMPORT FormsVBT;
IMPORT VBT;
IMPORT Thread, OSError, FileRd, IntRefTbl, Text, Random, Stdio, Wr;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented, Wr.Failure *>

TYPE
  TieBreak = { FirstMatch, Random };
  Contention = { FurthestFirst, Random };

REVEAL
  T = TPub BRANDED OBJECT
    pkt: Packet.Array;			 (* packet data *)
    activePkts: CARDINAL;		 (* active are in [0..activePkts-1] *)
    tieBreak: TieBreak;			 (* tie breaking heuristic *)
    contention: Contention;		 (* contention resolution heuristic *)
    outTbl: IntRefTbl.Default;		 (* Int -> EdgeRec table *)
    inTbl: IntRefTbl.Default;		 (* Int -> EdgeRec table *)
    minNbr: REF ARRAY OF CARDINAL;	 (* closest neighbors array *)
  OVERRIDES
    run := Run;
    newPkt := NewPkt;
  END;

(* For "alg: T", An entry "(i, l)" in "alg.outTbl" means that "l" is the list
   of packets leaving node "i". In this list, there is only one entry for each
   of "i"'s neighbors "n". If "e: EdgeRec" in "l", then "e.neighbor" is the
   index of the neighbor "n", and "e.head" is a list of packets scheduled to
   leave on the edge from "i" to "e.neighbor". In the case of the
   furthest-first contention heuristic, the list of packets for any particular
   neighbor has exactly one packet, namely, the packet with furthest distance
   to travel.

   "alg.inTbl" is only used if "alg.maxQSz # -1" (i.e., if the node queues
   have bounded size). An entry "(i, l)" in "alg.inTbl" means that "l" is the
   list of packets entering node "i". In this list, there is only one entry
   for each of "i"'s neighbors "n". The "EdgeRec"'s in "l" have the same
   meaning as those in "outTbl". Note, however, that there will be exactly
   *one* packet associated with each "EdgeRec" in "alg.inTbl", since at most
   one packet flows along any edge at each step. *)

PROCEDURE New(): Algorithm.T =
  BEGIN
    RETURN NEW(T, data := ZeusPanel.NewForm("AlgGreedy.fv")).init()
  END New;

PROCEDURE CreateGraph(alg: T): Graph.T
    RAISES {Topology.BadGraph, Thread.Alerted} =
(* Create a new graph according to the form values in "alg". Sets the value
   "alg.maxQSz". Raises "BadGraph" if the graph cannot be created according to
   the given form values. *)
  VAR
    topology: TEXT;			 (* name of topology choice *)
    bnd: BOOLEAN;			 (* is queue size bounded? *)
    maxQSz: INTEGER;			 (* max size of queue at each node *)
    res: Graph.T;
  BEGIN
    LOCK VBT.mu DO
      topology := FormsVBT.GetChoice(alg.data, "topology");
      bnd := FormsVBT.GetBoolean(alg.data, "bounded");
      maxQSz := FormsVBT.GetInteger(alg.data, "maxQueueSize")
    END;
    IF bnd THEN alg.maxQSz := maxQSz ELSE alg.maxQSz := -1 END;
    (* generate the graph *)
    CASE Topology.FromName(topology) OF
      Topology.Kind.Grid =>
        VAR w, h: INTEGER; BEGIN
          LOCK VBT.mu DO
            w := FormsVBT.GetInteger(alg.data, "gWidth");
            h := FormsVBT.GetInteger(alg.data, "gHeight");
          END;
          res := Topology.NewGrid(alg, w, h, maxQSz, bnd)
        END
    | Topology.Kind.Torus =>
        VAR w, h: INTEGER; BEGIN
          LOCK VBT.mu DO
            w := FormsVBT.GetInteger(alg.data, "tWidth");
            h := FormsVBT.GetInteger(alg.data, "tHeight");
          END;
          res := Topology.NewTorus(alg, w, h, maxQSz, bnd)
        END
    | Topology.Kind.Butterfly =>
        VAR dim: INTEGER; BEGIN
          LOCK VBT.mu DO dim := FormsVBT.GetInteger(alg.data, "butDim") END;
          res := Topology.NewButterfly(alg, dim, maxQSz, bnd)
        END
    | Topology.Kind.FromFile =>
        VAR fname: TEXT; BEGIN
          LOCK VBT.mu DO
            fname := FormsVBT.GetText(alg.data, "networkFilename")
          END;
          TRY
            res := Topology.NewFromFile(alg, FileRd.Open(fname), maxQSz, bnd)
          EXCEPT
            OSError.E =>
              RAISE Topology.BadGraph("Could not open file \"" & fname & "\"")
          END
        END
    END;
    IF res.numNodes() < 2 THEN
      RAISE Topology.BadGraph("Graph must have at least 2 nodes")
    END;
    RETURN res
  END CreateGraph;

PROCEDURE InitQueueSz(alg: T) =
(* Initialize "alg.qSz" to an array of 0 packets at each node. *)
  VAR n := alg.graph.numNodes(); BEGIN
    alg.qSz := NEW(QueueSz, n);
    FOR i := 0 TO n - 1 DO alg.qSz[i] := 0 END;
  END InitQueueSz;

PROCEDURE CreatePkts(alg: T): Packet.Array
    RAISES {Packet.BadPkts, Thread.Alerted} =
(* Create and return a new set of packets according to the form values in
   "alg". Raises "Packet.BadPkts" if the packets cannot be created according
   to the given form values. *)
  VAR choice: TEXT; BEGIN
    LOCK VBT.mu DO choice := FormsVBT.GetChoice(alg.data, "pktSources") END;
    CASE MakePackets.SourceFromName(choice) OF <* NOWARN *>
      MakePackets.Source.Random =>
        VAR total: INTEGER; BEGIN
          LOCK VBT.mu DO
            total := FormsVBT.GetInteger(alg.data, "pktsTotal")
          END;
          RETURN MakePackets.RandomPkts(alg, total)
        END
    | MakePackets.Source.Constant =>
        VAR num: INTEGER; BEGIN
          LOCK VBT.mu DO
            num := FormsVBT.GetInteger(alg.data, "pktsPerNode")
          END;
          RETURN MakePackets.ConstantPkts(alg, num)
        END
    | MakePackets.Source.FromFile =>
        VAR fname: TEXT; BEGIN
          LOCK VBT.mu DO
            fname := FormsVBT.GetText(alg.data, "pktsFilename")
          END;
          TRY RETURN MakePackets.FromFilePkts(alg, FileRd.Open(fname)) EXCEPT
            OSError.E =>
              RAISE Packet.BadPkts("Could not open file \"" & fname & "\"")
          END
        END
    END
  END CreatePkts;

TYPE
  PktList = REF RECORD
    index: CARDINAL;			 (* index into alg.pkt[] array *)
    dist: INTEGER;
    next: PktList := NIL;
  END;

  EdgeRec = REF RECORD
    neighbor: CARDINAL;
    head: PktList := NIL;
    next: EdgeRec
  END;

PROCEDURE MovePkts(alg: T) RAISES {Thread.Alerted} =
(* Move packets and generate the corresponding events until all packets have
   reached their final destinations. *)
  BEGIN
    WHILE alg.activePkts > 0 DO
      EVAL alg.outTbl.init(sizeHint := alg.graph.numNodes());
      FOR i := 0 TO alg.activePkts - 1 DO
        MovePktOut(alg, i, SelectNeighbor(alg, alg.pkt[i]))
      END;
      IF alg.contention = Contention.Random THEN
        SelectPktOut(alg)
      END;
      IF alg.maxQSz = -1 THEN
        MoveEligiblePktsOut(alg)
      ELSE
        MovePktsIn(alg);
        SelectPktsIn(alg);
        MoveEligiblePktsIn(alg)
      END;
      PktRouteIE.QueueSizes(alg, alg.qSz);
      DetectNonActive(alg);
      PktRouteIE.Step(alg)
    END
  END MovePkts;

PROCEDURE SelectNeighbor(alg: T; READONLY pkt: Packet.T): CARDINAL =
(* Return the index of the node adjacent to "pkt"'s current node for the
   packet "pkt" to move to next. *)
  VAR nextNbr: CARDINAL := 0;
  PROCEDURE AddMinNbr(n: CARDINAL) =
    BEGIN
      IF nextNbr = NUMBER(alg.minNbr^) THEN
	VAR new := NEW(REF ARRAY OF CARDINAL, 2 * nextNbr); BEGIN
	  SUBARRAY(new^, 0, nextNbr) := alg.minNbr^;
	  alg.minNbr := new
	END
      END;
      alg.minNbr[nextNbr] := n;
      INC(nextNbr)
    END AddMinNbr;
  VAR
    it := alg.graph.neighbors(pkt.curr());
    n: CARDINAL; wt: REAL;
    dist := alg.unweighted.dist(pkt.curr(), pkt.dest());
  BEGIN
    WHILE it.next(n, wt) DO
      IF 1 + alg.unweighted.dist(n, pkt.dest()) = dist THEN
        CASE alg.tieBreak OF
          TieBreak.FirstMatch => RETURN n
        | TieBreak.Random => AddMinNbr(n)
        END
      END
    END;
    CASE alg.tieBreak OF <* NOWARN *>
      TieBreak.Random =>
        <* ASSERT nextNbr > 0 *>
        RETURN alg.minNbr[alg.random.integer(min := 0, max := nextNbr - 1)]
    END
  END SelectNeighbor;

PROCEDURE MovePktOut(alg: T; pktIndex, neighbor: CARDINAL) =
(* Assert that the packet "alg.pkt[pktIndex]" is destined for the neighbor
   node with id "neighbor".

   If "alg.contention = Contention.FurthestFirst", then only one packet is
   stored per neighbor. Otherwise, all packets destined for each neighbor are
   stored. *)
  VAR
    ref: REFANY := NIL;
    outEdge: EdgeRec;
    pkt := alg.pkt[pktIndex];
    dist := alg.unweighted.dist(pkt.curr(), pkt.dest());
  BEGIN
    IF alg.outTbl.get(pkt.curr(), ref) THEN
      VAR curr: EdgeRec := ref; BEGIN
        WHILE curr # NIL DO
          IF neighbor = curr.neighbor THEN
            CASE alg.contention OF
              Contention.FurthestFirst =>
                VAR first := curr.head; BEGIN
                  IF dist > first.dist THEN
                    first.index := pktIndex;
                    first.dist := dist
                  END
                END
            | Contention.Random =>
                curr.head := NEW(PktList, index := pktIndex, dist := dist,
                  next := curr.head)
            END;
            RETURN
          END;
          curr := curr.next
        END
      END
    END;
    outEdge := NEW(EdgeRec, neighbor := neighbor, next := ref,
      head := NEW(PktList, index := pktIndex, dist := dist));
    EVAL alg.outTbl.put(pkt.curr(), outEdge)
  END MovePktOut;

PROCEDURE PktListLength(pl: PktList): CARDINAL =
  VAR res: CARDINAL := 0; BEGIN
    WHILE pl # NIL DO INC(res); pl := pl.next END;
    RETURN res
  END PktListLength;

PROCEDURE PktListNth(pl: PktList; n: CARDINAL): PktList =
(* Return the "n"th element of "pl", where the first element has index 0. It
   is a checked run-time error if "n" is not in the range "[0..|pl|-1]", where
   "|pl|" denotes the length of the list "pl". *)
  BEGIN
    <* ASSERT n >= 0 *>
    WHILE n > 0 DO DEC(n); pl := pl.next END;
    RETURN pl
  END PktListNth;

PROCEDURE SelectPktOut(alg: T) =
(* Change "alg.outTbl" so that exactly one packet is destined for each
   neighbor. Requires "alg.contention = Contention.Random". *)
  VAR from: INTEGER; ref: REFANY; it := alg.outTbl.iterate(); BEGIN
    <* ASSERT alg.contention = Contention.Random *>
    WHILE it.next(from, ref) DO
      VAR curr: EdgeRec := ref; BEGIN
        WHILE curr # NIL DO
          VAR len := PktListLength(curr.head); BEGIN
            IF len > 1 THEN
              curr.head := PktListNth(curr.head,
                alg.random.integer(min := 0, max := len - 1));
              curr.head.next := NIL
            END
          END;
          curr := curr.next
        END
      END
    END
  END SelectPktOut;

PROCEDURE MovePktsIn(alg: T) =
(* Transfers the packets in "alg.outTbl" to "alg.inTbl" so they are indexed by
   "to" node instead of "from" node for processing by "SelectPktsIn". Requires
   "alg.maxQSz # -1" (i.e., that the queue sizes are bounded). *)
  PROCEDURE InsertSorted(
      VAR (*INOUT*) curr: EdgeRec;
      e: EdgeRec; dist: INTEGER) =
    BEGIN
      IF curr = NIL OR curr.head.dist < dist
        THEN e.next := curr; curr := e
        ELSE InsertSorted(curr.next, e, dist)
      END
    END InsertSorted;
  PROCEDURE FindInNeighbor(from, to: CARDINAL; dist: INTEGER): EdgeRec =
  (* Return the "EdgeRec" in "alg.inTbl" denoting the edge from "from" to
     "to". The table should not contain such an edge. Add a new such edge to
     the table and return it. *)
    VAR res, head: EdgeRec; ref: REFANY; BEGIN
      IF alg.inTbl.get(to, ref) THEN head := ref END;
      res := NEW(EdgeRec, neighbor := from);
      InsertSorted(head, res, dist);
      EVAL alg.inTbl.put(to, head);
      RETURN res
    END FindInNeighbor;
  PROCEDURE AddInPkt(from, to: CARDINAL; pkt: PktList) =
  (* Add the packet "pkt" to the edge from "from" to "to" in "alg.inTbl". *)
    VAR edge: EdgeRec := FindInNeighbor(from, to, pkt.dist); BEGIN
      pkt.next := edge.head;
      edge.head := pkt
    END AddInPkt;
  VAR it := alg.outTbl.iterate(); from: INTEGER; ref: REFANY; BEGIN
    <* ASSERT alg.maxQSz # -1 *>
    EVAL alg.inTbl.init(sizeHint := alg.graph.numNodes());
    WHILE it.next(from, ref) DO
      VAR curr: EdgeRec := ref; pkt: PktList; BEGIN
        WHILE curr # NIL DO
          pkt := curr.head;
          WHILE pkt # NIL DO
            AddInPkt(from, curr.neighbor, NEW(PktList,
              index := pkt.index, dist := pkt.dist));
            pkt := pkt.next
          END;
          curr := curr.next
        END
      END
    END
  END MovePktsIn;

PROCEDURE TruncateSuffix(VAR (*INOUT*) curr: EdgeRec; cnt: INTEGER): EdgeRec=
  BEGIN
    IF curr = NIL OR cnt = 0
      THEN VAR res := curr; BEGIN curr := NIL; RETURN res END
      ELSE RETURN TruncateSuffix(curr.next, cnt - 1)
    END
  END TruncateSuffix;

PROCEDURE EdgeRecLength(l: EdgeRec): CARDINAL =
  VAR res: CARDINAL := 0; BEGIN
    WHILE l # NIL DO INC(res); l := l.next END;
    RETURN res
  END EdgeRecLength;

PROCEDURE DeleteNth(VAR (*INOUT*) curr: EdgeRec; n: CARDINAL): EdgeRec =
(* Destructively delete the "n"th "EdgeRec" structure from the list "curr",
   and return the deleted "EdgeRec". The value "n" must be in the interval
   "[0..EdgeRecLength(curr)-1]". *)
  BEGIN
    IF n = 0
      THEN VAR res := curr; BEGIN curr := curr.next; RETURN res END
      ELSE RETURN DeleteNth(curr.next, n - 1)
    END
  END DeleteNth;

PROCEDURE SelectPktsIn(alg: T) RAISES {Thread.Alerted} =
(* Change "alg.inTbl" so that the number of packets destined to each node does
   not exceed the number of empty spaces in the node's queue. Which packets
   are selected depends on the value of "alg.contention". Requires "alg.maxQSz
   # -1" (i.e., that the queue sizes are bounded). *)
  VAR to: INTEGER; ref: REFANY; it := alg.inTbl.iterate(); BEGIN
    <* ASSERT alg.maxQSz # -1 *>
    WHILE it.next(to, ref) DO
      VAR head: EdgeRec := ref; needed := alg.maxQSz - alg.qSz[to]; BEGIN
        CASE alg.contention OF
          Contention.FurthestFirst =>
            VAR tail := TruncateSuffix(head, needed); BEGIN
              WHILE tail # NIL DO
                PktRouteIE.Blocked(alg, alg.pkt[tail.head.index].id,
                  tail.neighbor, to);
                tail := tail.next
              END
            END
        | Contention.Random =>
            VAR
              len := EdgeRecLength(head);
              delCnt := len - needed;
              del: EdgeRec;
            BEGIN
              WHILE delCnt > 0 DO
                del := DeleteNth(head, alg.random.integer(min:=0, max:=len-1));
                PktRouteIE.Blocked(alg, alg.pkt[del.head.index].id,
                  del.neighbor, to);
                DEC(len); DEC(delCnt)
              END
            END
        END;
        EVAL alg.inTbl.put(to, head)
      END
    END
  END SelectPktsIn;

PROCEDURE MoveEligiblePktsOut(alg: T) RAISES {Thread.Alerted} =
(* Generate the "MovePacket" events corresponding to the packets in the
   "alg.outTbl" data structure, update each such packet's "curr" field, and
   update the queue sizes "alg.qSz". *)
  VAR from: INTEGER; ref: REFANY; it := alg.outTbl.iterate(); BEGIN
    WHILE it.next(from, ref) DO
      VAR curr: EdgeRec := ref; BEGIN
        WHILE curr # NIL DO
          MoveEligiblePkt(alg, alg.pkt[curr.head.index],
            from, curr.neighbor);
          curr := curr.next
        END
      END
    END
  END MoveEligiblePktsOut;

PROCEDURE MoveEligiblePktsIn(alg: T) RAISES {Thread.Alerted} =
(* Generate the "MovePacket" events corresponding to the packets in the
   "alg.inTbl" data structure, update each such packet's "curr" field, and
   update the queue sizes "alg.qSz". *)
  VAR to: INTEGER; ref: REFANY; it := alg.inTbl.iterate(); BEGIN
    WHILE it.next(to, ref) DO
      VAR curr: EdgeRec := ref; BEGIN
        WHILE curr # NIL DO
          MoveEligiblePkt(alg, alg.pkt[curr.head.index],
            curr.neighbor, to);
          curr := curr.next
        END
      END
    END
  END MoveEligiblePktsIn;

PROCEDURE MoveEligiblePkt(
    alg: T;
    VAR (*INOUT*) pkt: Packet.T;
    from, to: CARDINAL)
    RAISES {Thread.Alerted} =
  BEGIN
    PktRouteIE.MovePacket(alg, pkt.id, from, to);
    DEC(alg.qSz[from]);
    pkt.moveTo(to);
    INC(alg.qSz[to])
  END MoveEligiblePkt;

PROCEDURE DetectNonActive(alg: T) RAISES {Thread.Alerted} =
(* Generate "Absorb" events and decrement the appropriate queue sizes for
   packets that are no longer active because they have reached their final
   desintations. Swap any non-active packets in "alg.pkt[0..alg.activePkts-1]"
   to the end of that array, and decrement "alg.activePkts" accordingly. *) 
  VAR i := 0; BEGIN
    WHILE i < alg.activePkts DO
      WITH pkt = alg.pkt[i], last = alg.activePkts DO
        IF pkt.done() THEN
          PktRouteIE.Absorb(alg, pkt.id, pkt.dest());
          DEC(alg.qSz[pkt.dest()]);
          DEC(last);
          VAR t := pkt; BEGIN
            pkt := alg.pkt[last];
            alg.pkt[last] := t
          END
        ELSE
          INC(i)
        END
      END
    END
  END DetectNonActive;

PROCEDURE TieBreakFromName(nm: TEXT): TieBreak =
  BEGIN
    IF Text.Equal(nm, "firstMatch") THEN RETURN TieBreak.FirstMatch
    ELSIF Text.Equal(nm, "randomMatch") THEN RETURN TieBreak.Random
    ELSE <* ASSERT FALSE *>
    END
  END TieBreakFromName;

PROCEDURE ContentionFromName(nm: TEXT): Contention =
  BEGIN
    IF Text.Equal(nm, "furthestFirst") THEN RETURN Contention.FurthestFirst
    ELSIF Text.Equal(nm, "randomFirst") THEN RETURN Contention.Random
    ELSE <* ASSERT FALSE *>
    END
  END ContentionFromName;
    
PROCEDURE Run(alg: T) RAISES {Thread.Alerted} =
  VAR graph: Graph.T; BEGIN
    VAR fixed: BOOLEAN; BEGIN
      LOCK VBT.mu DO
        fixed := FormsVBT.GetBoolean(alg.data, "fixed")
      END;
      alg.random := NEW(Random.Default).init(fixed := fixed)
    END;
    TRY
      graph := CreateGraph(alg);
      alg.graph := graph;
      alg.unweighted := ASP.UnweightedFromGraph(graph);
      InitQueueSz(alg);
      alg.pkt := CreatePkts(alg);
      alg.activePkts := NUMBER(alg.pkt^);
      alg.outTbl := NEW(IntRefTbl.Default);
      alg.inTbl := NEW(IntRefTbl.Default);
      alg.minNbr := NEW(REF ARRAY OF CARDINAL, 10);
      VAR tieRes, contentionRes: TEXT; BEGIN
        LOCK VBT.mu DO
          tieRes := FormsVBT.GetChoice(alg.data, "tieRes");
          contentionRes := FormsVBT.GetChoice(alg.data, "contentionRes")
        END;
        alg.tieBreak := TieBreakFromName(tieRes);
        alg.contention := ContentionFromName(contentionRes);
      END;
      MovePkts(alg)
    EXCEPT
    | Topology.BadGraph (msg) =>
        Wr.PutText(Stdio.stderr, "Graph Error: " & msg & "\n");
        Wr.Flush(Stdio.stderr)
    | Packet.BadPkts (msg) =>
        Wr.PutText(Stdio.stderr, "Packet Error: " & msg & "\n");
        Wr.Flush(Stdio.stderr)
    END;
  END Run;

PROCEDURE NewPkt(<*UNUSED*> alg: T): Packet.T =
  BEGIN RETURN NEW(Packet.T) END NewPkt;

BEGIN
  ZeusPanel.RegisterAlg(New, "Greedy", "PktRoute")
END AlgGreedy.
