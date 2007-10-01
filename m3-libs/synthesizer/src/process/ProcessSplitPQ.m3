MODULE ProcessSplitPQ EXPORTS ProcessSplit;

IMPORT Signal;

IMPORT LongRealSeq, RefSeq, IntPQ;
IMPORT Thread;

TYPE
  Pool = RECORD
           pq: IntPQ.T;          (* priority queue with the position in the
                                    buffer as priority, we use it for fast
                                    access to the lowest position of the
                                    buffer still required *)
           pqelems: RefSeq.T;    (* sequence of PQElement *)
           buffer: LongRealSeq.T;  (* buffer of some past samples, shared
                                      by several streams *)
           x: Signal.T;    (* the input stream *)
         END;

  PQElement = IntPQ.Elt OBJECT stream: T;  END;
  (* the priority is the read position for this stream in the buffer *)

  T = Signal.T BRANDED OBJECT
        pool  : REF Pool;
        pqelem: PQElement;
        index : CARDINAL;        (* index of this stream in 'pqelems' *)
      OVERRIDES
        get  := Get;
        exit := Exit;
      END;

PROCEDURE New (x: Signal.T; number: CARDINAL; ):
  REF ARRAY OF Signal.T =
  VAR
    streams := NEW(REF ARRAY OF Signal.T, number);
    pqelems := NEW(RefSeq.T).init(sizeHint := number);
    pq      := NEW(IntPQ.Default).init(sizeHint := number);
    pool := NEW(REF Pool, pq := pq, pqelems := pqelems,
                buffer := NEW(LongRealSeq.T).init(sizeHint := 100), x := x);
  BEGIN
    FOR j := FIRST(streams^) TO LAST(streams^) DO
      WITH stream = NEW(T, pool := pool, index := j),
           pqelem = NEW(PQElement, priority := 0, stream := stream) DO
        stream.pqelem := pqelem;
        streams[j] := stream;
        pqelems.addhi(pqelem);
        pq.insert(pqelem);
      END;
    END;
    RETURN streams;
  END New;

PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  VAR
    pool                 := SELF.pool;
    bufferSize           := pool.buffer.size();
    y         : LONGREAL;
  <* FATAL IntPQ.NotInQueue, IntPQ.Empty *>
  BEGIN
    (* fetch next sample *)
    WITH pos = SELF.pqelem.priority DO
      <* ASSERT 0 <= pos AND pos <= bufferSize *>
      IF pos = bufferSize THEN pool.buffer.addhi(pool.x.get()); END;
      y := pool.buffer.get(pos);
      pool.pq.change(SELF.pqelem, pos + 1);
    END;

    (* flush an old sample if possible *)
    WITH minPos = pool.pq.min().priority DO
      <* ASSERT 0 <= minPos AND minPos <= 1 *>
      IF minPos = 1 THEN
        FOR j := 0 TO pool.pqelems.size() - 1 DO
          WITH pqelem = NARROW(pool.pqelems.get(j), PQElement) DO
            pool.pq.change(pqelem, pqelem.priority - 1);
          END;
        END;
        EVAL pool.buffer.remlo();
      END;
    END;

    RETURN y;
  END Get;

PROCEDURE Exit (SELF: T; ) =
  VAR
    pool        := SELF.pool;
    pqelemsSize := pool.pqelems.size();
  BEGIN
    <* ASSERT pqelemsSize > 0 *>

    (* remove the corresponding pqelem from pool.pqelems *)
    IF SELF.index + 1 = pqelemsSize THEN
      EVAL pool.pqelems.remhi();
    ELSE
      WITH hielem = NARROW(pool.pqelems.remhi(), PQElement) DO
        hielem.stream.index := SELF.index;
        pool.pqelems.put(SELF.index, hielem);
      END;
    END;

    IF pqelemsSize = 1 THEN
      pool.x.exit();
      pool.buffer := NIL;
      pool.pq := NIL;
      pool.pqelems := NIL;
      pool.x := NIL;
    END;
  END Exit;


BEGIN
END ProcessSplitPQ.
