MODULE ProcessSplitSeq EXPORTS ProcessSplit;

(* This is an implementation of ProcessSplit which does buffer flushs more
   seldom and thus don't need a priority queue. *)

IMPORT Signal;

IMPORT LongRealSeq;
IMPORT Thread;

TYPE
  Pool =
    RECORD
      streams: REF ARRAY OF T;   (* pointer to all streams, streams which
                                    already exit have a NIL pointer *)
      numStreams: CARDINAL;      (* number of active (i.e.  non-NIL)
                                    streams *)
      buffer: LongRealSeq.T;     (* buffer of some past samples, shared by
                                    several streams *)
      x: Signal.T;         (* the input stream *)
      flushCount: CARDINAL;      (* number of calls to 'get' of any of the
                                    split streams until the next buffer
                                    flush *)
      maxFlushCount: CARDINAL;   (* number of 'get' calls between two
                                    flushs *)
    END;

  T = Signal.T BRANDED OBJECT
        pool : REF Pool;
        index: CARDINAL;         (* index of this stream in 'streams' *)
        pos: CARDINAL;           (* the current position in the buffer for
                                    reading via this stream *)
      OVERRIDES
        get  := Get;
        exit := Exit;
      END;

CONST
  FlushThreshold = 10;
  BufferSizeHint = 1000;


PROCEDURE New (x: Signal.T; number: CARDINAL; ):
  REF ARRAY OF Signal.T =
  VAR
    streams       := NEW(REF ARRAY OF T, number);
    streamsCopy   := NEW(REF ARRAY OF Signal.T, number);
    maxFlushCount := FlushThreshold * number;
    pool := NEW(REF Pool, streams := streams, numStreams := number,
                buffer :=
                  NEW(LongRealSeq.T).init(sizeHint := BufferSizeHint),
                x := x, flushCount := maxFlushCount,
                maxFlushCount := maxFlushCount);
  BEGIN
    FOR j := FIRST(streams^) TO LAST(streams^) DO
      WITH stream = NEW(T, pool := pool, index := j, pos := 0) DO
        streams[j] := stream;
        streamsCopy[j] := stream;
      END;
    END;
    RETURN streamsCopy;
  END New;

PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  VAR
    pool           := SELF.pool;
    y   : LONGREAL;
  BEGIN
    (* fetch next sample *)
    WITH pos        = SELF.pos,
         bufferSize = pool.buffer.size() DO
      <* ASSERT 0 <= pos AND pos <= bufferSize *>
      IF pos = bufferSize THEN pool.buffer.addhi(pool.x.get()); END;
      y := pool.buffer.get(pos);
      INC(pos);
    END;

    DEC(pool.flushCount);
    IF pool.flushCount = 0 THEN
      (* flush as much old samples as possible *)
      VAR minPos: CARDINAL := LAST(CARDINAL);
      BEGIN
        FOR j := FIRST(pool.streams^) TO LAST(pool.streams^) DO
          WITH stream = pool.streams[j] DO
            IF stream # NIL AND minPos > stream.pos THEN
              minPos := stream.pos;
            END;
          END;
        END;

        FOR j := FIRST(pool.streams^) TO LAST(pool.streams^) DO
          WITH stream = pool.streams[j] DO
            IF stream # NIL THEN DEC(stream.pos, minPos); END;
          END;
        END;
        FOR j := 0 TO minPos - 1 DO EVAL pool.buffer.remlo(); END;
      END;
      pool.flushCount := pool.maxFlushCount;
    END;

    RETURN y;
  END Get;

PROCEDURE Exit (SELF: T; ) =
  VAR pool := SELF.pool;
  BEGIN
    <* ASSERT pool.streams[SELF.index] # NIL *>
    <* ASSERT pool.numStreams > 0 *>

    pool.streams[SELF.index] := NIL;

    DEC(pool.numStreams);
    IF pool.numStreams = 0 THEN
      pool.x.exit();
      pool.buffer := NIL;
      pool.streams := NIL;
      pool.x := NIL;
    END;
  END Exit;


BEGIN
END ProcessSplitSeq.
