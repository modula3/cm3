MODULE ProcessSplitList EXPORTS ProcessSplit;

(* This is an implementation of ProcessSplit which stores the signal in a
   list of signal chunks.  It let the garbage collector flush old
   blocks. *)

IMPORT Signal;

IMPORT Thread;

TYPE
  Pool = RECORD
           x: Signal.T;          (* the input stream *)
           numStreams: CARDINAL;  (* number of streams that are still
                                     active, we need this number to know
                                     when to close the shared input
                                     stream *)
         END;

  (* A chunk of samples *)
  Buffer = RECORD
             next: REF Buffer;   (* all chunks are linked by this
                                    pointer *)
             data: ARRAY [0 .. BufferSize - 1] OF LONGREAL;
             numValid: CARDINAL;  (* only the samples with indices from 0
                                     to (numValid-1) are valid, samples
                                     after that position must be fetched
                                     from the input stream.  It must be
                                     (numValid<=NUMBER(data)) *)
           END;

  T = Signal.T BRANDED OBJECT
        pool: REF Pool;
        buffer: REF Buffer;      (* pointer to the current buffer chunk, if
                                    a chunk is no longer referenced by any
                                    of the split streams the garbage
                                    collector will free it *)
        pos: CARDINAL;           (* the current position in the buffer for
                                    reading via this stream *)
      OVERRIDES
        get  := Get;
        exit := Exit;
      END;

CONST BufferSize = 100;


PROCEDURE New (x: Signal.T; number: CARDINAL; ): REF ARRAY OF Signal.T =
  VAR
    pool    := NEW(REF Pool, x := x, numStreams := number);
    buffer  := NEW(REF Buffer, next := NIL, numValid := 0);
    streams := NEW(REF ARRAY OF Signal.T, number);
  BEGIN
    FOR j := FIRST(streams^) TO LAST(streams^) DO
      streams[j] := NEW(T, pool := pool, buffer := buffer, pos := 0);
    END;
    RETURN streams;
  END New;

PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  VAR y: LONGREAL;
  BEGIN
    WITH pos    = SELF.pos,
         buffer = SELF.buffer DO
      <* ASSERT 0 <= pos AND pos <= NUMBER(buffer.data) *>
      (* Never read beyond the last valid sample. *)
      <* ASSERT pos <= buffer.numValid *>
      (* Only the last chunk is allowed to be incompletely filled. *)
      <* ASSERT buffer.numValid = BufferSize OR buffer.next = NIL *>

      (* Assert that the buffer has space for the next sample *)
      IF pos = NUMBER(buffer.data) THEN
        IF buffer.next = NIL THEN
          (* append a new chunk to the list of chunks *)
          buffer.next := NEW(REF Buffer, next := NIL, numValid := 0);
        END;
        buffer := buffer.next;
        pos := 0;
      END;

      (* Assert that the next read sample is valid *)
      IF pos = buffer.numValid THEN
        buffer.data[buffer.numValid] := SELF.pool.x.get();
        INC(buffer.numValid);
      END;

      y := buffer.data[pos];
      INC(pos);
    END;

    RETURN y;
  END Get;

PROCEDURE Exit (SELF: T; ) =
  VAR pool := SELF.pool;
  BEGIN
    (* check if the stream is still active, otherwise exit() was called
       twice *)
    <* ASSERT SELF.buffer # NIL AND SELF.pool # NIL *>
    SELF.buffer := NIL;
    SELF.pool := NIL;

    DEC(pool.numStreams);
    IF pool.numStreams = 0 THEN pool.x.exit(); pool.x := NIL; END;
  END Exit;


BEGIN
END ProcessSplitList.
