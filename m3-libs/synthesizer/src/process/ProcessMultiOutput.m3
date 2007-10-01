MODULE ProcessMultiOutput;

(* Cf.  ProcessSplitList *)

IMPORT Signal;

IMPORT Thread;

(* We distinguish between output signal and read channel.  Output signals
   are the ones the multi-output-process produces, read channels are the
   streams the user requested. *)

REVEAL
  T = Public BRANDED OBJECT
        numStreams: CARDINAL;    (* number of streams that are still
                                    active, we need this number to know
                                    when to close the shared input stream;
                                    this number is related to the number of
                                    _read_ streams (these maybe different
                                    from the number of distinguished
                                    outputs), numStreams <=
                                    NUMBER(channels^) *)
        channels: REF ARRAY OF Channel;
        head: REF ARRAY OF REF Buffer;  (* most recent buffers of each
                                           output signal *)
        numValid: CARDINAL;      (* in the head buffers, only the samples
                                    with indices from 0 to (numValid-1) are
                                    valid, samples after that position must
                                    be fetched from the input stream. *)
      OVERRIDES
        channel        := GetChannel;
        createChannels := CreateChannels;
      END;

TYPE
  (* A chunk of samples *)
  Buffer = RECORD
             next: REF Buffer;   (* all chunks are linked by this pointer;
                                    if NIL then this is a head buffer, and
                                    only 'numValid' samples are valid,
                                    otherwise all samples are valid *)
             data: ARRAY [0 .. BufferSize - 1] OF LONGREAL;
           END;

  Channel = Signal.T BRANDED OBJECT
              multi: T;
              buffer: REF Buffer;  (* pointer to the current buffer chunk,
                                      if a chunk is no longer referenced by
                                      any of the split streams the garbage
                                      collector will free it *)
              pos: CARDINAL;     (* the current position in the buffer for
                                    reading via this stream *)
            OVERRIDES
              get  := Get;
              exit := Exit;
            END;

CONST BufferSize = 100;


PROCEDURE CreateChannels (         SELF       : T;
                                   numOutputs : CARDINAL;
                          READONLY channelNums: ARRAY OF CARDINAL; ) =
  BEGIN
    SELF.numStreams := NUMBER(channelNums);
    SELF.numValid := 0;
    WITH head = SELF.head DO
      head := NEW(REF ARRAY OF REF Buffer, numOutputs);
      FOR j := FIRST(head^) TO LAST(head^) DO
        head[j] := NEW(REF Buffer, next := NIL);
      END;

      WITH channels = SELF.channels DO
        channels := NEW(REF ARRAY OF Channel, NUMBER(channelNums));
        FOR j := FIRST(channels^) TO LAST(channels^) DO
          channels[j] := NEW(Channel, multi := SELF,
                             buffer := head[channelNums[j]], pos := 0);
        END;
      END;
    END;
  END CreateChannels;

PROCEDURE GetChannel (SELF: T; num: CARDINAL; ): Signal.T =
  BEGIN
    RETURN SELF.channels[num];
  END GetChannel;

PROCEDURE Get (SELF: Channel; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  VAR y: LONGREAL;
  BEGIN
    WITH pos    = SELF.pos,
         buffer = SELF.buffer,
         multi  = SELF.multi,
         head   = multi.head   DO
      <* ASSERT 0 <= pos AND pos <= NUMBER(buffer.data) *>
      (* Never read beyond the last valid sample. *)
      <* ASSERT buffer.next # NIL OR pos <= multi.numValid *>

      (* Assert that the buffer has space for the next sample *)
      IF pos = NUMBER(buffer.data) THEN
        IF buffer.next = NIL THEN
          (* append new chunks to the lists of chunks *)
          FOR j := FIRST(head^) TO LAST(head^) DO
            head[j].next := NEW(REF Buffer, next := NIL);
            head[j] := head[j].next;
          END;
          multi.numValid := 0
        END;
        buffer := buffer.next;
        pos := 0;
      END;

      (* Assert that the next read sample is valid *)
      IF buffer.next = NIL AND pos = multi.numValid THEN
        WITH samples = SELF.multi.get()^ DO
          FOR j := FIRST(head^) TO LAST(head^) DO
            head[j].data[multi.numValid] := samples[j];
          END;
        END;
        INC(multi.numValid);
      END;

      y := buffer.data[pos];
      INC(pos);
    END;

    RETURN y;
  END Get;

PROCEDURE Exit (SELF: Channel; ) =
  VAR multi := SELF.multi;
  BEGIN
    (* check if the stream is still active, otherwise exit() was called
       twice *)
    <* ASSERT SELF.buffer # NIL AND SELF.multi # NIL *>
    SELF.buffer := NIL;
    SELF.multi := NIL;

    DEC(multi.numStreams);
    IF multi.numStreams = 0 THEN multi.exit(); END;
  END Exit;


BEGIN
END ProcessMultiOutput.
