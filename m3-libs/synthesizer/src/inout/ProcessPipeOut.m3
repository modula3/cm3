MODULE ProcessPipeOut;

FROM ProcessInOut IMPORT WordSize;

IMPORT Signal;
IMPORT CommandWr, FastBinIO, Wr;
IMPORT OSError, Thread;

<* FATAL OSError.E *>

  (* Clip and convert a real value between -1 and 1 to an INTEGER in the
     appropriate range of integers, e.g.  -127 to 127 for bytes.  It fails
     on NaN. *)
PROCEDURE RealToInt (x, k: LONGREAL; ): INTEGER =
  BEGIN
    RETURN ROUND(k * MIN(MAX(x, -1.0D0), 1.0D0));
  END RealToInt;


PROCEDURE Array (READONLY x       : Signal.Array;
                          prog    : TEXT;
                 READONLY params  : ARRAY OF TEXT;
                          wordSize: WordSize;      )
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR wr := CommandWr.Open(prog, params);
  BEGIN
    CASE wordSize OF
    | WordSize.Bits8 =>
        WITH k = FLOAT(LAST(FastBinIO.Byte), LONGREAL) DO
          FOR j := FIRST(x) TO LAST(x) DO
            FastBinIO.PutByte(RealToInt(x[j], k), wr);
          END;
        END;
    | WordSize.Bits16 =>
        WITH k = FLOAT(LAST(FastBinIO.Int16), LONGREAL) DO
          FOR j := FIRST(x) TO LAST(x) DO
            FastBinIO.PutInt16(RealToInt(x[j], k), wr);
          END;
        END;
    | WordSize.Bits32 =>
        WITH k = FLOAT(LAST(FastBinIO.Int32), LONGREAL) DO
          FOR j := FIRST(x) TO LAST(x) DO
            FastBinIO.PutInt32(RealToInt(x[j], k), wr);
          END;
        END;
    END;

    Wr.Close(wr);
  END Array;

PROCEDURE Stream (         x       : Signal.T;
                           prog    : TEXT;
                  READONLY params  : ARRAY OF TEXT;
                           wordSize: WordSize;      )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted} =
  VAR wr := CommandWr.Open(prog, params);
  BEGIN
    TRY
      TRY
        CASE wordSize OF
        | WordSize.Bits8 =>
            WITH k = FLOAT(LAST(FastBinIO.Byte), LONGREAL) DO
              LOOP FastBinIO.PutByte(RealToInt(x.get(), k), wr); END;
            END;
        | WordSize.Bits16 =>
            WITH k = FLOAT(LAST(FastBinIO.Int16), LONGREAL) DO
              LOOP FastBinIO.PutInt16(RealToInt(x.get(), k), wr); END;
            END;
        | WordSize.Bits32 =>
            WITH k = FLOAT(LAST(FastBinIO.Int32), LONGREAL) DO
              LOOP FastBinIO.PutInt32(RealToInt(x.get(), k), wr); END;
            END;
        END;
      EXCEPT
      | Signal.End =>            (* regular end of stream *)
      | Wr.Failure (err) =>
          RAISE
            Signal.Error(NEW(Signal.ErrorRoot).init("Wr.Failure", err));
      END;
    FINALLY
      x.exit();
    END;

    Wr.Close(wr);
  END Stream;

PROCEDURE MultiStream (READONLY x     : ARRAY OF Signal.T;
                                prog  : TEXT;
                       READONLY params: ARRAY OF TEXT;
                       wordSize := WordSize.Bits16; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted} =
  VAR wr := CommandWr.Open(prog, params);
  BEGIN
    TRY
      TRY
        CASE wordSize OF
        | WordSize.Bits8 =>
            WITH k = FLOAT(LAST(FastBinIO.Byte), LONGREAL) DO
              LOOP
                FOR c := FIRST(x) TO LAST(x) DO
                  FastBinIO.PutByte(RealToInt(x[c].get(), k), wr);
                END;
              END;
            END;
        | WordSize.Bits16 =>
            WITH k = FLOAT(LAST(FastBinIO.Int16), LONGREAL) DO
              LOOP
                FOR c := FIRST(x) TO LAST(x) DO
                  FastBinIO.PutInt16(RealToInt(x[c].get(), k), wr);
                END;
              END;
            END;
        | WordSize.Bits32 =>
            WITH k = FLOAT(LAST(FastBinIO.Int32), LONGREAL) DO
              LOOP
                FOR c := FIRST(x) TO LAST(x) DO
                  FastBinIO.PutInt32(RealToInt(x[c].get(), k), wr);
                END;
              END;
            END;
        END;
      EXCEPT
      | Signal.End =>            (* regular end of stream *)
      | Wr.Failure (err) =>
          RAISE
            Signal.Error(NEW(Signal.ErrorRoot).init("Wr.Failure", err));
      END;
    FINALLY
      FOR c := FIRST(x) TO LAST(x) DO x[c].exit(); END;
    END;

    Wr.Close(wr);
  END MultiStream;

BEGIN
END ProcessPipeOut.
