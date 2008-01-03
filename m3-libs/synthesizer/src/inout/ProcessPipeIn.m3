MODULE ProcessPipeIn;

FROM ProcessInOut IMPORT WordSize;
IMPORT Signal;

IMPORT FastBinIO AS BinIO;
IMPORT CommandRd, Rd, OSError, Thread;
IMPORT LongRealSeq;

<* FATAL OSError.E *>

PROCEDURE IntToReal (x: INTEGER; k: LONGREAL; ): LONGREAL =
  BEGIN
    RETURN k * FLOAT(x, LONGREAL);
  END IntToReal;



PROCEDURE Do (         prog    : TEXT;
              READONLY params  : ARRAY OF TEXT;
                       wordSize                  := WordSize.Bits16; ):
  Signal.RefArray RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    rd := CommandRd.Open(prog, params);
    x  := NEW(LongRealSeq.T).init(sizeHint := 1000);
  BEGIN
    TRY
      CASE wordSize OF
      | WordSize.Bits8 =>
          WITH k = 1.0D0 / FLOAT(LAST(BinIO.Byte), LONGREAL) DO
            LOOP x.addhi(IntToReal(BinIO.GetByte(rd), k)); END;
          END;
      | WordSize.Bits16 =>
          WITH k = 1.0D0 / FLOAT(LAST(BinIO.Int16), LONGREAL) DO
            LOOP x.addhi(IntToReal(BinIO.GetInt16(rd), k)); END;
          END;
      | WordSize.Bits32 =>
          WITH k = 1.0D0 / FLOAT(LAST(BinIO.Int32), LONGREAL) DO
            LOOP x.addhi(IntToReal(BinIO.GetInt32(rd), k)); END;
          END;
      END;
    EXCEPT
    | Rd.EndOfFile =>            (* treat like termination character *)
    END;

    Rd.Close(rd);

    WITH arr = NEW(Signal.RefArray, x.size()) DO
      FOR i := FIRST(arr^) TO LAST(arr^) DO arr[i] := x.get(i); END;
      RETURN arr;
    END;
  END Do;



REVEAL
  T = Public BRANDED OBJECT
        rd      : CommandRd.T;
        wordSize: WordSize;
        k       : LONGREAL;
      OVERRIDES
        init := Init;
        exit := Exit;
        get  := Get;
      END;

PROCEDURE Init (         SELF    : T;
                         prog    : TEXT;
                READONLY params  : ARRAY OF TEXT;
                         wordSize: WordSize;      ): T =
  BEGIN
    SELF.rd := CommandRd.Open(prog, params);
    SELF.wordSize := wordSize;
    CASE SELF.wordSize OF
    | WordSize.Bits8 =>
        SELF.k := 1.0D0 / FLOAT(LAST(BinIO.Byte), LONGREAL);
    | WordSize.Bits16 =>
        SELF.k := 1.0D0 / FLOAT(LAST(BinIO.Int16), LONGREAL);
    | WordSize.Bits32 =>
        SELF.k := 1.0D0 / FLOAT(LAST(BinIO.Int32), LONGREAL);
    END;
    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  <* FATAL Rd.Failure, Thread.Alerted *>
  BEGIN
    Rd.Close(SELF.rd);
  END Exit;

PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    TRY
      CASE SELF.wordSize OF
      | WordSize.Bits8 => RETURN IntToReal(BinIO.GetByte(SELF.rd), SELF.k);
      | WordSize.Bits16 =>
          RETURN IntToReal(BinIO.GetInt16(SELF.rd), SELF.k);
      | WordSize.Bits32 =>
          RETURN IntToReal(BinIO.GetInt32(SELF.rd), SELF.k);
      END;
    EXCEPT
    | Rd.EndOfFile => RAISE Signal.End;
    | Rd.Failure (err) =>
        RAISE Signal.Error(NEW(Signal.ErrorRoot).init("Rd.Failure", err));
    END;
  END Get;




REVEAL
  Multi = MultiPublic BRANDED OBJECT
            rd      : CommandRd.T;
            wordSize: WordSize;
            k       : LONGREAL;
            y       : REF ARRAY OF LONGREAL;
          OVERRIDES
            init := MultiInit;
            exit := MultiExit;
            get  := MultiGet;
          END;

PROCEDURE MultiInit (         SELF       : Multi;
                              numChannels: CARDINAL;
                     READONLY channels   : ARRAY OF CARDINAL;
                              prog       : TEXT;
                     READONLY params     : ARRAY OF TEXT;
                              wordSize   : WordSize;          ): Multi =
  BEGIN
    SELF.rd := CommandRd.Open(prog, params);
    SELF.wordSize := wordSize;
    CASE SELF.wordSize OF
    | WordSize.Bits8 =>
        SELF.k := 1.0D0 / FLOAT(LAST(BinIO.Byte), LONGREAL);
    | WordSize.Bits16 =>
        SELF.k := 1.0D0 / FLOAT(LAST(BinIO.Int16), LONGREAL);
    | WordSize.Bits32 =>
        SELF.k := 1.0D0 / FLOAT(LAST(BinIO.Int32), LONGREAL);
    END;

    SELF.y := NEW(REF ARRAY OF LONGREAL, numChannels);
    SELF.createChannels(numChannels, channels);

    RETURN SELF;
  END MultiInit;

PROCEDURE MultiExit (SELF: Multi; ) =
  <* FATAL Rd.Failure, Thread.Alerted *>
  BEGIN
    Rd.Close(SELF.rd);
  END MultiExit;

PROCEDURE MultiGet (SELF: Multi; ): REF ARRAY OF LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    TRY
      WITH y = SELF.y^ DO
        CASE SELF.wordSize OF
        | WordSize.Bits8 =>
            FOR c := FIRST(y) TO LAST(y) DO
              y[c] := IntToReal(BinIO.GetByte(SELF.rd), SELF.k);
            END;
        | WordSize.Bits16 =>
            FOR c := FIRST(y) TO LAST(y) DO
              y[c] := IntToReal(BinIO.GetInt16(SELF.rd), SELF.k);
            END;
        | WordSize.Bits32 =>
            FOR c := FIRST(y) TO LAST(y) DO
              y[c] := IntToReal(BinIO.GetInt32(SELF.rd), SELF.k);
            END;
        END;
      END;
      RETURN SELF.y;
    EXCEPT
    | Rd.EndOfFile => RAISE Signal.End;
    | Rd.Failure (err) =>
        RAISE Signal.Error(NEW(Signal.ErrorRoot).init("Rd.Failure", err));
    END;
  END MultiGet;


BEGIN
END ProcessPipeIn.
