MODULE TermObject EXPORTS Term;
IMPORT Thread;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    getChar := NullGetChar;
    wr := NullWr;
  END;

TYPE
  DefT = T BRANDED OBJECT
  OVERRIDES
    getChar := DefGetChar;
    wr := DefWr;
  END;

PROCEDURE NullGetChar(<*UNUSED*>self: T): CHAR =
  BEGIN
    LOOP
      Thread.Pause(1.0D6);
    END;
  END NullGetChar;

PROCEDURE NullWr(<*UNUSED*>self: T;
                 <*UNUSED*>s: TEXT;
                 <*UNUSED*>ln, flush := FALSE) =
  BEGIN
  END NullWr;

PROCEDURE DefGetChar(<*UNUSED*>self: T): CHAR =
  BEGIN
    RETURN GetChar();
  END DefGetChar;

PROCEDURE DefWr(<*UNUSED*>self: T;
                s: TEXT; ln, flush := FALSE) =
  BEGIN
    IF ln THEN
      WrLn(s, flush);
    ELSE
      Wr(s, flush);
    END;
  END DefWr;

PROCEDURE Default(raw:=TRUE): T =
  BEGIN
    MakeRaw(raw);
    (* not sure how to handle multiple terminals with different rawflags *)

    RETURN defT;
  END Default;

VAR
  defT := NEW(DefT);
BEGIN
END TermObject.
