(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE BuffOWr EXPORTS BuffOWr, BuffOWrDotT;

IMPORT OWr;
FROM OWr IMPORT Closed, Unseekable;

REVEAL
  T = OWr.T BRANDED OBJECT 
        wr: OWr.T;
        buff: REF ARRAY OF CHAR;
        cur: CARDINAL := 0;
        closedFlag := FALSE;
      OVERRIDES
        flush := Flush;
        preferredSize := PreferredSize;
        seek := Seek;
        close := Close;
        isClosed := IsClosed;
        isSeekable := IsSeekable; END;
 
  B = T OBJECT METHODS
        new (wr: OWr.T; size: CARDINAL := 0): B; END
      BRANDED OBJECT OVERRIDES
        new := NewB;
        putChar := BPutChar;
        putString := BPutString; END;

  L = T OBJECT METHODS
          new (wr: OWr.T; size: CARDINAL := 0): L; END
      BRANDED OBJECT OVERRIDES
          new := NewL;
          putChar := LPutChar; 
          putString := LPutString; END;
 
PROCEDURE NewB (self: B; wr: OWr.T; size: CARDINAL := 0): B =
  BEGIN
    IF size = 0 THEN 
      size := wr.preferredSize (); END;
    IF self = NIL THEN
      RETURN NEW (B, 
                  wr := wr,
                  buff := NEW (REF ARRAY OF CHAR, size));
    ELSE
      self.wr := wr;
      self.buff := NEW (REF ARRAY OF CHAR, size);
      RETURN self; END;
  END NewB;

PROCEDURE NewL (self: L; wr: OWr.T; size: CARDINAL := 0): L =
  BEGIN
    IF size = 0 THEN 
      size := wr.preferredSize (); END;
    IF self = NIL THEN
      RETURN NEW (L, 
                  wr := wr,
                  buff := NEW (REF ARRAY OF CHAR, size));
    ELSE
      self.wr := wr;
      self.buff := NEW (REF ARRAY OF CHAR, size);
      RETURN self; END;
  END NewL;



PROCEDURE PutChar (self: T; ch: CHAR) RAISES {Closed} =
  BEGIN
    IF self.closedFlag THEN RAISE Closed; END;

    IF self.cur = NUMBER (self.buff^) THEN
      self.wr.putString (self.buff^);
      self.cur := 0; END;
    self.buff [self.cur] := ch;
    INC (self.cur);
  END PutChar;

PROCEDURE BPutChar (self: B; ch: CHAR) RAISES {Closed} =
  BEGIN
    PutChar (self, ch);
  END BPutChar;

PROCEDURE LPutChar (self: L; ch: CHAR) RAISES {Closed} =
  BEGIN
    PutChar (self, ch);
    IF ch = '\n' AND self.cur # 0 THEN
      self.wr.putString (SUBARRAY (self.buff^, 0, self.cur));
      self.cur := 0; END;
  END LPutChar;



PROCEDURE PutString (self: T; READONLY a: ARRAY OF CHAR) RAISES {Closed} =
  BEGIN
    IF self.closedFlag THEN RAISE Closed; END;

    WITH inbuff = MIN (NUMBER (self.buff^) - self.cur, NUMBER (a)),
         left = NUMBER (a) - inbuff DO
      SUBARRAY (self.buff^, self.cur, inbuff) := SUBARRAY (a, 0, inbuff);
      IF self.cur = NUMBER (self.buff^) THEN
        self.wr.putString (self.buff^);
        self.cur := 0;
        IF left > NUMBER (self.buff^) THEN
          SUBARRAY (self.buff^, self.cur, left) := SUBARRAY(a, inbuff, left);
          self.cur := left;
        ELSE
          self.wr.putString (SUBARRAY (a, inbuff, left)); END; END; END;
  END PutString;

PROCEDURE BPutString (self: B; READONLY a: ARRAY OF CHAR) RAISES {Closed} =
  BEGIN
    PutString (self, a);
  END BPutString;

PROCEDURE LPutString (self: L; READONLY a: ARRAY OF CHAR) RAISES {Closed} =
  BEGIN
    PutString (self, a);
    FOR i := 0 TO NUMBER (a) DO
      IF a[i] = '\n' AND self.cur # 0 THEN
        self.wr.putString (SUBARRAY (self.buff^, 0, self.cur));
        EXIT; END; END;
  END LPutString;


PROCEDURE Flush (self: T) RAISES {Closed} =
  BEGIN 
    IF self.closedFlag THEN RAISE Closed; END;
    IF self.cur # 0 THEN
      self.wr.putString (SUBARRAY (self.buff^, 0, self.cur));
      self.cur := 0; END;
    self.wr.flush ();
  END Flush;

PROCEDURE PreferredSize (self: T): CARDINAL RAISES {Closed} =
  BEGIN
    IF self.closedFlag THEN RAISE Closed; END;
    RETURN (NUMBER (self.buff^));
  END PreferredSize;

PROCEDURE Seek (self:T; pos: CARDINAL) RAISES {Closed, Unseekable} =
  BEGIN
    IF self.closedFlag THEN RAISE Closed; END;
    IF NOT self.wr.isSeekable () THEN RAISE Unseekable; END;
    IF self.cur # 0 THEN
      self.wr.putString (SUBARRAY (self.buff^, 0, self.cur));
      self.cur := 0; END;
    self.wr.seek (pos);
  END Seek;

PROCEDURE Close (self: T) =
  BEGIN 
    IF self.closedFlag THEN RETURN; END;
    self.closedFlag := TRUE;
    self.wr.close ();
  END Close;

PROCEDURE IsClosed (self: T): BOOLEAN =
  BEGIN
    RETURN self.closedFlag;
  END IsClosed;

PROCEDURE IsSeekable (self: T): BOOLEAN RAISES {Closed} =
  BEGIN
    IF self.closedFlag THEN RAISE Closed; END;
    RETURN self.wr.isSeekable ();
  END IsSeekable;

BEGIN
END BuffOWr.
