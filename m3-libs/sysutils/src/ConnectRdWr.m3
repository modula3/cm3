MODULE ConnectRdWr;

(* All non-trivial code in this Module has been adapted from
   StreamRd by John D. Polstra *)

IMPORT Rd, Wr, Thread, RdClass, IO, Stdio, System, RdCopy (*, Usignal*);

CONST Bufsize = 30000;

REVEAL T = Public BRANDED OBJECT
  rd : Rd.T;
  wr : Wr.T;
  name : TEXT;
  offset := 0.0d0;
  killpid := 0;
  buf : REF ARRAY OF CHAR;
OVERRIDES
  init := Init;
  apply := Apply;
END;

PROCEDURE Init(self : T; rd : Rd.T; wr : Wr.T; name : TEXT; killpid := 0) : T =
  BEGIN
    self.rd := rd;
    self.wr := wr;
    self.name := name;
    self.killpid := killpid;
    self.buf := NEW(REF ARRAY OF CHAR, Bufsize);
    EVAL D(self, "initialized");
    RETURN self;
  END Init;

CONST Threshold = LAST(CARDINAL) DIV 4;

PROCEDURE Apply(self : T) : REFANY = 
  VAR
    lrr := NEW(REF LONGREAL);
    n: CARDINAL;
    w: LONGINT;
  BEGIN
    lrr^ := self.offset;
    TRY
      TRY
        WHILE NOT Rd.EOF(self.rd) DO
          n := Rd.CharsReady(self.rd);
          IF n > 0 THEN
            w := RdCopy.ToWriter(self.rd, self.wr, MIN(NUMBER(self.buf^), n));
            Wr.Flush(self.wr); 
          END;

          RdClass.Lock(self.rd);
          IF self.rd.hi >= Threshold THEN
            self.offset := self.offset + FLOAT(self.rd.lo, LONGREAL);
            DEC(self.rd.hi, self.rd.lo);
            DEC(self.rd.cur, self.rd.lo);
            self.rd.lo := 0;
          END;
          RdClass.Unlock(self.rd);

        END;
        
      EXCEPT
      | Rd.Failure(code) => RETURN D(self, "EXCEPTION RdFailure: " & System.AtomListToText(code));
      | Thread.Alerted => RETURN D(self, "EXCEPTION Thread.Alerted");
      | Wr.Failure(code) => RETURN D(self, "EXCEPTION WrFailure: " & System.AtomListToText(code));
      END;
    FINALLY
      EVAL D(self, "I'm closing the writer.", 2);
(*
      IF self.killpid # 0 THEN
        Thread.Pause(1.0d0);
        EVAL Usignal.kill(self.killpid, Usignal.SIGTERM); 
        Thread.Pause(1.0d0);
        EVAL Usignal.kill(self.killpid, Usignal.SIGKILL); 
      END;
*)
      TRY Wr.Close(self.wr);
      EXCEPT 
        Wr.Failure(code) => RETURN D(self, "EXCEPTION WrFailure (while closing): " & System.AtomListToText(code));
      | Thread.Alerted => RETURN D(self, "EXCEPTION Thread.Alerted");
      END;
    END;

    RETURN NIL;
  END Apply;

PROCEDURE D(self : T; s : TEXT; l : CARDINAL := 2) : TEXT = 
  BEGIN
    IF Debug >= l THEN
      IO.Put(self.name & ": " & s  & "\n", Stdio.stderr);
    END;
    RETURN s;
  END D;

BEGIN
END ConnectRdWr.
