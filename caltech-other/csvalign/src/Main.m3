(* $Id$ *)

MODULE Main;

(* 
   align columns of CSV file for terminal display

   Example.

   csvalign -i ../../frac_f/src/GC.csv
*)

IMPORT ParseParams, Stdio;
IMPORT Wr, Text, FileRd, TextSeq, RefSeq;
IMPORT Debug, Rd;
IMPORT Thread;
IMPORT OSError;
IMPORT AL;
IMPORT RdWrPipe;

<*FATAL Thread.Alerted, OSError.E, Rd.Failure*>

VAR
  rd      := Stdio.stdin;
  wr      := Stdio.stdout;
  lines   := NEW(RefSeq.T).init();
  maxflds := 0;
  sep     := " ";

PROCEDURE MaxWidth(s : TextSeq.T; VAR w : ARRAY OF CARDINAL) =
  BEGIN
    FOR i := 0 TO s.size()-1 DO
      w[i] := MAX(w[i], Text.Length(s.get(i)))
    END
  END MaxWidth;

PROCEDURE DumpLine(s : TextSeq.T; READONLY w : ARRAY OF CARDINAL) = 
  BEGIN
    TRY
      FOR i := 0 TO s.size()-1 DO
        WITH t = s.get(i) DO
          Wr.PutText(wr, t);
          FOR i := 0 TO w[i] - Text.Length(t) DO
            Wr.PutChar(wr, ' ')
          END;
          IF i # s.size()-1 THEN
            Wr.PutText(wr, sep)
          END
        END;
      END;
      Wr.PutChar(wr, '\n')
    EXCEPT
      Wr.Failure(x) => Debug.Error("Wr.Failure: " & AL.Format(x))
    END
  END DumpLine;

TYPE
  RdWrClosure = Thread.Closure OBJECT
    rd : Rd.T; 
    wr : Wr.T;
    mapper : PROCEDURE(c : CHAR) : CHAR;
  OVERRIDES
    apply := RWCApply;
  END;

  Failure = BRANDED OBJECT END;

PROCEDURE RWCApply(rwc : RdWrClosure) : REFANY =
  CONST 
    BufSiz = 1024;
  VAR
    buf : ARRAY [0..BufSiz-1] OF CHAR;
    r : CARDINAL;
  BEGIN
    TRY
      LOOP
        r := Rd.GetSub(rwc.rd, buf);
        IF r = 0 THEN 
          Wr.Close(rwc.wr);
          RETURN NIL 
        END;
        FOR i := 0 TO r-1 DO
          buf[i] := rwc.mapper(buf[i])
        END;
        Wr.PutString(rwc.wr, SUBARRAY(buf,0,r))
      END
    EXCEPT
      Rd.Failure, Wr.Failure, Thread.Alerted => RETURN NEW(Failure) 
    END
  END RWCApply;

PROCEDURE MapCRLF(c : CHAR) : CHAR = 
  BEGIN
    IF c = '\r' THEN RETURN '\n' ELSE RETURN c END
  END MapCRLF;

PROCEDURE FilterCR(rd : Rd.T) : Rd.T =
  VAR
    wr  : Wr.T;
    res : Rd.T;
    thr : Thread.T;
  BEGIN
    RdWrPipe.New(res, wr);
    
    thr := Thread.Fork(NEW(RdWrClosure, 
                           rd := rd, 
                           wr := wr, 
                           mapper := MapCRLF));
    RETURN res
  END FilterCR;

BEGIN
  TRY 
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-sep") OR pp.keywordPresent("-separator") THEN
        sep := pp.getNext()
      END;

      IF pp.keywordPresent("-i") THEN
        WITH fn = pp.getNext() DO
          rd := FileRd.Open(fn)
        END
      END;

      pp.skipParsed(); pp.finish()
    END;
  EXCEPT
    ParseParams.Error => Debug.Error("Couldn't parse cmd-line params")
  END;

  rd := FilterCR(rd); (* broken?? *)

  TRY
    LOOP
      CONST 
        Seps = SET OF CHAR { '\r', ',' };

      VAR
        line := Rd.GetLine(rd);
        len := Text.Length(line);
        seq := NEW(TextSeq.T).init();
        p := 0;

      BEGIN
        FOR i := 0 TO len-1 DO
          WITH c = Text.GetChar(line,i) DO
            (* should be careful with quotation marks etc *)
            IF c IN Seps OR i = len-1 THEN
              VAR j : CARDINAL;
              BEGIN
                IF NOT c IN Seps AND i = len-1 THEN
                  j := i + 1 (* include last char *)
                ELSE
                  j := i     (* do not include separator *)
                END;

                WITH w = Text.Sub(line, p, j-p) DO
                  seq.addhi(w)
                END
              END;
              p := i+1
            END
          END
        END;
        maxflds := MAX(maxflds, seq.size());
        lines.addhi(seq)
      END
    END
  EXCEPT
    Rd.EndOfFile =>
    TRY Rd.Close(rd) EXCEPT ELSE END
  END;


  VAR
    w := NEW(REF ARRAY OF CARDINAL, maxflds);
  BEGIN
    FOR i := FIRST(w^) TO LAST(w^) DO
      w[i] := 0
    END;

    FOR i := 0 TO lines.size()-1 DO
      MaxWidth(lines.get(i), w^)
    END;

    FOR i := 0 TO lines.size()-1 DO
      DumpLine(lines.get(i), w^);
    END
  END

END Main.
