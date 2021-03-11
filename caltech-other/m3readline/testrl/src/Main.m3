(* $Id$ *)

MODULE Main;
IMPORT ReadLine;
IMPORT Fmt;
IMPORT Thread, Rd, Debug, Wx, Text, FileRd, TextReader;
IMPORT Date, XTime AS Time;

CONST TE = Text.Equal;

TYPE 
  Closure = Thread.Closure OBJECT 
    rl : ReadLine.T; 
    quit := FALSE;
  OVERRIDES 
    apply := A 
  END;

VAR ct : Thread.T := NIL;
VAR quitClock := FALSE;

PROCEDURE StartClock(rl : ReadLine.T) =
  BEGIN
    IF ct # NIL THEN RETURN END;
    ct := Thread.Fork(NEW(Closure, rl := rl))
  END StartClock;

PROCEDURE StopClock() = 
  BEGIN 
    quitClock := TRUE;
    EVAL Thread.Join(ct);
    ct := NIL
  END StopClock;

PROCEDURE A(cl : Closure) : REFANY =
  BEGIN
    LOOP
      WITH d = Date.FromTime(Time.Now()) DO
        cl.rl.asyncDisplay(Fmt.F("%02s:%02s:%02s\n",
                                 Fmt.Int(d.hour),
                                 Fmt.Int(d.minute),
                                 Fmt.Int(d.second)));
        IF quitClock THEN quitClock := FALSE; RETURN NIL END
        
      END;
      Thread.Pause(1.0d0);
    END
  END A;

VAR
  cnt := 1;
BEGIN
  WITH t = NEW(ReadLine.Default).init() DO
    t.startProc();
    t.display("Hello there.\n");
    LOOP
      t.setPrompt(Fmt.Int(cnt) & " > ");
      WITH line = t.readLine(),
           reader = NEW(TextReader.T).init(line) DO
        Debug.Out("Got line " & line);
        
        IF Text.Length(line) # 0 THEN
          WITH cmd = reader.nextE(" ") DO
            IF    TE(cmd, "quit") THEN t.quit(); EXIT             
            ELSIF TE(cmd, "clock") THEN StartClock(t)
            ELSIF TE(cmd, "noclock") THEN StopClock()
            ELSIF TE(cmd, "cat") THEN
              WITH fn = reader.nextE(" "),
                   rd = FileRd.Open(fn),
                   wx = Wx.New() DO
                TRY
                  LOOP
                    WITH c = Rd.GetChar(rd) DO
                      IF c # ReadLine.Null THEN Wx.PutChar(wx, c) END
                    END
                  END
                EXCEPT
                  Rd.EndOfFile => Rd.Close(rd)
                END;
                t.display(Wx.ToText(wx))
              END
            END
          END
        END
      END;
      INC(cnt)
    END
  END
END Main.
