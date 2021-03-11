(* $Id$ *)

MODULE ReadLine;
IMPORT TCP, IP, Wr, Fmt, ConnRW, Pathname, OSError, TextWr;
IMPORT Thread, Rd, Debug, Wx, Text;
IMPORT ProcUtils;
IMPORT AtomList, Atom;
IMPORT RefSeq, FileWr;
IMPORT TextRefTbl;
IMPORT NetObj;
IMPORT ReadLineError AS Error;
IMPORT ReadLineTable;
IMPORT Stdio;
IMPORT RTParams;
IMPORT Env;

<* FATAL Thread.Alerted *>

REVEAL
  Default = Std BRANDED Brand OBJECT 
    port : IP.Port;
    conn : TCP.Connector;
    completion : ProcUtils.Completion;
    wr : Wr.T;
  OVERRIDES
    quit       := Quit;
    init       := Init;
    startProc  := StartProc;
    sendCmd    := SendCmd;
    sendCmdArg := SendCmdArg;
    readALine  := ReadALine;
  END;

  NonReadLine = Std BRANDED Brand & " (NonReadLine)" OBJECT
  OVERRIDES
    sendCmd := SendCmdNRL;
    sendCmdArg := SendCmdArgNRL;
    quit := DummyQuit;
    startProc := DummyStartProc;
    readALine := ReadALineNRL;
  END;

TYPE
  Std = U OBJECT
    (*
       mu and sMu serve different purposes:
       mu   -- keep normal user calls ordered
       sMu  -- keep sends ordered

       The distinction is necessary because of asyncDisplay 
    *)
    mu : MUTEX;
    sMu : MUTEX;
    closed := FALSE;
    rd : Rd.T; 
    manualStart : BOOLEAN;
    prompt : TEXT;

    logWr : Wr.T := NIL;
    logAsync : BOOLEAN;
    tblMode : TblMode;

    vars : TextRefTbl.T;

    (* the following three implement the "source" command 
       lines is the buffer of lines read so far from the reader.
       sourcing is set to Sourcing.Reading from right before the first line 
       appears in the 
       buffer until just after the last line appears in the buffer.
       This permits a test, LOCKed by mu, of the condition

         sourcing = Sourcing.Reading OR lines.size() > 0 

       to determine whether or not another line of data may appear in 
       the buffer.
    *)
    lines : RefSeq.T;
    sourcing := Sourcing.Idle;
    sc : Thread.Condition;
  METHODS
    sendCmd(c : CHAR) RAISES { Error.E };
    sendCmdArg(c : CHAR; t : TEXT) RAISES { Error.E };
    readALine() : TEXT RAISES { Rd.EndOfFile, Error.E };
  OVERRIDES
    init := InitStd;
    readLine   := ReadLine;
    setPrompt := SetPrompt;
    display := Display;
    displayTbl := DisplayTbl;
    asyncDisplay := AsyncDisplay;
    getPrompt := GetPrompt;

    startLogging := StartLogging;
    stopLogging := StopLogging;

    setVar := SetVar;
    getVar := GetVar;

    source := Source;
  END;

PROCEDURE StartLogging(t : Std; 
                       pn : Pathname.T; 
                       logAsync : BOOLEAN;
                       tblMode : TblMode) RAISES { OSError.E } =
  BEGIN 
    t.logWr := FileWr.OpenAppend(pn); 
    t.logAsync := logAsync;
    t.tblMode := tblMode
  END StartLogging;

PROCEDURE StopLogging(t : Std) RAISES { Error.E } =
  BEGIN 
    TRY
      Wr.Close(t.logWr); t.logWr := NIL 
    EXCEPT
      Wr.Failure(err) => RAISE Error.E(err)
    END
  END StopLogging;

PROCEDURE SendCmd(t : Default; c : CHAR) RAISES { Error.E } = 
  BEGIN 
    TRY 
      Wr.PutChar(t.wr, c); Wr.PutChar(t.wr, Null); Wr.Flush(t.wr) 
    EXCEPT Wr.Failure(err) => RAISE Error.E(err) END 
  END SendCmd;

PROCEDURE SendCmdArg(t : Default; c : CHAR; arg : TEXT) RAISES { Error.E } = 
  BEGIN 
    FOR i := 0 TO Text.Length(arg)-1 DO
      <* ASSERT Text.GetChar(arg,i) # Null *>
    END;

    TRY
      Wr.PutChar(t.wr, c); 
      Wr.PutText(t.wr, arg);
      Wr.PutChar(t.wr, Null);
      Wr.Flush(t.wr) 
    EXCEPT
      Wr.Failure(err) => RAISE Error.E(err)
    END
  END SendCmdArg;

PROCEDURE ReadALine(t : Default) : TEXT RAISES { Rd.EndOfFile, Error.E } =
  VAR wx := Wx.New();
  BEGIN
    LOCK t.mu DO
      LOCK t.sMu DO t.sendCmd('N') END;
      LOOP
        TRY
          WITH c = Rd.GetChar(t.rd) DO
            IF c = Null THEN EXIT ELSE Wx.PutChar(wx,c) END
          END
        EXCEPT
          Rd.Failure(err) => RAISE Error.E(err)
        END
      END;
      RETURN Wx.ToText(wx)
    END
  END ReadALine;

PROCEDURE Init(t : Default; startGetter : BOOLEAN) : T 
  RAISES { IP.Error, NetObj.Error } =
  BEGIN
    IF RTParams.Value("noreadline") # NIL THEN
      Debug.Out("ReadLine.Init: RTParam noreadline defined, readline support disabled.");
      RETURN NEW(NonReadLine).init(startGetter)
      
    ELSIF RTParams.Value("readline") = NIL AND 
          Env.Get("NOM3READLINE") # NIL THEN
      Debug.Out("ReadLine.Init: NOM3READLINE environment var defined, readline support disabled.");
      RETURN NEW(NonReadLine).init(startGetter)
    END;

    EVAL Std.init(t,startGetter);
    WITH ep = IP.Endpoint { IP.NullAddress, IP.NullPort },
         conn = TCP.NewConnector(ep) DO
      t.port := TCP.GetEndPoint(conn).port;
      Debug.Out("PORT " & Fmt.Int(t.port));
      t.conn := conn
    END;
    RETURN t
  END Init;

PROCEDURE StartProc(t : Default; doDebug : BOOLEAN) RAISES { Error.E } =
  VAR debug := "";
  BEGIN
    IF doDebug THEN debug := " -d" END;

    IF NOT t.manualStart THEN
      t.completion := ProcUtils.RunText(fePath & " " &Fmt.Int(t.port) & debug, 
                                        stdout := ProcUtils.Stdout(), 
                                        stdin := ProcUtils.Stdin(), 
                                        stderr := ProcUtils.Stderr())
    END;

    TRY
      WITH tcp = TCP.Accept(t.conn),
           wr = ConnRW.NewWr(tcp),
           rd = ConnRW.NewRd(tcp) DO
        t.wr := wr;
        t.rd := rd;
        t.sendCmd('S')
      END
    EXCEPT
      IP.Error(err) => RAISE Error.E(err)
    END
  END StartProc;

PROCEDURE Quit(t : Default) RAISES { Error.E } =
  BEGIN 
    TRY
      LOCK t.mu DO 
        LOCK t.sMu DO 
          t.sendCmd('Q'); Wr.Close(t.wr); Rd.Close(t.rd); t.closed := TRUE
        END;
        TRY
          t.completion.wait()
        EXCEPT
          ProcUtils.ErrorExit => RAISE Error.E(AtomList.List1(Atom.FromText("ProcUtils")))
        END
      END 
    EXCEPT
      Wr.Failure(err) => RAISE Error.E(err)
    |
      Rd.Failure(err) => RAISE Error.E(err)
    END
  END Quit;

PROCEDURE InitStd(t : Std; startGetter : BOOLEAN) : T =
  BEGIN
    t.manualStart := NOT startGetter;
    t.mu := NEW(MUTEX);
    t.sc := NEW(Thread.Condition);
    t.sMu := NEW(MUTEX);
    t.vars := NEW(TextRefTbl.Default).init();
    t.lines := NEW(RefSeq.T).init();
    RETURN t
  END InitStd;

(**********************************************************************)

PROCEDURE DummyQuit(<*UNUSED*>t : Std) = BEGIN END DummyQuit;

PROCEDURE DummyStartProc(<*UNUSED*>p : Public; 
                         <*UNUSED*>doDebug : BOOLEAN) =
  BEGIN END DummyStartProc;

PROCEDURE ReadALineNRL(t : NonReadLine) : TEXT 
  RAISES { Rd.EndOfFile, Error.E } =
  BEGIN 
    TRY
      Wr.PutText(Stdio.stdout, t.prompt); Wr.Flush(Stdio.stdout);
      RETURN Rd.GetLine(Stdio.stdin) 
    EXCEPT
      Rd.Failure(x) => RAISE Error.E(x)
    |
      Wr.Failure(x) => RAISE Error.E(x)
    END
  END ReadALineNRL;

PROCEDURE SendCmdNRL(<*UNUSED*>t : NonReadLine; c : CHAR) =
  BEGIN
    CASE c OF
      'S', 'I' => (* interrupt stuff not needed here *)
    |
      'N' => (* dont need this either *)
    |
      'E' => (* no longer used *)
    |
      'Q' => <*ASSERT FALSE*> (* would be a type mismatch *)
    ELSE
      <*ASSERT FALSE*> (* unknown *)
    END
  END SendCmdNRL;

PROCEDURE SendCmdArgNRL(<*UNUSED*>t : NonReadLine; c : CHAR; arg : TEXT) 
  RAISES { Error.E } =
  BEGIN
    TRY
      CASE c OF 
        'A', 'D' => Wr.PutText(Stdio.stdout, arg); Wr.Flush(Stdio.stdout)
      |
        'P' => (* skip, we have it in t.prompt *)
      ELSE
        <*ASSERT FALSE*>
      END
    EXCEPT
      Wr.Failure(x) => RAISE Error.E(x)
    END
  END SendCmdArgNRL;

(**********************************************************************)
(* user methods follow *)

PROCEDURE DisplayTbl(t : Std; what : Table; logFormat : LogFormat) 
  RAISES { Error.E, Rd.EndOfFile, NetObj.Error } = 
  VAR
    wr, logWr := NEW(TextWr.T).init();
    txt, logTxt : TEXT;
  BEGIN
    <* FATAL Wr.Failure *>
    BEGIN
      what.put(wr, LogFormat.Normal);
      what.put(logWr, logFormat)
    END;

    txt := TextWr.ToText(wr);
    logTxt := TextWr.ToText(logWr);

    LOCK t.mu DO 
      LOCK t.sMu DO
        t.sendCmdArg('D', txt);
        IF t.logWr # NIL THEN
          TRY

            IF t.tblMode = TblMode.SameStream THEN
              Wr.PutText(t.logWr, logTxt)
            ELSE
              WITH op = t.getPrompt() DO
                t.setPrompt("Log table to file:");
                TRY
                  WITH line = t.readLine(),
                       wr = FileWr.Open(line) DO
                    Wr.PutText(wr,logTxt);
                    Wr.Close(wr)
                  END
                FINALLY
                  t.setPrompt(op)
                END
              END
            END
          EXCEPT
            Wr.Failure(err) => RAISE Error.E(err)
          |
            OSError.E(err) => RAISE Error.E(err)
          END
        END
      END
    END 
  END DisplayTbl;

TYPE 
  SourceClosure = Thread.Closure OBJECT
    t : Std;
    rd : Rd.T;
  OVERRIDES 
    apply := SourceApply;
  END;

TYPE Sourcing = { Idle, Reading, Done };

PROCEDURE SourceApply(cl : SourceClosure) : REFANY =
  BEGIN
    LOCK cl.t.mu DO 
      cl.t.sourcing := Sourcing.Reading; 
      Thread.Broadcast(cl.t.sc)
    END;

    TRY
      LOOP
        WITH line = Rd.GetLine(cl.rd) DO
          LOCK cl.t.mu DO 
            cl.t.lines.addhi(line);
            Thread.Broadcast(cl.t.sc)
          END
        END
      END
    EXCEPT
      Rd.Failure(err) => 
      (* in case of an error we just add the AtomList and allow readLine
         to sort things out later *)
      cl.t.lines.addhi(err); RETURN NIL
    |
      Rd.EndOfFile => 
        LOCK cl.t.mu DO cl.t.sourcing := Sourcing.Done END; 
        Thread.Broadcast(cl.t.sc);
        RETURN NIL
    END
  END SourceApply;

PROCEDURE Source(t : Std; rd : Rd.T) =
  BEGIN
    EVAL Thread.Fork(NEW(SourceClosure, t := t, rd := rd));
    
    (* wait for the sourcing thread to start *)
    LOCK t.mu DO
      WHILE t.sourcing = Sourcing.Idle DO Thread.Wait(t.mu,t.sc) END
    END
  END Source;

PROCEDURE ReadLine(t : Std) : TEXT RAISES { Rd.EndOfFile, Error.E } =
  BEGIN
    LOCK t.mu DO
      WHILE t.lines.size() > 0 OR t.sourcing = Sourcing.Reading DO
        IF t.lines.size() > 0 THEN
          WITH r = t.lines.remlo() DO
            TYPECASE r OF
              TEXT => RETURN r
            |
              AtomList.T(err) => RAISE Error.E(err)
            ELSE
              <* ASSERT FALSE *>
            END
          END
        END;
        Thread.Wait(t.mu,t.sc)
      END
    END;

    RETURN t.readALine()
  END ReadLine;

PROCEDURE SetPrompt(t : Std; newPrompt : TEXT) RAISES { Error.E }  =
  BEGIN 
    LOCK t.mu DO 
      t.prompt := newPrompt;
      LOCK t.sMu DO
        t.sendCmdArg('P', newPrompt) 
      END
    END 
  END SetPrompt;

PROCEDURE GetPrompt(t : Std) : TEXT = 
  BEGIN 
    LOCK t.mu DO RETURN t.prompt END
  END GetPrompt;

PROCEDURE Display(t : Std; what : TEXT) RAISES { Error.E } = 
  BEGIN 
    LOCK t.mu DO 
      LOCK t.sMu DO
        t.sendCmdArg('D', what);
        IF t.logWr # NIL THEN
          TRY
            Wr.PutText(t.logWr, what)
          EXCEPT
            Wr.Failure(err) => RAISE Error.E(err)
          END
        END
      END
    END 
  END Display;

PROCEDURE AsyncDisplay(t : Std; what : TEXT) RAISES { Error.E } = 
  BEGIN 
    LOCK t.sMu DO
      t.sendCmdArg('A', what);
      IF t.logWr # NIL AND t.logAsync THEN
        TRY
          Wr.PutText(t.logWr, what)
        EXCEPT
          Wr.Failure(err) => RAISE Error.E(err)
        END
      END
    END
  END AsyncDisplay;

(**********************************************************************)

REVEAL
  Table = ReadLineTable.Public BRANDED Brand & " Table" OBJECT
    cols : CARDINAL;
    fmt : TEXT;
    headings : REF ARRAY OF TEXT;
    data : RefSeq.T;
  METHODS
    put(to : Wr.T; format : LogFormat) RAISES { Wr.Failure } := PutTable;
  OVERRIDES
    init := InitTable;
    addRow := AddTableRow;
    addHline := AddTableLine;
  END;

PROCEDURE InitTable(tbl : Table; cols : CARDINAL; fmt : TEXT; 
                    READONLY headings : ARRAY OF TEXT) : Table =
  BEGIN
    tbl.cols := cols;
    IF NUMBER(headings) = 0 THEN 
      tbl.headings := NIL
    ELSE
      tbl.headings := NEW(REF ARRAY OF TEXT, cols);
      tbl.headings^ := headings
    END;

    tbl.data := NEW(RefSeq.T).init();
    
    tbl.fmt := fmt; (* if it's NIL, the columns are auto-adjusting *)
    RETURN tbl
  END InitTable;

TYPE RAT = REF ARRAY OF TEXT;

PROCEDURE AddTableRow(tbl : Table; READONLY cols : ARRAY OF TEXT) =
  BEGIN
    WITH new = NEW(RAT, tbl.cols) DO
      new^ := cols;
      tbl.data.addhi(new)
    END
  END AddTableRow;

TYPE Line = OBJECT of : CHAR END;

PROCEDURE AddTableLine(tbl : Table; of : CHAR) =
  BEGIN
    tbl.data.addhi(NEW(Line, of := of))
  END AddTableLine;

PROCEDURE PutTable(tbl : Table; to : Wr.T; format : LogFormat) 
  RAISES { Wr.Failure } =

  PROCEDURE PutCSVRow(row : REF ARRAY OF TEXT) RAISES { Wr.Failure } =
    BEGIN
      FOR i := FIRST(row^) TO LAST(row^) DO
        Wr.PutText(to, row[i]);
        IF i # LAST(row^) THEN Wr.PutChar(to, ',') END
      END
    END PutCSVRow;

  PROCEDURE MeasureRowWidth(READONLY of : ARRAY OF TEXT; 
                            VAR into : ARRAY OF CARDINAL) =
    BEGIN
      FOR i := FIRST(of) TO LAST(of) DO
        into[i] := Text.Length(of[i])
      END
    END MeasureRowWidth;

  PROCEDURE MakeDashes(READONLY matching : ARRAY OF TEXT) : REF ARRAY OF TEXT=
    BEGIN
      WITH res = NEW(REF ARRAY OF TEXT, NUMBER(matching)) DO
        FOR i := FIRST(matching) TO LAST(matching) DO
          VAR l := Text.Length(matching[i]); d := ""; BEGIN
            FOR j := 0 TO l-1 DO
              d := d & "-" 
            END;
            res[i] := d
          END
        END;
        RETURN res
      END
    END MakeDashes;
    
  PROCEDURE MeasureWidths() : REF ARRAY OF CARDINAL =

    PROCEDURE DoMax() =
      BEGIN
        FOR i := FIRST(res^) TO LAST(res^) DO
          res[i] := MAX(res[i],one[i])
        END
      END DoMax;

    VAR
      one, res := NEW(REF ARRAY OF CARDINAL, tbl.cols);
    BEGIN
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := 0
      END;
      
      IF tbl.headings # NIL THEN MeasureRowWidth(tbl.headings^,one^) END;
      DoMax();

      FOR i := 0 TO tbl.data.size()-1 DO
        WITH r = tbl.data.get(i) DO
          IF ISTYPE(r,RAT) THEN
            MeasureRowWidth(NARROW(r,RAT)^,one^)
          END
        END;
        DoMax()
      END;

      RETURN res
    END MeasureWidths;

  VAR fmt : TEXT; BEGIN
    CASE format OF
      LogFormat.Normal =>
      fmt := tbl.fmt;
      WITH widths = MeasureWidths() DO
        IF fmt = NIL THEN
          (* make fmt *)
          fmt := "";
          FOR i := FIRST(widths^) TO LAST(widths^) DO
            fmt := fmt & "%" & Fmt.Int(widths[i]) & "s  "
          END;
          fmt := fmt & "\n"
        END;
        
        IF tbl.headings # NIL THEN
          Wr.PutText(to, Fmt.FN(fmt, tbl.headings^));
          Wr.PutText(to, Fmt.FN(fmt, MakeDashes(tbl.headings^)^))
        END;
        
        FOR i := 0 TO tbl.data.size()-1 DO
          WITH r = tbl.data.get(i) DO
            TYPECASE r OF
              RAT(rr) =>
              Wr.PutText(to, Fmt.FN(fmt, rr^))
            |
              Line(ll) =>
              VAR w := 0; BEGIN
                FOR i := FIRST(widths^) TO LAST(widths^) DO
                  INC(w,widths[i]+2)
                END;
                FOR i := 1 TO w DO
                  Wr.PutChar(to, ll.of)
                END;
                Wr.PutChar(to,'\n')
              END
            ELSE
              <*ASSERT FALSE*>
            END
          END
        END
      END
    |
      LogFormat.CSV =>
      IF tbl.headings # NIL THEN
        PutCSVRow(tbl.headings)
      END;
      FOR i := 0 TO tbl.data.size()-1 DO
        PutCSVRow(tbl.data.get(i))
      END
    END
  END PutTable;

PROCEDURE SetVar(t : Std; name : TEXT; val : REFANY) =
  BEGIN LOCK t.mu DO EVAL t.vars.put(name,val) END END SetVar;

PROCEDURE GetVar(t : Std; name : TEXT) : REFANY =
  VAR res : REFANY; BEGIN
    LOCK t.mu DO
      IF t.vars.get(name,res) THEN
        RETURN res
      ELSE
        RETURN NIL
      END
    END
  END GetVar;

VAR
  fePath : Pathname.T := "readlinefe";
BEGIN 
  WITH np = Env.Get("READLINEFE") DO
    IF np # NIL THEN fePath := np END
  END
END ReadLine.
