(* Copyright 1993 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjTest.m3 *)
(* Last modified on Tue Aug 24 17:01:51 PDT 1993 by wobber *)

UNSAFE MODULE Echo EXPORTS Main;

IMPORT EchoRW, TCPNetObj, IP, RdWrPipe, Time, Process, Pickle, 
       NetObj, Rd, Wr, IO, Params, Fmt, Thread, Pipe, FileRd, FileWr,
       Quaternion, Matrix4, Point3, Scan, OSError, FloatMode, Lex,
       SimpleMsgRW, MsgRd, MsgWr, Text;

CONST EchoObjName = "EchoObj";
      MyIPPort    = 7777;

(* invocation:

     echo <no args>
     echo hostname

        The former is a server, which forks a thread to
        echo all input on the argument reader to the argument
        writer with flushes any newlines.

        The latter passes stdin and stdout to the server,
        and waits until stdout is closed.
*)

(* server side code *)

TYPE
  (* The TrackerPosition RECORD is the position and orientation of the
     tracker *)
  TrackerPosition =
    RECORD
      time  : Time.T;			 (* the time of the report *)
      (* information bits *)
      fringe: BOOLEAN := FALSE;  (* is it in the fringe area *)
      out   : BOOLEAN := FALSE;  (* is it out of range *)

      (* the position of the tracker *)
      position  : Point3.T;		 (* position *)
      rotation_q: Quaternion.T;          (* orientation, in quaternions*)
      rotation_m : Matrix4.T;		 (* orientation matrix *)

      (* buttons *)
      suspend: BOOLEAN := FALSE; (* suspend button *)
      left   : BOOLEAN := FALSE; (* left button *)
      middle : BOOLEAN := FALSE; (* middle button *)
      right  : BOOLEAN := FALSE; (* right button *)
    END;

TYPE
  T = EchoRW.T OBJECT OVERRIDES
    echo := DoEcho;
    msgEcho := DoMsgEcho;
  END;

  Copier = Thread.Closure OBJECT
    rd: Rd.T;
    wr: Wr.T;
  OVERRIDES
    apply := DoCopy;
  END;

  MsgCopier = Thread.Closure OBJECT
    rd: MsgRd.T;
    wr: MsgWr.T;
  OVERRIDES
    apply := DoMsgCopy;
  END;

  <* FATAL NetObj.Error *>
  <* FATAL NetObj.Invalid *>
  <* FATAL Thread.Alerted *>
  <* FATAL Wr.Failure *>
  <* FATAL Rd.Failure *>
  <* FATAL IP.Error *>
  <* FATAL TCPNetObj.Failed *>
  <* FATAL Pickle.Error *>
  <* FATAL Rd.EndOfFile *>
  <* FATAL OSError.E *>
  <* FATAL FloatMode.Trap, Lex.Error *>

TYPE
  StupidT = REF RECORD
    t: Time.T;
  END;

  StupidTSpecial =  Pickle.Special OBJECT
  OVERRIDES
    write := WriteT;
    read := ReadT
  END;

PROCEDURE WriteT(
    <*UNUSED*> self: StupidTSpecial; 
    ref: REFANY; 
    wr: Pickle.Writer)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR 
    t: StupidT := ref; 
  BEGIN
    Wr.PutString(wr.wr, LOOPHOLE(t.t, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR));
  END WriteT;

PROCEDURE ReadT(
    <*UNUSED*> self: StupidTSpecial;
    rd: Pickle.Reader; 
    id: Pickle.RefID): REFANY
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR res := NEW(StupidT); 
  BEGIN
    IF Rd.GetSub(rd.rd, LOOPHOLE(res.t, 
                              ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR)) #
                              BYTESIZE(LONGREAL) THEN
      RAISE Pickle.Error("Rd terminated early");
    END;
    rd.noteRef(res, id);
    RETURN res;
  END ReadT;

PROCEDURE DoEcho (<*UNUSED*> t: T; rd: Rd.T; wr: Wr.T) =
  BEGIN
    EVAL Thread.Fork(NEW(Copier, rd := rd, wr := wr));
  END DoEcho;

PROCEDURE DoCopy(c: Copier): REFANY =
  BEGIN
    TRY
      LOOP
        Wr.PutText(c.wr, Rd.GetLine(c.rd) & "\n");
        Wr.Flush(c.wr);
      END;
    EXCEPT
    | Rd.Failure, Rd.EndOfFile, Wr.Failure, Thread.Alerted =>
    END;
    RETURN NIL;
  END DoCopy;

PROCEDURE DoMsgEcho (<*UNUSED*> t: T; rd: Rd.T; wr: Wr.T) =
  BEGIN
    EVAL Thread.Fork(NEW(MsgCopier, rd := SimpleMsgRW.NewRd(rd), 
                         wr := SimpleMsgRW.NewWr(wr)));
  END DoMsgEcho;

PROCEDURE DoMsgCopy(c: MsgCopier): REFANY =
  VAR 
    nb: CARDINAL := 0;
    i: CARDINAL := 0;
    buf: ARRAY [0..511] OF CHAR;
  BEGIN
    TRY
      EVAL c.rd.nextMsg();
      LOOP
        nb := 0;
        WHILE NOT Rd.EOF(c.rd) DO
          i := Rd.GetSub(c.rd, buf);
          Wr.PutString(c.wr, SUBARRAY(buf, 0, i));
          nb := nb + i;
        END;
        c.wr.nextMsg();
        IF NOT c.rd.nextMsg() THEN
          EXIT;
        END;
      END;
    EXCEPT
    | Rd.Failure, Wr.Failure, Thread.Alerted =>
    END;
    RETURN NIL;
  END DoMsgCopy;

(* common code *)
PROCEDURE MsgEchoTest(rd: MsgRd.T; wr: MsgWr.T; count: CARDINAL) =
  VAR
    out := ARRAY [0 .. 7] OF CHAR{'D', 'E', 'A', 'D', 'B', 'E', 'E',
                                  'F'};  
    filler := ARRAY [0 .. 7] OF CHAR{'d', 'e', 'a', 'd', 'b', 'e', 'e',
                                  'f'};  
    result := ARRAY [0 .. 15] OF CHAR{'D', 'E', 'A', 'D', 'B', 'E', 'E',
                                  'F', 'd', 'e', 'a', 'd', 'b', 'e',
                                  'e', 'f'};
    in: ARRAY [0 .. 799] OF CHAR;
    start, end, elapsed: Time.T;
    len: CARDINAL;
  BEGIN
    elapsed := 0.0D0;
    FOR i := 1 TO count DIV 100 DO
      start := Time.Now();
      FOR j := 0 TO 99 DO
        Wr.PutString(wr, out);
        wr.nextMsg();
        IF NOT rd.nextMsg() THEN
          Process.Crash("Rd.nextMsg(in) should be true!\n");
        END;
        IF j MOD 2 = 0 THEN
          len := Rd.GetSub(rd, SUBARRAY(in, j*8, 8));
          IF len # NUMBER(out) THEN
            Process.Crash("Rd.GetSub(in) returned too few characters ("
            & Fmt.Int(len) & ")\n");
          END;
          IF NOT Rd.EOF(rd) THEN
            Process.Crash("Rd.EOF(in) should be true!\n");
          END;
        ELSE
          SUBARRAY(in, j*8, 8) := filler;
        END;
      END;
      end := Time.Now();
      FOR j := FIRST(out) TO LAST(out) DO
        IF result[j MOD 16] # in[j] THEN
          Process.Crash("result["& Fmt.Int(j MOD 16)&"] = '" &
            Fmt.Char(result[j MOD 16]) & "', in["& Fmt.Int(j)&"] = '" &
            Fmt.Char(in[j]) & "'");
        END;
      END;
      IO.Put(Fmt.LongReal((end - start) / FLOAT(100, Time.T), prec := 3)& " ");
      elapsed := elapsed + (end - start);
    END;
    IO.Put("Average round trip time for array = " & 
      Fmt.LongReal(elapsed  / FLOAT(count, Time.T), prec := 3) &
      " (" & Fmt.Int(count) & " messages in " & Fmt.LongReal(elapsed) & 
      " seconds)\n");
  END MsgEchoTest;

(* common code *)
PROCEDURE EchoTest(rd: Rd.T; wr: Wr.T; count: CARDINAL) =
  VAR
    out := ARRAY [0 .. 8] OF CHAR{'D', 'E', 'A', 'D', 'B', 'E', 'E',
                                  'F', '\n'};  
    in: ARRAY [0 .. 899] OF CHAR;
    start, end, elapsed: Time.T;
    startref := NEW(REF Time.T);
    stupidref := NEW(StupidT);
    pwr := NEW(Pickle.Writer, wr := wr);
    prd := NEW(Pickle.Reader, rd := rd);
    tracker := NEW(REF TrackerPosition);
  BEGIN
    elapsed := 0.0D0;
    FOR i := 1 TO count DIV 100 DO
      start := Time.Now();
      FOR j := 0 TO 99 DO
        Wr.PutString(wr, out);
        Wr.Flush(wr);
        IF Rd.GetSub(rd, SUBARRAY(in, j*9, 9)) # NUMBER(out) THEN
          Process.Crash("Rd.GetSub(in) returned too few characters\n");
        END;
      END;
      end := Time.Now();
      FOR j := FIRST(out) TO LAST(out) DO
        IF out[j MOD 9] # in[j] THEN
          Process.Crash("out["& Fmt.Int(j)&"] = '" &
            Fmt.Char(out[j]) & "', in["& Fmt.Int(j MOD 9)&"] = '" &
            Fmt.Char(in[j MOD 9]) & "'");
        END;
      END;
      IO.Put(Fmt.LongReal((end - start) / FLOAT(100, Time.T), prec := 3)& " ");
      elapsed := elapsed + (end - start);
    END;
    IO.Put("Average round trip time for array = " & 
      Fmt.LongReal(elapsed  / FLOAT(count, Time.T), prec := 3) &
      " (" & Fmt.Int(count) & " messages in " & Fmt.LongReal(elapsed) & 
      " seconds)\n");

    elapsed := 0.0D0;
    FOR i := 1 TO count DIV 100 DO
      start := Time.Now();
      FOR j := 0 TO 99 DO
        startref^ := start;
        pwr.write(startref);
        Wr.PutString(wr, out);
        Wr.Flush(wr);
        TYPECASE prd.read() OF
        | REF Time.T(t) => 
          IF t^ # start THEN
            Process.Crash("Pickle.Read() returned wrong Time value\n");
          END;
        ELSE
          Process.Crash("Pickle.Read() returned wrong reference type!\n");
        END;
        IF Rd.GetSub(rd, SUBARRAY(in, j*9, 9)) # NUMBER(out) THEN
          Process.Crash("Rd.GetSub(in) returned too few characters\n");
        END;
      END;
      end := Time.Now();
      FOR j := FIRST(out) TO LAST(out) DO
        IF out[j MOD 9] # in[j] THEN
          Process.Crash("out["& Fmt.Int(j)&"] = '" &
            Fmt.Char(out[j]) & "', in["& Fmt.Int(j MOD 9)&"] = '" &
            Fmt.Char(in[j MOD 9]) & "'");
        END;
      END;
      IO.Put(Fmt.LongReal((end - start) / FLOAT(100, Time.T), prec := 3)& " ");
      elapsed := elapsed + (end - start);
    END;
    IO.Put("Average round trip time for array and REF Time.T = " & 
      Fmt.LongReal(elapsed  / FLOAT(count, Time.T), prec := 3) &
      " (" & Fmt.Int(count) & " messages in " & Fmt.LongReal(elapsed) & 
      " seconds)\n");

    elapsed := 0.0D0;
    FOR i := 1 TO count DIV 100 DO
      start := Time.Now();
      FOR j := 0 TO 99 DO
        stupidref.t := start;
        pwr.write(stupidref);
        Wr.PutString(wr, out);
        Wr.Flush(wr);
        TYPECASE prd.read() OF
        | StupidT(st) => 
          IF st.t # start THEN
            Process.Crash("Pickle.Read() returned wrong Time value\n");
          END;
        ELSE
          Process.Crash("Pickle.Read() returned wrong reference type!\n");
        END;
        IF Rd.GetSub(rd, SUBARRAY(in, j*9, 9)) # NUMBER(out) THEN
          Process.Crash("Rd.GetSub(in) returned too few characters\n");
        END;
      END;
      end := Time.Now();
      FOR j := FIRST(out) TO LAST(out) DO
        IF out[j MOD 9] # in[j] THEN
          Process.Crash("out["& Fmt.Int(j)&"] = '" &
            Fmt.Char(out[j]) & "', in["& Fmt.Int(j MOD 9)&"] = '" &
            Fmt.Char(in[j MOD 9]) & "'");
        END;
      END;
      IO.Put(Fmt.LongReal((end - start) / FLOAT(100, Time.T), prec := 3)& " ");
      elapsed := elapsed + (end - start);
    END;
    IO.Put("Average round trip time for array and my Pickle = " & 
      Fmt.LongReal(elapsed  / FLOAT(count, Time.T), prec := 3) &
      " (" & Fmt.Int(count) & " messages in " & Fmt.LongReal(elapsed) & 
      " seconds)\n");

    elapsed := 0.0D0;
    FOR i := 1 TO count DIV 100 DO
      start := Time.Now();
      FOR j := 0 TO 99 DO
        tracker.time := start;
        pwr.write(tracker);
        Wr.PutString(wr, out);
        Wr.Flush(wr);
        TYPECASE prd.read() OF
        | REF TrackerPosition(t) => 
          IF t.time # start THEN
            Process.Crash("Pickle.Read() returned wrong Time value\n");
          END;
        ELSE
          Process.Crash("Pickle.Read() returned wrong reference type!\n");
        END;
        IF Rd.GetSub(rd, SUBARRAY(in, j*9, 9)) # NUMBER(out) THEN
          Process.Crash("Rd.GetSub(in) returned too few characters\n");
        END;
      END;
      end := Time.Now();
      FOR j := FIRST(out) TO LAST(out) DO
        IF out[j MOD 9] # in[j] THEN
          Process.Crash("out["& Fmt.Int(j)&"] = '" &
            Fmt.Char(out[j]) & "', in["& Fmt.Int(j MOD 9)&"] = '" &
            Fmt.Char(in[j MOD 9]) & "'");
        END;
      END;
      IO.Put(Fmt.LongReal((end - start) / FLOAT(100, Time.T), prec := 3)& " ");
      elapsed := elapsed + (end - start);
    END;
    IO.Put("Average round trip time for array and Tracker Pickle = " & 
      Fmt.LongReal(elapsed  / FLOAT(count, Time.T), prec := 3) &
      " (" & Fmt.Int(count) & " messages in " & Fmt.LongReal(elapsed) & 
      " seconds)\n");
  END EchoTest;

BEGIN
  Pickle.RegisterSpecial(NEW(StupidTSpecial, sc := TYPECODE(StupidT)));
  
  IF Params.Count <= 1 THEN
    TRY
      NetObj.Export (EchoObjName, NEW(T), NIL);
      IO.Put("Exported target via netobjd\n");
    EXCEPT
    | NetObj.Error =>
        NetObj.Export (EchoObjName, NEW(T), TCPNetObj.Listen(MyIPPort));
        IO.Put("Exported target at private port\n");
    END;
    IO.Put("Echo server running, type <CR> to terminate\n");
    TRY EVAL IO.GetLine() EXCEPT IO.Error => END;
  ELSE
    VAR prog := Params.Get(0);
        host := Params.Get(1);
        addr: IP.Address;
        t: EchoRW.T;
        agent: NetObj.Address;
        count: CARDINAL := 100;
    BEGIN
      IF NOT IP.GetHostByName(host, addr) THEN
        IO.Put(Fmt.F("No such host \"%s\"\n", host));
      ELSE
        agent := NetObj.Locate(host);
        TRY
          t := NetObj.Import(EchoObjName, agent);
          IO.Put("Located target via netobjd\n");
        EXCEPT
        | NetObj.Error =>
            agent := TCPNetObj.Locate(IP.Endpoint{addr, MyIPPort});
            t := NetObj.Import(EchoObjName, agent);
            IO.Put("Located target at private port\n");
        END;
        VAR
          rd_out, rd_in: Rd.T;
          wr_out, wr_in: Wr.T;
        BEGIN
          IF Params.Count > 2 THEN
            count := Scan.Int(Params.Get(2));
          END;
          (* Test my pipes *)
          RdWrPipe.New(rd_out, wr_out, nm := "out");
          RdWrPipe.New(rd_in, wr_in, nm := "in");
          IF Text.Equal(prog, "msgechotest") THEN
            t.msgEcho(rd_out, wr_in);
            MsgEchoTest(SimpleMsgRW.NewRd(rd_in),
                        SimpleMsgRW.NewWr(wr_out), count); 
          ELSE
            t.echo(rd_out, wr_in);     
            EchoTest(rd_in, wr_out, count);
          END;
          Wr.Close(wr_out);
        END;  
        VAR
          hrChild, hwChild, hrSelf, hwSelf: Pipe.T;
        BEGIN
          (* Test system pipes *)
          Pipe.Open(hr := hrChild, hw := hwSelf);
          Pipe.Open(hr := hrSelf, hw := hwChild);
          IF Text.Equal(prog, "msgechotest") THEN
            t.msgEcho(NEW(FileRd.T).init(hrChild),
                      NEW(FileWr.T).init(hwChild));
            MsgEchoTest(SimpleMsgRW.NewRd(NEW(FileRd.T).init(hrSelf)),
                        SimpleMsgRW.NewWr(NEW(FileWr.T).init(hwSelf)), count);
          ELSE
            t.echo(NEW(FileRd.T).init(hrChild),
                   NEW(FileWr.T).init(hwChild));
            EchoTest(NEW(FileRd.T).init(hrSelf),
                     NEW(FileWr.T).init(hwSelf), count);
          END;
        END;  
      END;
    END;
  END;
END Echo.
