(* Copyright 1998, Compaq Computer Corporation               *)
(*                                                           *)
(* Last modified on Fri Dec 11 15:15:29 PST 1998 by heydon   *)

(* Syntax:

|  TestUDP -server
|  TestUDP -client [serverhost]

   The first command runs the program as a server, and the second command
   runs it as a client. The server program should be run first. The client
   program may then be run, specifying the name of the server machine in
   the "serverhost" argument. If "serverhost" is omitted, the local host
   is used.
*)

MODULE TestUDP EXPORTS Main;

IMPORT Atom, AtomList, Fmt, IP, Params, Process, Text, Time, UDP, Word, Wr;
FROM Stdio IMPORT stdout, stderr;

IMPORT Thread;
<* FATAL Thread.Alerted, Wr.Failure *>

CONST
  ServerPort = 20000;
  ClientPort = ServerPort + 1;
  ReceiveTimeout = 10.0d0;
  BuffSz = 4;
  Ack = "ACK";
  AckLen = 3;
  max = 100;

PROCEDURE PrintIPError(arg: AtomList.T) =
  BEGIN
    Wr.PutText(stderr, "IP Error");
    VAR i := 0; BEGIN
      WHILE arg # NIL DO
        IF i = 0
          THEN Wr.PutText(stderr, ": ")
          ELSE Wr.PutText(stderr, ", ")
        END;
        Wr.PutText(stderr, Atom.ToText(arg.head));
        arg := arg.tail;
        INC(i)
      END
    END
  END PrintIPError;

(* GetHostByAddr is obslolete
PROCEDURE HostPort(READONLY end: IP.Endpoint): TEXT RAISES {IP.Error} =
  BEGIN
    RETURN IP.GetCanonicalByAddr(end.addr) & ":" & Fmt.Int(end.port)
  END HostPort;
*)

PROCEDURE HostPort(READONLY end: IP.Endpoint): TEXT RAISES {IP.Error} =
  VAR
    ep := NEW(IP.Endpoint4);
    host,service : TEXT := "";
  BEGIN
    ep.adr := end.addr;
    ep.port := end.port;
    IP.GetNameInfo(ep,host,service);
    RETURN host & ":" & Fmt.Int(ep.port); 
  END HostPort;

PROCEDURE PickleInt(i: INTEGER; VAR (*INOUT*) bytes: ARRAY OF CHAR) =
  BEGIN
    FOR j := 0 TO 3 DO
      bytes[j] := VAL(Word.And(i, 16_ff), CHAR);
      i := Word.RightShift(i, 8)
    END
  END PickleInt;

PROCEDURE UnpickleInt(READONLY bytes: ARRAY OF CHAR): INTEGER =
  VAR res := 0; BEGIN
    FOR i := 3 TO 0 BY -1 DO
      res := Word.LeftShift(res, 8);
      res := Word.Or(res, ORD(bytes[i]))
    END;
    RETURN res
  END UnpickleInt;

PROCEDURE Server() RAISES {IP.Error} =
  VAR conn: UDP.T; d: UDP.Datagram; sent: INTEGER; n := 0; BEGIN
    d.bytes := NEW(REF ARRAY OF CHAR, BuffSz);
    Wr.PutText(stdout, "Starting server...\n");
    Wr.Flush(stdout);
    conn := NEW(UDP.T).init(ServerPort);
    Wr.PutText(stdout, "Server listening on port "&Fmt.Int(ServerPort)&".\n");
    Wr.Flush(stdout);
    TRY
      LOOP
        VAR packetReceived := FALSE; BEGIN
          REPEAT
            TRY
              conn.receive((*INOUT*) d, timeout := ReceiveTimeout);
              packetReceived := TRUE;
            EXCEPT
              UDP.Timeout =>
                Wr.PutText(stdout, "No packet received after " &
                  Fmt.LongReal(ReceiveTimeout) & " seconds; retrying...\n");
                Wr.Flush(stdout);
            END
          UNTIL packetReceived
        END;
        INC(n);
        VAR i := UnpickleInt(d.bytes^); BEGIN
          Wr.PutText(stdout, "Received packet " & Fmt.Int(i)
            & " from " & HostPort(d.other) &  "; sending ACK...\n");
          Wr.Flush(stdout);
        END;
        sent := conn.sendText(d.other, Ack);
        <* ASSERT sent = AckLen *>
        IF n > max THEN EXIT END;
      END
    FINALLY
      conn.close()
    END
  END Server;

PROCEDURE Client(server: IP.Address) RAISES {IP.Error} =
  VAR conn: UDP.T; d: UDP.Datagram; cnt := 0; BEGIN
    d.bytes := NEW(REF ARRAY OF CHAR, BuffSz);
    Wr.PutText(stdout, "Starting client...\n");
    Wr.Flush(stdout);
    conn := NEW(UDP.T).init(ClientPort);
    TRY
      LOOP
        VAR start: LONGREAL; sent: INTEGER; BEGIN
          (* record start time *)
          start := Time.Now();
  
          (* send packet *)
          d.other.addr := server;
          d.other.port := ServerPort;
          d.len := 4;
          PickleInt(cnt, d.bytes^);
          sent := conn.send(d);
          <* ASSERT sent = 4 *>
  
          (* wait for ack *)
          TRY
            conn.receive((*INOUT*) d, timeout := ReceiveTimeout);
            VAR t := Text.FromChars(SUBARRAY(d.bytes^, 0, d.len)); BEGIN
              <* ASSERT Text.Equal(t, Ack) *>
            END;

            (* print message *)
            Wr.PutText(stdout, "UDP round trip time = " &
              Fmt.LongReal((Time.Now() - start)*1000.0d0, prec:=1) & " ms\n");
            Wr.Flush(stdout);
          EXCEPT
            UDP.Timeout =>
              Wr.PutText(stdout, "No packet received after " &
                Fmt.LongReal(ReceiveTimeout) & " seconds...\n");
              Wr.Flush(stdout);
          END;

          (* wait before trying again *)
          Thread.Pause(1.0d0);
          INC(cnt);
          IF cnt > max THEN EXIT END;
        END
      END
    FINALLY
      conn.close()
    END
  END Client;

PROCEDURE SyntaxError(msg: TEXT) =
  BEGIN
    Wr.PutText(stderr, "Fatal error: " & msg & "\n");
    Wr.PutText(stderr, "Syntax: TestUDP (-server | -client [serverhost])\n");
    Process.Exit(1)
  END SyntaxError;

BEGIN
  IF Params.Count < 2 THEN
    SyntaxError("too few parameters")
  ELSIF Params.Count > 3 THEN
    SyntaxError("too many parameters")
  END;
  TRY
    VAR switch := Params.Get(1); BEGIN
      IF Text.Equal(switch, "-server") THEN
        Server()
      ELSIF Text.Equal(switch, "-client") THEN
        VAR addr: IP.Address; BEGIN
          IF Params.Count = 3 THEN
            IF NOT IP.GetHostByName(Params.Get(2), (*OUT*) addr) THEN
              Wr.PutText(stderr, "Fatal error: host name " & Params.Get(2)
                & " not found!");
              Process.Exit(1)
            END
          ELSE
            addr := IP.GetHostAddr()
          END;
          Client(addr);
        END
      ELSE
        SyntaxError("exptected -server or -client")
      END
    END
  EXCEPT
  | IP.Error (e) => PrintIPError(e);
  END
END TestUDP.
