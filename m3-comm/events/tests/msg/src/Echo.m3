(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* NetObjTest.m3 *)
(* Last modified on Tue Aug 24 17:01:51 PDT 1993 by wobber *)

UNSAFE MODULE Echo EXPORTS Main;

IMPORT Endpoint, TCPNetObj, IP, RdWrPipe, Process, NetObj, Rd, Wr, IO,
       Params, Fmt, Thread, Time, SimpleMsgRW, Text, Atom, WeakRef,
       AtomList, Pipe, FileRd, FileWr;
IMPORT Event, EventPort, EventSpaceID, EventStubLib, EventConnList,
       EventConn, NetObjNotifier, EventNumber;

CONST
  EndpointName = "EventEndpoint";
  MyIPPort     = 7777;

CONST MyEventID = 42;

(* invocation:

   echo <no args> echo hostname ...

   The former is a receiver, which waits for events to arrive and displays
   them.

   The latter spews events to all receivers listed.  It also echos any
   events it receives to those receivers. *)

TYPE
  T =
    Endpoint.T OBJECT
      hostList: EventConnList.T;
      ep      : EventPort.T;
    METHODS
      init (): T := DoInit;
      localConnect (id: EventSpaceID.T; rd: Rd.T; wr: Wr.T): Conn
                    RAISES {Event.Error} := DoLocalConnect;
      localDisconnect (c: Conn) RAISES {Event.Error} := DoLocalDisconnect;
    OVERRIDES
      space      := GetSpace;
      connect    := DoConnect;
      disconnect := DoDisconnect;
    END;

TYPE
  Conn = EventConn.T OBJECT
           ep   : Endpoint.T;
           local: BOOLEAN;
         OVERRIDES
           problem := ConnProblem;
         END;

TYPE
  MyNotifierClosure = NetObjNotifier.NotifierClosure OBJECT
                      OVERRIDES
                        notify := MyNotify;
                      END;

PROCEDURE MyNotify (<*UNUSED*> self: MyNotifierClosure;
                               obj : NetObj.T;
                    <*UNUSED*> st  : NetObjNotifier.OwnerState) =
  VAR hosts: EventConnList.T := sendList;
  BEGIN
    IO.Put("Notified of object being dead\n");
    LOCK mu DO
      WHILE hosts # NIL AND NARROW(hosts.head, Conn).ep # obj DO
        hosts := hosts.tail;
      END;
      <* ASSERT hosts # NIL *>
    END;
    ep.localDisconnect(NARROW(hosts.head, Conn));
  END MyNotify;

PROCEDURE Cleanup (<*UNUSED*> READONLY wref: WeakRef.T; ref: REFANY) =
  BEGIN
    TYPECASE ref OF
    | Rd.T (rd) => Rd.Close(rd); IO.Put("Cleaned up Rd.T\n");
    | Wr.T (wr) => Wr.Flush(wr); Wr.Close(wr); IO.Put("Cleaned up Wr.T\n");
    ELSE
      IO.Put("Invalid ref passed to Cleanup (not rd or wr)\n");
    END;
  END Cleanup;

PROCEDURE GetSpace (<*UNUSED*> self: T): EventSpaceID.T =
  BEGIN
    RETURN EventSpaceID.Mine();
  END GetSpace;

PROCEDURE DoInit (self: T): T =
  BEGIN
    self.ep := NEW(EventPort.T).init(FALSE);
    self.hostList := NIL;
    TRY
      self.ep.register(MyEventID, 0, MyDispatcher, self);
    EXCEPT
    | Event.Error => Process.Crash("Register of callback failed!?!?");
    END;
    RETURN self;
  END DoInit;

PROCEDURE DoConnect (self: T; id: EventSpaceID.T; rd: Rd.T; wr: Wr.T)
  RAISES {Event.Error} =
  VAR
    c: Conn;
  BEGIN
    LOCK mu DO
      c := NEW(Conn, space := id, rd := SimpleMsgRW.NewRd(rd),
               wr := SimpleMsgRW.NewWr(wr), local := FALSE);
      self.ep.connect(c);
      self.hostList := EventConnList.Cons(c, self.hostList);
    END;
  END DoConnect;

(*
PROCEDURE DoConnect (self: T; id: EventSpaceID.T; wr: Wr.T): Wr.T
  RAISES {Event.Error} =
  VAR
    rd: Rd.T;
    wr_ret: Wr.T;
    c: Conn;
  BEGIN
    LOCK mu DO
      RdWrPipe.New(rd, wr_ret, nm := "out");
      c := NEW(Conn, space := id, rd := SimpleMsgRW.NewRd(rd),
               wr := SimpleMsgRW.NewWr(wr), local := FALSE);
      self.ep.connect(c);
      self.hostList := EventConnList.Cons(c, self.hostList);
      RETURN wr_ret;
    END;
  END DoConnect;
*)

PROCEDURE DoLocalConnect (self: T; id: EventSpaceID.T; rd: Rd.T; wr: Wr.T):
  Conn RAISES {Event.Error} =
  VAR
    c := NEW(Conn, space := id, rd := SimpleMsgRW.NewRd(rd),
             wr := SimpleMsgRW.NewWr(wr), local := TRUE);
  BEGIN
    self.ep.connect(c);
    RETURN c;
  END DoLocalConnect;

PROCEDURE RemoveHost (hosts: EventConnList.T; id: EventSpaceID.T):
  EventConnList.T =
  BEGIN
    IF hosts = NIL THEN
      RETURN NIL;
    END;
    IF hosts.head.space = id THEN
      RETURN hosts.tail;
    ELSE
      VAR
        prev := hosts;
        ret  := hosts;
      BEGIN
        hosts := hosts.tail;
        WHILE hosts # NIL AND hosts.head.space # id DO
          prev := hosts;
          hosts := hosts.tail;
        END;
        IF hosts # NIL THEN
          prev.tail := hosts.tail;
        END;
        RETURN ret;
      END;
    END;
  END RemoveHost;

PROCEDURE DoDisconnect (self: T; id: EventSpaceID.T) RAISES {Event.Error} =
  BEGIN
    LOCK mu DO
      (* Two "EventConn.T"s with the same "space" are equal. *)
      WITH es = self.ep.disconnect(NEW(Conn, space := id)) DO
        IF es # NIL THEN
          IO.Put("Disconnected with " & Fmt.Int(es.size()) & " waiting\n");
        END;
      END;
      self.hostList := RemoveHost(self.hostList, id);
    END;
  END DoDisconnect;

PROCEDURE DoLocalDisconnect (self: T; c: Conn) RAISES {Event.Error} =
  BEGIN
    LOCK mu DO
      (* Two "EventConn.T"s with the same "space" are equal. *)
      WITH es = self.ep.disconnect(c) DO
        IF es # NIL THEN
          IO.Put(
            "Local Disconnect with " & Fmt.Int(es.size()) & " waiting\n");
        END;
      END;
      sendList := RemoveHost(sendList, c.space);
    END;
  END DoLocalDisconnect;

PROCEDURE ConnProblem (self: Conn; al: AtomList.T) =
  BEGIN
    IO.Put("*** Connection to " & EventSpaceID.ToText(self.space)
             & " has dropped\n");
    IF al # NIL THEN PrintAtomList("*** Error", al); END;

    TRY
      IF self.local THEN
        ep.localDisconnect(self);
      ELSE
        (* ep.disconnect(self.space); *)
        ep.hostList := RemoveHost(ep.hostList, self.space);
      END;
    EXCEPT
    | Event.Error =>
    END;
  END ConnProblem;

PROCEDURE MyDispatcher (ev: Event.T; data: REFANY)
  RAISES {Thread.Alerted} =
  VAR
    endpoint: T                   := NARROW(data, T);
    c       : EventStubLib.Handle;
    print   : BOOLEAN;
    val     : INTEGER;
    time    : LONGREAL;
  BEGIN
    (*
        c := EventStubLib.StartRead(ev);
        IO.Put("Received event with data: ");
        FOR j := 0 TO Rd.Length(ev.rd)-1 DO
          IO.Put(Fmt.Unsigned(EventStubLib.InByte(c)) & " ");
        END;
        EventStubLib.EndRead(c);
        IO.Put("\n");
    *)
    TRY
      TRY
        c := EventStubLib.StartRead(ev);
        print := EventStubLib.InBoolean(c);
        val := EventStubLib.InInteger(c);
        time := EventStubLib.InLongreal(c);
      FINALLY
        EventStubLib.EndRead(c);
      END;

      IF ev.from = EventSpaceID.Mine() THEN
        IF print THEN
          IO.Put("event " & Event.ToText(ev) & " val (" & Fmt.Int(val, 16)
                   & ") time (" & Fmt.LongReal(Time.Now() - time) & ")\n");
        END;
      ELSE
        IF print THEN
          IO.Put("event " & Event.ToText(ev) & " val (" & Fmt.Int(val, 16)
                   & ")\n");
        END;
        LOCK mu DO
          TRY
            IF endpoint.hostList # NIL THEN
              endpoint.ep.mcast(endpoint.hostList, ev);
            END;
          EXCEPT
          | Event.Error (al) =>
              PrintAtomList("Dispatcher MCast failed", al);
          END;
        END;
      END;
    EXCEPT
    | Event.Error (al) => PrintAtomList("Dispatcher Event.Error", al);
    | Rd.Failure (al) => PrintAtomList("Dispatcher Rd.Failure", al);
    END;
  END MyDispatcher;

PROCEDURE PrintAtomList (t: Text.T; al: AtomList.T) =
  BEGIN
    IO.Put(t & ":\n");
    WHILE al # NIL DO
      IO.Put("  " & Atom.ToText(al.head) & "\n");
      al := al.tail;
    END;
  END PrintAtomList;

VAR
  ep                          := NEW(T).init();
  prog                        := Params.Get(0);
  generate  : BOOLEAN;
  sendList  : EventConnList.T := NIL;
  count     : CARDINAL        := 100;
  mu                          := NEW(MUTEX);
  myNotifier                  := NEW(MyNotifierClosure);

<*FATAL TCPNetObj.Failed*>

BEGIN
  TRY
    NetObj.Export(EndpointName, ep, NIL);
    IO.Put("Exported target via netobjd\n");
  EXCEPT
  | NetObj.Error, Thread.Alerted =>
    TRY
      NetObj.Export(EndpointName, ep, TCPNetObj.Listen(MyIPPort));
      IO.Put("Exported target at private port\n");
    EXCEPT
    | NetObj.Error(ec) => PrintAtomList("Netobj export failure", ec);
      Process.Exit(1);
    | Thread.Alerted => Process.Exit(1);
    END;
  END;

  IF Params.Count < 2 THEN
    IO.Put(
      Fmt.F("Usage:\n  %s -g host ...\n  %s -r host ...\n", prog, prog));
    Process.Exit(1);
  END;
  IF Text.Equal(Params.Get(1), "-g") THEN
    generate := TRUE;
  ELSIF Text.Equal(Params.Get(1), "-r") THEN
    generate := FALSE;
  ELSE
    IO.Put(
      Fmt.F("Usage:\n  %s -g host ...\n  %s -r host ...\n", prog, prog));
    Process.Exit(2);
  END;

  IF Params.Count > 2 THEN
    VAR
      host : TEXT;
      addr : IP.Address;
      agent: NetObj.Address;
      link : Endpoint.T;

      rd_out, rd_in: Rd.T;
      wr_out, wr_in: Wr.T;

    (*
          hrChild, hwChild, hrSelf, hwSelf: Pipe.T;
    *)
    BEGIN
      LOCK mu DO
        FOR i := 2 TO Params.Count - 1 DO
          host := Params.Get(i);
          TRY
            IF NOT IP.GetHostByName(host, addr) THEN
              IO.Put(Fmt.F("No such host \"%s\"\n", host));
            ELSE
              agent := NetObj.Locate(host);
              TRY
                link := NetObj.Import(EndpointName, agent);
                IO.Put(Fmt.F("Located target on host \"%s\" via netobjd\n",
                             host));
              EXCEPT
              | NetObj.Error =>
                TRY
                  agent := TCPNetObj.Locate(IP.Endpoint{addr, MyIPPort});
                  link := NetObj.Import(EndpointName, agent);
                  IO.Put(
                    Fmt.F(
                      "Located target on host \"%s\" at private port\n",
                      host));
                EXCEPT
                | NetObj.Error(ec) => 
                  PrintAtomList("Netobj import failure", ec);
                | Thread.Alerted => Process.Exit(1);
                END;
              END;
              TRY
                WITH space = link.space() DO
                  (*
                  Pipe.Open(hr := hrChild, hw := hwSelf);
                  Pipe.Open(hr := hrSelf, hw := hwChild);
                  rd_out := NEW(FileRd.T).init(hrChild);
                  wr_in  := NEW(FileWr.T).init(hwChild);
                  rd_in  := NEW(FileRd.T).init(hrSelf);
                  wr_out := NEW(FileWr.T).init(hwSelf);
                  *)
                  (*
                  RdWrPipe.New(rd_in, wr_in, nm := "in");
                  EVAL WeakRef.FromRef(wr_in, Cleanup);
                  wr_out := link.connect(EventSpaceID.Mine(), wr_in);
                  *)
                  RdWrPipe.New(rd_out, wr_out, nm := "out");
                  RdWrPipe.New(rd_in, wr_in, nm := "in");
                  EVAL WeakRef.FromRef(rd_out, Cleanup);
                  EVAL WeakRef.FromRef(wr_in, Cleanup);
                  link.connect(EventSpaceID.Mine(), rd_out, wr_in);

                  WITH conn = ep.localConnect(space, rd_in, wr_out) DO
                    conn.ep := link;
                    sendList := EventConnList.Cons(conn, sendList);
                  END;
                  NetObjNotifier.AddNotifier(link, myNotifier);
                END;
              EXCEPT
              | Event.Error (al) =>
                  PrintAtomList("Event.Error connecting to eventport", al);
              | NetObj.Error (al) =>
                  PrintAtomList("NetObj.Error connecting to eventport", al);
              END;

            END;
          EXCEPT
          | IP.Error, NetObj.Error, NetObj.Invalid, Thread.Alerted =>
              IO.Put(Fmt.F("No such object at host \"%s\"\n", host));
          END;
        END;
      END;
    END;
  END;

  IF generate THEN
    VAR
      c : EventStubLib.Handle;
      ev: Event.T;
      en := EventNumber.New();
    BEGIN
      en.inc();
      FOR i := 1 TO count * 60 DO
        WITH mod = i MOD 60, div = i DIV 60 DO
          IF mod = 1 THEN
            IO.Put("sending event val (" & Fmt.Int(div, 16) & ")\n");
          END;
          TRY
            c := EventStubLib.StartCreate(MyEventID, 0,en);
            en.inc();
            EventStubLib.OutBoolean(c, mod = 1);
            EventStubLib.OutInteger(c, i);
            EventStubLib.OutLongreal(c, Time.Now());
            ev := EventStubLib.EndCreate(c);
            LOCK mu DO
              IF sendList = NIL THEN
                IO.Put("Nobody to send to!\n");
              ELSE
                ep.ep.mcast(sendList, ev); 
              END;
            END;
            (*
                    c := EventStubLib.StartRead(ev);
                    IO.Put("sent data: ");
                    FOR j := 0 TO Rd.Length(ev.rd)-1 DO
                      IO.Put(Fmt.Unsigned(EventStubLib.InByte(c)) & " ");
                    END;
                    EventStubLib.EndRead(c);
                    IO.Put("\n");
            *)
          EXCEPT
          | Thread.Alerted =>
            IO.Put("Thread.Alerted Exception sending event!\n");
          | Wr.Failure (al) =>
            PrintAtomList("Wr.Failure Exception sending event", al);
          | Rd.Failure (al) =>
            PrintAtomList("Rd.Failure Exception sending event", al);
          | Event.Error (al) =>
            PrintAtomList("Event.Error Exception sending event", al);
          END;
          Thread.Pause(2.0D0/60.0D0);
        END;
      END;
    END;
  ELSE
    TRY
      LOOP Thread.AlertPause(60.0D0); END;
    EXCEPT
    | Thread.Alerted =>
    END;
  END;

END Echo.
