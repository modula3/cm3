(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* TCPNetObj.m3                                            *)
(* Last modified on Mon Jul 15 09:52:35 PDT 1996 by wobber *)
(*      modified on Fri Oct 28 16:22:44 PDT 1994 by kalsow *)
(*      modified on Wed Jan 27 22:09:40 PST 1993 by owicki *)
(*      modified on Mon Aug  3 13:54:45 PDT 1992 by evers  *)

MODULE TCPNetObj EXPORTS TCPNetObj, TCPTransport;
   
IMPORT NetObj, IP, TCP, TCPSpecial, Transport;
IMPORT AtomList, Convert, Fmt, TextRefTbl, Thread, Text, Time;
IMPORT Rd, Wr, WeakRef, HeaderOps, ConnFD, ConnMsgRW, TransportUtils;
IMPORT Process, NetObjNotifier, StubLib;
IMPORT Atom, Stdio, FmtTime, RTParams;

CONST
   DefaultAgentPort = 9783;   (* random!!! *)
   UniqueEndpointProtocolName = "TCPV2-U";
   KnownEndpointProtocolName  = "TCPV2-K";

   InitPingCycles = 1;
   MaxPingCycles = 10;

TYPE
  T = Transport.T OBJECT
    mu: MUTEX;
    locationTbl: TextRefTbl.T := NIL;
    listener: ListenerClosure := NIL;
  OVERRIDES
    fromEndpoint := LocationFromEndpoint;
    toEndpoint := ListenerEndpoint;
    (* TransportUtils.Public *)
    enumerateLocs := EnumerateLocs;
  END;
    
  ConnT = StubLib.Conn OBJECT
    fd: ConnFD.T;
    nextConn: ConnT := NIL;
    nextFree: ConnT := NIL;
    thread: Thread.T := NIL;    (* server-side only *)
  METHODS
    close() := CloseConnT;
  END;

  Location = Transport.Location OBJECT
    mu: MUTEX;
    t: T;
    e: Transport.Endpoint;
    ep: IP.Endpoint;
    knownEP: BOOLEAN;
    activity: BOOLEAN := FALSE;
    reported: BOOLEAN := FALSE;
    connList: ConnT := NIL;
    freeList: ConnT := NIL;
    nCached: CARDINAL := 0;
    pingCount: CARDINAL := InitPingCycles;
    pingCycles: CARDINAL := InitPingCycles;
  OVERRIDES
    new := NewConnection;
    free := FreeConnection;
    (* TransportUtils.LocationP *)
    getInfo := GetInfo;
    getEp := GetEp;
  END;

  ListenerClosure = Thread.SizedClosure OBJECT
    t: T;
    ep: IP.Endpoint;
    c: TCP.Connector;
    rep: NetObj.Address;
  OVERRIDES
    apply := Listener;
  END;
  
  ScavengerClosure = Thread.Closure OBJECT
    t: T;
    pingQ: PingRQ := NIL;
    killQ: ConnT := NIL;
  OVERRIDES
    apply := Scavenger;
  END;

  PingRQ = Thread.Closure OBJECT
    next: PingRQ;
    loc: Location;
    conn: TCP.T := NIL;
    start: Time.T := 0.0D0;
    try: CARDINAL := 0;
  OVERRIDES
    apply := ProcessPing;
  END;  

  WR = REF WeakRef.T;

CONST MaxCachedConnections = 2;

VAR (*CONST*) thisTransport: T;
VAR (*CONST*) logging: BOOLEAN;

(* exported to TCPTransport *)

PROCEDURE New() : Transport.T =
  BEGIN
    <* ASSERT thisTransport = NIL *>
    thisTransport := NEW(T, mu := NEW(MUTEX));
    RETURN thisTransport;
  END New;

(* exported to TCPNetObj *)

PROCEDURE Locate (ep: IP.Endpoint) : NetObj.Address =
  BEGIN
    (* might want to cache the local ip address in IP.m3 *)
    IF ep.addr = IP.NullAddress THEN ep.addr := IP.GetHostAddr(); END;
    IF ep.port = IP.NullPort THEN ep.port := DefaultAgentPort; END;
    RETURN TCPEndpointToAddr(ep, FALSE);
  END Locate;
  
PROCEDURE Listen (port: IP.Port) : NetObj.Address RAISES {Failed} =
  VAR l: ListenerClosure;
  BEGIN
    (* assign well-known port for listening *)
    IF port = IP.NullPort THEN port := DefaultAgentPort; END;
    TRY
      l := DoListen(thisTransport, port);
    EXCEPT
    | IP.Error => RAISE Failed;
    END;
    RETURN l.rep;
  END Listen;

PROCEDURE DoListen(t: T; port: IP.Port) : ListenerClosure RAISES {IP.Error} =
  VAR l: ListenerClosure;
  BEGIN
    l := NEW(ListenerClosure,
           t := t, stackSize := 2 * Thread.GetDefaultStackSize(),
           c := TCP.NewConnector(IP.Endpoint{IP.NullAddress, port}));
    l.ep := TCP.GetEndPoint(l.c);
    l.rep := TCPEndpointToAddr(l.ep, TRUE);
    LOCK t.mu DO t.listener := l; END;
    EVAL Thread.Fork(l);
    RETURN l;
  END DoListen;
  

(* transport T methods *)

PROCEDURE LocationFromEndpoint(
    t: T; e: Transport.Endpoint) : Transport.Location =
  VAR ep: IP.Endpoint;
  VAR loc: Location := NIL;
  VAR r: REFANY;
  VAR wr: WR;
  BEGIN
    LOCK t.mu DO
      IF t.locationTbl = NIL THEN
        t.locationTbl := NEW(TextRefTbl.Default).init();
        EVAL Thread.Fork(NEW(ScavengerClosure, t := t));
      END;
      IF t.locationTbl.get(e, r) THEN
        loc := WeakRef.ToRef(NARROW(r, WR)^);
        IF loc # NIL THEN RETURN loc; END;
      END;
      IF TCPEndpointFromText(e, ep) THEN
        loc := NEW(Location,
          mu := NEW(MUTEX), t := t, e := e,
          knownEP := KnownEndpoint(e), ep := ep);
        wr := NEW(WR);
        wr^ := WeakRef.FromRef(loc, LocationCleanup);
        EVAL t.locationTbl.put(e, wr);
      END;
    END;
    RETURN loc;
  END LocationFromEndpoint;
  
PROCEDURE LocationCleanup(<*UNUSED*> READONLY wr: WeakRef.T; r: REFANY) =
  VAR loc := NARROW(r, Location);
      t := loc.t;
      cc: ConnT;
      x: REFANY;
  BEGIN
    LOCK t.mu DO
      IF t.locationTbl.get(loc.e, x) THEN
        IF WeakRef.ToRef(NARROW(x, WR)^) = NIL THEN
          EVAL t.locationTbl.delete(loc.e, x);
        END;
      END;
    END;
    LOCK loc.mu DO
      cc := loc.connList;
      loc.connList := NIL;
      loc.freeList := NIL;
      loc.nCached := 0;
    END;
    LogLocationCleanup(loc);
    WHILE cc # NIL DO
      cc.close();
      cc := cc.nextConn;
    END;
  END LocationCleanup;

PROCEDURE ListenerEndpoint(t: T) : Transport.Endpoint =
    <* FATAL IP.Error *>
  VAR l: ListenerClosure;
  BEGIN
    LOCK t.mu DO l := t.listener; END;
    IF l = NIL THEN l := DoListen(t, IP.NullPort); END;
    RETURN l.rep[0];
  END ListenerEndpoint;

PROCEDURE EnumerateLocs (t:T; p: TransportUtils.EnumProc; cl: REFANY := NIL) =
  VAR
    waste: TEXT;
    r: REFANY;
    it: TextRefTbl.Iterator;
    loc: Location;
  BEGIN
    LOCK t.mu DO
      it := t.locationTbl.iterate();
      WHILE it.next(waste, r) DO
        loc := WeakRef.ToRef(NARROW(r, WR)^);
        IF loc # NIL THEN
          IF p(loc, cl) THEN RETURN; END;
        END;
      END;
    END;
  END EnumerateLocs;

(* locations methods *)

PROCEDURE NewConnection(loc: Location) : StubLib.Conn
    RAISES {NetObj.Error, Thread.Alerted} =
  VAR conn: TCP.T := NIL;
  VAR cc: ConnT;
  VAR ec: AtomList.T;
  BEGIN
    LOCK loc.mu DO
      cc := loc.freeList;
      IF cc # NIL THEN
        loc.freeList := cc.nextFree;
        DEC(loc.nCached);
        RETURN cc;
      END;
    END;
    TRY
      conn := TCP.Connect(loc.ep);
    EXCEPT
    | IP.Error(ipErr) =>
        IF ipErr.head = IP.NoResources THEN
          ec := AtomList.Cons(NetObj.NoResources, ipErr);
        ELSE
          ec := AtomList.Cons(NetObj.CommFailure, ipErr);
        END;
        RAISE NetObj.Error(ec);
    END;
    TRY
      HeaderOps.Send(
        conn, HeaderOps.Op.Connect,
        loc.e, ListenerEndpoint(loc.t));
    EXCEPT
    | Thread.Alerted =>
        TCP.Close(conn);
        RAISE Thread.Alerted;
    | Wr.Failure(ec) =>
        TCP.Close(conn);
        RAISE NetObj.Error(AtomList.Cons(NetObj.CommFailure, ec));
    END;
    RETURN NewConnT(loc, conn);
  END NewConnection;

PROCEDURE FreeConnection(loc: Location; c: StubLib.Conn; reUse: BOOLEAN) =
  VAR cc := NARROW(c, ConnT);
  BEGIN
    LOCK loc.mu DO
      IF reUse THEN
        loc.activity := TRUE;
        IF loc.nCached < MaxCachedConnections THEN
          cc.nextFree := loc.freeList;
          loc.freeList := cc;
          INC(loc.nCached);
          RETURN;
        END;
      END;
    END;
    KillConnT(cc);
  END FreeConnection;
  
PROCEDURE NewConnT(loc: Location; fd: TCP.T) : ConnT =
  VAR cc := NEW(ConnT, fd := fd, loc := loc,
                   rd := ConnMsgRW.NewRd(fd),
                   wr := ConnMsgRW.NewWr(fd));
  BEGIN
    LOCK loc.mu DO cc.nextConn := loc.connList; loc.connList := cc; END;
    RETURN cc;
  END NewConnT;

PROCEDURE CloseConnT(cc: ConnT) =
  BEGIN
    TCP.Close(cc.fd);
  END CloseConnT;

PROCEDURE KillConnT(cc: ConnT) =
  VAR try, last: ConnT := NIL;
  VAR loc := NARROW(cc.loc, Location);
  BEGIN
    cc.close();
    LOCK loc.mu DO
            (* prune old dead connections from list *)
      try := loc.connList;
      WHILE try # NIL AND cc # try DO
        last := try;
        try := try.nextConn;
      END;
      IF try # NIL THEN
        IF last = NIL THEN
          loc.connList := cc.nextConn; 
        ELSE
          last.nextConn := cc.nextConn;
        END;
      END;
    END;
    (* help the GC *)
    cc.loc := NIL;
  END KillConnT;

PROCEDURE GetInfo (loc: Location): TEXT =
  BEGIN
    (* could get DNS name from loc.ep here *)
    RETURN loc.e; (* never written => loc.mu not needed *)
  END GetInfo;

PROCEDURE GetEp (loc: Location): Transport.Endpoint =
  BEGIN
    RETURN loc.e; (* never written => loc.mu not needed *)
  END GetEp;

(* main listener loop *)

PROCEDURE Listener(l: ListenerClosure) : REFANY =
  VAR conn: TCP.T := NIL;
      cc: ConnT := NIL;
      loc: Location := NIL;
      me, him: TEXT;
      op: HeaderOps.Op;
  BEGIN
    TRY
      LOOP
        TRY
          conn := TCP.Accept(l.c);
          EXIT;
        EXCEPT
        | IP.Error(x) =>
            IF x.head # IP.NoResources THEN
              RAISE IP.Error(x);
            END;
        END;
        (* pause and retry on IP.Error(NoResources) *)
        Thread.Pause(1.0D0);
      END;
      <*ASSERT conn # NIL *>
      EVAL Thread.Fork(l);    (* fork another listener *)
      op := HeaderOps.Receive(conn, -1.0D0, me, him);
      IF Text.Equal(me, l.rep^[0]) OR KnownEndpoint(me) THEN
            (* this guy's talking to me *)
        loc := LocationFromEndpoint(l.t, him);
      END;
      IF op = HeaderOps.Op.Ping THEN
        IF loc # NIL THEN
          LOCK loc.mu DO loc.activity := TRUE; END;
        END;
        IF loc # NIL THEN
          HeaderOps.Send(conn, HeaderOps.Op.PingAck);
        ELSE
          HeaderOps.Send(conn, HeaderOps.Op.PingError);
        END;
      ELSIF (loc # NIL) AND (op = HeaderOps.Op.Connect) THEN
        cc := NewConnT(loc, conn);
        cc.thread := Thread.Self();
        WHILE cc.rd.nextMsg() DO
          LOCK loc.mu DO loc.activity := TRUE; END;
          IF NOT l.t.serviceCall(cc) THEN EXIT; END;
        END;
      END;
    EXCEPT
    | ConnFD.TimedOut, IP.Error =>
    | Thread.Alerted, Rd.Failure, Wr.Failure =>
    END;
    IF cc # NIL THEN
      KillConnT(cc);
    ELSIF conn # NIL THEN
      TCP.Close(conn);
    END;
    (* help GC *)
    loc := NIL;
    cc := NIL;
    RETURN NIL;
  END Listener;


(* location scavenger *)

CONST ScavengerSleepSeconds = 6.0d1;

PROCEDURE Scavenger(sc: ScavengerClosure) : REFANY =
  BEGIN
    LOOP (* forever *)
      Thread.Pause(ScavengerSleepSeconds);
      EnumerateLocs(sc.t, ScavengeLocation, sc);
      WHILE sc.killQ # NIL DO
        KillConnT(sc.killQ);
        sc.killQ := sc.killQ.nextFree;
      END;
      WHILE sc.pingQ # NIL DO
        EVAL ProcessPing(sc.pingQ);
        sc.pingQ := sc.pingQ.next;
      END;
    END;
    <*NOWARN*> RETURN NIL;
  END Scavenger;
  
PROCEDURE ScavengeLocation (l: Transport.Location; cl: REFANY) : BOOLEAN =
  VAR sc := NARROW(cl, ScavengerClosure);
      loc := NARROW(l, Location);
      try: ConnT;
  BEGIN
    LOCK loc.mu DO
      (* kill the entire free queue *)
      try := loc.freeList;
      IF try # NIL THEN
        loc.nCached := 0;
        loc.freeList := NIL;
        IF sc.killQ = NIL THEN
          sc.killQ := try;
        ELSE
          VAR kQTail := sc.killQ; BEGIN
            WHILE kQTail.nextFree # NIL DO kQTail := kQTail.nextFree; END;
            kQTail.nextFree := try;
          END;
        END;
      END;
      (* check all server threads *)
      try := loc.connList;
      WHILE try # NIL DO
        IF try.thread # NIL THEN
          IF TCPSpecial.EOF(try.fd) THEN Thread.Alert(try.thread); END;
        END;
        try := try.nextConn;
      END;

      IF NOT loc.activity THEN
        (* If there has been no completed activity since the last visit,
           then ping the other side if there are outstanding connections
           (e.g. incoming or outgoing calls).  Otherwise, only ping
           unique endpoints that we aren't listening on, that haven't been
           reported "dead", and that have been visited "pingCycles" times
           since the last ping. *)
        IF loc.connList # NIL THEN
          sc.pingQ := NEW(PingRQ, next := sc.pingQ, loc := loc);
        ELSIF (NOT loc.reported) AND (NOT loc.knownEP) AND
              (loc.ep # sc.t.listener.ep) THEN
          DEC(loc.pingCount);
          IF loc.pingCount = 0 THEN
            sc.pingQ := NEW(PingRQ, next := sc.pingQ, loc := loc);
            IF loc.pingCycles # MaxPingCycles THEN INC(loc.pingCycles); END;
            loc.pingCount := loc.pingCycles;
          END;
        END;
      ELSE
        loc.reported := FALSE;
        loc.activity := FALSE;
        loc.pingCycles := InitPingCycles;
        loc.pingCount := InitPingCycles;
      END;
    END;
    RETURN FALSE;
  END ScavengeLocation;
 

(* connection establishment/ping protocol *)

CONST
  FastPingInterval = 5.0D0;    (* max time we'll wait in main thread *)
  SlowPingInterval = 6.0D1;    (* max time we'll wait in forked thread *)
  MaxPingTries     = 3;

PROCEDURE ProcessPing(ping: PingRQ) : REFANY =
  VAR readInterval := FastPingInterval;
      loc := ping.loc;
      dead := FALSE;
      state := NetObjNotifier.OwnerState.Failed;
      x: TEXT;
      cc: ConnT;
      doIt, retry: BOOLEAN;
  BEGIN
    REPEAT
      retry := FALSE;
      TRY
        (* check that we still haven't seen activity *)
        LOCK loc.mu DO doIt := NOT loc.activity; END;
        IF doIt THEN
          IF ping.conn = NIL THEN
            ping.start := Time.Now();
            ping.conn := TCPSpecial.StartConnect(loc.ep);
            IF NOT TCPSpecial.FinishConnect(ping.conn, FastPingInterval) THEN
              EVAL Thread.Fork(ping);
              loc := NIL;
              RETURN NIL;
            END;
          ELSE
            readInterval := SlowPingInterval;
            EVAL TCPSpecial.FinishConnect(ping.conn);
            HeaderOps.Send(
               ping.conn, HeaderOps.Op.Ping,
               loc.e, ListenerEndpoint(loc.t));
            IF HeaderOps.Receive(
               ping.conn, readInterval, x, x) = HeaderOps.Op.PingError THEN
              (* definitive answer -- wrong instance iff PingError *)
              LogPingFailure(ping, NIL, "WrongInstance", TRUE);
              state := NetObjNotifier.OwnerState.Dead;
              dead := TRUE;
            END;
          END;
        END;
      EXCEPT
      | Rd.Failure(x) =>
          LogPingFailure(ping, x, "Rd.Failure");
      | Wr.Failure(x) =>
          LogPingFailure(ping, x, "Wr.Failure");
      | ConnFD.TimedOut =>
          LogPingFailure(ping, NIL, "ConnFD.TimedOut");
      | Thread.Alerted =>
          LogPingFailure(ping, NIL, "Thread.Alerted");
      | IP.Error(ec) =>
          IF ec.head = TCP.Refused THEN
            (* this is dicey .. refused might mean "stopped" ?? *)
            state := NetObjNotifier.OwnerState.Dead;
            dead := TRUE;
          ELSIF ec.head # IP.NoResources THEN
            (* Eventually, we'll try to differentiate failure from
             death, but for now, all faiures imply transient death. *)
            INC(ping.try);
            IF ping.try < MaxPingTries THEN
              retry := TRUE;
            ELSE
              dead := TRUE;
            END;
          END;
          LogPingFailure(ping, ec, "IP.Error", dead);
      END;
      IF ping.conn # NIL THEN TCP.Close(ping.conn); ping.conn := NIL; END;
    UNTIL dead OR NOT retry;
    IF dead THEN
      LOCK loc.mu DO
        loc.reported := TRUE;
        loc.freeList := NIL;
        cc := loc.connList;
        loc.connList := NIL;
      END;
      WHILE cc # NIL DO
        IF cc.thread # NIL THEN
          Thread.Alert(cc.thread);
        END;
        cc.close();
        cc := cc.nextConn;
      END;
      loc.dead(state);
    END;
    ping.loc := NIL;
    loc := NIL;
    RETURN NIL;
  END ProcessPing;

PROCEDURE LogLocationCleanup(loc: Location) =
    <* FATAL Thread.Alerted *>
  BEGIN
    IF NOT logging THEN RETURN; END;
    TRY
      Wr.PutText(Stdio.stdout,
        Fmt.F("%s: NetObj location cleanup -- %s\n",
                 FmtTime.Short(Time.Now()), loc.e));
    EXCEPT
    | Wr.Failure =>
    END;
  END LogLocationCleanup;

PROCEDURE LogPingFailure(ping: PingRQ; a: AtomList.T; txt: TEXT;
                         targetDead: BOOLEAN := FALSE) =
  VAR type: TEXT;
    <* FATAL Thread.Alerted *>
  BEGIN
    IF NOT logging THEN RETURN; END;
    IF targetDead THEN
      type := "died";
    ELSE
      type := "failed";
    END;
    TRY
      Wr.PutText(Stdio.stdout,
        Fmt.F("%s: NetObj location %s (%s) after %s seconds -- %s\n",
                 FmtTime.Short(Time.Now()),
                 type, IPAddrText(ping.loc.ep),
                 Fmt.Int(ROUND(Time.Now() - ping.start)),
                 txt & ErrorList(a)));
    EXCEPT
    | Wr.Failure =>
    END;
  END LogPingFailure;

PROCEDURE ErrorList(a: AtomList.T): TEXT =
  BEGIN
    IF a = NIL THEN RETURN ""; END;
    IF a.head = NIL THEN RETURN ErrorList(a.tail); END;
    RETURN "(" & Atom.ToText(a.head) & ErrorList(a.tail) & ")";
  END ErrorList;

(* conversion to/from text representations *)

PROCEDURE TCPEndpointToAddr(
    ep: IP.Endpoint; unique: BOOLEAN) : NetObj.Address =
  VAR addr := NEW(NetObj.Address, 1);
  BEGIN
    IF unique THEN
      addr[0] := Fmt.F("%s:%s@%s.%s",
         UniqueEndpointProtocolName, IPAddrText(ep),
         Fmt.Unsigned(Process.GetMyID()),
         Fmt.Unsigned(ROUND(Time.Now())));
    ELSE
      addr[0] := KnownEndpointProtocolName & ":" & IPAddrText(ep);
    END;
    RETURN addr;
  END TCPEndpointToAddr;

PROCEDURE IPAddrText(ep: IP.Endpoint) : TEXT =
  BEGIN
    RETURN Fmt.F("%s.%s.%s.%s.%s",
      Fmt.Int(ep.addr.a[0]), Fmt.Int(ep.addr.a[1]),
      Fmt.Int(ep.addr.a[2]), Fmt.Int(ep.addr.a[3]),
      Fmt.Int(ep.port));
  END IPAddrText;

PROCEDURE TCPEndpointFromText(name: TEXT; VAR ipEP: IP.Endpoint): BOOLEAN =
  VAR
    buf: ARRAY [0..63] OF CHAR;
    pos, used, x: INTEGER;
    prot: TEXT;
    len := Text.Length(name);
  BEGIN
    IF len >= BYTESIZE(buf) THEN RETURN FALSE; END;
    pos := Text.FindChar(name, ':');
    IF pos = -1 THEN RETURN FALSE; END;
    prot := Text.Sub(name, 0, pos);
    IF NOT (Text.Equal(prot, KnownEndpointProtocolName) OR
           Text.Equal(prot, UniqueEndpointProtocolName)) THEN
      RETURN FALSE;
    END;
    INC(pos);
    Text.SetChars (buf, name);
    FOR i := 0 TO 4 DO
      x := Convert.ToUnsigned (SUBARRAY(buf, pos, len-pos), used, 10);
      INC(pos, used);
      IF x < 0 OR used = 0 THEN RETURN FALSE; END;
      IF i # 4 THEN
        IF x > 255 OR buf[pos] # '.' THEN RETURN FALSE; END;
        ipEP.addr.a[i] := x;
      ELSE
        IF x > LAST(IP.Port) THEN RETURN FALSE; END;
        ipEP.port := x;
      END;
      INC(pos);
    END;
    RETURN TRUE;
  END TCPEndpointFromText;

PROCEDURE KnownEndpoint(me: TEXT) : BOOLEAN =
  VAR
    pos: INTEGER;
    prot: TEXT;
  BEGIN
    pos := Text.FindChar(me, ':');
    IF pos = -1 THEN RETURN FALSE; END;
    prot := Text.Sub(me, 0, pos);
    RETURN Text.Equal(prot, KnownEndpointProtocolName);
  END KnownEndpoint;

BEGIN
  logging := RTParams.IsPresent("netobjlog");
END TCPNetObj.



