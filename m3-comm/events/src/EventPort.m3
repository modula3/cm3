(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Fri Jun  2 15:44:44 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Oct 24 11:49:40 1997
 * Update Count    : 139
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/EventPort.m3,v $
 * $Date: 2001-12-02 00:20:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.6  1998/09/25 15:22:26  bm
 * small bug in Event package
 * make binaries dynamically linked
 *
 * Revision 1.5  1997/10/24 19:31:32  bm
 * Added the ability to flush the readers and worker pool.
 *
 * Revision 1.4  1997/08/04 20:15:10  bm
 * Fixed BRANDs
 *
 * Revision 1.3  1997/01/23 15:26:38  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *)

UNSAFE MODULE EventPort;

IMPORT Rd, MsgWr, Wr, HostInfo, HostInfoTbl, Atom;
IMPORT EventSpaceID, EventProtocol, AtomList, Event, Thread, Work, 
       WorkerPool, RdClass, WrClass, EventIO, EventConn, EventSeq,
       EventConnList, Fingerprint;
FROM EventProtocol IMPORT ID, StubProtocol;
FROM Event IMPORT Error;

IMPORT IO, Fmt;

REVEAL RdClass.Private <: MUTEX;
REVEAL WrClass.Private <: MUTEX;

REVEAL
  T = Public BRANDED "EventPort.T" OBJECT
        num   : CARDINAL;
        debug : BOOLEAN;
        mu    : Thread.Mutex    := NIL;
        ports : HostInfoTbl.T;
        wp    : WorkerPool.T;
        cb    : ARRAY [FIRST(ID) .. LAST(ID)] OF EventCallBack;
      METHODS
        problem(conn: EventConn.T; at: AtomList.T) := DispatchProblem;
      OVERRIDES
        init       := Init;
        connect    := Connect;
        disconnect := Disconnect;
        send       := Send;
        mcast      := MCast;
        register   := Register;
        stealWorker := StealWorker;
        flushReader := FlushReader;
        flushWork := FlushWork;
      END;

VAR portNumber: CARDINAL := 0;

PROCEDURE DebugMsg (self: T; msg: TEXT) =
  BEGIN
    IF self.debug THEN
      IO.Put("EventPort" & Fmt.Int(self.num) & ": " & msg & "\n");
    END;
  END DebugMsg;

PROCEDURE StealWorker (self: T): BOOLEAN =
  BEGIN
    RETURN self.wp.stealWorker();
  END StealWorker;

PROCEDURE EventPortReadFlush (hinfo: HostInfo.T) RAISES {Thread.Alerted} =
  BEGIN
    LOCK hinfo.rdmu DO
      IF hinfo.blocking^ THEN RETURN END;
      Thread.AlertWait(hinfo.rdmu, hinfo.rdcv);
    END;
  END EventPortReadFlush;

PROCEDURE FlushReader(self: T) RAISES {Thread.Alerted} =
  VAR key: Fingerprint.T;
      val: HostInfo.T;
  BEGIN
    (* we must first flush all the incoming readers, and then wait for
       the worker pool to finish it's work *)
    WITH it = self.ports.iterate() DO
      WHILE it.next(key,val) DO
        EventPortReadFlush(val);
      END;
    END;
    IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
  END FlushReader;

PROCEDURE FlushWork(self: T) RAISES {Thread.Alerted} =
  BEGIN
    self.wp.flush();
    IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
  END FlushWork;

PROCEDURE Init (self: T; debug := FALSE): T = 
  BEGIN
    self.debug := debug;
    self.num := portNumber;
    INC(portNumber);
    self.ports := NEW(HostInfoTbl.Default).init();
    (* By default, we want extra space, size we are doing a lot before
       getting to the users code. *)
    self.wp := NEW(WorkerPool.T).init(maxThreads := 2,
                                      maxIdleThreads := 2,
                                      stackSize := 200000);
    self.mu := NEW(Thread.Mutex);
    FOR i := FIRST(ID) TO LAST(ID) DO
      self.cb[i].proc := DefaultDispatcher;
      self.cb[i].data := NIL;
    END;
    IF self.debug THEN
      DebugMsg(self, "initialized.");
    END;
    RETURN self;
  END Init;

PROCEDURE DefaultDispatcher (<*UNUSED*> ev  : Event.T;
                             <*UNUSED*> data: REFANY   ) =
  BEGIN
    (* Just drop it. *)
  END DefaultDispatcher;

PROCEDURE Connect (self: T; conn: EventConn.T) RAISES {Error} =
  VAR hinfo: HostInfo.T;
  BEGIN
    LOCK self.mu DO
      IF self.debug THEN
        DebugMsg(
            self, "connect called for " & EventSpaceID.ToText(conn.space));
      END;
      IF self.ports.get(conn.space, hinfo) THEN
        RAISE Error(AtomList.List1(Atom.FromText("Duplicate space")));
      END;
      hinfo :=
        NEW(HostInfo.T, conn := conn,
            rdmu := NEW(Thread.Mutex), rdcv := NEW(Thread.Condition),
            blocking := NEW (REF BOOLEAN),
            mu := NEW(Thread.Mutex), cv := NEW(Thread.Condition),
            es := NEW(EventSeq.T).init());
      hinfo.reader := Thread.Fork(NEW(RdConnectionClosure, conn := conn,
                                      mu := hinfo.rdmu, cv := hinfo.rdcv,
                                      blocking := hinfo.blocking,
                                      wp := self.wp, ep := self));
      hinfo.writer := Thread.Fork(NEW(WrConnectionClosure, ep := self,
                                      es := hinfo.es, mu := hinfo.mu,
                                      conn := conn, cv := hinfo.cv));
      EVAL self.ports.put(conn.space, hinfo);
      IF self.debug THEN
        DebugMsg(self, "connect succeeded.");
      END;
    END;
  END Connect;

PROCEDURE Disconnect (self: T; conn: EventConn.T): EventSeq.T
  RAISES {Error} =
  VAR hinfo: HostInfo.T;
  BEGIN
    LOCK self.mu DO
      IF self.debug THEN
        DebugMsg(self, "disconnect from " & EventSpaceID.ToText(conn.space));
      END;
      IF self.ports.delete(conn.space, hinfo) = FALSE THEN
        RAISE Error(AtomList.List1(Atom.FromText("Unknown space")));
      END;
    END;
    LOCK hinfo DO
      Thread.Alert(hinfo.reader);
      Thread.Alert(hinfo.writer);
      EVAL Thread.Join(hinfo.reader);
      EVAL Thread.Join(hinfo.writer);
      hinfo.reader := NIL;
      hinfo.writer := NIL;
      IF self.debug THEN
        DebugMsg(self, "disconnect succeeded.");
      END;
      IF hinfo.es.size() > 0 THEN RETURN hinfo.es; ELSE RETURN NIL; END;
    END;
  END Disconnect;

(* TODO: use prot! *)
PROCEDURE Register (           self: T;
                               id  : ID;
                    <*UNUSED*> prot: StubProtocol;
                               disp: Dispatcher;
                               data: REFANY        ) RAISES {Error} =
  BEGIN
    LOCK self.mu DO
      IF disp # DefaultDispatcher
           AND self.cb[id].proc # DefaultDispatcher THEN
        RAISE Error(AtomList.List1(
                      Atom.FromText("Dispatcher already registered")));
      END;
      self.cb[id].proc := disp;
      self.cb[id].data := data;
    END;
  END Register;

(*-----------------------sending events-------------------------------*)
PROCEDURE Send (self: T; conn: EventConn.T; ev: Event.T) RAISES {Error} =
  VAR hinfo: HostInfo.T;
      signal: BOOLEAN := FALSE;
  BEGIN
    IF self.debug THEN
      DebugMsg(self, "Send: enqueuing event " & Event.ToText(ev) &
        " for " & EventSpaceID.ToText(conn.space));
    END;

    LOCK self.mu DO
      IF NOT self.ports.get(conn.space, hinfo) OR hinfo.conn # conn THEN
        RAISE Error(AtomList.List1(Atom.FromText("Unregistered space")));
      END;
    END;
    
    LOCK hinfo DO
      LOCK hinfo.mu DO
        IF hinfo.es.size() = 0 THEN signal := TRUE END;
        ev.addRef();
        hinfo.es.addhi(ev);
      END;
      IF signal THEN Thread.Signal(hinfo.cv) END;
    END;
    IF self.debug THEN
      DebugMsg(self, "Send: enqueued event");
    END;
  END Send;

(* We guarantee to call send on every connection. *)
PROCEDURE MCast (self: T; cs: EventConnList.T; ev: Event.T)
  RAISES {Error} =
  VAR
    atomList: AtomList.T := NIL;
  BEGIN
    IF self.debug THEN
      DebugMsg(self, "mcast called.");
    END;
    WHILE cs # NIL DO
      TRY
        self.send(cs.head, ev);
      EXCEPT
      | Event.Error (al) =>
          atomList := AtomList.AppendD(AtomList.Cons(Atom.FromText(
                  "Send to " & EventSpaceID.ToText(cs.head.space)
                    & " raised error (next atom)"), al), atomList);
      END;
      cs := cs.tail;
    END;

    (* Now, reraise the exception. *)
    IF atomList # NIL THEN RAISE Event.Error(atomList) END;

    IF self.debug THEN
      DebugMsg(self, "mcast done.");
    END;
  END MCast;

TYPE
  WrConnectionClosure = Thread.Closure OBJECT
                        es: EventSeq.T;
                        mu: Thread.Mutex;
                        cv: Thread.Condition;
                        conn: EventConn.T;
                        ep  : T;
                      OVERRIDES
                        apply := EventPortWriteApply;
                      END;

PROCEDURE EventPortWriteApply (self: WrConnectionClosure): REFANY =
  VAR ev: Event.T;
      wr: MsgWr.T;
  BEGIN
    TRY
      LOOP
        IF self.ep.debug THEN
          DebugMsg(self.ep, "WrConnectionClosure writing next event.");
        END;

        LOCK self.mu DO
          WHILE self.es.size() = 0 DO Thread.AlertWait(self.mu, self.cv) END;
          ev := self.es.remlo();
          wr := self.conn.wr;
        END;
        (* If the writer is NIL, exit *)
        IF wr = NIL THEN EXIT END;

        DebugMsg(self.ep, "WrConnectionClosure: sending event " & 
          Event.ToText(ev) & " to " & EventSpaceID.ToText(self.conn.space));

        EventIO.Write(wr, ev);
        wr.nextMsg();
        wr.flush();
        ev.dropRef();
      END;
    EXCEPT
    | Thread.Alerted =>
    | Wr.Failure (al) =>
        self.ep.problem(self.conn, al);
    | Rd.Failure (al) =>
        self.ep.problem(self.conn, al);
    END;
    self.mu := NIL;
    self.cv := NIL;
    self.es := NIL;
    self.conn := NIL;
    IF self.ep.debug THEN
      DebugMsg(self.ep, "WrConnectionClosure exiting.");
    END;
    self.ep := NIL;
    RETURN NIL;
  END EventPortWriteApply; 

(*-----------------------reading events---------------*)
TYPE
  EventCallBack = RECORD
                    proc: Dispatcher;
                    data: REFANY;
                  END;

TYPE
  EventWork = Work.T OBJECT
                  next: EventWork := NIL;
                  event: Event.T;  
                  ep: T;
                OVERRIDES
                  handle := EventJobber;
                END;

  ProblemWork = Work.T OBJECT
                  conn: EventConn.T; 
                  al: AtomList.T;
                  ep: T;
                OVERRIDES
                  handle := ProblemJobber;
                END;

VAR idlework: EventWork := NIL;
    mu: MUTEX := NIL;

PROCEDURE NewEventWork(ep: T; event: Event.T): EventWork =
  VAR ret: EventWork;
  BEGIN
    LOCK mu DO
      IF idlework # NIL THEN
        ret := idlework;
        ret.ep := ep;
        ret.event := event;
        idlework := ret.next;
        ret.next := NIL;
      ELSE
        ret := NEW(EventWork, ep := ep, event := event);
      END;
    END;
    RETURN ret;
  END NewEventWork;

PROCEDURE EventJobber (work: EventWork)
  RAISES {Thread.Alerted} =
  BEGIN
    IF work.ep.debug THEN
      DebugMsg(work.ep, "Jobber received EventWork.");
    END;
    WITH id = work.event.hdr.rep.id DO
      work.ep.cb[id].proc(work.event, work.ep.cb[id].data);
      IF work.ep.debug THEN
        DebugMsg(work.ep, "Jobber handed off EventWork.");
      END;
    END;
    (* Free and cache the Event and EventWork *)
    LOCK mu DO
      work.next := idlework;
      idlework := work;
      work.event.dropRef();
      work.event := NIL;
      work.ep := NIL;
    END;
  END EventJobber;

PROCEDURE ProblemJobber (work: ProblemWork) =
  BEGIN
    IF work.ep.debug THEN
      DebugMsg(work.ep, "Jobber received ProblemWork.");
    END;
    work.conn.problem(work.al);
    work.conn.rd := NIL;
    work.conn.wr := NIL;
    IF work.ep.debug THEN
      DebugMsg(work.ep, "Jobber notified client of problem.");
    END;
  END ProblemJobber;

TYPE
  RdConnectionClosure = Thread.Closure OBJECT
                        conn: EventConn.T;
                        wp  : WorkerPool.T;
                        ep  : T;
                        mu: Thread.Mutex;
                        cv: Thread.Condition;
                        blocking: REF BOOLEAN;
                      OVERRIDES
                        apply := EventPortReadApply;
                      END;

PROCEDURE EventPortReadApply (self: RdConnectionClosure): REFANY =
  BEGIN
    TRY
      LOOP
        IF self.ep.debug THEN
          DebugMsg(self.ep, "RdConnectionClosure waiting for next event.");
        END;
        (* If the are no more characters, we caught up for now *)
        LOCK self.mu DO
          IF Rd.CharsReady(self.conn.rd) = 0 THEN
            Thread.Broadcast(self.cv);
            self.blocking^ := TRUE;
          END;
        END;
        IF NOT self.conn.rd.nextMsg() THEN
          EXIT;
        END;
        self.blocking^ := FALSE;
        WITH ev = EventIO.Read(self.conn.rd) DO
          ev.sender := self.conn;
          self.wp.add(NewEventWork(self.ep, ev));
          IF self.ep.debug THEN
            DebugMsg(
              self.ep, "RdConnectionClosure received event "
                       & Fmt.Int(ev.hdr.rep.id) & "/" & Fmt.Int(ev.prot)
                       & "/" & Fmt.Int(Rd.Length(self.conn.rd))
                       & " and added to work queue.");
          END;
        END;
      END;
      (* "nextMsg()" failed. *)
      self.ep.problem(self.conn, AtomList.List1(
                            Atom.FromText("EventPort.rd.nextMsg() failure")));
    EXCEPT
    | Thread.Alerted =>
    | Rd.Failure (al) =>
        self.ep.problem(self.conn, al);
    | Event.Error (al) =>
        self.ep.problem(self.conn, al);
    END;
    self.wp := NIL;
    self.conn := NIL;
    IF self.ep.debug THEN
      DebugMsg(self.ep, "RdConnectionClosure exiting.");
    END;
    self.ep := NIL;
    RETURN NIL;
  END EventPortReadApply; 

PROCEDURE DispatchProblem(self: T; conn: EventConn.T; al: AtomList.T) =
  BEGIN
    self.wp.add(NEW(ProblemWork, ep := self, conn := conn, al := al));
  END DispatchProblem;

BEGIN
  mu := NEW(MUTEX);
END EventPort.
