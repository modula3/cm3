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
 * Created On      : Wed Sep 13 12:24:52 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Jul  9 19:14:53 1998
 * Update Count    : 86
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/LocalObjectSpace.m3,v $
 * $Date: 2001-12-02 13:41:16 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:14:14  wagner
 * Blair MacIntyre's sharedobj package
 *
 * Revision 1.6  1998/07/14 02:34:01  bm
 * tried to fix the distribution problems -- not yet working, but closer
 *
 * Revision 1.5  1998/07/02 21:43:36  bm
 * small bug fixes
 *
 * Revision 1.4  1998/01/24 00:21:02  bm
 * timing bug
 *
 * Revision 1.3  1997/01/23 15:27:12  bm
 * Lot's of little bug fixes.
 *
 * 
 * HISTORY
 *)

MODULE LocalObjectSpace EXPORTS LocalObjectSpace, SharedObjRTF;

IMPORT Atom, AtomList, NetObj, Wr, Thread, EventWireRep, ObjectInfo,
       IO, SharedObj, SharedObjRep, EventConn, SpaceConn,
       EventSpaceID, EventNumber, EventNumberF, SpaceTbl, ObjectSpace,
       Process, Fmt, IP, TCP, ConnMsgRW, SharedObjRT,
       SharedObjError, ObjCopy, ObjCopyList;

FROM SharedObjRep IMPORT WireRep;
(* FROM SharedObjRTF IMPORT debug, debug_level, localSpace; *)
(* IMPORT Debug; *)

REVEAL T = ObjectSpace.Local BRANDED "LocalObjectSpace v1.0" OBJECT 
      spaceTbl: SpaceTbl.T;
      halfSpaceTbl: SpaceTbl.T;
      ipep: IP.Endpoint;
      tcpconn: TCP.Connector := NIL;
    METHODS
      init(): T := SpaceInit;
    OVERRIDES
      newSpace := SpaceNewSpace;
      getSpace := SpaceGetSpace;

      setDfltSequencer := SpaceSetDfltSequencer;
      getDfltSequencer := SpaceGetDfltSequencer;
      getSequencer := SpaceGetSequencer;
      space := SpaceSpace;
      endpoint := SpaceEndpoint;
      connect := SpaceConnect;
      disconnect := SpaceDisconnect;
      newObject := SpaceNewObject;
      newCopy := SpaceNewCopy;
      deleteCopy := SpaceDeleteCopy;
      lastCopy := SpaceLastCopy;
      get := SpaceGet;
      findObj := SpaceFindObject;
      printState := PrintState;
    END;

(* Our "EventConn.T" subtype has a pointer to its corresponding
   "ObjectSpace.T".  If there is a problem, we can drop our connection
   to that ObjectSpace, or whatever.  For now, just drop it. 
*)
TYPE
  Conn = SpaceConn.T OBJECT
           tcp: TCP.T;
         OVERRIDES
           problem := ConnProblem;
         END;

PROCEDURE ConnProblem (self: Conn; al: AtomList.T) =
  BEGIN
    IF self.connected THEN
(*
      IO.Put("*** Connection to " & EventSpaceID.ToText(self.space)
             & " has dropped\n");
      IF al # NIL THEN Debug.PrintAtomList("*** Error", al); END;
      IO.Put("*** DO SOME CLEANUP!!!!!!!\n");
*)
      TRY
        localSpace.disconnect(self.space);
      EXCEPT ELSE RETURN END;
    END;
    (* TRY *)
    (* What we want to do depends on what this one is.
       - hosts that receive updates can just be tossed and forgotten
       - sequencers must be handled more intelligently, first attempt
         will be to yell and die!!!
     *)
    (* EXCEPT
       | Event.Error =>
       END;
    *)
  END ConnProblem;

(* Register a space if it isn't already connected. *)
PROCEDURE SpaceNewSpace (self: T; space: ObjectSpace.T): 
  EventConn.T RAISES {SharedObj.Error} =
  VAR
    id: EventSpaceID.T;
    ipep: IP.Endpoint;
    conn: SpaceConn.T;
    myID: FingerPrintChars;
  BEGIN
    TRY
      id := space.space();
    EXCEPT
    | NetObj.Error(ec) => SharedObjError.RaiseNetObjFailure(ec);
    | Thread.Alerted => SharedObjError.RaiseNetObjAlerted();
    END;

    IF debug_level >= 1 THEN
      debug.print(1, "NewSpace: add connection to " & 
      EventSpaceID.ToText(id) & ".");
    END;

    (* ADD A NetObjNotifier TO THE space!! *)

    IF NOT self.spaceTbl.get(id, conn) THEN
      IF debug_level >= 5 THEN
        debug.print(5, "NewSpace: doesn't exist, adding.");
      END;
      
      (* If the space is in the halfspacetbl, then we can finish it
         off. *)
      IF self.halfSpaceTbl.get(id, conn) THEN
        IF debug_level >= 5 THEN
          debug.print(5, "NewSpace: halfway up. Finishing connect.");
        END;
        EVAL self.halfSpaceTbl.delete(id, conn);
        IF conn.objSpace = NIL THEN
          conn.objSpace := space;
        END;
      ELSE
        (* Create a new one that is just like the one that would have
           been in the halfSpaceTbl. *)
        conn := NEW(Conn, objSpace := space, space := id, tcp := NIL);
      END;

      (* Now, we've got the conn.  Finish initializing it.  Make the
         TCP connection if it hasn't been done. *) 
      WITH tconn = NARROW(conn, Conn) DO
        IF tconn.tcp = NIL THEN
          TRY
            (* First, make the connection via the objectspace *)
            space.connect(localSpace);

            (* Then the TCP connection. *)
            ipep := space.endpoint();
            WITH tcp = TCP.Connect(ipep) DO
              tconn.tcp := tcp;
              tconn.rd := ConnMsgRW.NewRd(tcp);
              tconn.wr := ConnMsgRW.NewWr(tcp);
            END;

            (* Convert my id to chars and write it to the TCP stream after
               a little identifying header. *)
            WITH myid = localSpace.space() DO
              FOR i := FIRST(myID) TO LAST(myID) DO
                myID[i] := VAL(ORD(myid.byte[i]), CHAR); 
              END;
              VAR hdr := ARRAY [0..1] OF CHAR{HdrChar0, HdrChar1};
              BEGIN
                tconn.tcp.put(hdr);
                tconn.tcp.put(myID);
              END;
              END
          EXCEPT 
          | NetObj.Error(ec) => SharedObjError.RaiseNetObjFailure(ec);
          | IP.Error(al) => 
            TCP.Close(tconn.tcp); 
            SharedObjError.RaiseIPFailure(al);
          | Wr.Failure(ec) => 
            TCP.Close(tconn.tcp); 
            SharedObjError.RaiseCommFailure(ec);
          | Thread.Alerted => 
            TCP.Close(tconn.tcp); 
            SharedObjError.RaiseAlerted();
          END; 
        END;
      END;

      EventPortConnect(conn);
      EVAL self.spaceTbl.put(id, conn);
    END;
    IF debug_level >= 1 THEN
      debug.print(1, "NewSpace: added conn " & SpaceConn.ToText(conn) & ".");
    END;
    RETURN conn;
  END SpaceNewSpace;

PROCEDURE SpaceGetSpace (self: T; id: EventSpaceID.T): 
  EventConn.T RAISES {SharedObj.Error} =
  VAR
    conn: SpaceConn.T;
  BEGIN
    LOCK objTblMu DO
      IF debug_level >= 1 THEN
        debug.print(1, "SpaceGetSpace: id " & EventSpaceID.ToText(id));
      END;
      IF NOT self.spaceTbl.get(id, conn) THEN
        SharedObjError.RaiseError(Atom.FromText("Space not connected"));
      END;
      RETURN conn;
    END;
  END SpaceGetSpace;

CONST
  HdrChar0 = 'B';
  HdrChar1 = 'M';

TYPE 
  FingerPrintChars = ARRAY [0..7] OF CHAR;
  AcceptClosure = Thread.Closure OBJECT 
       space: T;
     OVERRIDES
       apply := SpaceAccept;
     END;

PROCEDURE NewAcceptor(space: T) =
  BEGIN
    EVAL Thread.Fork(NEW(AcceptClosure, space := space));
  END NewAcceptor;

PROCEDURE SpaceAccept(self: AcceptClosure): REFANY =
  VAR
    conn: SpaceConn.T := NIL;
    id: EventSpaceID.T;
    hdr: ARRAY [0..1] OF CHAR;
    newID: FingerPrintChars;
  BEGIN
    TRY
      (* Accept and do a little check on it so someone running a tcp
         probe won't kill us!  Only wait a short time for the
         fingerprint, since it should get written immediately. *)
      WITH tcp = TCP.Accept(self.space.tcpconn) DO
        (* First, create another Acceptor *)
        NewAcceptor(self.space);

        (* Now, accept this connection.  First, read the SpaceID out of the
           tcp stream. Any errors and we'll just toss this connection. *)
        TRY 
          EVAL tcp.get(hdr, 5.0D0);
        EXCEPT ELSE 
          TCP.Close(tcp); 
          RETURN NIL;
        END;
        IF hdr[0] # HdrChar0 OR hdr[1] # HdrChar1 THEN
          TCP.Close(tcp);
          RETURN NIL;
        END;
        TRY
          EVAL tcp.get(newID, 5.0D0); 
        EXCEPT ELSE 
          TCP.Close(tcp); 
          RETURN NIL;
        END;

        (* Convert it to an EventSpaceID.T *)
        FOR i := FIRST(newID) TO LAST(newID) DO
          id.byte[i] := ORD(newID[i]);
        END;

        LOCK objTblMu DO
          IF debug_level >= 1 THEN
            debug.print(1, "Acceptor: got connection from " &
            EventSpaceID.ToText(id) & "\n");
          END;

          IF NOT self.space.halfSpaceTbl.get(id, conn) OR 
            NARROW(conn, Conn).tcp # NIL THEN

            WITH tconn = NARROW(conn, Conn) DO
              IF tconn.tcp # NIL THEN
                IO.Put("*** Dropping previous partial tcp connection from " &
                  EventSpaceID.ToText(id) & " in favour of new one.\n");
                IO.Put("*** How did this happen???!?!?!!!!!\n");
                TCP.Close(tconn.tcp);
              END;
            END;
            IF debug_level >= 5 THEN
              debug.print(5, "Acceptor: bringing up halfway and exiting.\n");
            END;
            (* Not in the halfSpaceTbl yet, so put it there.
               Eventually, someone will do a connect, find this and
               finish it off. *)
            conn := NEW(Conn, objSpace := NIL, space := id, 
                        tcp := tcp, rd := ConnMsgRW.NewRd(tcp), 
                        wr := ConnMsgRW.NewWr(tcp));
            EVAL self.space.halfSpaceTbl.put(id, conn);
          ELSE
            IF debug_level >= 5 THEN
              debug.print(5, "Acceptor: bringing up completely, exiting\n");
            END;
            (* This will finish the connection, so move it to the
               space table and set up the event package. *)
            EVAL self.space.halfSpaceTbl.delete(id, conn);
            WITH tconn = NARROW(conn, Conn) DO
              tconn.tcp := tcp;
              conn.rd := ConnMsgRW.NewRd(tconn.tcp);
              conn.wr := ConnMsgRW.NewWr(tconn.tcp);
            END;
            (* We've successfully built the connection, so finish
               off our side. *) 
            EventPortConnect(conn);
            EVAL self.space.spaceTbl.put(id, conn);
          END;
        END;
      END;
    EXCEPT
    | IP.Error => (* Ignore it and exit *)
    | Thread.Alerted => (* Ignore it and exit *)
    | SharedObj.Error => (* Ignore it and exit *)
    END;
    RETURN NIL;
  END SpaceAccept; 

(*--- public methods ---*)
PROCEDURE SpaceInit (self: T): T =
  BEGIN
    self.spaceTbl := NEW(SpaceTbl.Default).init();
    self.halfSpaceTbl := NEW(SpaceTbl.Default).init();
    self.ipep.addr := IP.GetHostAddr();
    self.ipep.port := IP.NullPort;
    TRY
      self.tcpconn := TCP.NewConnector(self.ipep);
    EXCEPT
    | IP.Error => Process.Crash("Cannot create TCP port");
    END;
    self.ipep := TCP.GetEndPoint(self.tcpconn);

    (* This will start an acceptor who will wait for a tcp
       connection and then finish the startup process. *)
    NewAcceptor(self);

    IF debug_level >= 1 THEN
      debug.print(1, "SpaceInit: space initialized.");
    END;
    RETURN self;
  END SpaceInit;

PROCEDURE SpaceConnect (self: T; from: ObjectSpace.T)
                 RAISES {SharedObj.Error} =
  VAR
    id: EventSpaceID.T;
    conn: SpaceConn.T;
  BEGIN
    TRY
      id := from.space();
    EXCEPT
    | NetObj.Error(ec) => SharedObjError.RaiseNetObjFailure(ec);
    | Thread.Alerted => SharedObjError.RaiseNetObjAlerted();
    END;
    LOCK objTblMu DO
      IF debug_level >= 1 THEN
        debug.print(1, "SpaceConnect: connect to space " &
        EventSpaceID.ToText(id) );
      END;
      (* ADD A NetObjNotifier TO THE space!! *)

      IF NOT self.spaceTbl.get(id, conn) THEN
        IF debug_level >= 5 THEN
          debug.print(5, "SpaceConnect: new space.");
        END;

        IF self.halfSpaceTbl.get(id, conn) THEN
          WITH tconn = NARROW(conn, Conn) DO
            conn.objSpace := from;
            IF tconn.tcp # NIL THEN
              IF debug_level >= 5 THEN
                debug.print(5,"SpaceConnect: halfway up. Finishing connect.");
              END;
              EVAL self.halfSpaceTbl.delete(id, conn);

              EventPortConnect(conn);
              EVAL self.spaceTbl.put(id, conn);
            END;
          END;
        ELSE
          conn := NEW(Conn, objSpace := from, space := id, tcp := NIL,
                      rd := NIL, wr := NIL);
          IF debug_level >= 5 THEN
            debug.print(5, "SpaceConnect: creating initial conn " & 
            SpaceConn.ToText(conn) & " and bringing halfway up.\n");
          END;
          EVAL self.halfSpaceTbl.put(id, conn);
        END;
      ELSE
        IF from # conn.objSpace THEN
          SharedObjError.RaiseError(
              Atom.FromText("Duplicate Space registered for SpaceID"));
        ELSE
          SharedObjError.RaiseError(
              Atom.FromText("Space already registered for SpaceID"));
        END;
      END;
    END;
  END SpaceConnect;

PROCEDURE SpaceDisconnect (self: T; id: EventSpaceID.T)
        RAISES {SharedObj.Error} =
  VAR
    conn: SpaceConn.T;
  BEGIN
    LOCK objTblMu DO
      IF debug_level >= 1 THEN
        debug.print(1, "SpaceDisconnect: from " & EventSpaceID.ToText(id));
      END;
      IF NOT self.spaceTbl.get(id, conn) THEN
        SharedObjError.RaiseError(Atom.FromText("Space not connected"));
      END;
      WITH es = EventPortDisconnect(conn) DO
        IF es # NIL THEN
          IF debug_level >= 5 THEN
            debug.print(5, "Disconnected with " & 
            Fmt.Int(es.size()) & " waiting");
          END;
        END;
      END;
      (* Indicate that the space has closed and remove it from the table. *)
      conn.connected := FALSE;
      WITH tconn = NARROW(conn, Conn) DO
        TCP.Close(tconn.tcp);
      END;
      EVAL self.spaceTbl.delete(id, conn);
      IF debug_level >= 5 THEN
        debug.print(5, "SpaceDisconnect: done");
      END;
    END;
  END SpaceDisconnect;

PROCEDURE SpaceNewObject (self: T; id: EventSpaceID.T; 
                          wrep: WireRep; seqNo: SharedObj.SequenceNumber)
                   RAISES {SharedObj.Error} =
  VAR
    conn: SpaceConn.T;
    objInfo: ObjectInfo.T;
  BEGIN
    LOCK objTblMu DO
      IF debug_level >= 1 THEN
        debug.print(1, "SpaceNewObject: new object " & 
        EventWireRep.ToText(wrep) & " in space " & EventSpaceID.ToText(id));
      END;
      IF NOT self.spaceTbl.get(id, conn) THEN
        SharedObjError.RaiseError(Atom.FromText("Space not connected"));
      END;
      IF GetObjInfo(wrep, objInfo) THEN
        SharedObjError.RaiseError(
            Atom.FromText("Object already exists in this space"));
      END;
      objInfo := RegisterObject(NIL, wrep, NIL, FALSE, seqNo);
    END;
    LOCK objInfo DO
      IF debug_level >= 1 THEN
        debug.print(1, "SpaceNewObject: new object info " & 
        ObjectInfo.ToText(objInfo));
      END;
      AddClient(objInfo, conn);
    END;
  END SpaceNewObject;

PROCEDURE SpaceNewCopy (self: T; id   : EventSpaceID.T;
                        seq  : ObjectSpace.T; wrep : WireRep;
                        seqNo: SharedObj.SequenceNumber): 
  SharedObj.SequenceNumber RAISES {SharedObj.Error} =
  VAR
    conn: SpaceConn.T;
    objInfo: ObjectInfo.T;
  BEGIN
    LOCK objTblMu DO
      IF debug_level >= 1 THEN
        debug.print(1, "SpaceNewcopy: new copy of " &EventWireRep.ToText(wrep)&
        " in space " & EventSpaceID.ToText(id));
      END;
      IF NOT self.spaceTbl.get(id, conn) THEN
        SharedObjError.RaiseError(Atom.FromText("Space not connected"));
      END;
      IF seq = self THEN
        IF debug_level >= 5 THEN
          debug.print(5, "SpaceNewcopy: we are the sequencer");
        END;
        IF NOT GetObjInfo(wrep, objInfo) THEN
          SharedObjError.RaiseError(Atom.FromText("Object doesn't exist"));
        END;
      ELSE
        (* Foreign sequencer! *)
        VAR seqConn : SpaceConn.T;
        BEGIN
          TRY
            IF debug_level >= 5 THEN
              debug.print(5, "SpaceNewcopy: getting id of sequencer.");
            END;
            seqConn := localSpace.newSpace(seq);
            IF debug_level >= 5 THEN
              debug.print(5, "SpaceNewcopy: sequencer conn is " &
              SpaceConn.ToText(seqConn));
            END;
            (* If we haven't seen this object before, register a new
              copy!  We will set our local seqNo to be the same as the
              sequencer, in that case, since we don't have a copy. *)
            IF NOT GetObjInfo(wrep, objInfo) THEN
              IF debug_level >= 5 THEN
                debug.print(5, "SpaceNewcopy: new for us. Register it.");
              END;
              objInfo := RegisterObject(NIL, wrep, seqConn, FALSE, seqNo);
              seqNo := seq.newCopy (self.space(), seq, wrep, seqNo);
            END;
          EXCEPT
          | NetObj.Error(ec) => SequencerFailed(seqConn, ec);
          | Thread.Alerted => SequencerFailed(seqConn, NIL);
          END;
        END;
      END;

      IF debug_level >= 5 THEN
        debug.print(5, "SpaceNewcopy: object info " &
          ObjectInfo.ToText(objInfo));
      END;

      AddClient(objInfo, conn);
      (* We make a new one, in case the client is local and they screw
         around with it after they get it! *)
      IF debug_level >= 5 THEN
        debug.print(5, "SpaceNewcopy: done.  Return seqNo " &
        objInfo.seqNo.fmt(10));
      END;
    END;

    LOCK objInfo DO
        WITH seqNo = EventNumber.New(objInfo.seqNo) DO
          RETURN seqNo;
        END;
     END;
  END SpaceNewCopy;

PROCEDURE SpaceDeleteCopy (self: T; id: EventSpaceID.T;
                           wrep: WireRep) 
                    RAISES {SharedObj.Error} =
  VAR
    conn: SpaceConn.T;
    objInfo: ObjectInfo.T;
  BEGIN
    LOCK objTblMu DO
      IF debug_level >= 1 THEN
        debug.print(1, "SpaceDeleteCopy: del copy of " & 
        EventWireRep.ToText(wrep) & " in space " & EventSpaceID.ToText(id));
      END;
      IF NOT self.spaceTbl.get(id, conn) THEN
        SharedObjError.RaiseError(Atom.FromText("Space not connected"));
      END;
      IF NOT GetObjInfo(wrep, objInfo) THEN
        SharedObjError.RaiseError(Atom.FromText("Object doesn't exist"));
      END;
    END;

    LOCK objInfo DO
      DeleteClient(objInfo, conn);
      IF debug_level >= 5 THEN
        debug.print(5, "SpaceDeleteCopy: done.");
      END;
    END;
  END SpaceDeleteCopy;

PROCEDURE SpaceGet (<*UNUSED*>self: T; obj: WireRep;
                    seqNo: SharedObj.SequenceNumber): SharedObj.T
             RAISES {SharedObj.Error} =
  VAR 
    objInfo: ObjectInfo.T;
    objRef: SharedObj.T := NIL;
  BEGIN
    IF debug_level >= 1 THEN
      debug.print(1, "SpaceGet: get obj " & EventWireRep.ToText(obj) & 
      " with seqNo >= " & seqNo.fmt(10));
    END;
    LOCK objTblMu DO
      IF NOT GetObjInfo(obj, objInfo) THEN
        SharedObjError.RaiseError(Atom.FromText("Object doesn't exist"));
      END;
    END;
   
    IF debug_level >= 5 THEN
      debug.print(5, "SpaceGet: found objinfo.  getting obj.");
    END;
    LOCK objInfo DO
      objRef := GetObjRef(objInfo);
      IF objRef = NIL THEN
        (* Otherwise, whine and complain?  Why do they always as me for
           everything, anyway???  I don't know _everything_.  Ask
           someone who has the object, for crying out loud! *)
        SharedObjError.RaiseError(Atom.FromText("Real object doesn't exist."));
      END;
    END;
    IF debug_level >= 5 THEN
      debug.print(5, "SpaceGet: found obj. waiting for new enough copy");
    END;

    (* Our sequencer will eventually send us the update to get us to
       this seqNo if we aren't there.  So, just wait!  If we are the
       sequencer, this should only happen if one of our clients owns
       the object.  If we are already up to this seqNo, this will
       return immediately.  It's ok to block with the space locked, I
       think.  The only way we get here is because of a critical race
       with passing objects and update events, so it will rarely
       happen. *)

    objRef.seqNoCnt.wait(seqNo);

    (* Now, make sure we have the most recent object reference. *)
    LOCK objInfo DO
      objRef := GetObjRef(objInfo);
      IF objRef = NIL THEN
        (* Really whine and complain!!  We had it and now it's gone!!
           Yikes!! *)
        SharedObjError.RaiseError(
            Atom.FromText("Real object existed by no longer exists!"));
      END;
    END;
    IF debug_level >= 5 THEN
      debug.print(5, "SpaceGet: returning new enough copy");
    END;
    RETURN objRef;
  END SpaceGet;
 
PROCEDURE SpaceFindObject (<*UNUSED*>self: T; wrep: SharedObjRep.WireRep; 
                           cbobj: ObjectSpace.FindObjCallBack) 
  RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error} =
  VAR
    objInfo: ObjectInfo.T;
    copy: ObjCopy.T;
    count := 0;
    seqNo : SharedObj.SequenceNumber;
  BEGIN
    LOCK objTblMu DO
      IF debug_level >= 1 THEN
        debug.print(1, "SpaceFindObject: find a space with obj " & 
          EventWireRep.ToText(wrep));
      END;
      (* We don't have the object, so we can't help! *)
      IF NOT GetObjInfo(wrep, objInfo) THEN
        SharedObjError.RaiseError(
            Atom.FromText("No copy of object in this sequencer"));
      END;
    END;

    (* tell them to ask for the object from each client in turn till
       one of the clients actually has it.  In all likelihood, the
       first client will respond. *)
    
    LOOP
      LOCK objInfo DO
        IF count >= ObjCopyList.Length(objInfo.clients) THEN
          SharedObjError.RaiseError(
              Atom.FromText("Tried all known clients"));
        END;
        copy := ObjCopyList.Nth(objInfo.clients, count);

        TRY
          seqNo := NEW(SharedObj.SequenceNumber).init(objInfo.seqNo);
          seqNo.dec();
        EXCEPT
        | EventNumber.Overflow => 
          Process.Crash("Event Number Underflowed!");
        END;
      END;

      TRY
        cbobj.try(seqNo, copy.conn.objSpace);
    
        IF debug_level >= 5 THEN
          debug.print(5, "SpaceFindObject: successfull found the object.");
        END;
        RETURN;
      EXCEPT
      | SharedObj.Error => 
      END;
      INC(count);
    END;
  END SpaceFindObject;

PROCEDURE SpaceLastCopy (<*UNUSED*>self: T; <*UNUSED*>wrep: WireRep; 
                         <*UNUSED*>seqNo: SharedObj.SequenceNumber) 
                  RAISES {SharedObj.Error} =
  BEGIN
    (* this will be obscenely hard to implement correctly, as messages
       can be flying around all over the system. Might do someday, but
       not now *)
    SharedObjError.RaiseError(Atom.FromText("Not implemented yet!!!"));
  END SpaceLastCopy;

PROCEDURE  SpaceSetDfltSequencer (<*UNUSED*>self: T; space: ObjectSpace.T)
  RAISES {SharedObj.Error} = 
  BEGIN
    SharedObjRT.SetDfltSequencer(space);
  END SpaceSetDfltSequencer;

PROCEDURE SpaceGetDfltSequencer(<*UNUSED*>self: T): ObjectSpace.T 
  RAISES {Thread.Alerted} =
  BEGIN
    WaitForSequencer();
    RETURN GetDfltSequencer();
  END SpaceGetDfltSequencer;

PROCEDURE SpaceSpace (<*UNUSED*> self: T): EventSpaceID.T =
  BEGIN
    RETURN EventSpaceID.Mine();
  END SpaceSpace; 

PROCEDURE SpaceEndpoint (self: T): IP.Endpoint =
  BEGIN
    RETURN self.ipep;
  END SpaceEndpoint;

PROCEDURE SpaceGetSequencer(<*UNUSED*> self: T; wrep: WireRep): ObjectSpace.T
  RAISES {SharedObj.Error} = 
  BEGIN
    RETURN GetSequencer(wrep);
  END SpaceGetSequencer;

PROCEDURE PrintState (self: T) =
  VAR 
    keySp: EventSpaceID.T;
    valSp: SpaceConn.T;
  BEGIN
    (* Print out the number of objects *)
    IO.Put("Shared Object Runtime State.\n");
    IO.Put("----------------------------\n");
    LOCK objTblMu DO
      IO.Put("Known Spaces: " & Fmt.Int(self.spaceTbl.size()) & "\n");
      WITH it = self.spaceTbl.iterate() DO
        WHILE it.next(keySp, valSp) DO
          IO.Put("  " & SpaceConn.ToText(valSp) & "\n");
        END;
      END;
      IO.Put("\n");
    END;
    IO.Put(ObjTblToText());
  END PrintState;

BEGIN
  localSpace := NEW(T).init();
END LocalObjectSpace.
