MODULE ObjectSpace_T_v1 EXPORTS ObjectSpace, ObjectSpace_T_v1;

IMPORT IP, Rd, EventNumber, StubLib, SharedObjRep, EventWireRep, Wr,
       SharedObj, NetObj, AtomList, Thread, Fingerprint, ObjectSpace;
CONST Protocol: StubLib.StubProtocol = 1;

TYPE
      Methods = {findObj, get, lastCopy, deleteCopy, newCopy, newObject,
        disconnect, connect, endpoint, space, getSequencer,
        getDfltSequencer, setDfltSequencer};
      ReturnCodes = {OK, SharedObj_Error};

  PROCEDURE Surrogate_setDfltSequencer(
      self: ObjectSpace.T;
      seq_arg: ObjectSpace.T) RAISES {SharedObj.Error, NetObj.Error,
      Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.setDfltSequencer));
          StubLib.OutRef(c, seq_arg);
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
    END Surrogate_setDfltSequencer;

  PROCEDURE Surrogate_getDfltSequencer(self: ObjectSpace.T): ObjectSpace.T
       RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: ObjectSpace.T;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.getDfltSequencer));
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            res := StubLib.InRef(c, rep, TYPECODE(ObjectSpace.T));
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
      RETURN res;
    END Surrogate_getDfltSequencer;

  PROCEDURE Surrogate_getSequencer(
      self: ObjectSpace.T;
      wrep_arg: EventWireRep.T): ObjectSpace.T RAISES {SharedObj.Error,
      NetObj.Error, Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: ObjectSpace.T;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.getSequencer));
          dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
              StubLib.OutInteger(c, wrep_arg.byte[i0]);
              END;
            END;
            rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            res := StubLib.InRef(c, rep, TYPECODE(ObjectSpace.T));
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
      RETURN res;
    END Surrogate_getSequencer;

  PROCEDURE Surrogate_space(self: ObjectSpace.T): Fingerprint.T
       RAISES {NetObj.Error, Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: Fingerprint.T;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.space));
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
              res.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
              END;
            reuse := TRUE;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
      RETURN res;
    END Surrogate_space;

  PROCEDURE Surrogate_endpoint(self: ObjectSpace.T): IP.Endpoint
       RAISES {NetObj.Error, Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: IP.Endpoint;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.endpoint));
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            FOR i0 := FIRST([0..3]) TO LAST([0..3]) DO
              res.addr.a[i0] := StubLib.InInteger(c, rep, 0, 255);
              END;
            res.port := StubLib.InInteger(c, rep, 0, 65535);
            reuse := TRUE;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
      RETURN res;
    END Surrogate_endpoint;

  PROCEDURE Surrogate_connect(self: ObjectSpace.T; from_arg: ObjectSpace.T)
       RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.connect));
          StubLib.OutRef(c, from_arg);
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
    END Surrogate_connect;

  PROCEDURE Surrogate_disconnect(
      self: ObjectSpace.T;
      id_arg: Fingerprint.T) RAISES {Thread.Alerted, NetObj.Error,
      SharedObj.Error} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.disconnect));
          dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
              StubLib.OutInteger(c, id_arg.byte[i0]);
              END;
            END;
            rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
    END Surrogate_disconnect;

  PROCEDURE Surrogate_newObject(
      self: ObjectSpace.T;
      id_arg: Fingerprint.T;
      wrep_arg: EventWireRep.T;
      seqNo_arg: EventNumber.T) RAISES {SharedObj.Error, NetObj.Error,
      Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.newObject));
          dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
              StubLib.OutInteger(c, id_arg.byte[i0]);
              END;
            END;
            dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
              StubLib.OutInteger(c, wrep_arg.byte[i0]);
              END;
            END;
            StubLib.OutRef(c, seqNo_arg);
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
    END Surrogate_newObject;

  PROCEDURE Surrogate_newCopy(
      self: ObjectSpace.T;
      id_arg: Fingerprint.T;
      seq_arg: ObjectSpace.T;
      wrep_arg: EventWireRep.T;
      seqNo_arg: EventNumber.T): EventNumber.T RAISES {SharedObj.Error,
      NetObj.Error, Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: EventNumber.T;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.newCopy));
          dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
              StubLib.OutInteger(c, id_arg.byte[i0]);
              END;
            END;
            StubLib.OutRef(c, seq_arg);
          dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
              StubLib.OutInteger(c, wrep_arg.byte[i0]);
              END;
            END;
            StubLib.OutRef(c, seqNo_arg);
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            res := StubLib.InRef(c, rep, TYPECODE(EventNumber.T));
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
      RETURN res;
    END Surrogate_newCopy;

  PROCEDURE Surrogate_deleteCopy(
      self: ObjectSpace.T;
      id_arg: Fingerprint.T;
      wrep_arg: EventWireRep.T) RAISES {SharedObj.Error, NetObj.Error,
      Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.deleteCopy));
          dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
              StubLib.OutInteger(c, id_arg.byte[i0]);
              END;
            END;
            dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
              StubLib.OutInteger(c, wrep_arg.byte[i0]);
              END;
            END;
            rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
    END Surrogate_deleteCopy;

  PROCEDURE Surrogate_lastCopy(
      self: ObjectSpace.T;
      wrep_arg: EventWireRep.T;
      seqNo_arg: EventNumber.T) RAISES {SharedObj.Error, NetObj.Error,
      Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.lastCopy));
          dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
              StubLib.OutInteger(c, wrep_arg.byte[i0]);
              END;
            END;
            StubLib.OutRef(c, seqNo_arg);
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
    END Surrogate_lastCopy;

  PROCEDURE Surrogate_get(
      self: ObjectSpace.T;
      obj_arg: EventWireRep.T;
      seqNo_arg: EventNumber.T): SharedObj.T RAISES {Thread.Alerted,
      NetObj.Error, SharedObj.Error} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: SharedObj.T;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.get));
          dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
              StubLib.OutInteger(c, obj_arg.byte[i0]);
              END;
            END;
            StubLib.OutRef(c, seqNo_arg);
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            res := StubLib.InRef(c, rep, TYPECODE(SharedObj.T));
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
      RETURN res;
    END Surrogate_get;

  PROCEDURE Surrogate_findObj(
      self: ObjectSpace.T;
      obj_arg: EventWireRep.T;
      cbobj_arg: ObjectSpace.FindObjCallBack) RAISES {Thread.Alerted,
      NetObj.Error, SharedObj.Error} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.findObj));
          dataPresent := TRUE;
          StubLib.OutBoolean(c, dataPresent);
          IF dataPresent THEN
            FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
              StubLib.OutInteger(c, obj_arg.byte[i0]);
              END;
            END;
            StubLib.OutRef(c, cbobj_arg);
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InInt32(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            reuse := TRUE;
          | ORD(ReturnCodes.SharedObj_Error) =>
            VAR arg: AtomList.T;
            BEGIN
              arg := StubLib.InRef(c, rep, TYPECODE(AtomList.T));
              reuse := TRUE;
              RAISE SharedObj.Error(arg);

            END;
          ELSE
            StubLib.RaiseUnmarshalFailure();
          END
        FINALLY
          StubLib.EndCall(c, reuse);
        END;

      EXCEPT
      | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
      | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
      END;
    END Surrogate_findObj;

PROCEDURE Invoke(
    c: StubLib.Conn;
    obj: NetObj.T;
    rep: StubLib.DataRep;
    stubProt: StubLib.StubProtocol)
    RAISES {NetObj.Error, Rd.Failure,
            Wr.Failure, Thread.Alerted} =
  VAR t := NARROW(obj, ObjectSpace.T);
  BEGIN
    IF stubProt # Protocol THEN StubLib.RaiseUnmarshalFailure() END;
    TRY
      CASE StubLib.InInt32(c, rep) OF
      | ORD(Methods.setDfltSequencer) => Stub_setDfltSequencer(t, c, rep);
      | ORD(Methods.getDfltSequencer) => Stub_getDfltSequencer(t, c, rep);
      | ORD(Methods.getSequencer) => Stub_getSequencer(t, c, rep);
      | ORD(Methods.space) => Stub_space(t, c, rep);
      | ORD(Methods.endpoint) => Stub_endpoint(t, c, rep);
      | ORD(Methods.connect) => Stub_connect(t, c, rep);
      | ORD(Methods.disconnect) => Stub_disconnect(t, c, rep);
      | ORD(Methods.newObject) => Stub_newObject(t, c, rep);
      | ORD(Methods.newCopy) => Stub_newCopy(t, c, rep);
      | ORD(Methods.deleteCopy) => Stub_deleteCopy(t, c, rep);
      | ORD(Methods.lastCopy) => Stub_lastCopy(t, c, rep);
      | ORD(Methods.get) => Stub_get(t, c, rep);
      | ORD(Methods.findObj) => Stub_findObj(t, c, rep);
      ELSE
        StubLib.RaiseUnmarshalFailure();
      END;
    EXCEPT
    | SharedObj.Error(arg) =>
        StubLib.StartResult(c);
        StubLib.OutInt32(c, ORD(ReturnCodes.SharedObj_Error));
        StubLib.OutRef(c, arg);
    END;
  END Invoke;

PROCEDURE Stub_setDfltSequencer(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR seq_arg: ObjectSpace.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    seq_arg := StubLib.InRef(c, rep, TYPECODE(ObjectSpace.T));
    self.setDfltSequencer(seq_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));

  END Stub_setDfltSequencer;

PROCEDURE Stub_getDfltSequencer(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR res: ObjectSpace.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    res := self.getDfltSequencer();
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));
    StubLib.OutRef(c, res);

  END Stub_getDfltSequencer;

PROCEDURE Stub_getSequencer(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR wrep_arg: EventWireRep.T;
      res: ObjectSpace.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
        wrep_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      res := self.getSequencer(wrep_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));
    StubLib.OutRef(c, res);

  END Stub_getSequencer;

PROCEDURE Stub_space(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted}=
  VAR res: Fingerprint.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    res := self.space();
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));
    FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
      StubLib.OutInteger(c, res.byte[i0]);
      END;

  END Stub_space;

PROCEDURE Stub_endpoint(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted}=
  VAR res: IP.Endpoint;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    res := self.endpoint();
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));
    FOR i0 := FIRST([0..3]) TO LAST([0..3]) DO
      StubLib.OutInteger(c, res.addr.a[i0]);
      END;
    StubLib.OutInteger(c, res.port);

  END Stub_endpoint;

PROCEDURE Stub_connect(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR from_arg: ObjectSpace.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    from_arg := StubLib.InRef(c, rep, TYPECODE(ObjectSpace.T));
    self.connect(from_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));

  END Stub_connect;

PROCEDURE Stub_disconnect(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR id_arg: Fingerprint.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
        id_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      self.disconnect(id_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));

  END Stub_disconnect;

PROCEDURE Stub_newObject(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR id_arg: Fingerprint.T;
      wrep_arg: EventWireRep.T;
      seqNo_arg: EventNumber.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
        id_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
        wrep_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      seqNo_arg := StubLib.InRef(c, rep, TYPECODE(EventNumber.T));
    self.newObject(id_arg, wrep_arg, seqNo_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));

  END Stub_newObject;

PROCEDURE Stub_newCopy(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR id_arg: Fingerprint.T;
      seq_arg: ObjectSpace.T;
      wrep_arg: EventWireRep.T;
      seqNo_arg: EventNumber.T;
      res: EventNumber.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
        id_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      seq_arg := StubLib.InRef(c, rep, TYPECODE(ObjectSpace.T));
    dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
        wrep_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      seqNo_arg := StubLib.InRef(c, rep, TYPECODE(EventNumber.T));
    res := self.newCopy(id_arg, seq_arg, wrep_arg, seqNo_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));
    StubLib.OutRef(c, res);

  END Stub_newCopy;

PROCEDURE Stub_deleteCopy(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR id_arg: Fingerprint.T;
      wrep_arg: EventWireRep.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..7]) TO LAST([0..7]) DO
        id_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
        wrep_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      self.deleteCopy(id_arg, wrep_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));

  END Stub_deleteCopy;

PROCEDURE Stub_lastCopy(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR wrep_arg: EventWireRep.T;
      seqNo_arg: EventNumber.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
        wrep_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      seqNo_arg := StubLib.InRef(c, rep, TYPECODE(EventNumber.T));
    self.lastCopy(wrep_arg, seqNo_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));

  END Stub_lastCopy;

PROCEDURE Stub_get(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR obj_arg: EventWireRep.T;
      seqNo_arg: EventNumber.T;
      res: SharedObj.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
        obj_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      seqNo_arg := StubLib.InRef(c, rep, TYPECODE(EventNumber.T));
    res := self.get(obj_arg, seqNo_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));
    StubLib.OutRef(c, res);

  END Stub_get;

PROCEDURE Stub_findObj(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR obj_arg: EventWireRep.T;
      cbobj_arg: ObjectSpace.FindObjCallBack;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    dataPresent := StubLib.InBoolean(c);
    IF dataPresent THEN
      FOR i0 := FIRST([0..15]) TO LAST([0..15]) DO
        obj_arg.byte[i0] := StubLib.InInteger(c, rep, 0, 255);
        END;
      END;
      cbobj_arg := StubLib.InRef(c, rep, TYPECODE(ObjectSpace.FindObjCallBack));
    self.findObj(obj_arg, cbobj_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));

  END Stub_findObj;

BEGIN
  StubLib.Register(TYPECODE(ObjectSpace.T), 1, TYPECODE(Surrogate_ObjectSpace_T), Invoke);
END ObjectSpace_T_v1.
