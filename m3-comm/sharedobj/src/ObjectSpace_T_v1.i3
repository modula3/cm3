INTERFACE ObjectSpace_T_v1;

IMPORT Rd, IP, StubLib, EventNumber, SharedObjRep, Wr, EventWireRep,
       NetObj, SharedObj, Fingerprint, Thread, ObjectSpace;
TYPE Surrogate_ObjectSpace_T = ObjectSpace.T OBJECT
      OVERRIDES
        setDfltSequencer := Surrogate_setDfltSequencer;
        getDfltSequencer := Surrogate_getDfltSequencer;
        getSequencer := Surrogate_getSequencer;
        space := Surrogate_space;
        endpoint := Surrogate_endpoint;
        connect := Surrogate_connect;
        disconnect := Surrogate_disconnect;
        newObject := Surrogate_newObject;
        newCopy := Surrogate_newCopy;
        deleteCopy := Surrogate_deleteCopy;
        lastCopy := Surrogate_lastCopy;
        get := Surrogate_get;
        findObj := Surrogate_findObj;
      END;

PROCEDURE Surrogate_setDfltSequencer(
    self: ObjectSpace.T;
    seq: ObjectSpace.T) RAISES {SharedObj.Error, NetObj.Error,
    Thread.Alerted};

PROCEDURE Surrogate_getDfltSequencer(self: ObjectSpace.T): ObjectSpace.T
     RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};

PROCEDURE Surrogate_getSequencer(self: ObjectSpace.T; wrep: EventWireRep.T)
    : ObjectSpace.T RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};

PROCEDURE Surrogate_space(self: ObjectSpace.T): Fingerprint.T
     RAISES {NetObj.Error, Thread.Alerted};

PROCEDURE Surrogate_endpoint(self: ObjectSpace.T): IP.Endpoint
     RAISES {NetObj.Error, Thread.Alerted};

PROCEDURE Surrogate_connect(self: ObjectSpace.T; from: ObjectSpace.T)
     RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error};

PROCEDURE Surrogate_disconnect(self: ObjectSpace.T; id: Fingerprint.T)
     RAISES {Thread.Alerted, NetObj.Error, SharedObj.Error};

PROCEDURE Surrogate_newObject(
    self: ObjectSpace.T;
    id: Fingerprint.T;
    wrep: EventWireRep.T;
    seqNo: EventNumber.T) RAISES {SharedObj.Error, NetObj.Error,
    Thread.Alerted};

PROCEDURE Surrogate_newCopy(
    self: ObjectSpace.T;
    id: Fingerprint.T;
    seq: ObjectSpace.T;
    wrep: EventWireRep.T;
    seqNo: EventNumber.T): EventNumber.T RAISES {SharedObj.Error,
    NetObj.Error, Thread.Alerted};

PROCEDURE Surrogate_deleteCopy(
    self: ObjectSpace.T;
    id: Fingerprint.T;
    wrep: EventWireRep.T) RAISES {SharedObj.Error, NetObj.Error,
    Thread.Alerted};

PROCEDURE Surrogate_lastCopy(
    self: ObjectSpace.T;
    wrep: EventWireRep.T;
    seqNo: EventNumber.T) RAISES {SharedObj.Error, NetObj.Error,
    Thread.Alerted};

PROCEDURE Surrogate_get(
    self: ObjectSpace.T;
    obj: EventWireRep.T;
    seqNo: EventNumber.T): SharedObj.T RAISES {Thread.Alerted,
    NetObj.Error, SharedObj.Error};

PROCEDURE Surrogate_findObj(
    self: ObjectSpace.T;
    obj: EventWireRep.T;
    cbobj: ObjectSpace.FindObjCallBack) RAISES {Thread.Alerted,
    NetObj.Error, SharedObj.Error};

PROCEDURE Stub_setDfltSequencer(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_getDfltSequencer(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_getSequencer(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_space(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted};

PROCEDURE Stub_endpoint(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted};

PROCEDURE Stub_connect(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_disconnect(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_newObject(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_newCopy(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_deleteCopy(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_lastCopy(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_get(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

PROCEDURE Stub_findObj(
    self: ObjectSpace.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

END ObjectSpace_T_v1.
