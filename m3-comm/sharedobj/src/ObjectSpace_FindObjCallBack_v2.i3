INTERFACE ObjectSpace_FindObjCallBack_v2;

IMPORT Rd, StubLib, EventNumber, Wr, SharedObj, NetObj, Thread,
       ObjectSpace;
TYPE Surrogate_ObjectSpace_FindObjCallBack = ObjectSpace.FindObjCallBack OBJECT
      OVERRIDES
        try := Surrogate_try;
      END;

PROCEDURE Surrogate_try(
    self: ObjectSpace.FindObjCallBack;
    seqNo: EventNumber.T;
    space: ObjectSpace.T) RAISES {Thread.Alerted, NetObj.Error,
    SharedObj.Error};

PROCEDURE Stub_try(
    self: ObjectSpace.FindObjCallBack;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted, SharedObj.Error};

END ObjectSpace_FindObjCallBack_v2.
