MODULE ObjectSpace_FindObjCallBack_v1 EXPORTS ObjectSpace, ObjectSpace_FindObjCallBack_v1;

IMPORT Rd, StubLib, EventNumber, Wr, SharedObj, NetObj, AtomList, Thread,
       ObjectSpace;
CONST Protocol: StubLib.StubProtocol = 1;

TYPE
      Methods = {try};
      ReturnCodes = {OK, SharedObj_Error};

  PROCEDURE Surrogate_try(
      self: ObjectSpace.FindObjCallBack;
      seqNo_arg: EventNumber.T;
      space_arg: ObjectSpace.T) RAISES {Thread.Alerted, NetObj.Error,
      SharedObj.Error} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutInt32(c, ORD(Methods.try));
          StubLib.OutRef(c, seqNo_arg);
          StubLib.OutRef(c, space_arg);
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
    END Surrogate_try;

PROCEDURE Invoke(
    c: StubLib.Conn;
    obj: NetObj.T;
    rep: StubLib.DataRep;
    stubProt: StubLib.StubProtocol)
    RAISES {NetObj.Error, Rd.Failure,
            Wr.Failure, Thread.Alerted} =
  VAR t := NARROW(obj, ObjectSpace.FindObjCallBack);
  BEGIN
    IF stubProt # Protocol THEN StubLib.RaiseUnmarshalFailure() END;
    TRY
      CASE StubLib.InInt32(c, rep) OF
      | ORD(Methods.try) => Stub_try(t, c, rep);
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

PROCEDURE Stub_try(
    self: ObjectSpace.FindObjCallBack;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted, SharedObj.Error}=
  VAR seqNo_arg: EventNumber.T;
      space_arg: ObjectSpace.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    seqNo_arg := StubLib.InRef(c, rep, TYPECODE(EventNumber.T));
    space_arg := StubLib.InRef(c, rep, TYPECODE(ObjectSpace.T));
    self.try(seqNo_arg, space_arg);
    StubLib.StartResult(c);
    StubLib.OutInt32(c, ORD(ReturnCodes.OK));

  END Stub_try;

BEGIN
  StubLib.Register(TYPECODE(ObjectSpace.FindObjCallBack), 1, TYPECODE(Surrogate_ObjectSpace_FindObjCallBack), Invoke);
END ObjectSpace_FindObjCallBack_v1.
