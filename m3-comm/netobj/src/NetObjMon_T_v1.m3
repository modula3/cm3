(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE NetObjMon_T_v1 EXPORTS NetObjMon, NetObjMon_T_v1, NetObjMonInit;

IMPORT RefList, Wr, Thread, StubLib, NetObj, Rd, NetObjMon;
CONST Protocol: StubLib.StubProtocol = 1;

TYPE
      Methods = {dumpNames, dump};
      ReturnCodes = {OK};

  PROCEDURE Surrogate_dump(self: NetObjMon.T): REFANY RAISES {NetObj.Error,
      Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: REFANY;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutCardinal(c, ORD(Methods.dump));
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InCardinal(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            res := StubLib.InRef(c, rep, -1);
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
    END Surrogate_dump;

  PROCEDURE Surrogate_dumpNames(self: NetObjMon.T): RefList.T
       RAISES {NetObj.Error, Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: RefList.T;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutCardinal(c, ORD(Methods.dumpNames));
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InCardinal(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            res := StubLib.InRef(c, rep, TYPECODE(RefList.T));
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
    END Surrogate_dumpNames;

PROCEDURE Invoke(
    c: StubLib.Conn;
    obj: NetObj.T;
    rep: StubLib.DataRep;
    stubProt: StubLib.StubProtocol)
    RAISES {NetObj.Error, Rd.Failure,
            Wr.Failure, Thread.Alerted} =
  VAR t := NARROW(obj, NetObjMon.T);
  BEGIN
    IF stubProt # Protocol THEN StubLib.RaiseUnmarshalFailure() END;
    TRY
      CASE StubLib.InCardinal(c, rep) OF
      | ORD(Methods.dump) => Stub_dump(t, c, rep);
      | ORD(Methods.dumpNames) => Stub_dumpNames(t, c, rep);
      ELSE
        StubLib.RaiseUnmarshalFailure();
      END;
    EXCEPT
    END;
  END Invoke;

PROCEDURE Stub_dump(
    self: NetObjMon.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted}=
  VAR res: REFANY;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    res := self.dump();
    StubLib.StartResult(c);
    StubLib.OutCardinal(c, ORD(ReturnCodes.OK));
    StubLib.OutRef(c, res);

  END Stub_dump;

PROCEDURE Stub_dumpNames(
    self: NetObjMon.T;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted}=
  VAR res: RefList.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    res := self.dumpNames();
    StubLib.StartResult(c);
    StubLib.OutCardinal(c, ORD(ReturnCodes.OK));
    StubLib.OutRef(c, res);

  END Stub_dumpNames;

PROCEDURE InitMonitorStubs() =
  BEGIN
    StubLib.Register(TYPECODE(NetObjMon.T), 1, TYPECODE(Surrogate_NetObjMon_T), Invoke);
  END InitMonitorStubs;

BEGIN
END NetObjMon_T_v1.
