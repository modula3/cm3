(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE NetObjMon_Registry_v1 EXPORTS NetObjMon, NetObjMon_Registry_v1,
                                                NetObjMonInit;

IMPORT Wr, Thread, StubLib, NetObj, Rd, NetObjMon;
CONST Protocol: StubLib.StubProtocol = 1;

TYPE
      Methods = {get, list, register};
      ReturnCodes = {OK};

  PROCEDURE Surrogate_register(
      self: NetObjMon.Registry;
      t_arg: NetObjMon.T;
      id_arg: TEXT) RAISES {NetObj.Error, Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutCardinal(c, ORD(Methods.register));
          StubLib.OutRef(c, t_arg);
          StubLib.OutRef(c, id_arg);
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InCardinal(c, rep) OF
          | ORD(ReturnCodes.OK) =>
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
    END Surrogate_register;

  PROCEDURE Surrogate_list(self: NetObjMon.Registry): REF ARRAY  OF TEXT
       RAISES {NetObj.Error, Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: REF ARRAY  OF TEXT;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutCardinal(c, ORD(Methods.list));
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
    END Surrogate_list;

  PROCEDURE Surrogate_get(self: NetObjMon.Registry; id_arg: TEXT)
      : NetObjMon.T RAISES {NetObj.Error, Thread.Alerted} =

    VAR reuse := FALSE;
        rep: StubLib.DataRep;
        c: StubLib.Conn;
        dataPresent: BOOLEAN; <* NOWARN *>
        res: NetObjMon.T;

    BEGIN
      TRY
        c := StubLib.StartCall(self, Protocol);
        TRY
          StubLib.OutCardinal(c, ORD(Methods.get));
          StubLib.OutRef(c, id_arg);
          rep := StubLib.AwaitResult(c);
          CASE StubLib.InCardinal(c, rep) OF
          | ORD(ReturnCodes.OK) =>
            res := StubLib.InRef(c, rep, TYPECODE(NetObjMon.T));
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
    END Surrogate_get;

PROCEDURE Invoke(
    c: StubLib.Conn;
    obj: NetObj.T;
    rep: StubLib.DataRep;
    stubProt: StubLib.StubProtocol)
    RAISES {NetObj.Error, Rd.Failure,
            Wr.Failure, Thread.Alerted} =
  VAR t := NARROW(obj, NetObjMon.Registry);
  BEGIN
    IF stubProt # Protocol THEN StubLib.RaiseUnmarshalFailure() END;
    TRY
      CASE StubLib.InCardinal(c, rep) OF
      | ORD(Methods.register) => Stub_register(t, c, rep);
      | ORD(Methods.list) => Stub_list(t, c, rep);
      | ORD(Methods.get) => Stub_get(t, c, rep);
      ELSE
        StubLib.RaiseUnmarshalFailure();
      END;
    EXCEPT
    END;
  END Invoke;

PROCEDURE Stub_register(
    self: NetObjMon.Registry;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted}=
  VAR t_arg: NetObjMon.T;
      id_arg: TEXT;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    t_arg := StubLib.InRef(c, rep, TYPECODE(NetObjMon.T));
    id_arg := StubLib.InRef(c, rep, -1);
    self.register(t_arg, id_arg);
    StubLib.StartResult(c);
    StubLib.OutCardinal(c, ORD(ReturnCodes.OK));

  END Stub_register;

PROCEDURE Stub_list(
    self: NetObjMon.Registry;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted}=
  VAR res: REF ARRAY  OF TEXT;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    res := self.list();
    StubLib.StartResult(c);
    StubLib.OutCardinal(c, ORD(ReturnCodes.OK));
    StubLib.OutRef(c, res);

  END Stub_list;

PROCEDURE Stub_get(
    self: NetObjMon.Registry;
    c: StubLib.Conn;
    <* NOWARN *> rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure,
    Wr.Failure, Thread.Alerted}=
  VAR id_arg: TEXT;
      res: NetObjMon.T;
      dataPresent: BOOLEAN <* NOWARN *>;

  BEGIN
    id_arg := StubLib.InRef(c, rep, -1);
    res := self.get(id_arg);
    StubLib.StartResult(c);
    StubLib.OutCardinal(c, ORD(ReturnCodes.OK));
    StubLib.OutRef(c, res);

  END Stub_get;

PROCEDURE InitRegistryStubs() =
  BEGIN
    StubLib.Register(TYPECODE(NetObjMon.Registry), 1, TYPECODE(Surrogate_NetObjMon_Registry), Invoke);
  END InitRegistryStubs;

BEGIN
END NetObjMon_Registry_v1.
