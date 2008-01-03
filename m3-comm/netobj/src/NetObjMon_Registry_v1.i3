(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE NetObjMon_Registry_v1;

IMPORT Wr, Thread, StubLib, NetObj, Rd, NetObjMon;
TYPE Surrogate_NetObjMon_Registry = NetObjMon.Registry OBJECT
      OVERRIDES
        register := Surrogate_register;
        list := Surrogate_list;
        get := Surrogate_get;
      END;

PROCEDURE Surrogate_register(
    self: NetObjMon.Registry;
    t: NetObjMon.T;
    id: TEXT) RAISES {NetObj.Error, Thread.Alerted};

PROCEDURE Surrogate_list(self: NetObjMon.Registry): REF ARRAY  OF TEXT
     RAISES {NetObj.Error, Thread.Alerted};

PROCEDURE Surrogate_get(self: NetObjMon.Registry; id: TEXT): NetObjMon.T
     RAISES {NetObj.Error, Thread.Alerted};

PROCEDURE Stub_register(
    self: NetObjMon.Registry;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted};

PROCEDURE Stub_list(
    self: NetObjMon.Registry;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted};

PROCEDURE Stub_get(
    self: NetObjMon.Registry;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted};

END NetObjMon_Registry_v1.
