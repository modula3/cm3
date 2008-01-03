(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE NetObjMon_T_v1;

IMPORT RefList, Wr, Thread, StubLib, NetObj, Rd, NetObjMon;

TYPE Surrogate_NetObjMon_T = NetObjMon.T OBJECT
      OVERRIDES
        dump := Surrogate_dump;
        dumpNames := Surrogate_dumpNames;
      END;

PROCEDURE Surrogate_dump(self: NetObjMon.T): REFANY RAISES {NetObj.Error,
    Thread.Alerted};

PROCEDURE Surrogate_dumpNames(self: NetObjMon.T): RefList.T
     RAISES {NetObj.Error, Thread.Alerted};

PROCEDURE Stub_dump(self: NetObjMon.T; c: StubLib.Conn; rep: StubLib.DataRep)
     RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted};

PROCEDURE Stub_dumpNames(
    self: NetObjMon.T;
    c: StubLib.Conn;
    rep: StubLib.DataRep) RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
    Thread.Alerted};

END NetObjMon_T_v1.
