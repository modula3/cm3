(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* AgentStubs.m3 *)
(* Last modified on Wed Feb 16 18:01:04 PST 1994 by wobber *)
(*      modified on Mon Aug 31 09:44:48 PDT 1992 by evers  *)
(*      modified on Mon May 25 22:48:00 1992 by owicki *)

MODULE AgentStubs EXPORTS NetObjInit;
   
IMPORT NetObj, StubLib,
       SpecialObj, WireRep, SpaceID, Transport, Rd, Wr, Thread;

FROM SpecialObj IMPORT EventID, FpTower, VersionList,
                       CleanBatch, CleanElem, DefaultBatchLen;

TYPE
  AgentProcs = {Get, Put, Dirty, Clean, GetAdr};
  ST = SpecialObj.Surrogate;
  
REVEAL
  SpecialObj.Surrogate = SpecialObj.ST BRANDED OBJECT OVERRIDES
    get := SurrogateGet;
    put := SurrogatePut;
    getAdr := SurrogateGetAdr;
    dirty := SurrogateDirty;
    clean := SurrogateClean;
  END;

(* surrogate methods for agent remote invocations *)

PROCEDURE SurrogatePut(t: ST; name: TEXT; obj: NetObj.T)
    RAISES {NetObj.Error, Thread.Alerted} =
  VAR reuse := FALSE;
      c: StubLib.Conn;
  BEGIN
    TRY
      c := StubLib.StartCall(t, StubLib.SystemStubProtocol);
      TRY
        StubLib.OutInt32(c, ORD(AgentProcs.Put));
        StubLib.OutRef(c, name);
        StubLib.OutRef(c, obj);
        EVAL StubLib.AwaitResult(c);
        reuse := TRUE;
      FINALLY
        StubLib.EndCall(c, reuse);
      END;
    EXCEPT
    | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
    | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
    END;
  END SurrogatePut;
  
PROCEDURE SurrogateGet(t: ST; name: TEXT) : NetObj.T
     RAISES {NetObj.Error, Thread.Alerted} =
  VAR reuse := FALSE;
      c: StubLib.Conn;
      o: NetObj.T;
      rep: StubLib.DataRep;
  BEGIN
    TRY
      c := StubLib.StartCall(t, StubLib.SystemStubProtocol);
      TRY
        StubLib.OutInt32(c, ORD(AgentProcs.Get));
        StubLib.OutRef(c, name);
        rep := StubLib.AwaitResult(c);
        o := StubLib.InRef(c, rep, TYPECODE(NetObj.T));
        reuse := TRUE;
      FINALLY
        StubLib.EndCall(c, reuse);
      END;
    EXCEPT
    | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
    | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
    END;
    RETURN o;
  END SurrogateGet;

PROCEDURE SurrogateDirty(
    t: ST;
    wrep: WireRep.T; eventID: EventID;
    VAR (*OUT*) vers: VersionList;
    <*UNUSED*> loc: Transport.Location := NIL): FpTower
    RAISES {NetObj.Error, Thread.Alerted} =
  VAR reuse := FALSE;
      c: StubLib.Conn;
      fpTower: FpTower;
      rep: StubLib.DataRep;
      nFpTower: INTEGER;
  BEGIN
    TRY
      c := StubLib.StartCall(t, StubLib.SystemStubProtocol);
      TRY
        StubLib.OutInt32(c, ORD(AgentProcs.Dirty));
        StubLib.OutBytes(c, wrep.byte);
        StubLib.OutInt32(c, eventID[0]);
        StubLib.OutInt32(c, eventID[1]);
        StubLib.OutInt32(c, NUMBER(vers));
        rep := StubLib.AwaitResult(c);
        FOR i := 0 TO LAST(vers) DO
          vers[i] := StubLib.InInt32(c, rep);
        END;
        nFpTower := StubLib.InInt32(c, rep, 0);
        IF nFpTower > 0 THEN
          fpTower := NEW(FpTower, nFpTower);
          FOR i := 0 TO nFpTower-1 DO
            StubLib.InBytes(c, fpTower[i].byte);
          END;
        ELSE
          fpTower := NIL;
        END;
        reuse := TRUE;
      FINALLY
        StubLib.EndCall(c, reuse);
      END;
    EXCEPT
    | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
    | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
    END;
    RETURN fpTower;
  END SurrogateDirty;

PROCEDURE SurrogateClean(
    t: ST;
    READONLY batch: CleanBatch; strong: BOOLEAN := FALSE;
    <*UNUSED*> loc: Transport.Location := NIL)
    RAISES {NetObj.Error, Thread.Alerted} =
  VAR reuse := FALSE;
      c: StubLib.Conn;
  BEGIN
    TRY
      c := StubLib.StartCall(t, StubLib.SystemStubProtocol);
      TRY
        StubLib.OutInt32(c, ORD(AgentProcs.Clean));
        StubLib.OutInt32(c, NUMBER(batch));
        FOR i := 0 TO LAST(batch) DO
          StubLib.OutBytes(c, batch[i].wrep.byte);
          StubLib.OutInt32(c, batch[i].id[0]);
          StubLib.OutInt32(c, batch[i].id[1]);
      	END;
        StubLib.OutBoolean(c, strong);
        EVAL StubLib.AwaitResult(c);
        reuse := TRUE;
      FINALLY
        StubLib.EndCall(c, reuse);
      END;
    EXCEPT
    | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
    | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
    END;
  END SurrogateClean;

PROCEDURE SurrogateGetAdr(t: ST; sp: SpaceID.T) : NetObj.Address
    RAISES {NetObj.Error, Thread.Alerted} =
  VAR reuse := FALSE;
      c: StubLib.Conn;
      adr: NetObj.Address;
      rep: StubLib.DataRep;
      n: INTEGER;
  BEGIN
    TRY
      c := StubLib.StartCall(t, StubLib.SystemStubProtocol);
      TRY
        StubLib.OutInt32(c, ORD(AgentProcs.GetAdr));
        StubLib.OutBytes(c, sp.byte);
        rep := StubLib.AwaitResult(c);
        n := StubLib.InInt32(c, rep, 0);
        IF n = 0 THEN
          adr := NIL;
        ELSE
          adr := NEW(NetObj.Address, n);
          FOR i := 0 TO n-1 DO
            adr[i] := StubLib.InRef(c, rep, TYPECODE(TEXT));
          END;
        END;
        reuse := TRUE;
      FINALLY
        StubLib.EndCall(c, reuse);
      END;
    EXCEPT
    | Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);
    | Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);
    END;
    RETURN adr;
  END SurrogateGetAdr;

(* dispatcher for RPC agent invocations *)

PROCEDURE Invoke(
    c: StubLib.Conn;
    obj: NetObj.T;
    rep: StubLib.DataRep;
    <*UNUSED*> stubProt: StubLib.StubProtocol)
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR v := NARROW(obj, SpecialObj.ST);
  BEGIN
    CASE StubLib.InInt32(c, rep, 0) OF
    | ORD(AgentProcs.Get) => GetStub(c, v, rep);
    | ORD(AgentProcs.Put) => PutStub(c, v, rep);
    | ORD(AgentProcs.Dirty) => DirtyStub(c, v, rep);
    | ORD(AgentProcs.Clean) => CleanStub(c, v, rep);
    | ORD(AgentProcs.GetAdr) => GetAdrStub(c, v, rep);
    ELSE
      StubLib.RaiseUnmarshalFailure();
    END;
  END Invoke;

PROCEDURE PutStub(c: StubLib.Conn; t: SpecialObj.ST; rep: StubLib.DataRep)
   RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR
    name: TEXT;
    o: NetObj.T;
  BEGIN
    name := StubLib.InRef(c, rep, TYPECODE(TEXT));
    o := StubLib.InRef(c, rep, TYPECODE(NetObj.T));
    t.put(name, o);
    StubLib.StartResult(c);
  END PutStub;
  
PROCEDURE GetStub(c: StubLib.Conn; t: SpecialObj.ST; rep: StubLib.DataRep)
   RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR
    name: TEXT;
    o: NetObj.T;
  BEGIN
    name := StubLib.InRef(c, rep, TYPECODE(TEXT));
    o := t.get(name);
    StubLib.StartResult(c);
    StubLib.OutRef(c, o);
  END GetStub;
  
PROCEDURE DirtyStub(c: StubLib.Conn; t: SpecialObj.ST;
     rep: StubLib.DataRep)
   RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR
    wrep: WireRep.T;
    fpTower: FpTower;
    eventID: EventID;
    inVListLen: CARDINAL;
    fixedVList: ARRAY [0..2] OF StubLib.StubProtocol;
    varVList: REF VersionList;
  BEGIN
    StubLib.InBytes(c, wrep.byte);
    eventID[0] := StubLib.InInt32(c, rep);
    eventID[1] := StubLib.InInt32(c, rep);
    inVListLen := StubLib.InInt32(c, rep, 0);
    IF inVListLen > NUMBER(fixedVList) THEN
      varVList := NEW(REF VersionList, inVListLen);
      fpTower := t.dirty(wrep, eventID, varVList^, c.loc);
      StubLib.StartResult(c);
      FOR i := 0 TO inVListLen-1 DO
        StubLib.OutInt32(c, varVList[i]);
      END;
    ELSE
      fpTower := t.dirty(
        wrep, eventID, SUBARRAY(fixedVList, 0, inVListLen), c.loc);
      StubLib.StartResult(c);
      FOR i := 0 TO inVListLen-1 DO
        StubLib.OutInt32(c, fixedVList[i]);
      END;
    END;
    IF fpTower = NIL THEN
      StubLib.OutInt32(c, 0);
    ELSE
      StubLib.OutInt32(c, NUMBER(fpTower^));
      FOR i := 0 TO LAST(fpTower^) DO
        StubLib.OutBytes(c, fpTower[i].byte);
      END;
    END;
  END DirtyStub;

PROCEDURE CleanStub(c: StubLib.Conn; t: SpecialObj.ST; rep: StubLib.DataRep)
   RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR
    inBatchLen: CARDINAL;
    fixedBatch: ARRAY [0..DefaultBatchLen-1] OF CleanElem;
    varBatch: REF CleanBatch;
    strong: BOOLEAN;
  BEGIN
    inBatchLen := StubLib.InInt32(c, rep, 0);
    IF inBatchLen > NUMBER(fixedBatch) THEN
      varBatch := NEW(REF CleanBatch, inBatchLen);
      FOR i := 0 TO inBatchLen-1 DO
        StubLib.InBytes(c, varBatch[i].wrep.byte);
        varBatch[i].id[0] := StubLib.InInt32(c, rep);
        varBatch[i].id[1] := StubLib.InInt32(c, rep);
      END;
      strong     := StubLib.InBoolean(c);
      t.clean(varBatch^, strong, c.loc);
    ELSE
      FOR i := 0 TO inBatchLen-1 DO
        StubLib.InBytes(c, fixedBatch[i].wrep.byte);
        fixedBatch[i].id[0] := StubLib.InInt32(c, rep);
        fixedBatch[i].id[1] := StubLib.InInt32(c, rep);
      END;
      strong     := StubLib.InBoolean(c);
      t.clean(SUBARRAY(fixedBatch, 0, inBatchLen), strong, c.loc);
    END;
    StubLib.StartResult(c);
  END CleanStub;

PROCEDURE GetAdrStub(c: StubLib.Conn; t: SpecialObj.ST;
      <*UNUSED*> rep: StubLib.DataRep)
   RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR
    sp : SpaceID.T;
    adr: NetObj.Address;
  BEGIN
    StubLib.InBytes(c, sp.byte);
    adr := t.getAdr(sp);
    StubLib.StartResult(c);
    IF adr = NIL THEN
      StubLib.OutInt32(c, 0);
    ELSE
      StubLib.OutInt32(c, NUMBER(adr^));
      FOR i := 0 TO LAST(adr^) DO
        StubLib.OutRef(c, adr[i]);
      END;
    END;
  END GetAdrStub;

PROCEDURE NullDispatcher(
    <*UNUSED*> inv: StubLib.Conn;
    <*UNUSED*> o: NetObj.T;
    <*UNUSED*> rep: StubLib.DataRep;
    <*UNUSED*> stubProt: StubLib.StubProtocol) RAISES {NetObj.Error} =
  BEGIN
    StubLib.RaiseUnmarshalFailure();
  END NullDispatcher;
  
PROCEDURE InitAgentStubs() =
  BEGIN
    StubLib.Register(
      TYPECODE(NetObj.T), StubLib.SystemStubProtocol,
      TYPECODE(NetObj.T), NullDispatcher);
    StubLib.Register(
      TYPECODE(SpecialObj.ST), StubLib.SystemStubProtocol,
      TYPECODE(ST), Invoke); 
  END InitAgentStubs;

BEGIN
END AgentStubs.
