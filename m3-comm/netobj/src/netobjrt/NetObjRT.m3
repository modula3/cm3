(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjRT.m3 *)
(* Last modified on Tue Jan 31 08:46:23 PST 1995 by kalsow *)
(*      modified on Wed Aug 31 16:26:05 PDT 1994 by wobber *)
(*      modified on Mon Feb  8 11:43:23 PST 1993 by owicki *)
(*      modified on Tue Sep 15 10:36:23 PDT 1992 by evers  *)

MODULE NetObjRT EXPORTS NetObjRT, NetObjNotifier,
                        NGCMonitor, SpecialObj, StubLib;
  
<* PRAGMA LL *>

IMPORT Atom, AtomList;
IMPORT NetObj, NetObjRep, Transport, TransportUtils, TransportRegistry;
IMPORT SpaceID, WireRep, Fingerprint;
IMPORT IntRefTbl, TextRefTbl, ObjTbl, ObjElem,
       FPRefTbl, DirtyTbl, DirtyElem;
IMPORT Thread, RefList;
IMPORT RTAllocator, RTType, RTTypeFP, RTTypeSRC, WeakRef;

VAR
  mu := NEW(MUTEX); 
  cond := NEW(Thread.Condition);
  MissingDispatcher: Atom.T;

VAR                             <* LL >= {mu} *>
  objTbl: ObjTbl.T;

(* The table "objTbl" maps wire representations to network objects;
   it maps "wr" to "ob" if and only if "ob" is an exported object
   represented by "wr" or "ob" is a surrogate for the object represented
   by "wr".

| T1  ob reachable /\ ob.w # WireRep.NullT =>
|     ob.w in domain(objTbl) /\ ob'.w = ob.w => ob' = ob

   Note that after InitAgent() we have WireRep.SpecialT in domain(objTbl),
   but there is no object ob with ob.w = WireRep.SpecialT.  The special
   object itself, sp = objTbl[WireRep.SpecialT], has sp.w = WireRep.NullT
   if it has not been explicitly exported or sp.w = some proper WireRep.T
   if it has.
*)   

(* invariant: for any "v: ObjElem.T" in "range(objTbl)",
|  v.ref # NIL => TYPECODE(v.ref.r) = TYPECODE(ExportInfo)
|  v.ref = NIL /\ v.ready =>
|     v.weakRef `is a valid (possibly dead)` WeakRef.T `for a
|     surrogate` NetObj.T *)

VAR <* LL >= {mu} *>
  typeTbl: IntRefTbl.T;
  fpToTc: FPRefTbl.T;

CONST MaxVersions = 3;

TYPE
  TypeInfo = REF RECORD
    pureTC: Typecode;
    fp: Fingerprint.T;
    nvers: CARDINAL := 0;
    vers := ARRAY [0..MaxVersions-1] OF StubProtocol {NullStubProtocol, ..};
    surrTCs := ARRAY [0..MaxVersions-1] OF Typecode {LAST(Typecode), ..};
    procs := ARRAY [0..MaxVersions-1] OF Dispatcher {NIL, ..};
    fpTower: FpTower := NIL;
  END;


(* The table "typeTbl" maps typecodes to the "TypeInfo" needed to 
   handle method invocations and type inquiries for objects of that 
   type.  If it maps "tc" to "r", then "r" refers to stub information 
   for the first supertype of "tc" for which this address space has 
   stubs.  Furthermore if this address space has stubs for a type, 
   then "typeTbl" has an entry for the type.  Also, if this address 
   space has ever marshaled an object of allocated type "t" then 
   "typeTbl" has an entry for "t".

   The stub-generated code for marshalling objects of type "T" calls
   "Register" to make entries in "typeTbl" before any objects are
   marshaled or unmarshaled.

   The table "fpToTc" maps fingerprints to typeinfo object.  If it maps "fp"
   to "info", then "info" describes the most specific type for which
   this address space has stubs. 

   The table "fpToTc" is strictly a performance optimatization for 
   converting fingerprints into surrogate type codes.  Given an ordered 
   list of fingerprints representing the type structure of a network 
   object, it is always possible to traverse the list from subtype 
   to supertype searching for a match in the local Modula-3 runtime 
   fingerprint table. If a match is discovered, then the resulting 
   type code can be looked up in "typeTable".  If an entry exists 
   there, then the type code is the the narrowest subtype of the 
   network object for which the local address space has stubs.  
   "fpToTc" is used to cache the results of this computation. 
*)

VAR <* LL >= {mu} *> 
  spaceTbl: FPRefTbl.T; (* Table(SpaceID, NetObj.Address) *) 

(* "spaceTbl" maps "s" to information needed for communication with the
   address space identified by "s".  "adr" is the address of "s".  There is
   an entry in "spaceTbl" for this address space, and an entry
   for every address space that is the owner of a surrogate in this address
   space.  *)

TYPE
  Notifier = REF RECORD
    wr: WeakRef.T;    (* to a surrogate object *)
    cl: NotifierClosure;
  END;

(* Objects of type "Notifier" are appended to each "Transport.Location"
   and used to inform clients when object owners become inaccessible. *)

REVEAL
  Transport.Location = TransportUtils.LocationP BRANDED OBJECT
    <* LL >= {mu} *>
    exports: DirtyTbl.T := NIL;
    (* maps exported w: WireRep.T for which this location is dirty, or
       for which the latest clean call received from this location was
       strong, to the latest EventID received from this location
       for w.  *)
    notifiers: RefList.T := NIL;
    (* a list of Notifier objects for surrogates whose
       owner is the target of this location. *)
    cleaner: Cleaner := NIL;
    locTblIndex: CARDINAL;
    isDead: BOOLEAN := FALSE;
  OVERRIDES
    dead := DeadLocation;
  END;


VAR
  <* LL >= {mu} *>
  locTbl: RECORD
    locs: REF ARRAY OF Transport.Location := NIL;
    free: CARDINAL := 0;
  END;

(* "locTbl" is a list of locations which hold surrogates for objects
   owned by this address space.  It is required to prevent the cleanup
   of locations for which the transport is not maintaining any active
   server threads, (There are no other references to locations in this
   state.)
*)

TYPE
  ExportInfo = BRANDED OBJECT
    typeInfo: TypeInfo;
    <* LL >= {mu} *>
    pinCount: CARDINAL := 0;(* # of incomplete marshalling actions from owner + 
      	      	      	       # of dirty locations for this object *)
  END;

(* For any exported concrete object c, writing c.xi = NARROW (c.r, ExportInfo),

   c.xi.pinCount =  # loc: Location st. ( c.w in domain(loc.exports) /\
                                         loc.exports[c.w].dirty )
                 + # incomplete outbound marshalling attempts for c from
                     this address space (which is c's owner)

   c is `exported' iff c.w # WireRep.NullT. *)

TYPE
  AgentT = ST OBJECT
    table: TextRefTbl.T;
  OVERRIDES
    get := AgentGet;
    put := AgentPut;
    getAdr := AgentGetAdr;
    dirty := AgentDirty;
    clean := AgentClean;
  END;
  
VAR  <* LL >= {mu} *>
  started: BOOLEAN := FALSE;
  localAgentT: AgentT;


(*------------------------- exports to NetObjRT ----------------------------*)

PROCEDURE FindTarget(
    wrep: WireRep.T;
    stubProt: StubProtocol;
    VAR dispatcher: Dispatcher) : NetObj.T
    RAISES {NetObj.Error} =
  <* LL.sup < mu *>
  VAR oe: ObjElem.T;
  BEGIN
    LOCK mu DO
      IF NOT started THEN RuntimeInit(); END;
      IF NOT objTbl.get(wrep, oe) OR oe.ref = NIL THEN
        RaiseError(NetObj.MissingObject);
      END;
      dispatcher := FindDispatcher(oe.ref.r, stubProt);
      IF dispatcher = NIL THEN RaiseError(MissingDispatcher); END;
      RETURN oe.ref;
    END;
  END FindTarget;
    
PROCEDURE FindDispatcher(exp: ExportInfo; stubProt: StubProtocol): Dispatcher =
  VAR tt := exp.typeInfo;
  BEGIN
    FOR i := 0 TO LAST(tt.vers) DO
      IF stubProt = tt.vers[i] THEN RETURN tt.procs[i]; END;
    END;
    RETURN NIL;
  END FindDispatcher;

PROCEDURE Find (wrep: WireRep.T; loc: Transport.Location): NetObj.T
    RAISES {NetObj.Error, Thread.Alerted} =
  <* LL.sup < mu *>
  VAR
    oe: ObjElem.T;
    res: NetObj.T;
    ts: EventID;
  BEGIN
    LOCK mu DO
      IF NOT started THEN RuntimeInit(); END;
      WHILE objTbl.get(wrep, oe) DO
      	(* concrete object ?*)
      	IF oe.ref # NIL THEN
      	  <* ASSERT TYPECODE(oe.ref.r) = TYPECODE(ExportInfo) *>
          (* Note that we are not logging concrete object lookup *)
      	  RETURN oe.ref
      	ELSE
      	  IF NOT oe.ready THEN
      	    Thread.Wait(mu, cond)
      	    (* reenter loop; last attempt at surrogate creation might
               have failed, so we have to test wrep again. *)
      	  ELSE
      	    res := WeakRef.ToRef(oe.weakRef);
      	    IF res # NIL THEN
              RETURN res;
      	    ELSE
      	      EXIT;
      	      (* the cleanup for val.weakRef's death is scheduled but has
      	    	 not yet started.  There is no sensible way to resurrect
      	    	 the old surrogate at this stage. *)
      	    END;
      	  END;
      	END;
      END;
      (* make a new surrogate, possibly overwriting a dead old one *)
      oe.ref := NIL;
      oe.ready := FALSE;
      EVAL objTbl.put(wrep, oe);
      ts := NextEventID();
    END;
    TRY
      res := NIL;
      res := NewSrgt(wrep, loc, ts);
    FINALLY
      LOCK mu DO
        IF res # NIL THEN
      	  (* no exception was raised *)
      	  oe.weakRef := WeakRef.FromRef(res, CleanupSrgt);
      	  oe.ready := TRUE;
          EVAL objTbl.put(wrep, oe);
        ELSE
      	  (* an exception was raised or NewSrgt returned NIL *)
          EVAL objTbl.delete(wrep, oe);
      	  VAR r: REFANY; loc: Transport.Location;
          BEGIN
	    IF spaceTbl.get(WireRep.GetSpaceID(wrep), r) THEN
              loc := TransportRegistry.LocationFromAdr(r);
              IF loc # NIL THEN CleanerEnqueue(loc, wrep, TRUE); END;
      	    END;
            (* else we can't make a special for owner(wrep).  This
               implies we've never been able to make a dirty call,
               and need not send a clean. *)
          END;
        END;
      END;
      Thread.Broadcast(cond);
    END;
    RETURN res;
  END Find;

PROCEDURE InsertAndPin (o: NetObj.T) : WireRep.T =
  <* LL.sup < mu *>
  VAR oe: ObjElem.T;
  BEGIN
    TYPECASE o.r OF
    | NULL, ExportInfo =>
      LOCK mu DO
	IF NOT started THEN RuntimeInit(); END;
	IF o.w = WireRep.NullT THEN
	  (* need to export this (concrete) object *)
      	  IF o.r = NIL THEN
	    o.r := NewExportInfo(TYPECODE(o));
      	  ELSE
      	    <* ASSERT NARROW(o.r, ExportInfo).pinCount = 0 *>
      	  END;
	  oe.ref := o;
	  o.w := WireRep.New();
	  EVAL objTbl.put(o.w, oe);
	END;
	INC(NARROW(o.r, ExportInfo).pinCount);
	(* o will live whilst pinCount > 0 since => in range(objTbl) *)
      END;
      RETURN o.w;
    | Transport.Location =>
      RETURN o.w; (* surrogate *)
    ELSE
      <* NOWARN *> Die();
    END;
  END InsertAndPin;

PROCEDURE Unpin(READONLY arr: ARRAY OF NetObj.T) =
  <* LL.sup < mu *>
  BEGIN
    LOCK mu DO
      FOR i := 0 TO LAST(arr) DO
      	TYPECASE arr[i].r OF
      	| ExportInfo(ei) =>
      	  DEC(ei.pinCount);
      	  CheckedRemove(arr[i]);
      	ELSE (* skip *)
      	END;
      END;
    END;
  END Unpin;

PROCEDURE DeadLocation(loc: Transport.Location; st: OwnerState) =
  <* LL.sup < mu *>
  VAR
    wrep: WireRep.T;
    de: DirtyElem.T;
    it: DirtyTbl.Iterator;
    oe: ObjElem.T;
    list: RefList.T;
    n: Notifier;
    obj: NetObj.T;
  BEGIN
    LOCK mu DO
      IF loc.exports # NIL THEN
      	it := loc.exports.iterate();
        WHILE it.next(wrep, de) DO
          IF de.dirty THEN
            IF NOT objTbl.get(wrep, oe) THEN Die(); END;
      	    DEC(NARROW(oe.ref.r, ExportInfo).pinCount);
            CheckedRemove(oe.ref);
          END;
        END;
      	locTbl.locs[loc.locTblIndex] := NIL;
        locTbl.free := MIN(locTbl.free, loc.locTblIndex);
        loc.exports := NIL;
      END;
      list := loc.notifiers;
      IF st = OwnerState.Dead THEN loc.isDead := TRUE; END;
    END;
    WHILE list # NIL DO
      n := list.head;
      obj := WeakRef.ToRef(n.wr);
      IF obj # NIL THEN n.cl.notify(obj, st); END;
      list := list.tail;
    END;
  END DeadLocation;

<* INLINE *>
PROCEDURE CheckedRemove(c: NetObj.T) =
  <* LL.sup = mu *>
  VAR xi: ExportInfo := c.r;
      waste: ObjElem.T;
  BEGIN
    IF xi.pinCount = 0 THEN
      IF NOT objTbl.delete(c.w, waste) THEN Die(); END;
      c.w := WireRep.NullT;
    END;
  END CheckedRemove;

PROCEDURE CleanupSrgt (READONLY w: WeakRef.T; x: REFANY) =
  <* LL.sup < mu *>
  VAR
    oe: ObjElem.T;
    s: NetObj.T := x;
    loc: Transport.Location := s.r;
  BEGIN
    WITH wrep = s.w DO
      LOCK mu DO
        s.r := NIL;
        IF NOT objTbl.get(wrep, oe) THEN
      	  RETURN; (* beaten by fresh unmarshal, death, cleanup sequence *)
      	END;
	<* ASSERT oe.ref = NIL *>
	IF NOT oe.ready OR w # oe.weakRef THEN
      	      	      	(*  WeakRef.ToRef(oe.weakRef) # NIL also works *)
      	  RETURN; (* beaten by fresh unmarshal *)
      	END;
	EVAL objTbl.delete(wrep, oe);
	CleanerEnqueue(loc, wrep, FALSE);
      END;    
    END;
  END CleanupSrgt;


TYPE
  Cleaner = Thread.Closure OBJECT
    loc: Transport.Location;
    queue, queueTail: CleanQElem := NIL;
  OVERRIDES
    apply := CleanerApply;
  END;

TYPE
  CleanQElem = REF RECORD
    next: CleanQElem := NIL;
    elem: CleanElem;
    strong: BOOLEAN;
  END;

PROCEDURE CleanerEnqueue(
    loc: Transport.Location; wrep: WireRep.T; strong: BOOLEAN) =
  <* LL.sup = mu *>
  VAR e := NEW(CleanQElem,
              elem := CleanElem{wrep, NextEventID()}, strong := strong);
  BEGIN
    IF loc.isDead THEN RETURN; END;
    IF loc.cleaner = NIL THEN
      loc.cleaner := NEW(Cleaner, loc := loc, queue := e, queueTail := e);
      EVAL Thread.Fork(loc.cleaner);
    ELSE
      loc.cleaner.queueTail.next := e;
      loc.cleaner.queueTail := e;
    END;
  END CleanerEnqueue;

PROCEDURE CleanerApply(cl: Cleaner): REFANY =
  CONST tries = 3;
  (* The next call after a failed call is guaranteed to make a new connection.
     If that connection cannot be made, the location will be declare dead. *)
  VAR
    st := New(cl.loc);
    nElem: CARDINAL;
    batch: ARRAY [0..DefaultBatchLen-1] OF CleanElem;
    ok := TRUE;
    strong: BOOLEAN;
  BEGIN
    LOOP
      nElem := 0;
      LOCK mu DO
        WHILE ok AND cl.queue # NIL AND nElem < NUMBER(batch)
                 AND (nElem = 0 OR strong = cl.queue.strong) DO
          batch[nElem] := cl.queue.elem;
          strong := cl.queue.strong;
          INC(nElem);
          cl.queue := cl.queue.next;
        END;
        IF nElem = 0 THEN cl.loc.cleaner := NIL; cl.loc := NIL; EXIT; END;
      END;
      FOR try := 1 TO tries DO
        IF cl.loc.isDead THEN EXIT; END;
        TRY
          st.clean(SUBARRAY(batch, 0, nElem), strong);
          EXIT;
        EXCEPT
        | NetObj.Error, Thread.Alerted  =>
            IF try = tries THEN
              ok := FALSE;   (* give up !!! *)
            ELSE
              Thread.Pause(6.0d1);
            END;
        END;
      END;
    END;
    RETURN NIL;
  END CleanerApply;


(*------------------------- exports to NetObjNotifier ----------------------*)

PROCEDURE AddNotifier(obj: NetObj.T; cl: NotifierClosure) =
  <* LL.sup < mu *>
  VAR n: Notifier;
  BEGIN
    LOCK mu DO
      TYPECASE obj.r OF
      | NULL =>
      | Transport.Location(loc) =>
          n := NEW(Notifier, wr := WeakRef.FromRef(obj), cl := cl);
          loc.notifiers := RefList.Cons(n, loc.notifiers);
      ELSE
      END;
    END;
  END AddNotifier;


(*------------------------- exports to StubLib -----------------------------*)

PROCEDURE Register(
    pureTC: Typecode;
    vers: StubProtocol;
    surrTC: Typecode;
    disp  : Dispatcher) =
  <* LL.sup < mu *>
  VAR
    info: TypeInfo;
    r: REFANY;
  BEGIN
    LOCK mu DO
      IF typeTbl.get(pureTC, r) THEN
        info := NARROW(r, TypeInfo);
      ELSE
        info := NEW(TypeInfo, pureTC := pureTC,
                              fp := RTTypeFP.ToFingerprint(pureTC));
        EVAL typeTbl.put(pureTC, info);
        EVAL fpToTc.put(info.fp, info);
      END;
      VAR n := info.nvers; BEGIN
        IF n = NUMBER(info.vers) THEN Die(); END;
        info.vers[n] := vers;
        info.procs[n] := disp;
        info.surrTCs[n] := surrTC;
        INC(info.nvers);
      END;
    END;
  END Register;
  
PROCEDURE BuildFpTower(tt: TypeInfo) : FpTower =
  <* LL.sup = mu *>
  VAR n: CARDINAL := 0;
      tc: Typecode := tt.pureTC;
      r: REFANY;
  BEGIN
    IF tt.fpTower = NIL THEN
      WHILE tc # TYPECODE(NetObj.T) DO
        IF typeTbl.get(tc, r) THEN INC(n); END;
        tc := RTType.Supertype(tc);
        IF tc = RTType.NoSuchType THEN Die(); END;
      END;
      IF n # 0 THEN
        tt.fpTower := NEW(FpTower, n);
        n := 0;
        tc := tt.pureTC;
        WHILE tc # TYPECODE(NetObj.T) DO
          IF typeTbl.get(tc, r) THEN
            tt.fpTower[n] := NARROW(r, TypeInfo).fp;
            INC(n);
          END;
          tc := RTType.Supertype(tc);
          IF tc = RTType.NoSuchType THEN Die(); END;
        END;
      END;
    END;
    RETURN tt.fpTower;
  END BuildFpTower;
  

(*------------------------------ utilities ---------------------------------*)

(* "NewSrgt(wrep, loc)" returns a surrogate for the object represented by
   "wrep", assuming that "loc" is a location which issues connections to
   the address space from which "wrep" was received.  This need not be a
   location of owner of the object. *)

PROCEDURE NewSrgt(
    READONLY wrep: WireRep.T; loc: Transport.Location; ts: EventID) : NetObj.T
    RAISES {NetObj.Error, Thread.Alerted} =
  <* LL.sup < mu *>
  VAR 
    st := SpaceToSpecial(WireRep.GetSpaceID(wrep), loc);
    tc: CARDINAL; 
    fpTower: FpTower;
    vers: ARRAY [0..MaxVersions-1] OF Int32;
  BEGIN
    fpTower := st.dirty(wrep, ts, vers);
    tc := TowerToSurrogateTC(fpTower, vers);
    VAR res: NetObj.T := RTAllocator.NewTraced(tc); BEGIN
      res.w := wrep;
      res.r := st.r;
      RETURN res
    END
  END NewSrgt;

PROCEDURE SpaceToSpecial(space: SpaceID.T; loc: Transport.Location) : ST
    RAISES {NetObj.Error, Thread.Alerted} =
    <* LL.sup < mu *>
  VAR r: REFANY;
      adr: NetObj.Address;
      found: BOOLEAN;
      rloc: Transport.Location;
  BEGIN
    LOCK mu DO
      found := spaceTbl.get(space, r);
    END;
    IF found THEN
      adr := r;
    ELSE
      adr := New(loc).getAdr(space);
      LOCK mu DO
        EVAL spaceTbl.put(space, adr);
      END;
    END;
    rloc := TransportRegistry.LocationFromAdr(adr);
    IF rloc = NIL THEN RaiseError(NetObj.NoTransport); END;
    RETURN New(rloc);
  END SpaceToSpecial;

PROCEDURE TowerToSurrogateTC(fpTower: FpTower; VAR vers: VersionList): Typecode
  <* LL.sup < mu *> =
  VAR r: REFANY;
      i: CARDINAL := 0;
      tc: Typecode := TYPECODE(NetObj.T);
      firstInfo: TypeInfo := NIL;
  BEGIN
    LOCK mu DO
      IF fpTower # NIL THEN
        REPEAT
          IF fpToTc.get(fpTower[i], r) THEN
            (* r = NIL means no fingerprint match *)
            IF r = NIL THEN EXIT; END;
            IF firstInfo = NIL THEN firstInfo := r; END;
            IF MatchVersion(r, vers, tc) THEN EXIT; END;
          END;
          INC(i)
        UNTIL i = NUMBER(fpTower^);
        IF i # 0 THEN
          EVAL fpToTc.put(fpTower[0], firstInfo);
        END;
      END;
    END;
    RETURN tc;
  END TowerToSurrogateTC;  

PROCEDURE MatchVersion(
    info: TypeInfo; VAR vers: VersionList;
    VAR (*out*) tc: Typecode) : BOOLEAN =
  BEGIN
    FOR i := 0 TO LAST(vers) DO
      FOR j := 0 TO info.nvers-1 DO
        IF vers[i] = info.vers[j] THEN
          tc := info.surrTCs[j]; RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END MatchVersion;

PROCEDURE NewExportInfo(tc: Typecode) : ExportInfo =
  <* LL.sup = mu *>
  VAR r: REFANY; res: ExportInfo;
  BEGIN
    IF tc = RTType.NoSuchType THEN Die(); END;
    IF typeTbl.get(tc, r) THEN
      res := NEW(ExportInfo, typeInfo := r);
    ELSE
      res := NewExportInfo(RTType.Supertype(tc));
      EVAL typeTbl.put(tc, res.typeInfo);
    END;
    RETURN res;
  END NewExportInfo;



(*----------------------- exports to SpecialObj -----------------------------*)

(* for now we're assuming that the NetObj runtime needn't be 'started'
   until either NetObj.Import or NetObj.Export are called.  Both of those
   procedures must invoke SpecialObj.New to do anything.

   This might need to be made more explicit at some later time.
*)

PROCEDURE New(loc: Transport.Location) : ST =
  <* LL.sup ???? *>
  VAR st: ST;
  BEGIN
    st := RTAllocator.NewTraced(TYPECODE(Surrogate));
    st.w := WireRep.SpecialT;
    st.r := loc;
    RETURN st;
  END New;

(* special object init + methods *)

PROCEDURE InitAgent() =
  <* LL.sup = mu *>
  BEGIN
    localAgentT := NEW(AgentT,
      table := NEW(TextRefTbl.Default).init(),
      r := NewExportInfo(TYPECODE(AgentT)));
    (* Note that this object is inserted into the table without
       filling in the st.w field.  It is never removed.
       It could be inserted into the table in another slot under
       some other concrete non-special wire rep. *)
    EVAL objTbl.put(WireRep.SpecialT, ObjElem.T{ref := localAgentT});
  END InitAgent;

TYPE
  Note = NotifierClosure OBJECT
    name: TEXT;
  OVERRIDES
    notify := DeadEntry;
  END;

PROCEDURE DeadEntry(n: Note; obj: NetObj.T; <*UNUSED*> st: OwnerState) =
    <* LL.sup < mu *>
  VAR r: REFANY;
  BEGIN
    LOCK mu DO
      IF localAgentT.table.get(n.name, r) AND r = obj THEN
        EVAL localAgentT.table.delete(n.name, r);
      END;
    END;
  END DeadEntry;

PROCEDURE AgentGet(t: AgentT; name: TEXT) : NetObj.T =
    <* LL.sup < mu *>
  VAR r: REFANY;
  BEGIN
    LOCK mu DO
      IF NOT t.table.get(name, r) THEN
        RETURN NIL;
      ELSE
        RETURN r;
      END;
    END;
  END AgentGet;

PROCEDURE AgentPut(t: AgentT; name: TEXT; obj: NetObj.T) =
    <* LL.sup < mu *>
  BEGIN
    IF obj # NIL THEN AddNotifier(obj, NEW(Note, name := name)); END;
    LOCK mu DO EVAL t.table.put(name, obj); END;
  END AgentPut;
    
PROCEDURE AgentGetAdr(
    <*UNUSED*> t: AgentT; sp: SpaceID.T) : NetObj.Address =
  <* LL.sup < mu *>
  VAR r: REFANY;
  BEGIN
    LOCK mu DO
      IF spaceTbl.get(sp, r) THEN 
        RETURN r;
      ELSE
        RETURN NIL;
      END;
    END;
  END AgentGetAdr;

PROCEDURE AgentDirty(
    <*UNUSED*> t: AgentT;
    wrep: WireRep.T; eventID: EventID;
    VAR (*OUT*) vers: VersionList;
    loc: Transport.Location := NIL): FpTower
    RAISES {NetObj.Error} =
  <* LL.sup < mu *>
  VAR
    oe: ObjElem.T;
    de: DirtyElem.T;
    rti: REFANY := NIL;
    c: NetObj.T;
    bump: BOOLEAN; (* did we make a clean -> dirty transition ? *)
  BEGIN
    LOCK mu DO
      IF NOT objTbl.get(wrep, oe) OR (oe.ref = NIL) THEN
        RaiseError(NetObj.MissingObject);
      END;
      (* loc # NIL because the Agent stubs provide it *)
      c := oe.ref;
      IF NOT typeTbl.get(TYPECODE(c), rti) THEN
        (* There must be an entry for "TYPECODE(c)", since "c" has been
           marshaled by this address space *)
        Die();
      END;
      IF loc.exports = NIL THEN
        loc.exports := NEW(DirtyTbl.Default).init();
        AddToLocTbl(loc);
      END;
      IF loc.exports.get(c.w, de) THEN
      	IF EventLE(eventID, de.ts) THEN
      	  (* dirty call orphaned or beaten by a clean - ignore *)
      	  RaiseError(NetObj.CommFailure);
      	ELSIF de.keep THEN
      	  bump := NOT de.dirty;
      	  de := DirtyElem.T {dirty := TRUE, keep := TRUE, ts := eventID};
      	ELSE
      	  <* ASSERT de.dirty *>
      	  bump := FALSE;
    	  de := DirtyElem.T {dirty := TRUE, keep := FALSE, ts := eventID};
      	END;
      ELSE
      	bump := TRUE;
    	de := DirtyElem.T {dirty := TRUE, keep := FALSE, ts := eventID};
      END;
      EVAL loc.exports.put(c.w, de);
      IF bump THEN
        INC(NARROW(c.r, ExportInfo).pinCount);
      END;
      VAR tt := NARROW(c.r, ExportInfo).typeInfo;
      BEGIN
        IF tt.nvers >= NUMBER(vers) THEN
          vers := SUBARRAY(tt.vers, 0, NUMBER(vers));
        ELSE
          vers := tt.vers;
          FOR i := tt.nvers TO LAST(vers) DO vers[i] := NullStubProtocol; END;
        END;
        RETURN BuildFpTower(tt);
      END;
    END;
  END AgentDirty;

PROCEDURE AgentClean(
    <*UNUSED*> t: AgentT;
    READONLY batch: CleanBatch; strong: BOOLEAN := FALSE;
    loc: Transport.Location := NIL) =
  <* LL.sup < mu *>
  VAR
    oe: ObjElem.T;
    de, wasteDE: DirtyElem.T;
    c: NetObj.T;
    removals: BOOLEAN := FALSE;
  BEGIN
    LOCK mu DO
      FOR i := 0 TO LAST(batch) DO
        IF objTbl.get(batch[i].wrep, oe) AND (oe.ref # NIL) THEN
          (* we ignore a missing object here *)
          c := oe.ref;
          IF loc.exports # NIL THEN
            IF loc.exports.get(c.w, de) AND
                 NOT EventLE(batch[i].id, de.ts) THEN
              IF strong OR de.keep THEN
      	        EVAL loc.exports.put(c.w,
 	          DirtyElem.T{
                    dirty := FALSE, keep := TRUE, ts := batch[i].id})  
              ELSE
      	        EVAL loc.exports.delete(c.w, wasteDE);
              END;
      	      IF de.dirty THEN
      	        removals := TRUE;
                DEC(NARROW(c.r, ExportInfo).pinCount);
      	        CheckedRemove(c);
      	      END;
            END;
          ELSE
            (* clean call orphaned or beaten by a dirty - ignore *)
          END;
        END;
      END;
      IF removals AND loc.exports.size() = 0 THEN
        locTbl.locs[loc.locTblIndex] := NIL;
        locTbl.free := MIN(locTbl.free, loc.locTblIndex);
      	loc.exports := NIL;
      END;
    END;
  END AgentClean;
  

(*----------------------- SpecialObj.EventIDs -----------------------------*)

VAR
  eCount := EventID {0, 0}; <* LL > { mu } *>
  (* two bits of count thrown away in the interests of ease of inspection
     by people *)

PROCEDURE NextEventID (): EventID =
  <* LL.sup = mu *>
  BEGIN
    IF eCount[0] = LAST(Int32) THEN
      INC(eCount[1]);
      eCount[0] := 0;
    ELSE
      INC(eCount[0]);
    END;
    RETURN eCount;
  END NextEventID;

PROCEDURE EventLE (e1, e2: EventID): BOOLEAN =
  BEGIN
    IF e1[1] = e2[1] THEN
      IF e1[0] <= e2[0] THEN RETURN TRUE
      ELSE RETURN FALSE
      END;
    ELSIF e1[1] < e2[1] THEN RETURN TRUE
    ELSE RETURN FALSE
    END;
  END EventLE;


(*----------------------- exports to NCGMonitor ----------------------------*)

PROCEDURE MonitorDump (): Dump =
  <* LL.sup < mu *>
  VAR res := NEW(Dump);
      it: ObjTbl.Iterator;
      wrep: WireRep.T;
      oe: ObjElem.T;

  PROCEDURE DumpLoc (loc: Transport.Location; <*UNUSED*> cl: REFANY): BOOLEAN=
    VAR
      ld := NEW(LDump);
      wrep: WireRep.T;
      de: DirtyElem.T;
      it: DirtyTbl.Iterator;
    BEGIN
      ld.info := loc.getInfo();
      ld.ep   := loc.getEp();
      IF loc.exports # NIL THEN
	it := loc.exports.iterate();
        WHILE it.next(wrep, de) DO
	  ld.exports := RefList.Cons(
            NEW (DDump, wrep := wrep, de := de), ld.exports);
        END;
      END;
      res.locs := RefList.Cons(ld, res.locs);
      RETURN FALSE;
    END DumpLoc;

  (* <* FATAL ANY *> *)
  BEGIN
    LOCK mu DO
      IF NOT started THEN RuntimeInit(); END;
      it := objTbl.iterate();
      WHILE it.next(wrep, oe) DO
        IF oe.ref # NIL THEN
      	  (* exported concrete object *)
      	  VAR c := oe.ref; xi := NARROW(c.r, ExportInfo); tc := TYPECODE(c);
          BEGIN
      	    <* ASSERT wrep = c.w OR wrep = WireRep.SpecialT *>
	    res.concs := RefList.Cons(
              NEW(CDump,
	        obj := wrep, fp := RTTypeFP.ToFingerprint(tc),
                typeName := RTTypeSRC.TypecodeName(tc),
	        pinCount := xi.pinCount),
              res.concs);
      	  END;
        ELSIF oe.ready THEN
      	  VAR s: NetObj.T := WeakRef.ToRef(oe.weakRef); tc := TYPECODE(s);
          BEGIN
            IF s # NIL THEN
      	      (* live surrogate object *)
      	      <* ASSERT wrep = s.w *>
      	      res.srgts := RefList.Cons(
                NEW(SDump,
      	          obj := s.w, fp := RTTypeFP.ToFingerprint(tc),
                  typeName := RTTypeSRC.TypecodeName(tc),
      	          owner := NARROW(s.r, Transport.Location).getEp()),
                res.srgts);
      	    END;
      	  END;
        ELSE (* creating a surrogate *)
        END;
      END;
      VAR it := TransportRegistry.Iterate (); tr: Transport.T; BEGIN
        WHILE it.next (tr) DO tr.enumerateLocs (DumpLoc) END;
      END;
      RETURN res;
    END;
  END MonitorDump;


PROCEDURE MonitorDumpNames (): RefList.T =
  <* LL.sup < mu *>
  VAR
    res: RefList.T := NIL;
    key: TEXT; r: REFANY;
    it: TextRefTbl.Iterator;
  BEGIN
    LOCK mu DO
      IF NOT started THEN RuntimeInit(); END;
      it := localAgentT.table.iterate();
      WHILE it.next(key, r) DO
        VAR w: WireRep.T; BEGIN
          IF r = NIL THEN
            w := WireRep.NullT;
          ELSE
            w := NARROW(r, NetObj.T).w;
          END;
          res := RefList.Cons(NEW(NDump, name := key, obj := w), res);
        END;
      END;
      RETURN res;
    END;
  END MonitorDumpNames;


(*-------------------------- initialization -------------------------------*)

PROCEDURE RuntimeInit() =
  <* LL.sup = mu *>
  BEGIN
    objTbl := NEW(ObjTbl.Default).init();
    spaceTbl := NEW(FPRefTbl.Default).init();
    EVAL spaceTbl.put(SpaceID.Mine(), TransportRegistry.LocalAdr());
    InitAgent();
    started := TRUE;
  END RuntimeInit;
  
PROCEDURE AddToLocTbl(loc: Transport.Location) =
  VAR ol := locTbl.locs;
  BEGIN
    IF ol = NIL THEN
      locTbl.free := 0;
      locTbl.locs := NEW(REF ARRAY OF Transport.Location, 10);
      FOR i := 0 TO LAST(locTbl.locs^) DO locTbl.locs[i] := NIL; END;
    ELSE
      WHILE locTbl.locs[locTbl.free] # NIL DO
        INC(locTbl.free);
        IF locTbl.free = NUMBER(locTbl.locs^) THEN
          locTbl.locs := NEW(REF ARRAY OF Transport.Location, NUMBER(ol^) * 2);
          SUBARRAY(locTbl.locs^, 0, NUMBER(ol^)) := ol^;
          FOR i := NUMBER(ol^) TO LAST(locTbl.locs^) DO
            locTbl.locs[i] := NIL;
          END;
        END;
      END;
    END;
    locTbl.locs[locTbl.free] := loc;
    loc.locTblIndex := locTbl.free;
  END AddToLocTbl;

PROCEDURE RaiseError(a: Atom.T) RAISES {NetObj.Error} =
  BEGIN
    RAISE NetObj.Error(AtomList.List1(a));
  END RaiseError;

EXCEPTION FatalError;

PROCEDURE Die() RAISES {} =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Die;

BEGIN
  <* ASSERT BITSIZE (Int32) = 32 *>
  (* TODO: remove this assertion when the compiler problem mentioned in
     SpecialObj.i3 is fixed. *)
  MissingDispatcher := Atom.FromText("NetObj.MissingDispatcher");
  typeTbl := NEW(IntRefTbl.Default).init();
  fpToTc := NEW(FPRefTbl.Default).init();
END NetObjRT.



(*  old version of TowerToSurrogateTC

PROCEDURE TowerToSurrogateTC(fpTower: FpTower; VAR vers: VersionList): Typecode
  <* LL.sup < mu *> =
  VAR tc: Typecode;
      r: REFANY;
      firstInfo: TypeInfo := NIL;
  BEGIN
    LOCK mu DO
      IF fpToTc.get(fpTower[0], r) THEN
        RETURN MatchVer(r);
      END;
      FOR i := 0 TO LAST(fpTower^) DO
        tc := RTTypeFP.FromFingerprint(fpTower[i]);
        IF tc # RTType.NoSuchType THEN
          IF typeTbl.get(tc, r) THEN
            info := NARROW(r, TypeInfo);
            EVAL fpToTc.put(fpTower[0], info);
            RETURN MatchVer(info);
          END;
        END;
      END;
    END;
    Die();
  END TowerToSurrogateTC;
*)

