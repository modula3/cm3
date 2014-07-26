(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(*  Portions Copyright 1996-2000, Critical Mass, Inc.        *)
(* See file COPYRIGHT-CMASS for details.                     *)
(*                                                           *)
(*| Last modified on Thu May  4 14:02:27 PDT 1995 by kalsow  *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)
(*|      modified on Tue Mar  9 08:45:18 PST 1993 by jdd     *)

UNSAFE MODULE RTAllocator
EXPORTS RTAllocator, RTAllocCnts, RTHooks, RTHeapRep;

IMPORT Cstdlib, RT0, RTMisc, RTOS, RTType, Scheduler, RTThread;
IMPORT RuntimeError AS RTE, Word;
FROM RTType IMPORT Typecode;

(* In the following procedures, "RTType.Get(tc)" will fail if "tc" is not
   proper. *)

TYPE
  TK = RT0.TypeKind;

(*----------------------------------------------------------- RTAllocator ---*)

PROCEDURE NewTraced(tc: Typecode): REFANY
  RAISES {OutOfMemory} =
  VAR res := GetTraced(RTType.Get(tc));
  BEGIN
    IF (res = NIL) THEN RAISE OutOfMemory; END;
    RETURN res;
  END NewTraced;

PROCEDURE NewUntraced(tc: Typecode): ADDRESS
  RAISES {OutOfMemory} =
  VAR res := GetUntracedRef(RTType.Get(tc));
  BEGIN
    IF (res = NIL) THEN RAISE OutOfMemory; END;
    RETURN res;
  END NewUntraced;

PROCEDURE NewUntracedObject(tc: Typecode): UNTRACED ROOT
  RAISES {OutOfMemory} =
  VAR res := GetUntracedObj(RTType.Get(tc));
  BEGIN
    IF (res = NIL) THEN RAISE OutOfMemory; END;
    RETURN res;
  END NewUntracedObject;

PROCEDURE NewTracedArray(tc: Typecode; READONLY s: Shape): REFANY
  RAISES {OutOfMemory} =
  VAR res := GetOpenArray(RTType.Get(tc), s);
  BEGIN
    IF (res = NIL) THEN RAISE OutOfMemory; END;
    RETURN res;
  END NewTracedArray;

PROCEDURE NewUntracedArray(tc: Typecode; READONLY s: Shape): ADDRESS
  RAISES {OutOfMemory} =
  VAR res := GetUntracedOpenArray(RTType.Get(tc), s);
  BEGIN
    IF (res = NIL) THEN RAISE OutOfMemory; END;
    RETURN res;
  END NewUntracedArray;

PROCEDURE Clone (ref: REFANY): REFANY
  RAISES {OutOfMemory} =
  VAR
    hdr: RefHeader;  def: RT0.TypeDefn;  dataSize: CARDINAL;
    res: ADDRESS;
    thread := RTThread.MyHeapState();
  BEGIN
    IF (ref = NIL) THEN RETURN NIL; END;
    IF Word.And(LOOPHOLE(ref, Word.T), 1) # 0 THEN RETURN ref; END;

    hdr := LOOPHOLE(ref, ADDRESS) - ADRSIZE(Header);
    def := RTType.Get(hdr.typecode);
    dataSize := ReferentSize(hdr);

    INC(thread.inCritical);
    res := AllocTraced(dataSize, def.dataAlignment, thread^);
    IF res = NIL THEN DEC(thread.inCritical); RAISE OutOfMemory; END;
    LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
        Header{typecode := def.typecode, dirty := TRUE};
    RTMisc.Copy(LOOPHOLE(ref, ADDRESS), res, dataSize);
    IF def.kind = ORD (TK.Array) THEN
      (* open array: update the internal pointer *)
      LOOPHOLE(res, UNTRACED REF ADDRESS)^ := res + def.dataSize;
    END;
    DEC(thread.inCritical);

    IF countsOn THEN
      IF def.kind = ORD (TK.Array)
        THEN BumpSize(def.typecode, dataSize)
        ELSE BumpCnt (def.typecode);
      END;
    END;
    IF (callback # NIL) THEN callback (LOOPHOLE(res, REFANY)) END;
    RETURN LOOPHOLE(res, REFANY);
  END Clone;

(*--------------------------------------------------------------- RTHooks ---*)

PROCEDURE Allocate (defn: ADDRESS): REFANY =
  VAR res := GetTraced(defn);
  BEGIN
    IF (res = NIL) THEN RTE.Raise(RTE.T.OutOfMemory); END;
    RETURN res;
  END Allocate;

PROCEDURE AllocateTracedRef (defn: ADDRESS): REFANY =
  VAR res := GetTracedRef(defn);
  BEGIN
    IF (res = NIL) THEN RTE.Raise(RTE.T.OutOfMemory); END;
    RETURN res;
  END AllocateTracedRef;

PROCEDURE AllocateTracedObj (defn: ADDRESS): ROOT =
  VAR res := GetTracedObj(defn);
  BEGIN
    IF (res = NIL) THEN RTE.Raise(RTE.T.OutOfMemory); END;
    RETURN res;
  END AllocateTracedObj;

PROCEDURE AllocateUntracedRef (defn: ADDRESS): ADDRESS =
  VAR res := GetUntracedRef(defn);
  BEGIN
    IF (res = NIL) THEN RTE.Raise(RTE.T.OutOfMemory); END;
    RETURN res;
  END AllocateUntracedRef;

PROCEDURE AllocateUntracedObj (defn: ADDRESS): UNTRACED ROOT =
  VAR res := GetUntracedObj(defn);
  BEGIN
    IF (res = NIL) THEN RTE.Raise(RTE.T.OutOfMemory); END;
    RETURN res;
  END AllocateUntracedObj;

PROCEDURE AllocateOpenArray (defn: ADDRESS; READONLY s: Shape): REFANY =
  VAR res := GetOpenArray(defn, s);
  BEGIN
    IF (res = NIL) THEN RTE.Raise(RTE.T.OutOfMemory); END;
    RETURN res;
  END AllocateOpenArray;

PROCEDURE AllocateUntracedOpenArray (defn : ADDRESS;
                                     READONLY s: Shape): ADDRESS =
  VAR res := GetUntracedOpenArray(defn, s);
  BEGIN
    IF (res = NIL) THEN RTE.Raise(RTE.T.OutOfMemory); END;
    RETURN res;
  END AllocateUntracedOpenArray;

PROCEDURE DisposeUntracedRef (VAR a: ADDRESS) =
  BEGIN
    IF a # NIL THEN
      Scheduler.DisableSwitching();
      Cstdlib.free(a);
      a := NIL;
      Scheduler.EnableSwitching();
    END;
  END DisposeUntracedRef;

PROCEDURE DisposeUntracedObj (VAR a: UNTRACED ROOT) =
  VAR p: ADDRESS;
  BEGIN
    IF a # NIL THEN
      p := a - MAX(BYTESIZE(Header), RTType.Get(TYPECODE (a)).dataAlignment);
      Scheduler.DisableSwitching();
      Cstdlib.free(p);
      Scheduler.EnableSwitching();
      a := NIL;
    END;
  END DisposeUntracedObj;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE GetTraced (def: RT0.TypeDefn): REFANY =
  BEGIN
    CASE def.kind OF
    | ORD(TK.Ref) => RETURN GetTracedRef(def);
    | ORD(TK.Obj) => RETURN GetTracedObj(def);
    ELSE
      RTE.Raise(RTE.T.ValueOutOfRange);
    END;
    RETURN NIL;
  END GetTraced;      

PROCEDURE GetTracedRef (def: RT0.TypeDefn): REFANY =
  VAR
    res: ADDRESS;
    thread := RTThread.MyHeapState();
  BEGIN
    IF def.typecode = 0 OR def.traced # 1 OR def.kind # ORD(TK.Ref) THEN
      RTE.Raise(RTE.T.ValueOutOfRange);
    END;

    INC(thread.inCritical);
    res := AllocTraced(def.dataSize, def.dataAlignment, thread^);
    IF res = NIL THEN DEC(thread.inCritical); RETURN NIL; END;
    LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
        Header{typecode := def.typecode, dirty := TRUE};
    IF def.initProc # NIL THEN def.initProc(res) END;
    DEC(thread.inCritical);

    IF countsOn THEN BumpCnt(def.typecode) END;
    IF (callback # NIL) THEN callback (LOOPHOLE(res, REFANY)) END;
    RETURN LOOPHOLE(res, REFANY);
  END GetTracedRef;

PROCEDURE GetTracedObj (def: RT0.TypeDefn): ROOT =
  VAR
    res: ADDRESS;
    thread := RTThread.MyHeapState();
  BEGIN
    IF def.typecode = 0 OR def.traced # 1 OR def.kind # ORD(TK.Obj) THEN
      RTE.Raise(RTE.T.ValueOutOfRange);
    END;
    
    INC(thread.inCritical);
    res := AllocTraced(def.dataSize, def.dataAlignment, thread^);
    IF res = NIL THEN DEC(thread.inCritical); RETURN NIL; END;
    LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
        Header{typecode := def.typecode, dirty := TRUE};
    InitObj (res, LOOPHOLE(def, RT0.ObjectTypeDefn));
    DEC(thread.inCritical);

    IF countsOn THEN BumpCnt(def.typecode) END;
    IF (callback # NIL) THEN callback (LOOPHOLE(res, REFANY)) END;
    RETURN LOOPHOLE(res, REFANY);
  END GetTracedObj;

PROCEDURE GetUntracedRef (def: RT0.TypeDefn): ADDRESS =
  VAR res : ADDRESS;
      dataSize := def.dataSize;
  BEGIN
    IF def.typecode = 0 OR def.traced # 0 OR def.kind # ORD(TK.Ref) THEN
      RTE.Raise(RTE.T.ValueOutOfRange);
    END;
    Scheduler.DisableSwitching();
    res := Cstdlib.calloc(1, dataSize);
    Scheduler.EnableSwitching();
    IF res = NIL THEN RETURN NIL END;
    IF def.initProc # NIL THEN def.initProc(res); END;
    IF countsOn THEN BumpCnt(def.typecode) END;
    RETURN res;
  END GetUntracedRef;

PROCEDURE GetUntracedObj (def: RT0.TypeDefn): UNTRACED ROOT =
  (* NOTE: result requires special treatment by DisposeUntracedObj *)
  VAR
    hdrSize := MAX(BYTESIZE(Header), def.dataAlignment);
    size    := hdrSize + def.dataSize;
    res     : ADDRESS;
  BEGIN
    IF def.typecode = 0 OR def.traced # 0 OR def.kind # ORD(TK.Obj) THEN
      RTE.Raise(RTE.T.ValueOutOfRange);
    END;
    Scheduler.DisableSwitching();
    res := Cstdlib.calloc(1, size);
    Scheduler.EnableSwitching();
    IF res = NIL THEN RETURN NIL END;
    res := res + hdrSize;
    LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
        Header{typecode := def.typecode};
    InitObj (res, LOOPHOLE(def, RT0.ObjectTypeDefn));
    IF countsOn THEN BumpCnt (def.typecode) END;
    RETURN res;
  END GetUntracedObj;

PROCEDURE InitObj (res: ADDRESS;  def: RT0.ObjectTypeDefn) =
  BEGIN
    LOOPHOLE(res, UNTRACED REF ADDRESS)^ := def.defaultMethods;
    WHILE def # NIL DO
      IF def.common.initProc # NIL THEN def.common.initProc(res); END;
      IF def.common.kind # ORD (TK.Obj) THEN EXIT; END;
      def := LOOPHOLE (def.parent, RT0.ObjectTypeDefn);
    END;
  END InitObj;

PROCEDURE GetOpenArray (def: RT0.TypeDefn; READONLY s: Shape): REFANY =
  VAR
    res: ADDRESS;
    dataSize: CARDINAL;
    thread := RTThread.MyHeapState();
  BEGIN
    IF def.typecode = 0 OR def.traced # 1 OR def.kind # ORD(TK.Array) THEN
      RTE.Raise(RTE.T.ValueOutOfRange);
    END;
    dataSize := ArraySize(LOOPHOLE(def, RT0.ArrayTypeDefn), s);

    INC(thread.inCritical);
    res := AllocTraced(dataSize, def.dataAlignment, thread^);
    IF res = NIL THEN DEC(thread.inCritical); RETURN NIL; END;
    LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
        Header{typecode := def.typecode, dirty := TRUE};
    InitArray (res, LOOPHOLE(def, RT0.ArrayTypeDefn), s);
    DEC(thread.inCritical);

    IF countsOn THEN BumpSize (def.typecode, dataSize) END;
    IF (callback # NIL) THEN callback (LOOPHOLE(res, REFANY)) END;
    RETURN LOOPHOLE(res, REFANY);
  END GetOpenArray;

PROCEDURE GetUntracedOpenArray (def: RT0.TypeDefn; READONLY s: Shape): ADDRESS =
  VAR
    res : ADDRESS;
    dataSize: CARDINAL;
  BEGIN
    IF def.typecode = 0 OR def.traced # 0 OR def.kind # ORD(TK.Array) THEN
      RTE.Raise(RTE.T.ValueOutOfRange);
    END;
    dataSize := ArraySize(LOOPHOLE(def, RT0.ArrayTypeDefn), s);

    Scheduler.DisableSwitching();
    res := Cstdlib.calloc(1, dataSize);
    Scheduler.EnableSwitching();
    IF res = NIL THEN RETURN NIL END;
    InitArray (res, LOOPHOLE(def, RT0.ArrayTypeDefn), s);

    IF countsOn THEN BumpSize (def.typecode, dataSize) END;
    RETURN res;
  END GetUntracedOpenArray;

PROCEDURE InitArray (res: ADDRESS; def: RT0.ArrayTypeDefn; READONLY s: Shape) =
  BEGIN
    WITH data_start = res + def.common.dataSize DO
      LOOPHOLE(res, UNTRACED REF ADDRESS)^ := data_start;
    END;
    FOR i := 0 TO NUMBER(s) - 1 DO
      LOOPHOLE(res + ADRSIZE(ADDRESS) + i * ADRSIZE(INTEGER),
               UNTRACED REF INTEGER)^ := s[i];
    END;
    IF def.common.initProc # NIL THEN def.common.initProc(res) END;
  END InitArray;

PROCEDURE ArraySize (def: RT0.ArrayTypeDefn;  READONLY s: Shape): CARDINAL =
  VAR n_elts := 1;  c: CARDINAL;
  BEGIN
    FOR i := 0 TO NUMBER(s) - 1 DO
      c := s[i];  (* force a range check *)
      n_elts := c * n_elts;
    END;
    RETURN RTMisc.Upper(def.common.dataSize + def.elementSize * n_elts,
                        BYTESIZE(Header));
  END ArraySize;

PROCEDURE AllocTraced (dataSize, dataAlignment: CARDINAL;
                       VAR thread: ThreadState): ADDRESS =
  VAR
    res       := thread.pool.next + ADRSIZE(Header);
    cur_align := Word.And(LOOPHOLE(res, INTEGER), MaxAlignMask);
    alignment := align[cur_align, dataAlignment];
    nextPtr   := res + (alignment + dataSize);
  BEGIN
    IF nextPtr > thread.pool.limit THEN
      (* not enough space left in the pool, take the long route *)
      res := NIL;  nextPtr := NIL;  (* in case of GC... *)
      TRY
        <*ASSERT thread.inCritical > 0*>
        DEC(thread.inCritical);
        RTOS.LockHeap();
        (* make sure the collector gets a chance to keep up with NEW... *)
        CollectEnough();
        RETURN LongAlloc (dataSize, dataAlignment, thread.pool);
      FINALLY
        RTOS.UnlockHeap();
        INC(thread.inCritical);
      END;
    END;

    (* Align the referent *)
    IF alignment # 0 THEN
      InsertFiller(thread.pool.next, alignment);
      thread.pool.next := thread.pool.next + alignment;
      res := thread.pool.next + ADRSIZE(Header);
    END;

    thread.pool.next := nextPtr;
    RETURN res;
  END AllocTraced;

(*---------------------------------------------------------- RTAllocCnts ---*)

PROCEDURE BumpCnt (tc: RT0.Typecode) =
  BEGIN
    TRY
      RTOS.LockHeap();
      IF (tc >= n_types) THEN ExpandCnts (tc); END;
      WITH z = n_objects[tc] DO z := Word.Plus (z, 1) END;
    FINALLY
      RTOS.UnlockHeap();
    END;
  END BumpCnt;

PROCEDURE BumpSize (tc: RT0.Typecode;  size: INTEGER) =
  BEGIN
    TRY
      RTOS.LockHeap();
      IF (tc >= n_types) THEN ExpandCnts (tc); END;
      WITH z = n_objects[tc] DO z := Word.Plus (z, 1)    END;
      WITH z = n_bytes[tc]   DO z := Word.Plus (z, size) END;
    FINALLY
      RTOS.UnlockHeap();
    END;
  END BumpSize;

PROCEDURE ExpandCnts (tc: RT0.Typecode) =
  VAR
    goal      := MAX (tc, RTType.MaxTypecode ());
    new_cnt   : INTEGER := 512;
    new_mem   : INTEGER;
    new_cnts  : ADDRESS;
    new_sizes : ADDRESS;
    old_cnts  := n_objects;
    old_sizes := n_bytes;
  BEGIN
    IF (n_types > 0) THEN new_cnt := n_types; END;
    WHILE (new_cnt <= goal) DO INC (new_cnt, new_cnt); END;

    new_mem   := new_cnt * BYTESIZE (INTEGER);
    new_cnts  := Malloc (new_mem);
    new_sizes := Malloc (new_mem);

    IF (old_cnts # NIL) THEN
      RTMisc.Copy (old_cnts,  new_cnts,  n_types * BYTESIZE (INTEGER)); 
      RTMisc.Copy (old_sizes, new_sizes, n_types * BYTESIZE (INTEGER));
    END;

    n_objects := new_cnts;
    n_bytes   := new_sizes;
    n_types   := new_cnt;
    (* "n_types" is assigned last in case anyone is reading the arrays
       while we're updating them... *)

    IF (old_cnts # NIL) THEN
      Cstdlib.free (old_cnts);
      Cstdlib.free (old_sizes);
    END;
  END ExpandCnts;
    
PROCEDURE Malloc (size: INTEGER): ADDRESS =
  VAR res: ADDRESS;
  BEGIN
    Scheduler.DisableSwitching();
    res := Cstdlib.calloc(1, size);
    Scheduler.EnableSwitching();
    IF res = NIL THEN RTE.Raise(RTE.T.OutOfMemory) END;
    RETURN res;
  END Malloc;

BEGIN
END RTAllocator.
