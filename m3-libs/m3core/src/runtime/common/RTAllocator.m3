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

IMPORT Cstdlib, RT0, RTHeap, RTMisc, RTOS, RTType;
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
    IF (res = NIL) THEN RAISE OutOfMemory;  END;
    RETURN res;
  END NewUntracedArray;

PROCEDURE Clone (ref: REFANY): REFANY
  RAISES {OutOfMemory} =
  VAR x: REFANY;  defn: RT0.TypeDefn;  tc: INTEGER;
  BEGIN
    IF (ref = NIL) THEN RETURN NIL; END;

    tc := TYPECODE (ref);
    defn := RTType.Get (tc);

    CASE defn.kind OF
    | ORD (RT0.TypeKind.Ref), ORD (RT0.TypeKind.Obj) =>
        x := NewTraced (tc);
    | ORD (RT0.TypeKind.Array) =>
        VAR nDims: INTEGER;  shape: UnsafeArrayShape;  BEGIN
          UnsafeGetShape (ref, nDims, shape);
          x := NewTracedArray (tc, SUBARRAY (shape^, 0, nDims));
        END;
    ELSE
        x := NIL; (* force a crash *)
    END;

    (* finally, copy the data into the new object *)
    RTMisc.Copy (RTHeap.GetDataAdr (ref), RTHeap.GetDataAdr (x),
                 RTHeap.GetDataSize (ref));

    RETURN x;
  END Clone;

(*--------------------------------------------------------------- RTHooks ---*)

PROCEDURE Allocate (defn: ADDRESS): REFANY =
  VAR res := GetTraced(defn);
  BEGIN
    IF (res = NIL) THEN  RAISE RTE.E (RTE.T.OutOfMemory);  END;
    RETURN res;
  END Allocate;

PROCEDURE AllocateUntracedRef (defn: ADDRESS): ADDRESS =
  VAR res := GetUntracedRef(defn);
  BEGIN
    IF (res = NIL) THEN  RAISE RTE.E (RTE.T.OutOfMemory);  END;
    RETURN res;
  END AllocateUntracedRef;

PROCEDURE AllocateUntracedObj (defn: ADDRESS): UNTRACED ROOT =
  VAR res := GetUntracedObj(defn);
  BEGIN
    IF (res = NIL) THEN  RAISE RTE.E (RTE.T.OutOfMemory);  END;
    RETURN res;
  END AllocateUntracedObj;

PROCEDURE AllocateOpenArray (defn: ADDRESS; READONLY s: Shape): REFANY =
  VAR res := GetOpenArray(defn, s);
  BEGIN
    IF (res = NIL) THEN  RAISE RTE.E (RTE.T.OutOfMemory);  END;
    RETURN res;
  END AllocateOpenArray;

PROCEDURE AllocateUntracedOpenArray (defn : ADDRESS;
                            READONLY s    : Shape): ADDRESS =
  VAR res := GetUntracedOpenArray(defn, s);
  BEGIN
    IF (res = NIL) THEN  RAISE RTE.E (RTE.T.OutOfMemory);  END;
    RETURN res;
  END AllocateUntracedOpenArray;

PROCEDURE DisposeUntracedRef (VAR a: ADDRESS) =
  BEGIN
    RTOS.LockHeap();
    IF a # NIL THEN Cstdlib.free(a); a := NIL; END;
    RTOS.UnlockHeap();
  END DisposeUntracedRef;

PROCEDURE DisposeUntracedObj (VAR a: UNTRACED ROOT) =
  VAR def: RT0.TypeDefn;
  BEGIN
    RTOS.LockHeap();
    IF a # NIL THEN
      def := RTType.Get (TYPECODE (a));
      Cstdlib.free (a - MAX(BYTESIZE(Header), def.dataAlignment));
      a := NIL;
    END;
    RTOS.UnlockHeap();
  END DisposeUntracedObj;

(*-------------------------------------------------------------- internal ---*)

VAR
  initCache: ARRAY [0 .. 4095] OF ADDRESS; (* initialized contents for
                                              freshly allocated objects *)

PROCEDURE GetTraced (defn: ADDRESS): REFANY =
  VAR
    def : RT0.TypeDefn := defn;
    tc  : Typecode := def.typecode;
    res : ADDRESS;
    sz  := BYTESIZE (Header) + def.dataSize;
  BEGIN
    IF (tc = 0) OR (def.traced = 0) OR (def.kind = ORD (TK.Array)) THEN
      <*NOWARN*> EVAL VAL (-1, CARDINAL); (* force a range fault *)
    END;

    RTOS.LockHeap();
    BEGIN
      res := AllocTraced(def.dataSize, def.dataAlignment, newPool);
      IF (res = NIL) THEN  RTOS.UnlockHeap(); RETURN NIL;  END;

      BumpCnt (tc);

      IF (tc <= LAST (initCache)) AND (initCache[tc] # NIL) THEN
        RTMisc.Copy(initCache[tc], res - ADRSIZE(Header), sz);
      ELSE
        InitRef (res, def);
        IF (def.dataSize <= BYTESIZE(def^)) AND (tc <= LAST (initCache)) THEN
          VAR copy := Cstdlib.malloc(sz); BEGIN
            IF (copy # NIL) THEN
              initCache[tc] := copy;
              RTMisc.Copy(res - ADRSIZE(Header), copy, sz);
            END;
          END;
        END;
      END;
    END;
    RTOS.UnlockHeap();
    IF (callback # NIL) THEN callback (LOOPHOLE (res, REFANY)); END;
    RETURN LOOPHOLE(res, REFANY);
  END GetTraced;

PROCEDURE GetUntracedRef (defn: ADDRESS): ADDRESS =
  VAR
    def : RT0.TypeDefn := defn;
    tc  : Typecode := def.typecode;
    res : ADDRESS;
  BEGIN
    IF (tc = 0) OR (def.traced # 0) OR (def.kind # ORD (TK.Ref)) THEN
      <*NOWARN*> EVAL VAL (-1, CARDINAL); (* force a range fault *)
    END;
    RTOS.LockHeap();
    BEGIN
      res := Cstdlib.malloc(def.dataSize);
      IF (res = NIL) THEN RTOS.UnlockHeap(); RETURN NIL; END;
      BumpCnt (tc);
    END;
    RTOS.UnlockHeap();
    RTMisc.Zero (res, def.dataSize);
    IF def.initProc # NIL THEN def.initProc(res); END;
    RETURN res;
  END GetUntracedRef;

PROCEDURE GetUntracedObj (defn: ADDRESS): UNTRACED ROOT =
  (* NOTE: result requires special treatment by DisposeUntracedObj *)
  VAR
    def     : RT0.TypeDefn := defn;
    hdrSize := MAX(BYTESIZE(Header), def.dataAlignment);
    tc      : Typecode := def.typecode;
    res     : ADDRESS;
  BEGIN
    IF (tc = 0) OR (def.traced # 0) OR (def.kind # ORD (TK.Obj)) THEN
      <*NOWARN*> EVAL VAL (-1, CARDINAL); (* force a range fault *)
    END;
    RTOS.LockHeap();
    BEGIN
      res := Cstdlib.malloc(hdrSize + def.dataSize);
      IF (res = NIL) THEN RTOS.UnlockHeap(); RETURN NIL; END;
      BumpCnt (tc);
    END;
    RTOS.UnlockHeap();
    res := res + hdrSize;
    InitRef (res, def);
    RETURN res;
  END GetUntracedObj;

PROCEDURE InitRef (res: ADDRESS;  def: RT0.TypeDefn) =
  VAR hdr := LOOPHOLE(res - ADRSIZE(Header), RefHeader);
  BEGIN
    hdr^ := RT0.RefHeader {};
    hdr.typecode := def.typecode;
    RTMisc.Zero(res, def.dataSize);

    IF (def.kind = ORD(TK.Obj)) THEN
      VAR objdef := LOOPHOLE (def, RT0.ObjectTypeDefn); BEGIN
        LOOPHOLE(res, UNTRACED REF ADDRESS)^ := objdef.defaultMethods;
        WHILE objdef # NIL DO
          IF objdef.common.initProc # NIL THEN objdef.common.initProc(res); END;
          IF objdef.common.kind # ORD (TK.Obj) THEN EXIT; END;
          objdef := LOOPHOLE (objdef.parent, RT0.ObjectTypeDefn);
        END;
      END;
    ELSE
      IF def.initProc # NIL THEN def.initProc(res); END;
    END;
  END InitRef;

TYPE
  ArrayInfo = RECORD
    def        : RT0.ArrayTypeDefn;
    alignment  : INTEGER;
    nDataBytes : INTEGER;
    nBytes     : INTEGER;
    tc         : Typecode;
  END;

PROCEDURE GetOpenArray (defn: ADDRESS; READONLY s: Shape): REFANY =
  VAR res: ADDRESS;  info: ArrayInfo;
  BEGIN
    GetArrayInfo (defn, s, info, TRUE);

    RTOS.LockHeap();
    BEGIN
      res := AllocTraced(info.nBytes, info.alignment, newPool);
      IF (res = NIL) THEN  RTOS.UnlockHeap(); RETURN NIL;  END;
      LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
        Header{typecode := info.tc, forwarded := FALSE};
      BumpSize (info.tc, info.nBytes);
    END;
    RTOS.UnlockHeap();

    InitArray (res, s, info);
    IF (callback # NIL) THEN callback (LOOPHOLE (res, REFANY)); END;
    RETURN LOOPHOLE(res, REFANY);
  END GetOpenArray;

PROCEDURE GetUntracedOpenArray (defn: ADDRESS;  READONLY s: Shape): ADDRESS =
  VAR res: ADDRESS;  info: ArrayInfo;
  BEGIN
    GetArrayInfo (defn, s, info, FALSE);

    RTOS.LockHeap();
    BEGIN
      res := Cstdlib.malloc(info.nBytes);
      IF (res = NIL) THEN RTOS.UnlockHeap(); RETURN NIL; END;
      BumpSize (info.tc, info.nBytes);
    END;
    RTOS.UnlockHeap();

    InitArray (res, s, info);
    RETURN res;
  END GetUntracedOpenArray;

PROCEDURE GetArrayInfo (def: RT0.TypeDefn;  READONLY s: Shape;
                        VAR ai: ArrayInfo;  traced: BOOLEAN) =
  VAR n_elts := 1;  c: CARDINAL;
  BEGIN
    ai.def        := LOOPHOLE (def, RT0.ArrayTypeDefn);
    ai.tc         := def.typecode;
    ai.alignment  := def.dataAlignment;

    IF (def.typecode = 0)
      OR (def.traced # ORD (traced))
      OR (def.kind # ORD (TK.Array))
      OR (NUMBER(s) # ai.def.nDimensions) THEN
      <*NOWARN*> EVAL VAL (-1, CARDINAL); (* force a range fault *)
    END;

    FOR i := 0 TO NUMBER(s) - 1 DO
      c := s[i];  (* force a range check *)
      n_elts := c * n_elts;
    END;

    ai.nDataBytes := ai.def.elementSize * n_elts;
    ai.nBytes     := RTMisc.Upper(def.dataSize + ai.nDataBytes, BYTESIZE(Header));
  END GetArrayInfo;

PROCEDURE InitArray (res: ADDRESS;  READONLY s: Shape;  VAR info: ArrayInfo) =
  VAR data_start := res + info.def.common.dataSize;
  BEGIN
    LOOPHOLE(res, UNTRACED REF ADDRESS)^ := data_start;
    FOR i := 0 TO NUMBER(s) - 1 DO
      LOOPHOLE(res + ADRSIZE(ADDRESS) + i * ADRSIZE(INTEGER),
               UNTRACED REF INTEGER)^ := s[i];
    END;
    RTMisc.Zero(data_start, info.nDataBytes);

    IF info.def.common.initProc # NIL THEN info.def.common.initProc(res); END;
  END InitArray;

(*---------------------------------------------------------- RTAllocCnts ---*)

PROCEDURE BumpCnt (tc: RT0.Typecode) =
  BEGIN
    IF (tc >= n_types) THEN ExpandCnts (tc); END;
    WITH z = n_objects[tc] DO z := Word.Plus (z, 1) END;
  END BumpCnt;

PROCEDURE BumpSize (tc: RT0.Typecode;  size: INTEGER) =
  BEGIN
    IF (tc >= n_types) THEN ExpandCnts (tc); END;
    WITH z = n_objects[tc] DO z := Word.Plus (z, 1)    END;
    WITH z = n_bytes[tc]   DO z := Word.Plus (z, size) END;
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
  VAR res := Cstdlib.malloc (size);
  BEGIN
    IF (res = NIL) THEN RAISE RTE.E (RTE.T.OutOfMemory); END;
    RTMisc.Zero (res, size);
    RETURN res;
  END Malloc;

BEGIN
END RTAllocator.
