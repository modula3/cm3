(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(*| Last modified on Thu May  4 14:02:27 PDT 1995 by kalsow  *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)
(*|      modified on Tue Mar  9 08:45:18 PST 1993 by jdd     *)

UNSAFE MODULE RTAllocator EXPORTS RTAllocator, RTHooks;

IMPORT Cstdlib, RT0, RT0u, RTMisc, RTOS, RTType, Word;
FROM RTType IMPORT Typecode;
FROM RTMisc IMPORT FatalErrorI;
FROM RTHeapRep IMPORT Header, RefHeader, AllocForNew, Malloc;

(* In the following procedures, "RTType.Get(tc)" will fail if "tc" is not
   proper. *)

(*----------------------------------------------------------- RTAllocator ---*)

(* NewTraced returns a reference to a freshly allocated and initialized,
   traced referent with typecode "tc".  It is a checked runtime error if
   "tc" does not name a traced reference type other than REFANY, or if
   its referent is an open array. *)

PROCEDURE NewTraced(tc: Typecode): REFANY =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced = 0 OR def.nDimensions # 0 THEN
      FatalErrorI("RTAllocator.NewTraced: improper typecode: ", tc);
    END;
    RETURN Allocate(def);
  END NewTraced;

(* NewUntraced returns a reference to a freshly allocated and initialized,
   untraced referent with typecode "tc".  It is a checked runtime error if
   "tc" does not name an untraced reference type other than ADDRESS, or if
   it names an untraced object type, or if its referent is an open
   array. *)

PROCEDURE NewUntraced(tc: Typecode): ADDRESS =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced # 0 OR def.defaultMethods # NIL
         OR def.nDimensions # 0 THEN
      FatalErrorI("RTAllocator.NewUntraced: improper typecode: ", tc);
    END;
    RETURN AllocateUntracedRef(def);
  END NewUntraced;

(* NewUntracedObject returns a freshly allocated and initialized, untraced
   object with typecode "tc".  It is a checked runtime error if "tc" does
   not name an untraced object type. *)

PROCEDURE NewUntracedObject(tc: Typecode): UNTRACED ROOT =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced # 0 OR def.defaultMethods = NIL THEN
      FatalErrorI("RTAllocator.NewUntracedObject: improper typecode:", tc);
    END;
    RETURN AllocateUntracedObj(def);
  END NewUntracedObject;

(* NewTracedArray returns a reference to a freshly allocated and
   initialized, traced open array referent with typecode "tc" and sizes
   "s[0]", ..., "s[LAST(s)]".  It is a checked runtime error if "tc" does
   not name a traced reference to an open array, if any s[i] is negative,
   or if "NUMBER(s)" does not equal the number of open dimensions of the
   array. *)

PROCEDURE NewTracedArray(tc: Typecode; READONLY s: Shape): REFANY =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced = 0 OR def.nDimensions = 0 THEN
      FatalErrorI("RTAllocator.NewTracedArray: improper typecode: ", tc);
    END;
    IF NUMBER(s) # def.nDimensions THEN
      FatalErrorI("RTAllocator.NewTracedArray: bad NUMBER(s): ", NUMBER(s));
    END;
    RETURN AllocateOpenArray(def, s);
  END NewTracedArray;

(* NewUntracedArray returns a reference to a freshly allocated and
   initialized, untraced open array referent with typecode "tc" and sizes
   "s[0]", ..., "s[LAST(s)]".  It is a checked runtime error if "tc" does
   not name an untraced reference to an open array, if any s[i] is
   negative, or if "NUMBER(s)" does not equal the number of open
   dimensions of the array. *)

PROCEDURE NewUntracedArray(tc: Typecode; READONLY s: Shape): ADDRESS =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced # 0 OR def.nDimensions = 0 THEN
      FatalErrorI("RTAllocator.NewUntracedArray: improper typecode: ", tc);
    END;
    IF NUMBER(s) # def.nDimensions THEN
      FatalErrorI("RTAllocator.NewUntracedArray: bad NUMBER(s): ", NUMBER(s));
    END;
    RETURN AllocateUntracedOpenArray(def, s);
  END NewUntracedArray;

(*--------------------------------------------------------------- RTHooks ---*)

VAR
  initCache: ARRAY [0 .. 4095] OF ADDRESS; (* initialized contents for
                                              freshly allocated objects *)

PROCEDURE Allocate (defn: ADDRESS): REFANY =
  VAR
    def : RT0.TypeDefn := defn;
    tc  : Typecode := def.typecode;
    res : ADDRESS;
  BEGIN
    RTOS.LockHeap();
    BEGIN
      WITH z = RT0u.alloc_cnts[tc] DO z := Word.Plus (z, 1) END;
      res := AllocForNew(def.dataSize, def.dataAlignment);
      IF (tc <= LAST (initCache)) AND (initCache[tc] # NIL) THEN
        RTMisc.Copy(initCache[tc], res - ADRSIZE(Header),
                    def.dataSize + BYTESIZE(Header));
      ELSE
        LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
          Header{typecode := tc, forwarded := FALSE};
        RTMisc.Zero(res, def.dataSize);
        IF def.defaultMethods # NIL THEN
          LOOPHOLE(res, UNTRACED REF ADDRESS)^ := def.defaultMethods;
        END;
        VAR d := def;
        BEGIN
          WHILE d # NIL DO
            IF d.initProc # NIL THEN d.initProc(res); END;
            d := d.parent;
          END;
        END;
        IF (def.dataSize <= BYTESIZE(def^)) AND (tc <= LAST (initCache)) THEN
          initCache[tc] := Malloc(def.dataSize + BYTESIZE(Header));
          RTMisc.Copy(res - ADRSIZE(Header), initCache[tc],
                      BYTESIZE(Header) + def.dataSize);
        END;
      END;
    END;
    RTOS.UnlockHeap();
    IF (callback # NIL) THEN callback (LOOPHOLE (res, REFANY)); END;
    RETURN LOOPHOLE(res, REFANY);
  END Allocate;

PROCEDURE AllocateUntracedRef (defn: ADDRESS): ADDRESS =
  VAR
    def : RT0.TypeDefn := defn;
    res := Malloc(def.dataSize);
    tc  : Typecode := def.typecode;
  BEGIN
    WITH z = RT0u.alloc_cnts[tc] DO z := Word.Plus (z, 1) END;
    IF def.initProc # NIL THEN def.initProc(res); END;
    RETURN res;
  END AllocateUntracedRef;

PROCEDURE AllocateUntracedObj (defn: ADDRESS): UNTRACED ROOT =
  VAR
    def     : RT0.TypeDefn := defn;
    hdrSize := MAX(BYTESIZE(Header), def.dataAlignment);
    res     := Malloc(hdrSize + def.dataSize) + hdrSize;
    tc      : Typecode := def.typecode;
    (* res requires special treatment by DisposeUntracedObj *)
  BEGIN
    WITH z = RT0u.alloc_cnts[tc] DO z := Word.Plus (z, 1) END;
    LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
      Header{typecode := tc, forwarded := FALSE};
    IF def.defaultMethods # NIL THEN
      LOOPHOLE(res, UNTRACED REF ADDRESS)^ := def.defaultMethods;
    END;
    WHILE def # NIL DO
      IF def.initProc # NIL THEN def.initProc(res); END;
      def := def.parent;
    END;
    RETURN res;
  END AllocateUntracedObj;

PROCEDURE AllocateOpenArray (defn: ADDRESS; READONLY s: Shape): REFANY =
  VAR
    def     : RT0.TypeDefn := defn;
    res     : ADDRESS;
    nbElems := OpenArrayCount(s);
    nBytes  := def.dataSize + nbElems * def.elementSize;
    tc      : Typecode := def.typecode;
  BEGIN
    RTOS.LockHeap();
    BEGIN
      WITH z = RT0u.alloc_cnts[tc]  DO z := Word.Plus (z, 1) END;
      WITH z = RT0u.alloc_bytes[tc] DO z := Word.Plus (z, nBytes) END;
      res := AllocForNew(RTMisc.Upper(nBytes, BYTESIZE(Header)),
                                   def.dataAlignment);
      LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
        Header{typecode := tc, forwarded := FALSE};
      LOOPHOLE(res, UNTRACED REF ADDRESS)^ := res + def.dataSize;
      FOR i := 0 TO NUMBER(s) - 1 DO
        LOOPHOLE(res + ADRSIZE(ADDRESS) + i * ADRSIZE(INTEGER),
                 UNTRACED REF INTEGER)^ := s[i];
      END;
      RTMisc.Zero(res + def.dataSize, nbElems * def.elementSize);
      WHILE def # NIL DO
        IF def.initProc # NIL THEN def.initProc(res); END;
        def := def.parent;
      END;
    END;
    RTOS.UnlockHeap();
    IF (callback # NIL) THEN callback (LOOPHOLE (res, REFANY)); END;
    RETURN LOOPHOLE(res, REFANY);
  END AllocateOpenArray;

PROCEDURE AllocateUntracedOpenArray (defn : ADDRESS;
                            READONLY s    : Shape): ADDRESS =
  VAR
    def     : RT0.TypeDefn := defn;
    nbElems := OpenArrayCount(s);
    nBytes  := def.dataSize + nbElems * def.elementSize;
    res     := Malloc(nBytes);
    tc      : Typecode := def.typecode;
  BEGIN
    WITH z = RT0u.alloc_cnts[tc]  DO z := Word.Plus (z, 1) END;
    WITH z = RT0u.alloc_bytes[tc] DO z := Word.Plus (z, nBytes) END;
    LOOPHOLE(res, UNTRACED REF ADDRESS)^ := res + def.dataSize;
    FOR i := 0 TO NUMBER(s) - 1 DO
      LOOPHOLE(res + ADRSIZE(ADDRESS) + i * ADRSIZE(INTEGER),
               UNTRACED REF INTEGER)^ := s[i];
    END;
    WHILE def # NIL DO
      IF def.initProc # NIL THEN def.initProc(res); END;
      def := def.parent;
    END;
    RETURN res;
  END AllocateUntracedOpenArray;

PROCEDURE DisposeUntracedRef (VAR a: ADDRESS) =
  BEGIN
    IF a # NIL THEN Cstdlib.free(a); a := NIL; END;
  END DisposeUntracedRef;

PROCEDURE DisposeUntracedObj (VAR a: UNTRACED ROOT) =
  VAR def: RT0.TypeDefn;
  BEGIN
    IF a # NIL THEN
      def := RTType.Get (TYPECODE (a));
      Cstdlib.free (a - MAX(BYTESIZE(Header), def.dataAlignment));
      a := NIL;
    END;
  END DisposeUntracedObj;

(*-------------------------------------------------------------- internal ---*)

(* OpenArrayCount computes the number of elements given by a Shape.  It
   also checks that all bounds are non-negative. *)

PROCEDURE OpenArrayCount (READONLY s: Shape): CARDINAL =
  VAR n := 1;
  BEGIN
    FOR i := 0 TO NUMBER(s) - 1 DO
      WITH si = s[i] DO
        IF (si < 0) THEN
          RTMisc.FatalErrorI("negative size passed to NEW (open array): ", si);
        END;
        n := si * n;
      END;
    END;
    RETURN n;
  END OpenArrayCount;

BEGIN
END RTAllocator.
