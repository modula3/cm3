(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* File: RefListDebug.m3                                       *)
(* Last modified on Tue Aug  6 22:34:26 PDT 1996 by detlefs    *)

UNSAFE MODULE RefListDebug;

IMPORT RTHeapMap, RT0, RTModule;
IMPORT IntIntTbl, RefList, RefSeq;

TYPE
  T = RTHeapMap.Visitor OBJECT
    n: INTEGER;
    tcSizes: IntIntTbl.T;
    currentTC: INTEGER;
    todo: RefSeq.T;
    gen: INTEGER;
   METHODS
    init(): T := Init;
    maxTc(VAR (*OUT*) tc, n: INTEGER) := MaxTc;
   OVERRIDES
    apply := Apply;
  END (* OBJECT *);

VAR gen := 0;

PROCEDURE Init(t: T): T =
  BEGIN
    t.tcSizes := NEW(IntIntTbl.Default).init();
    t.n := 0;
    t.todo := NEW(RefSeq.T).init();
    INC(gen); IF gen = 255 THEN gen := 1 END; t.gen := gen;
    RETURN t
  END Init;

PROCEDURE MaxTc(t: T; VAR (*OUT*) tc, n: INTEGER) =
  BEGIN
    tc := -1;
    n := 0;
    VAR iter := t.tcSizes.iterate(); ttc, tn: INTEGER; BEGIN
      WHILE iter.next(ttc, tn) DO
        IF tn > n THEN
          tc := ttc; n := tn
        END (* IF *)
      END (* WHILE *)
    END (* BEGIN *)
  END MaxTc;

VAR rlTC := TYPECODE(RefList.T);

PROCEDURE Apply(v: T; a: ADDRESS) =
  VAR rra := LOOPHOLE(a, REF REFANY);
      ra := rra^;
  BEGIN
    IF ra # NIL THEN
      VAR tc := TYPECODE(ra); 
          hra: RTHeapMap.ObjectPtr :=
              LOOPHOLE(ra, ADDRESS)-BYTESIZE(RT0.RefHeader);
      BEGIN
        IF tc = rlTC THEN
          VAR rlSize := RefListSize(ra, v.gen); BEGIN
            INC(v.n, rlSize);
            VAR tcSize: INTEGER; BEGIN
              IF NOT v.tcSizes.get(v.currentTC, tcSize) THEN
                tcSize := 0
              END (* IF *);
              INC(tcSize, rlSize);
              EVAL v.tcSizes.put(v.currentTC, tcSize)
            END (* BEGIN *)
          END (* BEGIN *)
        ELSE
          IF hra.spare = v.gen THEN
            RETURN
          ELSE
            hra.spare := v.gen;
            v.todo.addhi(ra)
          END (* IF *)
        END (* IF *)
      END (* BEGIN *)
    END (* IF *)
  END Apply;

PROCEDURE RefListSize(ra: REFANY; gen: INTEGER): INTEGER =
  BEGIN
    TYPECASE ra OF
    | NULL =>
        RETURN 0
    | RefList.T(rl) =>
        VAR h: RTHeapMap.ObjectPtr :=
            LOOPHOLE(rl, ADDRESS)-BYTESIZE(RT0.RefHeader);
        BEGIN
          IF h.spare = gen THEN
            RETURN 0
          ELSE
            h.spare := gen;
            RETURN RefListSize(rl.head, gen) + RefListSize(rl.tail, gen) + 1
          END (* IF *);
        END (* BEGIN *)
    ELSE
        RETURN 0
    END (* TYPECASE *)
  END RefListSize;

PROCEDURE Do(r: REFANY;
             VAR (*OUT*) topTypeCode, nForTopType: INTEGER): INTEGER =
  VAR v := NEW(T).init(); BEGIN
    RETURN DoWork(v, r, topTypeCode, nForTopType)
  END Do;

PROCEDURE DoWork(v: T; r: REFANY;
                 VAR (*OUT*) topTypeCode, nForTopType: INTEGER): INTEGER =
  BEGIN
    v.todo.addhi(r);
    WHILE v.todo.size() > 0 DO
      VAR rr := v.todo.remlo();
          h: RTHeapMap.ObjectPtr :=
              LOOPHOLE(rr, ADDRESS)-BYTESIZE(RT0.RefHeader);
      BEGIN
        v.currentTC := TYPECODE(rr);
        RTHeapMap.WalkRef(h, v);
      END (* BEGIN *)
    END (* WHILE *);
    v.maxTc(topTypeCode, nForTopType);
    RETURN v.n
  END DoWork;

(*----------------------------------------------------------------------*)

TYPE
  ModT = RTHeapMap.Visitor OBJECT
    addr: ADDRESS;
    mod: RT0.String;
    size: INTEGER;
    m: INTEGER;
    v: T;
   METHODS
    init(v: T): ModT := ModInit;
   OVERRIDES
    apply := ModApply;
  END (* OBJECT *);

PROCEDURE ModInit(t: ModT; v: T): ModT =
  BEGIN
    t.size := 0;
    t.v := v;
    RETURN t
  END ModInit;

PROCEDURE ModApply(t: ModT; a: ADDRESS) =
  VAR rra := LOOPHOLE(a, REF REFANY);
      ra := rra^;
  BEGIN
    IF ra # NIL THEN
      VAR tc, ntc: INTEGER;
          n := DoWork(t.v, ra, tc, ntc);
      BEGIN
        IF n > t.size THEN
          t.addr := a;
          t.mod := RTModule.Get(t.m).file;
          t.size := n
        END (* IF *)
      END (* BEGIN *)
    END (* IF *)
  END ModApply;

PROCEDURE DoMods(VAR (*OUT*) modName: RT0.String;
                 VAR (*OUT*) addr: INTEGER): INTEGER =
  VAR t := NEW(ModT).init(NEW(T).init()); BEGIN
    FOR m := 0 TO RTModule.Count()-1 DO
      t.m := m;
      RTHeapMap.WalkModuleGlobals(t, m)
    END (* FOR *);
    modName := t.mod;
    addr := LOOPHOLE(t.addr, INTEGER);
    RETURN t.size
  END DoMods;

BEGIN
END RefListDebug.
