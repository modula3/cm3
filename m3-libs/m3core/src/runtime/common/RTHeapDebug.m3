(*| Copyright (C) 1994, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*|                                                             *)
(*| Last modified on Mon May  1 16:30:49 PDT 1995 by kalsow     *)
(*|      modified on Wed May 25 14:41:19 PDT 1994 by detlefs    *)

UNSAFE MODULE RTHeapDebug;

IMPORT RT0, RTCollector, RTHeapRep, RTHeapMap, RTIO, RTParams, RTTypeSRC;
IMPORT Text, WeakRef, Word;

CONST (* Log[n_bytes] = j  =>  2^j = n_bits,  n_bits = 8 * n_bytes *)
  Log = ARRAY [4..16] OF INTEGER {
          2, -1, -1, -1, 3, -1, -1, -1, -1, -1, -1, -1, 4 };

CONST
  MapGrain = 2 * BYTESIZE (RT0.RefHeader);  (* = 1 bit in the map *)
  MapBitsPerHeapPage = RTHeapRep.BytesPerPage DIV MapGrain;
  MapWordsPerHeapPage = MapBitsPerHeapPage DIV BITSIZE (Word.T);
  LogWordBits = Log [BYTESIZE (Word.T)] + 3;
  LogMapGrain = Log [MapGrain];

TYPE
  Map     = REF ARRAY OF Word.T;
  IntList = REF ARRAY OF INTEGER;
  WRList  = REF ARRAY OF WeakRef.T;

TYPE
  Visitor = RTHeapMap.Visitor OBJECT
    freeAddrs : IntList := NIL;
    visited   : Map     := NIL;
    n_to_find : INTEGER := 0;
    heap_min  : INTEGER := 0;
    heap_max  : INTEGER := 0;
    root      : INTEGER := 0;
    tos       : INTEGER := 0;
    stack     : IntList := NIL;
  OVERRIDES
    apply := PushRefAtAddress;
  END;

VAR
  maxFree  : CARDINAL := GetMaxFree ();
  n_free   : CARDINAL := 0;
  freeRefs := NEW (WRList, maxFree);

PROCEDURE Free(r: REFANY) =
  BEGIN
    freeRefs[ n_free MOD maxFree ] := WeakRef.FromRef (r);
    INC (n_free);
  END Free;

PROCEDURE PushRefAtAddress(v: Visitor;  a: ADDRESS) =
  VAR
    ref := LOOPHOLE(a, UNTRACED REF INTEGER)^;
    optr, map_bit, map_word, mask, visited: INTEGER;
  BEGIN
    IF ref = LOOPHOLE (NIL, INTEGER) THEN RETURN END;
    IF (v.n_to_find <= 0) THEN RETURN END;
    IF (ref < v.heap_min) OR (v.heap_max <= ref) THEN RETURN END;

    IF (v.root = 0) THEN
      v.root := LOOPHOLE (a, INTEGER);
      PushRefAtAddress (v, a);
      WHILE (v.tos > 0) DO
        DEC (v.tos);
        ref := v.stack [v.tos];
        IF Word.And (ref, 1) = 0 THEN
          (* this is the first time we've seen this ref *)
          v.stack [v.tos] := Word.Or (ref, 1); (* mark it *)
          INC (v.tos); (* and push it back on the stack *)
          optr := ref - BYTESIZE(RT0.RefHeader);
          RTHeapMap.WalkRef (LOOPHOLE (optr, RTHeapMap.ObjectPtr), v);
        END;
      END;
      v.root := 0;

    ELSE (* non-root ref => check for a hit & push it on the stack *)
      map_bit  := Word.RightShift (ref - v.heap_min, LogMapGrain);
      map_word := Word.RightShift (map_bit, LogWordBits);
      mask     := Word.LeftShift (1, Word.And (map_bit, BITSIZE(Word.T)-1));
      visited  := v.visited [map_word];


      IF (Word.And (mask, visited) # 0) THEN (*already visited*) RETURN END;
      v.visited[map_word] := Word.Or (visited, mask);

      (* check for a hit *)
      FOR i := 0 TO v.n_to_find - 1 DO
        IF (v.freeAddrs[i] = ref) THEN
          Dump (v, ref);
          DEC (v.n_to_find);
          v.freeAddrs[i] := v.freeAddrs[v.n_to_find];
        END;
      END;

      (* push the new ref *)
      IF (v.tos >= NUMBER (v.stack^)) THEN ExpandStack (v); END;
      v.stack[v.tos] := ref;
      INC (v.tos);
    END;
  END PushRefAtAddress;


PROCEDURE Dump (v: Visitor;  ref: INTEGER) =
  VAR tc, xx: INTEGER;
  BEGIN
    Out ("Path to 'free' object:\n", "   Ref in root", v.root);
    FOR j := 0 TO MIN (v.tos-1, LAST (v.stack^)) DO
      xx := v.stack[j];
      IF Word.And (xx, 1) # 0 THEN
        xx := Word.And (xx, -2);
        tc := TYPECODE (LOOPHOLE(xx, REFANY));
        Out ("   Object of type ", RTTypeSRC.TypecodeName(tc), xx);
      END;
    END;
    tc := TYPECODE (LOOPHOLE(ref, REFANY));
    Out ("   Free object of type ", RTTypeSRC.TypecodeName(tc), ref);
  END Dump;

PROCEDURE Out (a, b: TEXT;  i: INTEGER) =
  BEGIN
    IF (a # NIL) THEN RTIO.PutText (a); END;
    IF (b # NIL) THEN RTIO.PutText (b); END;
    RTIO.PutText (" at address ");
    RTIO.PutHex  (i);
    RTIO.PutText ("...\n");
  END Out;

PROCEDURE ExpandStack (v: Visitor) =
  VAR n := NUMBER (v.stack^);  xx := NEW (IntList, n + n);
  BEGIN
    SUBARRAY (xx^, 0, n) := v.stack^;
    v.stack := xx;
  END ExpandStack;

PROCEDURE CheckHeap() = 
  VAR
    v       := NEW (Visitor);
    n_pages := RTHeapRep.p1 - RTHeapRep.p0;
    old_ref := freeRefs;
    new_ref := NEW (WRList, maxFree);
    n_alive : CARDINAL := 0;
    ref     : REFANY;
  BEGIN
    v.freeAddrs := NEW (IntList, maxFree);
    v.visited   := NEW (Map, n_pages * MapWordsPerHeapPage);
    v.stack     := NEW (IntList, 4096);

    RTCollector.Disable();

      v.heap_min := RTHeapRep.p0 * RTHeapRep.BytesPerPage;
      v.heap_max := v.heap_min + n_pages * RTHeapRep.BytesPerPage;
      (* == the limits of the heap described by "v.visited" *)

      FOR i := 0 TO MIN (n_free, maxFree) - 1 DO
        ref := WeakRef.ToRef (old_ref[i]);
        IF ref # NIL THEN
          new_ref[n_alive] := old_ref[i];
          v.freeAddrs[n_alive] := LOOPHOLE (ref, INTEGER);
          INC (n_alive);
        END;
      END;

      freeRefs := new_ref;
      n_free := n_alive;

      IF n_alive > 0 THEN
        v.n_to_find := n_alive;
        RTHeapMap.WalkGlobals(v);
      END;

    RTCollector.Enable();
    RTIO.Flush ();

    (* give the collector a chance... *)
    v.freeAddrs := NIL;
    v.visited   := NIL;
    v.stack     := NIL;
    v := NIL;
  END CheckHeap;

PROCEDURE GetMaxFree (): CARDINAL =
  VAR
    txt : TEXT    := RTParams.Value ("heapDebugMaxFree");
    n   : INTEGER := 0;
    ch  : INTEGER;
  BEGIN
    IF (txt = NIL) OR Text.Length (txt) = 0 THEN RETURN MaxFree END;
    FOR i := 0 TO Text.Length(txt)-1 DO
      ch := ORD (Text.GetChar (txt, i)) - ORD ('0');
      IF (ch < 0) OR (9 < ch) THEN RETURN MaxFree END;
      n := 10 * n + ch;
    END;
    IF (n > 0)
      THEN RETURN n;
      ELSE RETURN MaxFree;
    END;
  END GetMaxFree;

BEGIN
  <*ASSERT BYTESIZE (REFANY) = BYTESIZE (INTEGER)*>
END RTHeapDebug.
