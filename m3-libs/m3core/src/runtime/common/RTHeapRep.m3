(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*| Last modified on Thu Jun 10 16:08:20 PDT 1993 by kalsow  *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)
(*|      modified on Tue Mar  9 08:45:18 PST 1993 by jdd     *)

UNSAFE MODULE RTHeapRep;

IMPORT RT0u, RTType, RTMisc;

(*----------------------------------------------------------- open arrays ---*)

PROCEDURE UnsafeGetShape (r: REFANY;  VAR nDims: INTEGER;  VAR s: ArrayShape) =
  VAR def := RTType.Get (TYPECODE (r));
  BEGIN
    nDims := def.nDimensions;
    IF nDims # 0 THEN
      s := LOOPHOLE(LOOPHOLE(r, ADDRESS) + ADRSIZE(ADDRESS), ArrayShape);
    END;
  END UnsafeGetShape;

(*-------------------------------------------------------------- monitors ---*)

TYPE
  PublicMonitorClosure = OBJECT
                         METHODS
                           before ();
                           after  ();
                         END;

REVEAL
  MonitorClosure =
    PublicMonitorClosure BRANDED "RTHeap.MonitorClosure" OBJECT
      next, prev: MonitorClosure;
    OVERRIDES
      before := Noop;
      after  := Noop;
    END;

VAR monitorsHead, monitorsTail: MonitorClosure;

PROCEDURE InvokeMonitors (before: BOOLEAN) =
  VAR m: MonitorClosure;
  BEGIN
    IF before THEN
      m := monitorsHead;
      WHILE m # NIL DO m.before(); m := m.next; END;
    ELSE
      m := monitorsTail;
      WHILE m # NIL DO m.after(); m := m.prev; END;
    END;
  END InvokeMonitors;

PROCEDURE RegisterMonitor (cl: MonitorClosure) =
  BEGIN
    cl.next := monitorsHead;
    IF monitorsHead = NIL THEN
      monitorsTail := cl;
    ELSE
      monitorsHead.prev := cl;
    END;
    monitorsHead := cl;
  END RegisterMonitor;

PROCEDURE UnregisterMonitor (cl: MonitorClosure) =
  BEGIN
    IF cl = monitorsHead THEN
      IF cl = monitorsTail THEN
        monitorsHead := NIL;
        monitorsTail := NIL;
      ELSE
        monitorsHead := monitorsHead.next;
        monitorsHead.prev := NIL;
      END;
    ELSE
      IF cl = monitorsTail THEN
        monitorsTail := monitorsTail.prev;
        monitorsTail.next := NIL;
      ELSE
        cl.prev.next := cl.next;
        cl.next.prev := cl.prev;
      END;
    END;
  END UnregisterMonitor;

PROCEDURE Noop (<*UNUSED*> cl: MonitorClosure) =
  BEGIN
  END Noop;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE CheckTypes () =
  (* called by RTType.Init after type registration, but before any
     allocation *)
  VAR
    is_power: ARRAY [0 .. 8] OF BOOLEAN;
    size    : INTEGER;
  BEGIN
    (* check that it's safe to eliminate the #A call to upper ... *)
    FOR i := 0 TO RT0u.nTypes - 1 DO
      WITH def = RTType.Get (i)^ DO
        IF (def.traced # 0) AND (def.nDimensions = 0) THEN
          size := def.dataSize;
          <*ASSERT size = RTMisc.Upper (size, BYTESIZE (Header)) *>
        END;
      END;
    END;

    (* compute the small powers of two *)
    FOR i := FIRST(is_power) TO LAST(is_power) DO is_power[i] := FALSE END;
    is_power[1] := TRUE;
    is_power[2] := TRUE;
    is_power[4] := TRUE;
    is_power[8] := TRUE;

    (* check that all data alignments are small powers of two so that
       "RTMisc.Align (addr, alignment)" can be safely replaced by "addr +
       align [Word.And (addr, 7), alignment]" in Gcalloc.*)
    FOR i := 0 TO RT0u.nTypes - 1 DO
      WITH def = RTType.Get (i)^ DO
        IF (def.traced # 0) THEN
          <*ASSERT is_power [def.dataAlignment] *>
        END;
      END;
    END;
  END CheckTypes;

BEGIN
END RTHeapRep.
