(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*| Last modified on Thu Jun 10 16:08:20 PDT 1993 by kalsow  *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)
(*|      modified on Tue Mar  9 08:45:18 PST 1993 by jdd     *)

UNSAFE MODULE RTHeapRep;

IMPORT RT0, RTType;

(*----------------------------------------------------------- open arrays ---*)

PROCEDURE UnsafeGetShape (r: REFANY;  VAR nDims: INTEGER;  VAR s: ArrayShape) =
  TYPE TK = RT0.TypeKind;
  VAR def := RTType.Get (TYPECODE (r));
  BEGIN
    nDims := 0;
    IF (def.kind = ORD (TK.Array)) THEN
      nDims := LOOPHOLE (def, RT0.ArrayTypeDefn).nDimensions;
      IF nDims # 0 THEN
        s := LOOPHOLE(LOOPHOLE(r, ADDRESS) + ADRSIZE(ADDRESS), ArrayShape);
      END;
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

BEGIN
END RTHeapRep.
