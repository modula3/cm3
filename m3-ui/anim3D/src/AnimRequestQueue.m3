(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jul 18 13:54:17 PDT 1994 by najork                   *)


MODULE AnimRequestQueue EXPORTS AnimRequestQueue, AnimRequestQueuePrivate;

IMPORT AnimHandle, AnimHandlePrivate, Prop;

REVEAL 
  T = Private BRANDED OBJECT
  OVERRIDES
    init     := Init;
    insert   := Insert;
    duration := Duration;
  END;


PROCEDURE Init (self : T; ah : AnimHandle.T) : T =
  BEGIN
    self.list := NIL;
    self.ah := ah;
    ah.attach (self);
    RETURN self;
  END Init;


PROCEDURE Insert (self : T; req : Prop.Request) RAISES {Prop.BadInterval} =

  PROCEDURE InsertIntoList (VAR l : List) RAISES {Prop.BadInterval} =
    BEGIN
      IF l = NIL THEN
        l := NEW (List, req := req, next := NIL);
      ELSIF Before (req.start, req.dur, l.req.start, l.req.dur) THEN
        l := NEW (List, req := req, next := l);
      ELSE
        InsertIntoList (l.next);
      END;
    END InsertIntoList;

  BEGIN
    LOCK self.ah.mu DO
      InsertIntoList (self.list);
    END;
  END Insert;


(* Returns TRUE if interval1 is before interval2, 
           FALSE if interval1 is after interval2,
   raises Prop.BadInterval if they overlap. *)
PROCEDURE Before (start1, dur1, start2, dur2 : REAL) : BOOLEAN 
    RAISES {Prop.BadInterval} =
  BEGIN
    IF dur1 = 0.0 AND dur2 = 0.0 THEN
      IF    start1 < start2 THEN 
        RETURN TRUE;
      ELSIF start1 > start2 THEN 
        RETURN FALSE;
      ELSE 
        RAISE Prop.BadInterval;
      END;
    ELSE
      IF start1 + dur1 <= start2 THEN 
        RETURN TRUE;
      ELSIF start2 + dur2 <= start1 THEN 
        RETURN FALSE;
      ELSE 
        RAISE Prop.BadInterval;
      END;
    END;
  END Before;


PROCEDURE Duration (self : T) : REAL =
  VAR
    l   : List;
    max := 0.0;
  BEGIN
    l := self.list;
    WHILE l # NIL DO
      max := MAX (max, l.req.start + l.req.dur);
      l := l.next;
    END;
    RETURN max;
  END Duration;


BEGIN
END AnimRequestQueue.
