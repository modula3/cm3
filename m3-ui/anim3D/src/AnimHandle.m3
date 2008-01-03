(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat May 28 20:25:43 PDT 1994 by najork                   *)
(*       Created on Mon Feb 21 15:34:26 PST 1994 by najork                   *)


MODULE AnimHandle EXPORTS AnimHandle, AnimHandlePrivate, AnimHandleProxy;

IMPORT Anim3D, AnimRequestQueue, AnimServer, Thread;

REVEAL
  T = Private BRANDED OBJECT
    list : List;
  OVERRIDES
    init    := Init;
    attach  := Attach;
    animate := Animate;
  END;

TYPE
  List = REF RECORD
    head : AnimRequestQueue.T;
    tail : List;
  END;


PROCEDURE Init (self : T) : T =
  BEGIN
    self.mu := NEW (MUTEX);
    self.cv := NEW (Thread.Condition);
    self.list := NIL;
    self.activated := FALSE;

    IF MkProxyT # NIL THEN
      MkProxyT (self);
    END;
      
    RETURN self;
  END Init;


PROCEDURE New () : T =
  BEGIN
    RETURN NEW (T).init ();
  END New;


PROCEDURE Attach (self : T; q : AnimRequestQueue.T) =

  PROCEDURE AddIfNew (VAR list : List) =
    BEGIN
      IF list = NIL THEN
        list := NEW (List, head := q, tail := list);
      ELSIF list.head # q THEN
        AddIfNew (list.tail);
      END;  
    END AddIfNew;

  BEGIN
    AddIfNew (self.list);
  END Attach;


PROCEDURE Animate (self : T) =
  VAR
    max := 0.0;
    tmp : List;
  BEGIN
    LOCK self.mu DO
      tmp := self.list;
      self.activated := TRUE;
      self.starttime := Anim3D.Now ();
      WHILE tmp # NIL DO
        max := MAX (max, tmp.head.duration ());
        tmp := tmp.tail;
      END;
      self.endtime := self.starttime + FLOAT (max, LONGREAL);
      
      AnimServer.PauseAnimHandle (self);
      
      self.activated := FALSE;
      
      (* Flush all the animation request queues attached 
         to the animation handle. *)
      tmp := self.list;
      WHILE tmp # NIL DO
        tmp.head.flush ();
        tmp := tmp.tail;
      END;
    END;
  END Animate;
  

BEGIN
END AnimHandle.
