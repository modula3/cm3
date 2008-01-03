(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 29 13:55:54 PDT 1994 by najork                   *)
(*       Created on Fri Feb 18 10:58:07 PST 1994 by najork                   *)


GENERIC MODULE CBStack (AnyCB);

IMPORT CB, GO;

REVEAL
  T = Public BRANDED OBJECT
    mu   : MUTEX;
    list : List;
  OVERRIDES
    init      := Init;
    invokeTop := InvokeTop;
    push      := Push;
    pop       := Pop;
    remove    := Remove;
  END;


TYPE 
  List = REF RECORD
    head : AnyCB.T;
    tail : List;
  END;


PROCEDURE Init (self : T) : T =
  BEGIN
    self.list := NIL;
    self.mu   := NEW (MUTEX);
    RETURN self;
  END Init;


PROCEDURE InvokeTop (self : T; rec : AnyCB.Rec) RAISES {CB.BadMethod} =
  VAR
    top : AnyCB.T;
  BEGIN
    LOCK self.mu DO
      IF self.list # NIL THEN
        top := self.list.head;
      END;
    END;
    IF top # NIL THEN
      top.invoke (rec);
    END;
  END InvokeTop;


PROCEDURE Push (self : T; cb : AnyCB.T) =
  BEGIN
    LOCK self.mu DO
      self.list := NEW (List, head := cb, tail := self.list);
    END;
  END Push;


PROCEDURE Pop (self : T) RAISES {GO.StackError} = 
  BEGIN
    LOCK self.mu DO
      IF self.list = NIL THEN
        RAISE GO.StackError;
      ELSE
        self.list := self.list.tail;
      END;
    END;
  END Pop;


PROCEDURE Remove (self : T; cb : AnyCB.T) RAISES {GO.StackError} =

  PROCEDURE Remove (VAR list : List) RAISES {GO.StackError} =
    BEGIN
      IF list = NIL THEN
        RAISE GO.StackError;
      ELSIF list.head = cb THEN
        list := list.tail;
      ELSE
        Remove (list.tail);
      END;
    END Remove;

  BEGIN
    LOCK self.mu DO
      Remove (self.list);
    END;
  END Remove;


BEGIN
END CBStack.
