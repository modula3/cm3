(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Jul 27 23:08:18 PDT 1994 by najork                   *)
(*       Created on Thu May 19 14:43:45 PDT 1994 by najork                   *)


MODULE Prop EXPORTS Prop, PropPrivate, PropProxy;

IMPORT AnimServer, GO, GOPrivate, ProxiedObj;


PROCEDURE Equal (a, b : T) : BOOLEAN =
  BEGIN
    RETURN a.n = b.n AND a.v = b.v;
  END Equal;

REVEAL 
  T = PrivateT BRANDED OBJECT
  OVERRIDES
    init          := InitT;
    adjust        := AdjustT;
  END;


PROCEDURE InitT (self : T; n : Name; v : Val) : T =
  BEGIN
    self.n := n;
    self.v := v;
    IF MkProxyT # NIL THEN
      MkProxyT (self);
    END;
    RETURN self;
  END InitT;


PROCEDURE AdjustT (self : T; time : LONGREAL; caller : GO.T) =
  BEGIN
    TRY
      IF self.v.adjust (time) THEN
        self.n.damage (caller);
      END;
    EXCEPT
    | BadMethod (msg) =>
      AnimServer.ReportError (msg & "\n");
    END;
  END AdjustT;


(*****************************************************************************)
(* Type "Name"                                                               *)
(*****************************************************************************)

REVEAL 
  Name = PrivateName BRANDED OBJECT
  OVERRIDES
    init   := InitName;
    damage := DamageName;
  END;


PROCEDURE InitName (self : Name) : Name =
  BEGIN
    self.id := next;
    INC (next);

    list := NEW (NameList, this := self, next := list);

    RETURN self;
  END InitName;


PROCEDURE DamageName (self : Name; caller : GO.T) =
  BEGIN
    caller.damageIfDependent (self);
  END DamageName;


(*****************************************************************************)
(* Type "Val"                                                                *)
(*****************************************************************************)


REVEAL 
  Val = PrivateVal BRANDED OBJECT END;


(*****************************************************************************)
(* Type "Beh"                                                                *)
(*****************************************************************************)

REVEAL
  Beh = ProxiedObj.T BRANDED OBJECT END;


REVEAL 
  Request = PublicRequest BRANDED OBJECT
  OVERRIDES
    init := InitRequest;
  END;


PROCEDURE InitRequest (self : Request; start, dur : REAL) : Request =
  BEGIN
    self.proxy := NIL;
    self.start := start;
    self.dur   := dur;
    RETURN self;
  END InitRequest;


(*****************************************************************************)
(* Type "Stack"                                                              *)
(*****************************************************************************)

REVEAL
  Stack = BRANDED OBJECT END;


(*****************************************************************************)
(* Property Name Management                                                  *)
(*****************************************************************************)


TYPE 
  NameList = REF RECORD 
    this : Name;
    next : NameList;
  END;

VAR
  list : NameList := NIL;
  next : INTEGER  := 0;


PROCEDURE NewStacks () : REF ARRAY OF Stack =
  VAR
    stacks : REF ARRAY OF Stack;
    tmp    : NameList;
  BEGIN
    <* ASSERT next > 0 *>
    stacks := NEW (REF ARRAY OF Stack, next);
    tmp := list;
    WHILE tmp # NIL DO
      WITH pn = tmp.this DO
        stacks[pn.id] := pn.newStack();
      END;
      tmp := tmp.next;
    END;
    RETURN stacks;
  END NewStacks;


BEGIN
END Prop.
