(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jul 26 17:50:01 PDT 1994 by najork                   *)
(*       Created on Fri Jul 22 16:15:16 PDT 1994 by najork                   *)


MODULE MouseCB EXPORTS MouseCB, MouseCBProxy;

IMPORT CB;

REVEAL 
  T = Public BRANDED OBJECT 
  OVERRIDES
    init   := Init;
    invoke := Invoke;
  END;


PROCEDURE Init (self : T) : T =
  BEGIN
    self.proxy := NIL;
    IF MkProxyT # NIL THEN
      MkProxyT (self);
    END;
    RETURN self;
  END Init;


PROCEDURE Invoke (self : T; mr : Rec) RAISES {CB.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      NARROW (self.proxy, Proxy).invoke (mr);
    ELSE
      RAISE CB.BadMethod("MouseCB.T.invoke method is undefined");
    END;
  END Invoke;


BEGIN
END MouseCB.
