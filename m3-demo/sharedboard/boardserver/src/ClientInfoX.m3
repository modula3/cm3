(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE ClientInfoX;

IMPORT RectR,
       Callback;

REVEAL T = Public BRANDED OBJECT
    scope: RectR.T;
    cb: Callback.T;
  OVERRIDES
    init := Init;
    setScope := SetScope;
    getScope := GetScope;
    getCallback := GetCallback;
  END;

PROCEDURE Init (ci: T; cb: Callback.T): T =
  BEGIN 
    ci.scope := RectR.Empty;
    ci.cb := cb;
    RETURN ci;
  END Init;

PROCEDURE SetScope (ci: T; scope: RectR.T) =
  BEGIN ci.scope := scope END SetScope;

PROCEDURE GetScope (ci: T): RectR.T =
  BEGIN RETURN ci.scope END GetScope;

PROCEDURE GetCallback (ci: T): Callback.T =
  BEGIN RETURN ci.cb END GetCallback;
  
BEGIN
END ClientInfoX.

