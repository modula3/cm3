(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov  7 11:33:03 PST 1994 by kalsow    *)

INTERFACE SilMacro;

IMPORT Rect, SilObject;

TYPE
  T <: Tx;
  Tx = SilObject.T OBJECT METHODS
    init (READONLY bbox: Rect.T;  body: Defn): T;
  END;

TYPE
  Defn <: REFANY;

PROCEDURE NewDefn (): Defn;
PROCEDURE AddObj (defn: Defn;  obj: SilObject.T);

PROCEDURE Contents (t: T): SilObject.T;
(* return the list of objects that comprise 't' *)

END SilMacro.


