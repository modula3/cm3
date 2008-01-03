(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sun Oct 30 14:36:42 PST 1994 by kalsow                   *)
(*      modified on Tue Jul 26 15:20:14 PDT 1994 by najork                   *)
(*       Created on Thu May 19 09:12:23 PDT 1994 by najork                   *)


INTERFACE Prop;

IMPORT ProxiedObj;

CONST
  Brand = "Prop";

TYPE 
  T    <: ProxiedObj.T;
  Name <: ProxiedObj.T;
  Val  <: ProxiedObj.T;
  Beh  <: ProxiedObj.T;

  Request <: PublicRequest;
  PublicRequest = ProxiedObj.T OBJECT
    start : REAL;
    dur   : REAL;
  METHODS
    init (start, dur : REAL) : Request;
  END;

EXCEPTION BadMethod (TEXT);
EXCEPTION BadInterval;

PROCEDURE Equal (a, b : T) : BOOLEAN;  (* tests just for pointer equality *)

END Prop.
