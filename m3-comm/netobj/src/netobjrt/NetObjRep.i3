(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjRep.i3 *)
(* Last modified on Tue Jul 28 03:22:16 1992 by wobber *)

(* The "NetObjRep" interface reveals the internal structure of the 
   type "NetObj.T". *)
   
INTERFACE NetObjRep;

IMPORT NetObj, WireRep;

REVEAL
  NetObj.T = BRANDED "NetObj.T" OBJECT
    w: WireRep.T := WireRep.NullT;
    r: REFANY;
  END;

END NetObjRep.

(* If "o" is a "NetObj.T", then "o.w" is its wire representation and "o.r" 
   depends on whether it is a surrogate.  If "o" is a surrogate, then
   "o.r" is a "Transport.Location".  Otherwise "o.r" is a type private
   to the implementaton of "NetObjRT". *)


