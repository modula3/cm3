(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjRT.i3 *)
(* Last modified on Wed Apr 14 10:00:34 PDT 1993 by wobber     *)
(*      modified on Sun Nov 29 14:13:50 1992 by gnelson    *)
(*      modified on Wed Aug  5 17:49:51 PDT 1992 by evers  *)

(* The "NetObjRT" interface defines procedures which are necessary for
   the maintenance of the network object runtime {\it object table}.
   This table is a mapping from "WireRep.T" to "NetObj.T".  All
   surrogate objects appear in this table, as do all concrete objects
   which might be remotely referenced. *)
   
INTERFACE NetObjRT;

IMPORT NetObj, StubLib, Thread, Transport, WireRep;

PROCEDURE FindTarget(
    wrep: WireRep.T;
    stubProt: StubLib.StubProtocol;
    VAR dispatcher: StubLib.Dispatcher) : NetObj.T
  RAISES {NetObj.Error};
    
(* "FindTarget" searches the object table for an object matching "wrep". 
   If one is found, and it is a concrete object, then the object is 
   returned along with the "StubLib.Dispatcher" procedure corrsponding 
   to its type and to the supplied "stubProt".  Otherwise, "NetObj.Error"
   is raised with the argument "NetObj.MissingObject".  (We might want
   to raise an explicit error here if the object exists but no dispatcher
   matching "stubProt" is found.)

   "FindTarget" is intended for use only when the supplied "wrep" is 
   expected to correspond to a local concrete object.  This is the 
   case, for example, when the network object runtime unmarshals the 
   target object of a remote invocation. *)
   
PROCEDURE Find(
    wrep: WireRep.T;
    loc: Transport.Location) : NetObj.T
  RAISES {NetObj.Error, Thread.Alerted};

(* "Find" searches the object table for an object matching "wrep".
   If one is found, it is returned.  Otherwise, a surrogate object
   is constructed for "wrep".  "loc" must generate connections back to
   the party who provided "wrep".  This need not be the owner of the
   concrete object, but if not, it must at least be able to provide
   sufficient information to construct a "Transport.Location" to the
   real owner. This procedure is called as part of unmarshalling a
   "NetObj.T".
   
   If "wrep" is "WireRep.NullT", then "Find" produces undefined results. *)

PROCEDURE InsertAndPin(o: NetObj.T) : WireRep.T;

(* If "o" is not registered in the local object table, "InsertAndPin" 
   assigns a wire representation to "o" and inserts it into the table. 
   (This can happen only for concrete objects.)  In all cases, the 
   wire representation of "o" is returned.  This procedure is called 
   as part of marshalling a "NetObj.T".
   
   "InsertAndPin" increments a reference count associated with "o" 
   which {\it pins} it in the object table, this is, "o" is considered 
   unremovable.  When marshalling of "o" is complete, and the 
   destination is known to have either successfully unmarshalled "o" 
   or failed in doing so, then this reference count must be decremented 
   with "Unpin".
   
   It is a checked runtime error for "o" to be "NIL". *)

PROCEDURE Unpin(READONLY arr: ARRAY OF NetObj.T);

(* "Unpin" decrements the pin count associated with each element of
   "arr".  When this count becomes zero for any object, that object
   can be removed from the object table (by some surrogate-holder
   reporting in clean).
   
   It is a checked runtime error for any element of "arr" to be "NIL",
   or to have a pin count of zero. *)

END NetObjRT.

