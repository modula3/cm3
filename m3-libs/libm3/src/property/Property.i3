(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
 
(* Last modified on Fri Jan 18  9:14:04 PST 1991 by mjordan    *)

INTERFACE Property;

(* A Property.Set is a set of non-nil traced references, all of different 
   types.  
   
   Clients should not activate procedures in this interface concurrently
   on the same Set (see MProperty for monitored sets). *)

TYPE
  Set <: Set_public;
  Set_public = OBJECT
  METHODS
    put(ref: REFANY);
    (* Add ref to 'ps', replacing any existing reference of the same type 
       as 'ref'.  This is a checked runtime error if 'ref' is NIL. *)

    get(tc: CARDINAL): REFANY;
    (* Return the element of ps with typecode 'tc', or return NIL if no 
       such element exists. *)

    getSub(tc: CARDINAL): REFANY;
    (* Return an element of 'ps' which is a subtype of the type with
    typecode 'tc', or return NIL if no such element exists. If there
    is more than one element which satisfies the test, it is
    indeterminate which one is returned. *)

    remove(tc: CARDINAL);
    (* Remove the element of 'ps' with typecode 'tc', if one exists. *)

    removeSub(tc: CARDINAL);
    (* Remove the element of 'ps' which is a subtype of the type with
       typecode 'tc', if one exists. *)
  END;

PROCEDURE New(): Set;
(* Create the empty set*)

END Property.

