(* $Id$ *)

INTERFACE SchemeM3;
IMPORT Scheme, SchemePrimitive;
IMPORT SchemeJailBreak, SchemeM3TableOps;

TYPE 
  T <: Public;

  Public = Scheme.T OBJECT METHODS
    setJailBreak(jb : SchemeJailBreak.T);
    setTableOps(to : SchemeM3TableOps.T);
  END;

 (* a "basic extended" Scheme interpreter *)

CONST Brand = "SchemeM3";

PROCEDURE ExtendWithM3(definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner;
  (* extend a given ExtDefiner with the "Modula-3 primitives"
     defined in this module *)

PROCEDURE GetPrims() : SchemePrimitive.ExtDefiner;
  (* can grab this to extend the prims before allocating a new T *)

END SchemeM3.
