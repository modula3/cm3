(* $Id$ *)
INTERFACE AdGrid;
IMPORT LRScalarField, LRPoint, AdGridQSet;

(* An AdGrid.T is a wrapper around an LRScalarField.T (i.e., a wrapper
   around a multivariate function).

   Currently, only two independent variables are supported.  More 
   independent variables will be supported in a later release.

   Use an AdGrid.T as follows:

   f := NEW(LRScalarField.T, eval := EvalF);
   m := NEW(AdGrid.T).init(f,ll,ur,3); (* e.g. *)

   If you want to define multiple functions over the same grid, 
   those functions can be added AT ANY TIME using mapNewLRSF:

   g := NEW(LRScalarField.T, eval := EvalG);
   n := m.mapNewLRSF(g);

   After this, m and n will be associated: tiles (see AdGridQ.T) used 
   in the representation of one will be compatible with the other.
*)

TYPE
  T <: Public;

  Public = LRScalarField.T OBJECT METHODS
    init(f : LRScalarField.T; ll, ur : LRPoint.T; initLevels := 0) : T;

    (* evalP: evaluate the function at a certain point.  This may or
       may not evaluate the base function f; in most cases, it will
       simply generate a mesh in the neighborhood of "at" with the
       stated precision.

       If the mesh already exists, it will not be regenerated.

       In any case, the value returned will NOT be the return value of f;
       instead it will be the bilinear interpolation of f over the tile
       of the mesh where at is found. 
       
       Note that eval still works (it is overridden from LRScalarField.T);
       it gives less control over evaluation, though (no prec).
    *)

    evalP(READONLY at : LRPoint.T; prec : LONGREAL := 0.01d0) : LONGREAL;
    setPrec(prec : LONGREAL);

    (* get the set of tiles containing a given level curve *)
    getQuadsContainingLevel(level : LONGREAL; 
                            set : AdGridQSet.T := NIL) : AdGridQSet.T;

    getQuadsContainingLevelOrLower(level : LONGREAL; 
                            set : AdGridQSet.T := NIL) : AdGridQSet.T;

    getQuadsContainingLevelOrHigher(level : LONGREAL; 
                            set : AdGridQSet.T := NIL) : AdGridQSet.T;

    getAllQuads() : AdGridQSet.T;
    (* extend mesh with another function *)
    mapNewLRSF(f : LRScalarField.T) : T;
  END;

CONST Brand = "AdGrid";

PROCEDURE SubdivideSet(set : AdGridQSet.T; levels : CARDINAL :=1);

END AdGrid.
