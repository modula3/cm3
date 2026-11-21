GENERIC INTERFACE LRScalarPll(Base);

(* simple parallel version (wrapper) of LRScalarField.T *)

TYPE
  T <: Public;

  Public = Base.T OBJECT METHODS
    init(from : Base.T) : T;
    clearTbls(); (* clear memoized data *)
  END;

CONST Brand = "LRScalarPll(" & Base.Brand & ")";

END LRScalarPll.
