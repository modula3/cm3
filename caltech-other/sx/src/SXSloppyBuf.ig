(* $Id$ *)

GENERIC INTERFACE SXSloppyBuf(SXType);

TYPE
  T <: Public;

  Spec <: SXType.T;

  Public = Spec OBJECT METHODS
    init(src : SXType.T) : T;
  END;

CONST Brand = "SXSloppyBuf(" & SXType.Brand & ")";

END SXSloppyBuf.
