(*--------------------------------------------------------------------------*)
INTERFACE StdDepGraphEdge;

CONST Brand = "StdDepGraphEdge 0.1";

TYPE 
  T = BRANDED "StdDepGraphEdge Object 0.1" OBJECT
    val : INTEGER;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE New(v : INTEGER) : T;

END StdDepGraphEdge.
