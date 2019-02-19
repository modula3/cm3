(* $Id$ *)

INTERFACE M3toSRefany;
IMPORT Scheme, RT0;

(* interface that allows unpacking of REFANYs from Modula-3 to Scheme
   safely *)

PROCEDURE ToScheme(r : REFANY) : Scheme.Object RAISES { Scheme.E };

CONST FromScheme : FromSchemeProc = NIL; (* hmm... we can't write to 
                                            this type of table, then? *)

TYPE T = REFANY;

CONST Brand = "SchemeM3Refany";

(* call this to register a new type *)
PROCEDURE Register(tc : RT0.Typecode; 
                   toScheme : ToSchemeProc);

TYPE 
  ToSchemeProc = PROCEDURE (r : REFANY) : Scheme.Object RAISES { Scheme.E };
  FromSchemeProc = PROCEDURE (o : Scheme.Object) : REFANY RAISES { Scheme.E };

END M3toSRefany.
