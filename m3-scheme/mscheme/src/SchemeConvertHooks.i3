(* $Id$ *)

INTERFACE SchemeConvertHooks;
IMPORT Scheme, Refany;
IMPORT RT0;

TYPE
  T = OBJECT METHODS
    convert(from : REFANY) : REFANY RAISES { Scheme.E };
  END;

PROCEDURE RegisterToScheme(fromTC : RT0.Typecode; converter : T);
  (* only used for debug printing! *)

PROCEDURE RegisterToModula(fromTC, toTC : RT0.Typecode; converter : T);

(* the following routines return TRUE on a successful conversion to the
   target typecode (or one of its descendants);
   returns FALSE if no conversion routine could be found;
   raises Scheme.E if a conversion routine that could be found failed with
   an exception *)

PROCEDURE AttemptConvertToScheme(VAR x : REFANY) : BOOLEAN
  RAISES { Scheme.E };

PROCEDURE AttemptConvertToModula(tgt : RT0.Typecode; VAR x : REFANY) : BOOLEAN
  RAISES { Scheme.E };

CONST Brand = "SchemePrintHooks";

CONST Equal = Refany.Equal;
                           
END SchemeConvertHooks.
