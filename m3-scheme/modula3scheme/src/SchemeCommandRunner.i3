(* $Id$ *)

INTERFACE SchemeCommandRunner;
IMPORT SchemePrimitive;
IMPORT Scheme;
IMPORT Rd;

CONST Brand = "SchemeCommandRunner";

PROCEDURE Extend(outputParser : OutputParser;
                 definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner;

TYPE
  OutputParser = OBJECT METHODS
    parseRd(rd : Rd.T) : Scheme.Object RAISES { Scheme.E };
    (* take output of a Unix shell command and turn it into a Scheme Object *)
  END;

  TextOutputParser <: OutputParser;

END SchemeCommandRunner.
