(* $Id: SchemeCommandRunner.i3,v 1.3 2010/12/29 17:16:04 mika Exp $ *)

INTERFACE SchemeCommandRunner;
IMPORT SchemePrimitive;
IMPORT Scheme;
IMPORT Rd;

(* interface for extending Scheme interpreter with new primitives to run
   Unix commands as follows

     (run-command <cmd> <arg1> <arg2> ... ) 

     (run-command-with-timeout <timeo> <cmd> <arg1> <arg2> ... ) 

     (run-command-with-hooks
                   <timeout-hook> 
                   <error-hook> 
                   <timeo> <cmd> <arg1> <arg2> ... ) 

     (run-raw-command-with-hooks                      ;; output is TEXT
                   <timeout-hook> 
                   <error-hook> 
                   <timeo> <cmd> <arg1> <arg2> ... ) 

*)

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
