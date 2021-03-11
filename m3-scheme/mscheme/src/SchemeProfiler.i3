(* $Id$ *)

INTERFACE SchemeProfiler;
IMPORT SchemeProcedure, Time, TextIntTbl;

PROCEDURE EnterProcedure(p : SchemeProcedure.T);
  (* no point in having a LeaveProcedure, owing to tail recursion *)
  
PROCEDURE Enable();

PROCEDURE Disable();

TYPE By = { CPU, Wallclock };
  
PROCEDURE TopN(n : CARDINAL; by : By) : REF ARRAY OF Stats;

TYPE
  Stats = RECORD
    name : TEXT;
    wall : Time.T;
    cpu : Time.T;
    callsFrom : TextIntTbl.T;
  END;

CONST Brand = "SchemeProfiler";

END SchemeProfiler.
