INTERFACE DynDebug;

(* dynamic debugging *)

PROCEDURE DebugThis(moduleName : TEXT;
                    minLevel   : CARDINAL := 0) : REF BOOLEAN;

(* returns a REF to a boolean value that will take on the same default value 
   as 

   Debug.DebugThis(moduleName) AND Debug.GetLevel() >= minLevel

   but will update asynchronously if the debug variables are overriden.

   Example usage:


   VAR doDebug := DebugThis("MyModule", 10); (* at top level of module *)
   

   IF doDebug^ THEN Debug.Out("I am here.") END (* anywhere in module *)

*)

END DynDebug.
