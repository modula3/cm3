(* $Id$ *)

INTERFACE SchemeEnvironmentInstanceRep;
IMPORT SchemeEnvironment, SchemeEnvironmentClass, AtomRefTbl, AtomList;
FROM Scheme IMPORT Symbol, Object, E;
IMPORT Scheme;

REVEAL SchemeEnvironment.Instance <: Rep;

CONST QuickVars = 10;

TYPE QuickMap = RECORD var : Symbol; val : Object END;

TYPE
  Rep = SchemeEnvironmentClass.Private BRANDED OBJECT
    (* vars, vals not necessary *)
    dictionary : AtomRefTbl.T;

    quick : ARRAY [0..QuickVars - 1] OF QuickMap;

    parent : SchemeEnvironment.T;

    dead := FALSE; (* for debugging *)

  METHODS
    initDict(vars, vals : Object; 
         VAR canRecyclePairs : BOOLEAN) : BOOLEAN;
    initDictEval(vars, argsToEval : Object;
                 evalEnv : SchemeEnvironment.T;
                 interp : Scheme.T) : BOOLEAN RAISES { E };

    getLocalNames() : AtomList.T;
  END;

TYPE Unsafe = SchemeEnvironment.Unsafe;
TYPE Safe   = SchemeEnvironment.Safe;

CONST Brand = SchemeEnvironment.Brand & " InstanceRep";

END SchemeEnvironmentInstanceRep.

 
    
