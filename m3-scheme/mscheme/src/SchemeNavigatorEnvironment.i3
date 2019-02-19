(* $Id$ *)

INTERFACE SchemeNavigatorEnvironment;
IMPORT SchemeEnvironment;
IMPORT SchemeObject, SchemePrimitive;

TYPE 
  T <: Public;

  Public = SchemeEnvironment.Safe (* safe *) OBJECT METHODS
    attachChildEnvironment(env : SchemeEnvironment.T; tag : SchemeObject.T);
  END;

CONST Brand = "SchemeNavigatorEnvironment";

PROCEDURE ExtendWithNavigator(definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner;
  (* extend the primitive definer with the following primitives,
     which allow navigating in the environments... 

     (up-environment)             ;; attach to the parent environment
     (down-environment <Env>)     ;; go to the selected child environment
     (list-child-environments) ;; return list of (tag . env) pairs
  *)

CONST MagicEnvironmentVariable = "***child-environments***";
      ThisEnvironmentVariable  = "***this-environment***";

END SchemeNavigatorEnvironment.
