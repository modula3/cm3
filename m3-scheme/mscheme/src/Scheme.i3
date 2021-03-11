(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE Scheme;
IMPORT SchemeEnvironmentSuper, SchemeObject;
IMPORT SchemeSymbol;
IMPORT SchemeVector;
IMPORT Pathname;
IMPORT Rd, Wr;

EXCEPTION E(TEXT);
 (* This declaration is, unfortunately, a source of trouble.
    Since exceptions aren't declarations in Modula-3, putting it here
    means "everything" has to import this interface, which leads to
    a lot of circular import possiblities.

    Best would be to move E to a special interface, say SchemeError. 
 *)

(* 

   IMPORTANT NOTE ON CONCURRENCY ISSUES IN THE DESIGN OF THE SCHEME
   INTERPRETER---

   The MScheme system is intended for multi-threaded operation.
   It is intended that different Scheme interpreters could share
   a global environment (even over a network or via a database system,
   if desired).

   However, a SINGLE Scheme.T (less its environment) is ALWAYS a
   single-threaded machine, and its methods are not intended to be
   called concurrently from different execution threads. Accordingly,
   none of its methods are synchronized, and while a Scheme.T itself
   has no visible state, it does have internal state used for optimizing
   various aspects of its operation (mainly to reduce the cost of
   garbage-collected memory management).

*)
   


TYPE 
  (* aliases for basic Scheme types *)
  Object      = SchemeObject.T;
  Symbol      = SchemeSymbol.T;
  Vector      = SchemeVector.T;
  Environment = REFANY (* SchemeEnvironment.T -- avoid circular deps *);

  (* a Scheme interpreter *)
  T <: Public;

  Public = OBJECT METHODS
    defineInGlobalEnv(var, val : Object);

    init(READONLY files : ARRAY OF Pathname.T;
         globalEnv : Environment := NIL) : T 
      RAISES { E };

    (* we should also be able to pass in Rd.Ts or some other structure
       that lets us read bundles, among other things.  Or perhaps just
       an iterator over Rd.Ts. *)

    init2(input : Rd.T; 
          output : Wr.T;
          READONLY files : ARRAY OF Pathname.T;
          globalEnv : Environment := NIL) : T 
      RAISES { E };

    readEvalWriteLoop(interrupter : Interrupter := NIL) RAISES { Wr.Failure };

    setInterrupter(interrupter : Interrupter);

    loadFile(fn : Object) : Object RAISES { E };

    loadRd(rd : Rd.T; fileName : Pathname.T := NIL) : Object RAISES { E } ;

    loadText(text : TEXT) : Object RAISES { E };

    loadPort(port : Object (* must be SchemeInputPort *)) : Object 
      RAISES { E };
    (* returns True() *)

    eval(x : REFANY; env : SchemeEnvironmentSuper.T) : Object RAISES { E };

    loadEval(rd : Rd.T) : Object RAISES { E };
    (* evaluate data on rd and return last object evaluated *)

    loadEvalText(txt : TEXT) : Object RAISES { E };
    (* evaluate data in txt and return last object evaluated *)

    evalInGlobalEnv(x : Object) : Object RAISES { E };

    evalList(list : Object; env : SchemeEnvironmentSuper.T) : Object RAISES { E };
    (* always a SchemePair *)
     
    bind(var : REFANY; val : Object);
    (* 
       bind (define) some object to symbol in var from Modula-3.

       var should be EITHER a SchemeSymbol.T OR a TEXT 
    *)
    
    setInGlobalEnv(var : Symbol; val : Object) RAISES { E };
    (* set! var val *)

    setPrimitives(schemePrimDefiner : REFANY (*SchemePrimitive.Definer*));

    changeGlobalEnvironment(env : SchemeEnvironmentSuper.T);

    getGlobalEnvironment() : SchemeEnvironmentSuper.T;

    attemptToMapRuntimeErrors() : BOOLEAN;
    (* should the REPL attempt to map runtime errors to an exception-abort *)

    setRTErrorMapping(to : BOOLEAN);
    (* set the above (default TRUE) *)

  END;

TYPE Interrupter = OBJECT METHODS interrupt() : BOOLEAN; END;
     (* if passed in, interpreter will call interrupt at regular 
        intervals, and if it returns TRUE, will interrupt evaluation *)

CONST Brand = "Scheme";

PROCEDURE SymbolCheck(x : Object) : SchemeSymbol.T RAISES { E }; 
  (* this has to be here because of the declaration of E *)

PROCEDURE VectorCheck(x : Object) : SchemeVector.T RAISES { E };
  (* same reason *)

VAR DoTracebacks := TRUE;
    EnvDisablesTracebacks : BOOLEAN;

TYPE RunInteractionHook = 
  PROCEDURE (env : REFANY (*SchemeEnvironment.T*); 
             do : Object (* run interaction if not #f *) ) : Object RAISES { E };

PROCEDURE SetInteractionHook(hook : RunInteractionHook);

PROCEDURE IsSpecialForm(s : Symbol) : BOOLEAN;

END Scheme.
