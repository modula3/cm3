(* $Id$ *)

INTERFACE SchemeProcedureStubs;

IMPORT Scheme, SchemeObject, Atom;
IMPORT SchemePrimitive;
IMPORT RT0;

TYPE 
  P = PROCEDURE (interp : Scheme.T; 
                 args, excHandler : SchemeObject.T) : SchemeObject.T
      RAISES { Scheme.E };

PROCEDURE Register(name : Qid; proc : P);

TYPE Qid = REF RECORD intf, item : Atom.T END; (* from Type.i3 *)

PROCEDURE Extend(prims : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner;

PROCEDURE CallScheme(interp : Scheme.T;
                     proc : SchemeObject.T; 
                     args : SchemeObject.T) : SchemeObject.T;
  (* call scheme procedure proc with args of Modula-3 routine *)

PROCEDURE RegisterNew(typeName : Qid; newProc : NewProc);

TYPE NewProc = PROCEDURE(interp : Scheme.T;
                         initAssocList : SchemeObject.T) : SchemeObject.T 
  RAISES { Scheme.E };

PROCEDURE RegisterOp(typeId : RT0.Typecode;
                     name : TEXT;
                     proc : OpProc);

TYPE OpProc = PROCEDURE (interp : Scheme.T; 
                         object : SchemeObject.T;
                         args   : SchemeObject.T) : SchemeObject.T 
  RAISES { Scheme.E };

PROCEDURE RegisterTC(tc : RT0.Typecode; name : TEXT);

PROCEDURE RegisterTypePickle(READONLY typeCodes : ARRAY OF [-1..LAST(RT0.Typecode)];
                             READONLY names : ARRAY OF TEXT;
                             READONLY pickle : ARRAY OF CHAR);
  (* types is a list of typecodes, or -1 for non-reference types *)
  (* pickle is a Scheme list of the type descriptors for each of the
     types listed *)

(* will implementation attempt to map runtime errors to exception? *)
PROCEDURE GetMapRuntimeErrors() : BOOLEAN;
PROCEDURE SetMapRuntimeErrors(to : BOOLEAN);

END SchemeProcedureStubs.
