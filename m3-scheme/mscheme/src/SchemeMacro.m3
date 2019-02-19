(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeMacro;
IMPORT Scheme, SchemeSymbol;
FROM Scheme IMPORT Object, E;
FROM SchemeUtils IMPORT Cons, First, Rest;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    expand := Expand;
  END;

PROCEDURE Expand(t : T; 
                 interpreter : Scheme.T; 
                 oldPair : Pair; args : Object) : Pair RAISES { E } =
  BEGIN
    WITH expansion = t.apply(interpreter,args) DO
      IF expansion # NIL AND ISTYPE(expansion,Pair) THEN
        WITH p = NARROW(expansion, Pair) DO
          oldPair.first := p.first;
          oldPair.rest := p.rest
        END
      ELSE
        oldPair.first := SchemeSymbol.Symbol("begin");
        oldPair.rest := Cons(expansion,NIL)
      END
    END;
    RETURN oldPair
  END Expand;

PROCEDURE MacroExpand(interpreter : Scheme.T; x : Object) : Object RAISES { E } =
  BEGIN
    IF x = NIL OR NOT ISTYPE(x,Pair) THEN RETURN x END;

    WITH fn = interpreter.evalInGlobalEnv(First(x)) DO
      IF fn = NIL OR NOT ISTYPE(fn,T) THEN RETURN x END;
      RETURN NARROW(fn,T).expand(interpreter, x, Rest(x))
    END
  END MacroExpand;
      

BEGIN END SchemeMacro.
