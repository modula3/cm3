(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeEnvironment;

(* One of the few things not taken directly from Norvig... *)

IMPORT SchemeEnvironmentInstanceRep;

IMPORT SchemePrimitive, SchemeSymbol, SchemeProcedure;
IMPORT SchemeProcedureClass;
FROM Scheme IMPORT Symbol, Object, E;
FROM SchemeUtils IMPORT Error, Warn, StringifyT, First, Stringify;
IMPORT SchemeUtils;
IMPORT Text;
IMPORT SchemePair;
IMPORT Scheme;
IMPORT AtomList, SchemeLongReal;
IMPORT SchemeEnvironmentBinding;

TYPE Pair = SchemePair.T;
TYPE Binding = SchemeEnvironmentBinding.T;

CONST TE = Text.Equal;


REVEAL
  Instance = SchemeEnvironmentInstanceRep.Rep BRANDED Brand OBJECT
  OVERRIDES
    bind         :=  GetBinding;
    initDict     :=  InitDict;
    initDictEval :=  InitDictEval2;
    initEval     :=  InitEval;
    lookup       :=  Lookup;
    define       :=  Define;
    set          :=  Set;
    defPrim      :=  DefPrim;
    markAsDead   :=  MarkAsDead;
    getParent    :=  GetParent;
    haveBinding  :=  HaveBinding;
  END;

PROCEDURE GetParent(t : Instance) : T = BEGIN RETURN t.parent END GetParent;

(**********************************************************************)

PROCEDURE MarkAsDead(t : Instance) = BEGIN t.dead := TRUE END MarkAsDead;

PROCEDURE InitEval(t                : Instance; 
                   vars, argsToEval : Object;
                   evalEnv          : T; 
                   interp           : Scheme.T;
                   parent           : T) : Instance 
  RAISES { E } =
  BEGIN
    EVAL t.initEmpty();
    t.parent := parent;
    IF NOT t.initDictEval(vars, argsToEval, evalEnv, interp) THEN
        EVAL Warn("wrong number of arguments: expected " &
          StringifyT(vars) & " got " & StringifyT(interp.evalList(argsToEval,evalEnv)))
    END;
    RETURN t
  END InitEval;

PROCEDURE InitDict(t                   : Instance; 
                   vars, vals          : Object;
                   VAR canRecyclePairs : BOOLEAN) : BOOLEAN =
  BEGIN
    IF vars = NIL AND vals = NIL THEN 
      RETURN TRUE 
    ELSIF vars # NIL AND ISTYPE(vars, Symbol) THEN
      canRecyclePairs := FALSE;
      t.put(vars,vals);
      RETURN TRUE
    ELSIF vars # NIL AND vals # NIL AND 
          ISTYPE(vars, Pair) AND ISTYPE(vals, Pair) THEN
      WITH varp = NARROW(vars,Pair), valp = NARROW(vals,Pair) DO
        IF varp.first # NIL AND ISTYPE(varp.first,Symbol) THEN
          t.put(varp.first, valp.first);
        END;
        RETURN InitDict(t, varp.rest, valp.rest, canRecyclePairs)
      END
    ELSE
      RETURN FALSE
    END
  END InitDict;

PROCEDURE InitDictEval2(t                : Instance; 
                        vars, argsToEval : Object; 
                        evalEnv          : T;
                        interp           : Scheme.T) : BOOLEAN RAISES { E }=
  BEGIN
    LOOP
      IF vars = NIL AND argsToEval = NIL THEN 
        RETURN TRUE 
      ELSIF vars # NIL AND ISTYPE(vars, Symbol) THEN
        t.put(vars,interp.evalList(argsToEval,evalEnv));
        RETURN TRUE
      ELSIF vars # NIL AND argsToEval # NIL AND 
        ISTYPE(vars, Pair) AND ISTYPE(argsToEval, Pair) THEN
        WITH varp = NARROW(vars,Pair), argp = NARROW(argsToEval,Pair) DO
          IF varp.first # NIL AND ISTYPE(varp.first,Symbol) THEN
            t.put(varp.first, interp.eval(argp.first,evalEnv));
          END;
          vars := varp.rest ; argsToEval := argp.rest
        END
      ELSE
        RETURN FALSE
      END
    END
  END InitDictEval2;

PROCEDURE HaveBinding(t : Instance; symbol : Symbol) : BOOLEAN =
  VAR o : Object;
  BEGIN
    <*ASSERT NOT t.dead*>
    IF t.get(symbol,o) THEN RETURN TRUE END;
    
    IF t.parent # NIL THEN 
      RETURN t.parent.haveBinding(symbol)
    ELSE 
      RETURN FALSE
    END
  END HaveBinding;

PROCEDURE Lookup(t : Instance; symbol : Symbol) : Object RAISES { E } =
  VAR o : Object;
  BEGIN
    <*ASSERT NOT t.dead*>
    IF t.get(symbol,o) THEN
      RETURN o
    END;
    
    IF t.parent # NIL THEN 
      RETURN t.parent.lookup(symbol)
    ELSE 
      RETURN Error("Unbound variable in lookup: " & Stringify(symbol)) 
    END
  END Lookup;

(**********************************************************************)

PROCEDURE GetBinding(t : Instance; sym : Symbol) : Binding RAISES { E } =
  VAR o : Object;
  BEGIN
    <*ASSERT NOT t.dead*>
    IF t.get(sym,o) THEN
      RETURN NEW(SimpleBinding, e := t, s := sym)
    END;
    
    IF t.parent # NIL THEN 
      RETURN t.parent.bind(sym)
    ELSE 
      RETURN Error("Unbound variable attempting to bind: " & Stringify(sym)) 
    END
  END GetBinding;

TYPE 
  SimpleBinding = SchemeEnvironmentBinding.T OBJECT
    e : Instance;
    s : Symbol;
  OVERRIDES
    name := SBName;
    env  := SBEnv;
    get  := SBGet;
    setB := SBSetB;
  END;

PROCEDURE SBName(sb : SimpleBinding) : Symbol = BEGIN RETURN sb.s END SBName;

PROCEDURE SBEnv(sb : SimpleBinding) : Object = BEGIN RETURN sb.e END SBEnv;

PROCEDURE SBGet(sb : SimpleBinding) : Object =
  VAR
    v : Object;
  BEGIN 
    WITH gotit = sb.e.get(sb.s,v) DO
      <*ASSERT gotit*>
      RETURN v
    END
  END SBGet;

PROCEDURE SBSetB(sb : SimpleBinding; v : Object) =
  BEGIN sb.e.put(sb.s,v) END SBSetB;

(**********************************************************************)


PROCEDURE Define(t : Instance; var, val : Object) : Object =
  BEGIN
    <*ASSERT NOT t.dead*>
    IF var = NIL OR NOT ISTYPE(var, Symbol) THEN RETURN var END;

    t.put(var,val);

    TYPECASE val OF
      NULL => (* skip *)
    |
      SchemeProcedure.T(p) => 
      IF TE(p.name, SchemeProcedureClass.DefaultName) THEN
        p.name := SchemeSymbol.ToText(var)
      END
    ELSE (* skip *)
    END;

    RETURN var
  END Define;

PROCEDURE Set(t : Instance; var, val : Object) : Object RAISES { E } =
  VAR dummy : Object;
  BEGIN
    <*ASSERT NOT t.dead*>
    IF var = NIL OR NOT ISTYPE(var, Symbol) THEN
      RETURN Error("Attempt to set a non-symbol: " &
             SchemeUtils.Stringify(var))
    END;

    IF t.get( var, dummy) THEN
      t.put(var, val);
      RETURN val (* ?? *)
    END;

    IF t.parent # NIL THEN
      RETURN t.parent.set(var, val)
    ELSE
      RETURN Error("Unbound variable in set!: " & SchemeSymbol.ToText(var))
    END
  END Set;

PROCEDURE DefPrim(t                : Instance; 
                  name             : TEXT; 
                  id               : INTEGER;
                  definer          : REFANY;
                  minArgs, maxArgs : CARDINAL) : T =
  BEGIN
    <*ASSERT NOT t.dead*>
    EVAL t.define(SchemeSymbol.Symbol(name), 
                  NEW(SchemePrimitive.T).init(id, definer, minArgs, maxArgs));
    RETURN t
  END DefPrim;

PROCEDURE ListPrimitivesApply(<*UNUSED*>p      : SchemeProcedure.T; 
                              <*UNUSED*>interp : Scheme.T; 
                              args             : Object) : Object 
  RAISES { E } =
  
  BEGIN
    WITH x = First(args) DO
      IF x = NIL OR NOT ISTYPE(x, Instance) THEN
        RETURN Error("Not a SchemeEnvironment.Instance : " & Stringify(x))
      END;

      VAR e := NARROW(x,Instance);
          names : AtomList.T;
      BEGIN
        names := e.getLocalNames();

        VAR res : Pair := NIL;
            p := names;
        BEGIN
          WHILE p # NIL DO
            WITH val = e.lookup(p.head) DO
              TYPECASE val OF
                NULL => (* skip *) |
                SchemePrimitive.T(prim) =>
                res := NEW(Pair,
                           first := SchemeUtils.List4(p.head,
                                      SchemeLongReal.FromI(prim.getId()),
                                      SchemeLongReal.FromI(prim.getMinArgs()),
                                      SchemeLongReal.FromI(prim.getMaxArgs())
                                         ),
                           rest := res)
              ELSE
                (* skip *)
              END
            END;
            p := p.tail
          END;
          RETURN res
        END
      END
    END
  END ListPrimitivesApply;

PROCEDURE ExtendWithIntrospectionPrimitives(pa : REFANY) = 
  VAR
    prims := NARROW(pa, SchemePrimitive.ExtDefiner);
  BEGIN
    prims.addPrim("list-primitives", NEW(SchemeProcedure.T,
                                         apply := ListPrimitivesApply),
                  1, 1)
  END ExtendWithIntrospectionPrimitives;
    
BEGIN END SchemeEnvironment.
