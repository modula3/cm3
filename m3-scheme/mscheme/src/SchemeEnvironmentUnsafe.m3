(* $Id$ *)

MODULE SchemeEnvironmentUnsafe EXPORTS SchemeEnvironment;

IMPORT SchemeEnvironmentInstanceRep;
IMPORT AtomRefTbl;
FROM Scheme IMPORT Object, Symbol, E;
IMPORT AtomList;
FROM SchemeUtils IMPORT Warn, StringifyT, Error, Stringify;
IMPORT SchemeEnvironmentBinding;
IMPORT Atom;
IMPORT Debug;
FROM Fmt IMPORT F, Int;

TYPE Binding = SchemeEnvironmentBinding.T;
     
REVEAL
  Instance <: SchemeEnvironmentInstanceRep.Rep;
  
  Unsafe = Instance BRANDED Brand & " Unsafe" OBJECT OVERRIDES
    init          := Init;
    initEmpty     := InitEmptyUnsafe;
    put           := UnsafePut;
    get           := UnsafeGet;
    getLocalNames := GetLocalNames;
    bind          := GetBinding;
    copy          := Copy;
    initCopy      := InitCopy;
  END;
  
PROCEDURE Copy(t : Unsafe) : T =
  BEGIN
    WITH new = NEW(Unsafe) DO
      t.initCopy(new);
      RETURN new
    END
  END Copy;

PROCEDURE CopyDictionary(dict : AtomRefTbl.T) : AtomRefTbl.T =
  VAR
    res := NEW(AtomRefTbl.Default).init();
    k : Atom.T;
    v : REFANY;
    iter := dict.iterate();
  BEGIN
    WHILE iter.next(k,v) DO
      EVAL res.put(k,v)
    END;
    RETURN res
  END CopyDictionary;

PROCEDURE QuickN(t : Unsafe) : CARDINAL =
  VAR
    res : CARDINAL := 0;
  BEGIN
    FOR i := FIRST(t.quick) TO LAST(t.quick) DO
      IF t.quick[i].var # NIL THEN
        INC(res)
      END
    END;
    RETURN res
  END QuickN;
  
PROCEDURE InitCopy(t : Unsafe; newA : T) =
  VAR
    new : Unsafe := newA;
  BEGIN
    IF t.dictionary = NIL THEN
      Debug.Out("SchemeEnvironmentUnsafe.InitCopy : t.dictionary = NIL");
      new.dictionary := NIL
    ELSE
      new.dictionary := CopyDictionary(t.dictionary);

      Debug.Out(F("SchemeEnvironmentUnsafe.InitCopy : t.dictionary.size() = %s ; new.dictionary.size() = %s",
                  Int(t.dictionary.size()), Int(new.dictionary.size())));

    END;
    new.quick      := t.quick;
    Debug.Out(F("SchemeEnvironmentUnsafe.InitCopy : t.quick = %s ; new.quick = %s", Int(QuickN(t)), Int(QuickN(new))));

    new.parent     := t.parent;
    new.dead       := t.dead
  END InitCopy;
  
PROCEDURE InitEmptyUnsafe(t : Unsafe; parent : T) : Instance =
  BEGIN 
    t.dictionary := NIL;
    t.parent := parent;
    FOR i := FIRST(t.quick) TO LAST(t.quick) DO
      t.quick[i].var := NIL
    END;

    RETURN t 
  END InitEmptyUnsafe;

PROCEDURE Init(t                   : Instance;
               vars, vals          : Object; 
               parent              : T;
               VAR canRecyclePairs : BOOLEAN) : Instance =
  BEGIN
    EVAL t.initEmpty(parent);
    IF NOT t.initDict(vars,vals,canRecyclePairs) THEN
      TRY
        EVAL Warn("wrong number of arguments: expected " &
          StringifyT(vars) & " got " & StringifyT(vals))
      EXCEPT
      ELSE
      END
    END;
    RETURN t
  END Init;

PROCEDURE GetLocalNames(e : Instance) : AtomList.T =
  VAR res : AtomList.T := NIL;
  BEGIN
    FOR i := FIRST(e.quick) TO LAST(e.quick) DO
      IF e.quick[i].var # NIL THEN 
        res := AtomList.Cons(e.quick[i].var,res)
      ELSE
        EXIT
      END
    END;

    IF e.dictionary # NIL THEN
      WITH iter = e.dictionary.iterate() DO
        VAR a : Symbol; o : REFANY; BEGIN
          WHILE iter.next(a,o) DO
            res := AtomList.Cons(a,res)
          END
        END
      END
    END;

    RETURN res
  END GetLocalNames;
  
PROCEDURE UnsafeGet(t : Instance; var : Symbol; VAR val : Object) : BOOLEAN =
  BEGIN 
    IF var = NIL THEN RETURN FALSE END;

    FOR i := FIRST(t.quick) TO LAST(t.quick) DO
      IF t.quick[i].var = var THEN val := t.quick[i].val; RETURN TRUE END
    END;
    
    IF t.dictionary # NIL THEN
      RETURN t.dictionary.get(var,val) 
    ELSE
      RETURN FALSE
    END
  END UnsafeGet;

PROCEDURE UnsafePut(t : Instance; var : Symbol; READONLY val : Object) =
  BEGIN 
    FOR i := FIRST(t.quick) TO LAST(t.quick) DO
      IF t.quick[i].var = var OR t.quick[i].var = NIL THEN 
        t.quick[i].var := var; 
        t.quick[i].val := val;
        RETURN
      END
    END;
    (* failed *)
    IF t.dictionary = NIL THEN
      t.dictionary := NEW(AtomRefTbl.Default).init()
    END;
    
    EVAL t.dictionary.put(var,val) 
  END UnsafePut;

(**********************************************************************)

PROCEDURE GetBinding(t : Instance; sym : Symbol) : Binding RAISES { E } =
  VAR o : Object;
  BEGIN
    <*ASSERT NOT t.dead*>
    IF sym = NIL THEN RETURN NEW(MyBinding, e := t, s := sym)  END;

    FOR i := FIRST(t.quick) TO LAST(t.quick) DO
      IF t.quick[i].var = sym THEN 
        RETURN NEW(MyBinding, e := t, q := i, s := sym)
      END
    END;

    IF t.get(sym,o) THEN
      RETURN NEW(MyBinding, e := t, s := sym)
    END;
    
    IF t.parent # NIL THEN 
      RETURN t.parent.bind(sym)
    ELSE 
      RETURN Error("Unbound variable attempting to bind: " & Stringify(sym)) 
    END
  END GetBinding;

TYPE 
  MyBinding = SchemeEnvironmentBinding.T OBJECT
    e : Unsafe;
    s : Symbol;
    q : [ -1..LAST(CARDINAL) ] := -1;
  OVERRIDES
    name := SBName;
    env  := SBEnv;
    get  := SBGet;
    setB := SBSetB;
  END;

PROCEDURE SBName(sb : MyBinding) : Symbol = BEGIN RETURN sb.s END SBName;

PROCEDURE SBEnv(sb : MyBinding) : Object = BEGIN RETURN sb.e END SBEnv;

PROCEDURE SBGet(sb : MyBinding) : Object =
  VAR
    v : Object;
  BEGIN 
    IF sb.q # -1 THEN RETURN sb.e.quick[sb.q].val END;

    WITH gotit = sb.e.get(sb.s,v) DO
      <*ASSERT gotit*>
      RETURN v
    END
  END SBGet;

PROCEDURE SBSetB(sb : MyBinding; v : Object) =
  BEGIN sb.e.put(sb.s,v) END SBSetB;

BEGIN END SchemeEnvironmentUnsafe.
