MODULE SchemeEnvironmentSafe EXPORTS SchemeEnvironment;

IMPORT SchemeEnvironmentInstanceRep;

FROM Scheme IMPORT Symbol, Object;
IMPORT AtomList;
FROM SchemeEnvironmentInstanceRep IMPORT QuickMap;

REVEAL
  Safe = Unsafe BRANDED Brand & " Safe" OBJECT 
    mu : MUTEX    := NIL;
  OVERRIDES
    initEmpty     :=  InitEmpty;
    getLocalNames :=  SafeGetLocalNames;
    put           :=  SafePut;
    get           :=  SafeGet;
  END;

PROCEDURE SafeGetLocalNames(x : Safe) : AtomList.T =
  BEGIN 
    LOCK x.mu DO RETURN Unsafe.getLocalNames(x) END 
  END SafeGetLocalNames;

PROCEDURE SafeGet(t : Safe; var : Symbol; VAR val : Object) : BOOLEAN =
  BEGIN 
    LOCK t.mu DO RETURN Unsafe.get(t,var,val) END
  END SafeGet;

PROCEDURE SafePut(t : Safe; var : Symbol; READONLY val : Object) =
  BEGIN
    LOCK t.mu DO Unsafe.put(t,var,val) END
  END SafePut;

PROCEDURE InitEmpty(t : Safe; parent : T) : Instance =
  BEGIN 
    IF t.mu = NIL THEN t.mu := NEW(MUTEX) END;

    (* why lock it? well if it's a safe version, it might still
       be accessed from other threads *)
    LOCK t.mu DO
      t.parent := parent;
      t.dictionary := NIL;
      FOR i := FIRST(t.quick) TO LAST(t.quick) DO
        t.quick[i] := QuickMap { NIL, NIL };
      END
    END;

    RETURN t 
  END InitEmpty;

BEGIN END SchemeEnvironmentSafe.
