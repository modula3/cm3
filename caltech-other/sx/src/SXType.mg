(* $Id$ *)

GENERIC MODULE SXType(Elem);
IMPORT XTime AS Time, SXClass, SX, SXRoot;
FROM SX IMPORT Uninitialized;
IMPORT SXIterator, Fmt;

REVEAL 
  T = Public BRANDED Brand OBJECT
    v         : Elem.T;
    updates   : CARDINAL := 0;
    name      : TEXT;
    validator : Validator := NIL;
  OVERRIDES 
    update       := Update;
    updateLocked := UpdateLocked;
    value        := GetValue;
    waitFor      := WaitFor;
    numUpdates   := NumUpdates;
    uninitialize := Uninitialize;
    initialized  := Initialized;
    attachName   := AttachName;
    getName      := GetName;
    setValidator := SetValidator;
    type         := TreeType;
    debugInfo    := TDebugInfo;
  END;

  Var = PublicVar BRANDED Brand & " Var" OBJECT 
  OVERRIDES
    set := SetVar;
    setLocked := SetVarLocked;
    initVal := InitVal;
    dependsOn := SXIterator.NullNull;
    type := VarType;
  END;

  Const = PublicConst BRANDED Brand & " Const" OBJECT
  OVERRIDES
    init := InitConst;
    dependsOn := SXIterator.NullNull;
    type := ConstType;
  END;

PROCEDURE TreeType(<*UNUSED*>t : T) : SXRoot.Type = 
  BEGIN RETURN SXRoot.Type.Tree END TreeType;

PROCEDURE VarType(<*UNUSED*>t : T) : SXRoot.Type = 
  BEGIN RETURN SXRoot.Type.Var END VarType;

PROCEDURE ConstType(<*UNUSED*>t : T) : SXRoot.Type = 
  BEGIN RETURN SXRoot.Type.Const END ConstType;

PROCEDURE TDebugInfo(t : T) : TEXT =
  BEGIN RETURN Fmt.F("updates %s", Fmt.Int(t.updates)) END TDebugInfo;
  (* we could add a lot of debuginfo in SX, threads, etc. *)

PROCEDURE Uninitialize(t : T) = BEGIN t.updates := 0 END Uninitialize;

PROCEDURE Initialized(t : T) : BOOLEAN = 
  BEGIN RETURN t.updates # 0 END Initialized;

PROCEDURE InitVal(var : Var; val : Elem.T) : Var =
  BEGIN
    WITH me = NARROW(T.init(var),Var) DO
      IF me.validator # NIL THEN EVAL me.validator.validQ(val) END;
      me.v := val;
      me.updates := 1;
      RETURN me
    END
  END InitVal;

PROCEDURE SetValidator(t : T; v : Validator) = 
  BEGIN t.validator := v END SetValidator;

PROCEDURE AttachName(t : T; name : TEXT) =
  BEGIN t.name := name END AttachName;

PROCEDURE GetName(t : T) : TEXT =
  BEGIN RETURN t.name END GetName;

PROCEDURE NumUpdates(t : T) : CARDINAL =
  BEGIN RETURN t.updates END NumUpdates;

PROCEDURE GetValue(t : T) : Elem.T RAISES { Uninitialized } = 
  BEGIN 
    IF t.updates = 0 THEN RAISE Uninitialized END;
    RETURN t.v 
  END GetValue;

PROCEDURE WaitFor(t : T; val : Elem.T) =
  BEGIN
    LOOP
      TRY
        IF Elem.Equal(t.value(),val) THEN RETURN END
      EXCEPT
        Uninitialized => (* skip *)
      END;
      t.wait()
    END
  END WaitFor;

(**********************************************************************)

PROCEDURE Update(v : T; newValue : Elem.T; when : Time.T) : BOOLEAN =
  BEGIN 
    LOCK v.mu DO RETURN v.updateLocked(newValue,when) END
  END Update;

PROCEDURE UpdateLocked(v : T; newValue : Elem.T; when : Time.T) : BOOLEAN =
  BEGIN 
    LOCK SX.mu DO
      IF v.validator # NIL THEN EVAL v.validator.validQ(newValue) END;
      IF v.v = newValue AND v.updates > 0 THEN
        RETURN FALSE
      ELSE
        v.v := newValue; v.updated := when; INC(v.updates);
        RETURN TRUE
      END
    END 
  END UpdateLocked;

PROCEDURE SetVar(v : Var; newValue : Elem.T; when : Time.T) =
  BEGIN 
    IF when = FIRST(Time.T) THEN when := Time.Now() END;
    IF v.update(newValue, when) THEN
      v.propagate(when)
    END
  END SetVar;

PROCEDURE SetVarLocked(v : Var; newValue : Elem.T; when : Time.T) =
  BEGIN
    IF when = FIRST(Time.T) THEN when := Time.Now() END;
    IF v.updateLocked(newValue, when) THEN
      v.propagate(when,TRUE)
    END
  END SetVarLocked;

PROCEDURE InitConst(c : Const; value : Elem.T) : Const = 
  BEGIN
    c.v := value;
    c.updates := 1;
    RETURN T.init(c)
  END InitConst;

PROCEDURE BaseCompare(a, b : Base) : INTEGER =
  BEGIN RETURN Elem.Compare(a,b) END BaseCompare;

PROCEDURE NewConst(v : Elem.T) : Const = 
  BEGIN RETURN NEW(Const).init(v) END NewConst;

BEGIN END SXType.










