(* $Id$ *)

GENERIC MODULE SXProxy(Elem);
IMPORT SX, VarUI, VarProxyClass;
IMPORT Thread, SXSelect;
IMPORT ReadLineUI;
IMPORT SXConversion;

REVEAL
  SuperClass = PubSuper BRANDED Brand & " Super" OBJECT 
  OVERRIDES
    set := Set;
    show := Show;
  END;

PROCEDURE Set(p : SuperClass; n, to : TEXT) RAISES { ReadLineUI.Error } =
  BEGIN
    TRY 
      p.setValue(p.fromText(to))
    EXCEPT
      SXConversion.Error(err) =>
      RAISE ReadLineUI.Error("setting " & n & " <- " & to & 
              " : valid instance of " & Elem.Brand & " expected : " & err)
    END
  END Set;

PROCEDURE Show(p : SuperClass; n : TEXT) : TEXT RAISES { ReadLineUI.Error } =
  BEGIN
    TRY
      RETURN p.toText(p.getValue())
    EXCEPT
      SX.Uninitialized => RAISE ReadLineUI.Error("SX.Uninitialized: attempting to read uninitialized variable " & n)
    END
  END Show;

REVEAL
  T = Public BRANDED Brand OBJECT
    in : SXElem;
  OVERRIDES
    init := Init;
    getValue := GetValue;
    setValue := SetValue;
    sx := GetSX;
  END;

PROCEDURE GetValue(t : T) : Elem.T =
  <* FATAL SX.Uninitialized *> (* can't happen *)
  BEGIN RETURN t.in.value() END GetValue;

PROCEDURE SetValue(t : T; v : Elem.T) =
  BEGIN t.in.set(v) (* notify is handled by watching thread *) END SetValue;

PROCEDURE Init(t : T; initVal : Elem.T; mode : VarUI.ProxyMode) : T =
  BEGIN
    EVAL VarUI.VarProxy.init(t, mode);
    t.in := NEW(SXElem, p := t).initVal(initVal);
    EVAL Thread.Fork(NEW(Closure, t := t, stackSize := 4096));
    RETURN t
  END Init;

PROCEDURE GetSX(t : T) : SXElem = BEGIN RETURN t.in END GetSX;

TYPE 
  Closure = Thread.SizedClosure OBJECT
    t : T;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(cl : Closure) : REFANY =
  BEGIN
    SX.Lock(SX.Array { cl.t.in });
    LOOP
      SXSelect.Wait1(cl.t.in);
      cl.t.notify()
    END
  END Apply;

REVEAL
  Watcher = PubWatcher BRANDED Brand & " Watcher" OBJECT
    in : SXType;
    wt : Thread.T := NIL;
  OVERRIDES
    init := InitW;
    getValue := GetValueW;
    sx := GetSXW;
    registerCallback := RegisterCallbackW;
  END;

PROCEDURE RegisterCallbackW(t : Watcher; cb : VarUI.Callback) =
  BEGIN
    (* we don't need the watcher thread unless there are callbacks registered,
       so don't start it until the first registration. *)
    IF t.wt = NIL THEN
      t.wt := Thread.Fork(NEW(WClosure, t := t))
    END;
    PubWatcher.registerCallback(t,cb)
  END RegisterCallbackW;

PROCEDURE InitW(t : Watcher; sx : SXType) : Watcher =
  BEGIN
    EVAL VarUI.VarProxy.init(t, VarUI.ProxyMode.ReadOnly);
    t.in := sx;
    RETURN t
  END InitW;

PROCEDURE GetValueW(t : Watcher) : Elem.T RAISES { SX.Uninitialized } =
  BEGIN RETURN t.in.value() END GetValueW;

PROCEDURE GetSXW(t : Watcher) : SXType =
  BEGIN RETURN t.in END GetSXW;

TYPE 
  WClosure = Thread.Closure OBJECT
    t : Watcher;
  OVERRIDES
    apply := WApply;
  END;

PROCEDURE WApply(cl : WClosure) : REFANY =
  BEGIN
    SX.Lock(SX.Array { cl.t.in });
    LOOP
      SXSelect.Wait1(cl.t.in);
      cl.t.notify()
    END
  END WApply;

BEGIN END SXProxy.






