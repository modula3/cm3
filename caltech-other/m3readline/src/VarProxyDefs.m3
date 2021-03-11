(* $Id$ *)

MODULE VarProxyDefs EXPORTS VarUI;
IMPORT VarProxyClass;
IMPORT ReadLineUI;
IMPORT FloatMode, Lex, Scan, Fmt;
IMPORT RefList, RefSeq, Thread;
IMPORT SXInt, SXLongReal AS SXLR, SXBool, SXText;

REVEAL
  VarProxy = VarProxyClass.Private BRANDED Brand & " VarProxy" OBJECT 
    cbs : RefList.T := NIL;
  OVERRIDES
    init := VPInit;
    notify := ScheduleVPNotify;
    registerCallback := VPRCB;
  END;

PROCEDURE VPInit(vp : VarProxy; mode : ProxyMode) : VarProxy = 
  BEGIN vp.mu := NEW(MUTEX); vp.mode := mode; RETURN vp END VPInit;

(* note that the UI locks each mu before calling set and show.
   we have to lock the mu before getValue and setValue if we are assuming
   naive clients. *)

PROCEDURE VPRCB(vp : VarProxy; cb : Callback) =
  BEGIN vp.cbs := RefList.Cons(cb,vp.cbs) END VPRCB;

PROCEDURE ScheduleVPNotify(vp : VarProxy) =
  BEGIN
    LOCK nMu DO
      pendingNotifies.addhi(vp);
      Thread.Signal(nc)
    END
  END ScheduleVPNotify;

PROCEDURE VPNotify(vp : VarProxy) =
  VAR p := vp.cbs; BEGIN 
    WHILE p # NIL DO NARROW(p.head,Callback).notify(vp); p := p.tail END 
  END VPNotify;

VAR 
  pendingNotifies := NEW(RefSeq.T).init();
  nc := NEW(Thread.Condition);
  nMu := NEW(MUTEX);

<*UNUSED*>
VAR
  nThread := Thread.Fork(NEW(Thread.Closure, apply := NotifyApply));

PROCEDURE NotifyApply(<*UNUSED*>cl : Thread.Closure) : REFANY =
  VAR vp : VarProxy; BEGIN
    LOOP
      LOCK nMu DO
        WHILE pendingNotifies.size() = 0 DO Thread.Wait(nMu,nc) END;
        vp := pendingNotifies.remlo()
      END;
      VPNotify(vp)
    END
  END NotifyApply;

(**********************************************************************)

REVEAL
  DefIntProxy = PubIntProxy BRANDED Brand & " DefIntProxy" OBJECT 
    v : INTEGER;
    s : SXInt.Var := NIL;
  OVERRIDES
    init := InitIntProxy;
    getValue := GetIntValue;
    setValue := SetIntValue;
    set := SetInt;
    show := ShowInt;
    sx := IntSX;
  END;

PROCEDURE IntSX(p : DefIntProxy) : SXInt.T =
  BEGIN
    IF p.s = NIL THEN
      WITH s = NEW(SXInt.Var).initVal(p.v) DO
        p.s := s;
        p.registerCallback(NEW(Callback, notify := IntSXNotify))
      END
    END;
    RETURN p.s
  END IntSX;

PROCEDURE IntSXNotify(<*UNUSED*>cb : Callback; var : VarProxy) =
  BEGIN WITH p = NARROW(var,DefIntProxy) DO p.s.set(p.v) END END IntSXNotify;

PROCEDURE InitIntProxy(p : DefIntProxy; 
                       val : INTEGER; mode : ProxyMode) : DefIntProxy =
  BEGIN
    EVAL VarProxy.init(p,mode);
    p.v := val;
    RETURN p
  END InitIntProxy;

PROCEDURE GetIntValue(p : DefIntProxy) : INTEGER = 
  BEGIN LOCK p.mu DO RETURN p.v END END GetIntValue;

PROCEDURE SetIntValue(p : DefIntProxy; v : INTEGER) = 
  BEGIN LOCK p.mu DO p.v := v END; p.notify() END SetIntValue;

PROCEDURE SetInt(p : DefIntProxy; n, to : TEXT) RAISES { ReadLineUI.Error } =
  BEGIN
    TRY
      p.v := Scan.Int(to); p.notify() 
    EXCEPT
      Lex.Error, FloatMode.Trap => 
        RAISE ReadLineUI.Error("setting " & n & " <- " & to & 
              " : integer expected")
    END
  END SetInt;

PROCEDURE ShowInt(p : DefIntProxy; <*UNUSED*>n : TEXT) : TEXT =
  BEGIN RETURN Fmt.Int(p.v) END ShowInt;

(**********************************************************************)

REVEAL
  DefLRProxy = PubLRProxy BRANDED Brand & " DefLRProxy" OBJECT 
    v : LONGREAL;
    s : SXLR.Var := NIL;
  OVERRIDES
    init := InitLRProxy;
    getValue := GetLRValue;
    setValue := SetLRValue;
    set := SetLR;
    show := ShowLR;
    sx := LRSX;
  END;

PROCEDURE LRSX(p : DefLRProxy) : SXLR.T =
  BEGIN
    IF p.s = NIL THEN
      WITH s = NEW(SXLR.Var).initVal(p.v) DO
        p.s := s;
        p.registerCallback(NEW(Callback, notify := LRSXNotify))
      END
    END;
    RETURN p.s
  END LRSX;

PROCEDURE LRSXNotify(<*UNUSED*>cb : Callback; var : VarProxy) =
  BEGIN WITH p = NARROW(var,DefLRProxy) DO p.s.set(p.v) END END LRSXNotify;

PROCEDURE InitLRProxy(p : DefLRProxy; 
                      val : LONGREAL; mode : ProxyMode) : DefLRProxy =
  BEGIN
    EVAL VarProxy.init(p,mode);
    p.v := val;
    RETURN p
  END InitLRProxy;

PROCEDURE GetLRValue(p : DefLRProxy) : LONGREAL = 
  BEGIN LOCK p.mu DO RETURN p.v END END GetLRValue;

PROCEDURE SetLRValue(p : DefLRProxy; v : LONGREAL) = 
  BEGIN LOCK p.mu DO p.v := v END; p.notify()  END SetLRValue;

PROCEDURE SetLR(p : DefLRProxy; n, to : TEXT) RAISES { ReadLineUI.Error } =
  BEGIN
    TRY
      p.v := Scan.LongReal(to); p.notify() 
    EXCEPT
      Lex.Error, FloatMode.Trap => 
        RAISE ReadLineUI.Error("setting " & n & " <- " & to & 
              " : LONGREAL expected")
    END
  END SetLR;

PROCEDURE ShowLR(p : DefLRProxy; <*UNUSED*>n : TEXT) : TEXT =
  BEGIN RETURN Fmt.LongReal(p.v) END ShowLR;

(**********************************************************************)

REVEAL
  DefTextProxy = PubTextProxy BRANDED Brand & " DefTextProxy" OBJECT 
    v : TEXT;
    s : SXText.Var;
  OVERRIDES
    init := InitTextProxy;
    getValue := GetTextValue;
    setValue := SetTextValue;
    set := SetText;
    show := ShowText;
    sx := TextSX;
  END;

PROCEDURE TextSX(p : DefTextProxy) : SXText.T =
  BEGIN
    IF p.s = NIL THEN
      WITH s = NEW(SXText.Var).initVal(p.v) DO
        p.s := s;
        p.registerCallback(NEW(Callback, notify := TextSXNotify))
      END
    END;
    RETURN p.s
  END TextSX;

PROCEDURE TextSXNotify(<*UNUSED*>cb : Callback; var : VarProxy) =
  BEGIN WITH p = NARROW(var,DefTextProxy) DO p.s.set(p.v) END END TextSXNotify;

PROCEDURE InitTextProxy(p : DefTextProxy; 
                       val : TEXT; mode : ProxyMode) : DefTextProxy =
  BEGIN
    EVAL VarProxy.init(p,mode);
    p.v := val;
    RETURN p
  END InitTextProxy;

PROCEDURE GetTextValue(p : DefTextProxy) : TEXT = 
  BEGIN LOCK p.mu DO RETURN p.v END END GetTextValue;

PROCEDURE SetTextValue(p : DefTextProxy; v : TEXT) = 
  BEGIN LOCK p.mu DO p.v := v END; p.notify()  END SetTextValue;

PROCEDURE SetText(p : DefTextProxy; 
                  <*UNUSED*>n : TEXT;
                  to : TEXT) =
  BEGIN p.v := to; p.notify()  END SetText;

PROCEDURE ShowText(p : DefTextProxy; <*UNUSED*>n : TEXT) : TEXT =
  BEGIN RETURN p.v END ShowText;

(**********************************************************************)


REVEAL
  DefBoolProxy = PubBoolProxy BRANDED Brand & " DefBoolProxy" OBJECT 
    v : BOOLEAN;
    s : SXBool.Var;
  OVERRIDES
    init := InitBoolProxy;
    getValue := GetBoolValue;
    setValue := SetBoolValue;
    set := SetBool;
    show := ShowBool;
    sx := BoolSX;
  END;

PROCEDURE BoolSX(p : DefBoolProxy) : SXBool.T =
  BEGIN
    IF p.s = NIL THEN
      WITH s = NEW(SXBool.Var).initVal(p.v) DO
        p.s := s;
        p.registerCallback(NEW(Callback, notify := BoolSXNotify))
      END
    END;
    RETURN p.s
  END BoolSX;

PROCEDURE BoolSXNotify(<*UNUSED*>cb : Callback; var : VarProxy) =
  BEGIN WITH p = NARROW(var,DefBoolProxy) DO p.s.set(p.v) END END BoolSXNotify;

PROCEDURE InitBoolProxy(p : DefBoolProxy; 
                       val : BOOLEAN; mode : ProxyMode) : DefBoolProxy =
  BEGIN
    EVAL VarProxy.init(p,mode);
    p.v := val; p.notify(); 
    RETURN p
  END InitBoolProxy;

PROCEDURE GetBoolValue(p : DefBoolProxy) : BOOLEAN = 
  BEGIN LOCK p.mu DO RETURN p.v END END GetBoolValue;

PROCEDURE SetBoolValue(p : DefBoolProxy; v : BOOLEAN) = 
  BEGIN LOCK p.mu DO p.v := v END; p.notify()  END SetBoolValue;

PROCEDURE SetBool(p : DefBoolProxy; n, to : TEXT) RAISES { ReadLineUI.Error } =
  BEGIN
    TRY
      p.v := Scan.Bool(to)
    EXCEPT
      Lex.Error =>
        RAISE ReadLineUI.Error("setting " & n & " <- " & to & 
              " : BOOLEAN expected")
    END
  END SetBool;

PROCEDURE ShowBool(p : DefBoolProxy; <*UNUSED*>n : TEXT) : TEXT =
  BEGIN RETURN Fmt.Bool(p.v) END ShowBool;

(**********************************************************************)
BEGIN END VarProxyDefs.



