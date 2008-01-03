(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jan 31 00:03:04 PST 1995 by najork                   *)
(*       Created on Sun May 22 00:10:27 PDT 1994 by najork                   *)


MODULE RealProp EXPORTS RealProp, RealPropPrivate, RealPropProxy;

IMPORT Anim3D, AnimHandle, AnimHandlePrivate, AnimRequestQueue, 
       AnimRequestQueuePrivate, AnimServer, GraphicsBase, 
       GraphicsBasePrivate, Prop, PropPrivate;

(*****************************************************************************)
(* Type "Name"                                                               *)
(*****************************************************************************)

REVEAL
  Name = PrivateName BRANDED OBJECT
    default : Base;
  OVERRIDES
    init      := InitName;
    bind      := BindName;
    makeProxy := MakeProxyName;
    push      := PushName;
    pop       := PopName;
    newStack  := NewStack;
    getState  := GetState;
  END;


PROCEDURE InitName (self : Name; default : Base) : Name =
  BEGIN
    EVAL Prop.Name.init (self);
    self.default := default;
    IF NamePM # NIL THEN
      NamePM (self);
    END;
    RETURN self;
  END InitName;


PROCEDURE BindName (self : Name; val : Val) : Prop.T =
  BEGIN
    RETURN NEW (Prop.T).init (self, val);
  END BindName;


PROCEDURE MakeProxyName (self : Name) =
  BEGIN
    IF self.proxy = NIL AND NamePM # NIL THEN
      NamePM (self);
    END;
  END MakeProxyName;


PROCEDURE PushName (self : Name; state : GraphicsBase.T; pv : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], Stack),
         val   = NARROW (pv, Val).val DO
      stack.push (val);
    END;
  END PushName;


PROCEDURE PopName (self : Name; state : GraphicsBase.T) =
  BEGIN
    EVAL NARROW (state.stacks[self.id], Stack).pop ();
  END PopName;


PROCEDURE NewStack (self : Name) : PropPrivate.Stack =
  BEGIN
    RETURN NEW (Stack).init (self.default);
  END NewStack;


PROCEDURE GetState (self : Name; state : GraphicsBase.T) : Base =
  BEGIN
    RETURN NARROW (state.stacks[self.id], Stack).top;
  END GetState;


(*****************************************************************************)
(* Type "Val"                                                                *)
(*****************************************************************************)

REVEAL 
  Val = PrivateVal BRANDED OBJECT
  OVERRIDES 
    init     := InitVal;
    get      := GetVal;
    value    := ValueVal;
    adjust   := AdjustVal;
  END;


PROCEDURE InitVal (self : Val; beh : Beh) : Val =
  BEGIN
    self.beh  := beh;
    self.time := Anim3D.Now () - 10.0d0;

    IF ValPM # NIL THEN
      ValPM (self);
    END;
    RETURN self;
  END InitVal;


PROCEDURE GetVal (self : Val) : Base RAISES {Prop.BadMethod} =
  BEGIN
    RETURN self.beh.value (Anim3D.Now ());
  END GetVal;


PROCEDURE ValueVal (self : Val; time : LONGREAL) : Base
    RAISES {Prop.BadMethod} =
  BEGIN
    IF time = self.time THEN
      RETURN self.val;
    ELSE
      RETURN self.beh.value (time);
    END;
  END ValueVal;


PROCEDURE AdjustVal (self : Val; time : LONGREAL) : BOOLEAN 
    RAISES {Prop.BadMethod} =
  BEGIN
    IF time # self.time THEN
      WITH val = self.beh.value (time) DO
        self.damaged := val # self.val;
        self.time    := time;
        self.val     := val;
      END;
    END;
    RETURN self.damaged;
  END AdjustVal;


(*****************************************************************************)
(* Type "Beh"                                                                *)
(*****************************************************************************)

REVEAL 
  Beh = PrivateBeh BRANDED OBJECT 
  OVERRIDES
    init := InitBeh;
  END;


PROCEDURE InitBeh (self : Beh) : Beh =
  BEGIN
    RETURN self;
  END InitBeh;


(*****************************************************************************)
(* Type "ConstBeh"                                                           *)
(*****************************************************************************)

REVEAL
  ConstBeh = PublicConstBeh BRANDED OBJECT
    p : Base;
  OVERRIDES
    init   := InitConstBeh;
    set    := SetConstBeh;
    value  := ValueConstBeh;
  END;


PROCEDURE InitConstBeh (self : ConstBeh; p : Base) : ConstBeh =
  BEGIN
    EVAL Beh.init (self);
    self.p := p;
    IF ConstBehPM # NIL THEN
      ConstBehPM (self);
    END;
    RETURN self;
  END InitConstBeh;


PROCEDURE SetConstBeh (self : ConstBeh; p : Base) =
  BEGIN
    self.p := p;
  END SetConstBeh;


PROCEDURE ValueConstBeh (             self : ConstBeh; 
                         <* UNUSED *> time : LONGREAL) : Base =
  BEGIN
    RETURN self.p;
  END ValueConstBeh;


PROCEDURE NewConst (p : Base) : Val =
  BEGIN
    RETURN NEW (Val).init (NEW (ConstBeh).init (p));
  END NewConst;


(*****************************************************************************)
(* Type "AsyncBeh"                                                           *)
(*****************************************************************************)

REVEAL
  AsyncBeh = PublicAsyncBeh BRANDED OBJECT
  OVERRIDES
    init    := InitAsyncBeh;
    value   := ValueAsyncBeh;
    compute := ComputeAsyncBeh;
  END;


PROCEDURE InitAsyncBeh (self : AsyncBeh) : AsyncBeh =
  BEGIN
    EVAL Beh.init (self);
    IF AsyncBehPM # NIL THEN
      AsyncBehPM (self);
    END;
    RETURN self;
  END InitAsyncBeh;


PROCEDURE ValueAsyncBeh (self : AsyncBeh; time : LONGREAL) : Base 
    RAISES {Prop.BadMethod} =
  BEGIN
    RETURN self.compute (time);
  END ValueAsyncBeh;


PROCEDURE ComputeAsyncBeh (self : AsyncBeh; time : LONGREAL) : Base 
    RAISES {Prop.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, AsyncBehProxy).compute (time);
    ELSE
      RAISE Prop.BadMethod("RealProp.AsyncBeh.compute method is undefined");
    END;
  END ComputeAsyncBeh;
    

PROCEDURE NewAsync (b : AsyncBeh) : Val =
  BEGIN
    RETURN NEW (Val).init (b);
  END NewAsync;


(*****************************************************************************)
(* Type "DepBeh"                                                             *)
(*****************************************************************************)

REVEAL
  DepBeh = PublicDepBeh BRANDED OBJECT
    hot : BOOLEAN;
  OVERRIDES
    init    := InitDepBeh;
    value   := ValueDepBeh;
    compute := ComputeDepBeh;
  END;


PROCEDURE InitDepBeh (self : DepBeh) : DepBeh =
  BEGIN
    EVAL Beh.init (self);
    self.hot := FALSE;
    IF DepBehPM # NIL THEN
      DepBehPM (self);
    END;
    RETURN self;
  END InitDepBeh;


PROCEDURE ValueDepBeh (self : DepBeh; time : LONGREAL) : Base 
    RAISES {Prop.BadMethod} =
  BEGIN
    (* "hot" is set to true while the value of the behavior is computed. 
       So, if "hot" is currently true, we have cyclic dependencies. 
       If unchecked, this would lead to an infinite recursion. 
       We raise an exception instead. *)
    IF self.hot THEN
      RAISE Prop.BadMethod("RealProp.DepBeh occurs in a dependency cycle");
    END;

    TRY
      self.hot := TRUE;
      RETURN self.compute (time);
    FINALLY
      self.hot := FALSE;
    END;
  END ValueDepBeh;


PROCEDURE ComputeDepBeh (self : DepBeh; time : LONGREAL) : Base 
    RAISES {Prop.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, DepBehProxy).compute (time);
    ELSE
      RAISE Prop.BadMethod("RealProp.DepBeh.compute method is undefined");
    END;
  END ComputeDepBeh;


PROCEDURE NewDep (b : DepBeh) : Val =
  BEGIN
    RETURN NEW (Val).init (b);
  END NewDep;


(*****************************************************************************)
(* Type "SyncBeh"                                                            *)
(*****************************************************************************)

REVEAL 
  SyncBeh = PublicSyncBeh BRANDED OBJECT
    queue : MyAnimRequestQueue;
  OVERRIDES
    init        := InitSyncBeh;
    value       := ValueSyncBeh;
    addRequest  := AddRequest;
    linChangeTo := LinChangeTo;
    linChangeBy := LinChangeBy;
  END;


PROCEDURE InitSyncBeh (self : SyncBeh; 
                       ah   : AnimHandle.T;
                       p    : Base) : SyncBeh = 
  BEGIN
    EVAL Beh.init (self);
    self.queue := NEW (MyAnimRequestQueue).init (ah, p);

    IF SyncBehPM # NIL THEN
      SyncBehPM (self);
    END;
    RETURN self;
  END InitSyncBeh;


PROCEDURE ValueSyncBeh (self : SyncBeh; time : LONGREAL) : Base 
    RAISES {Prop.BadMethod} =
  BEGIN
    RETURN self.queue.value (time);
  END ValueSyncBeh;


PROCEDURE AddRequest (self : SyncBeh; r : Request) RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (r);
  END AddRequest;


PROCEDURE LinChangeTo (self : SyncBeh; p : Base; start : REAL; dur : REAL) 
    RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (LinChangeToReq).init (start, dur, p));
  END LinChangeTo;


PROCEDURE LinChangeBy (self : SyncBeh; p : Base; start : REAL; dur : REAL) 
    RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (LinChangeByReq).init (start, dur, p));
  END LinChangeBy;


PROCEDURE NewSync (ah : AnimHandle.T; p : Base) : Val =
  BEGIN
    RETURN NEW (Val).init (NEW (SyncBeh).init (ah, p));
  END NewSync;


(*****************************************************************************)
(* Request Subtypes                                                          *)
(*****************************************************************************)


REVEAL 
  Request = PublicRequest BRANDED OBJECT 
  OVERRIDES
    init  := InitRequest;
    value := ValueRequest;
  END;


PROCEDURE InitRequest (self : Request; start, dur : REAL) : Request =
  BEGIN
    EVAL Prop.Request.init (self, start, dur);
    IF RequestPM # NIL THEN
      RequestPM (self);
    END;
    RETURN self;
  END InitRequest;


PROCEDURE ValueRequest (self     : Request; 
                        startval : Base; 
                        reltime  : REAL) : Base RAISES {Prop.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, RequestProxy).value (startval, reltime);
    ELSE
      RAISE Prop.BadMethod("BooleanProp.Request.value method is undefined");
    END;
  END ValueRequest;


TYPE 
  LinChangeToReq = Request BRANDED OBJECT
    p : Base;
  METHODS
    init (start, dur : REAL; val : Base) : LinChangeToReq 
      := LinChangeToReqInit;
  OVERRIDES 
    value := LinChangeToReqValue;
  END;


PROCEDURE LinChangeToReqInit (self       : LinChangeToReq; 
                              start, dur : REAL;
                              val        : Base) : LinChangeToReq =
  BEGIN
    EVAL Request.init (self, start, dur);
    self.p := val;
    RETURN self;
  END LinChangeToReqInit;


PROCEDURE LinChangeToReqValue (self     : LinChangeToReq; 
                               startval : Base; 
                               reltime  : REAL) : Base =
  VAR
    fraction : REAL;
  BEGIN
    IF self.dur # 0.0 THEN
      fraction := (reltime - self.start) / self.dur;
    ELSE
      fraction := 1.0;
    END;
    RETURN startval + (self.p - startval) * fraction; 
  END LinChangeToReqValue;


TYPE 
  LinChangeByReq = Request BRANDED OBJECT
    p : Base;
  METHODS
    init (start, dur : REAL; val : Base) : LinChangeByReq 
      := LinChangeByReqInit;
  OVERRIDES 
    value := LinChangeByReqValue;
  END;


PROCEDURE LinChangeByReqInit (self       : LinChangeByReq; 
                              start, dur : REAL;
                              val        : Base) : LinChangeByReq =
  BEGIN
    EVAL Request.init (self, start, dur);
    self.p := val;
    RETURN self;
  END LinChangeByReqInit;


PROCEDURE LinChangeByReqValue (self     : LinChangeByReq; 
                               startval : Base; 
                               reltime  : REAL) : Base =
  VAR
    fraction : REAL;
  BEGIN
    IF self.dur # 0.0 THEN
      fraction := (reltime - self.start) / self.dur;
    ELSE
      fraction := 1.0;
    END;
    RETURN startval + self.p * fraction;
  END LinChangeByReqValue;


(*****************************************************************************)
(* Animation queue for synchronous real property value behavior             *)
(*****************************************************************************)


TYPE 
  MyAnimRequestQueue = AnimRequestQueue.T BRANDED OBJECT
    p : Base;  (* The initial value of the pv *)
  METHODS
    init (ah : AnimHandle.T; p : Base) : MyAnimRequestQueue 
      := MyAnimRequestQueue_Init;
    value (time : LONGREAL) : Base RAISES {Prop.BadMethod}
      := MyAnimRequestQueue_Value;
  OVERRIDES
    flush := MyAnimRequestQueue_Flush;
  END;


PROCEDURE MyAnimRequestQueue_Init (self : MyAnimRequestQueue; 
                                   ah   : AnimHandle.T;
                                   p    : Base) : MyAnimRequestQueue =
  BEGIN
    EVAL AnimRequestQueue.T.init (self, ah);
    self.p := p;
    RETURN self;
  END MyAnimRequestQueue_Init;


PROCEDURE MyAnimRequestQueue_Value (self : MyAnimRequestQueue; 
                                    time : LONGREAL) : Base 
                                    RAISES {Prop.BadMethod} = 
  VAR
    l       := self.list;
    req     : Request;
    p       : Base;
    reltime : REAL;
  BEGIN
    IF self.ah.activated THEN
      reltime := FLOAT (time - self.ah.starttime);
      p := self.p;
      WHILE l # NIL DO
        req := l.req;
        IF reltime < req.start  THEN
          RETURN p;
        ELSIF reltime < req.start + req.dur THEN
          RETURN req.value (p, reltime);
        ELSE
          p := req.value (p, req.start + req.dur);
          l := l.next;
        END;
      END;
      RETURN p;
    ELSE
      RETURN self.p;
    END;
  END MyAnimRequestQueue_Value;


PROCEDURE MyAnimRequestQueue_Flush (self : MyAnimRequestQueue) =
  VAR
    req : Request;
  BEGIN
    WHILE self.list # NIL DO
      req := self.list.req;
      TRY
        self.p := req.value (self.p, req.start + req.dur);
      EXCEPT
        Prop.BadMethod (msg) => AnimServer.ReportError (msg);
      END;  
      self.list := self.list.next;
    END;
  END MyAnimRequestQueue_Flush;


(*****************************************************************************)
(* Stack                                                                     *)
(*****************************************************************************)

REVEAL
  Stack = PublicStack BRANDED OBJECT
    cnt  : INTEGER;
    vals : REF ARRAY OF Base;
  METHODS
    init (top : Base) : Stack := InitStack;
  OVERRIDES
    push := PushStack;
    pop  := PopStack;
  END;


PROCEDURE InitStack (self : Stack; top : Base) : Stack =
  BEGIN
    self.cnt  := 0;
    self.vals := NEW (REF ARRAY OF Base, 10);
    self.top  := top;
    RETURN self;
  END InitStack;


PROCEDURE PushStack (self : Stack; val : Base) =
  BEGIN
    IF self.cnt >= LAST (self.vals^) THEN
      WITH tmp = NEW (REF ARRAY OF Base, 2 * NUMBER (self.vals^)) DO
        SUBARRAY (tmp^, 0, NUMBER (self.vals^)) := self.vals^;
        self.vals := tmp;
      END;
    END;
    self.vals[self.cnt] := self.top;
    self.top := val;
    INC (self.cnt);
  END PushStack;


PROCEDURE PopStack (self : Stack) : Base =
  BEGIN
    DEC (self.cnt);
    self.top := self.vals[self.cnt];
    RETURN self.top;
  END PopStack;    


BEGIN
END RealProp.
