(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jan 31 00:06:56 PST 1995 by najork                   *)
(*       Created on Sun May 22 11:34:38 PDT 1994 by najork                   *)


MODULE MarkerTypeProp EXPORTS MarkerTypeProp, 
                              MarkerTypePropPrivate,
                              MarkerTypePropProxy;

IMPORT Anim3D, AnimHandle, AnimHandlePrivate, AnimRequestQueue, 
       AnimRequestQueuePrivate, AnimServer, 
       GraphicsBase, GraphicsBasePrivate, Prop, PropPrivate;

(*****************************************************************************)
(* Type "Name"                                                               *)
(*****************************************************************************)

REVEAL
  Name = PrivateName BRANDED OBJECT
    default : Kind;
  OVERRIDES
    init      := InitName;
    bind      := BindName;
    makeProxy := MakeProxyName;
    push      := PushName;
    pop       := PopName;
    newStack  := NewStack;
    getState  := GetState;
  END;


PROCEDURE InitName (self : Name; default : Kind) : Name =
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


PROCEDURE GetState (self: Name; state : GraphicsBase.T) : Kind =
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


PROCEDURE GetVal (self : Val) : Kind RAISES {Prop.BadMethod} =
  BEGIN
    RETURN self.beh.value (Anim3D.Now ());
  END GetVal;


PROCEDURE ValueVal (self : Val; time : LONGREAL) : Kind
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
    kind : Kind;
  OVERRIDES
    init   := InitConstBeh;
    set    := SetConstBeh;
    value  := ValueConstBeh;
  END;


PROCEDURE InitConstBeh (self : ConstBeh; kind : Kind) : ConstBeh =
  BEGIN
    EVAL Beh.init (self);
    self.kind := kind;
    IF ConstBehPM # NIL THEN
      ConstBehPM (self);
    END;
    RETURN self;
  END InitConstBeh;


PROCEDURE SetConstBeh (self : ConstBeh; kind : Kind) =
  BEGIN
    self.kind := kind;
  END SetConstBeh;


PROCEDURE ValueConstBeh (             self : ConstBeh; 
                         <* UNUSED *> time : LONGREAL) : Kind =
  BEGIN
    RETURN self.kind;
  END ValueConstBeh;


PROCEDURE NewConst (kind : Kind) : Val =
  BEGIN
    RETURN NEW (Val).init (NEW (ConstBeh).init (kind));
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


PROCEDURE ValueAsyncBeh (self : AsyncBeh; time : LONGREAL) : Kind 
    RAISES {Prop.BadMethod} =
  BEGIN
    RETURN self.compute (time);
  END ValueAsyncBeh;


PROCEDURE ComputeAsyncBeh (self : AsyncBeh; time : LONGREAL) : Kind 
    RAISES {Prop.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, AsyncBehProxy).compute (time);
    ELSE
      RAISE Prop.BadMethod("MarkerTypeProp.AsyncBeh.compute method is undefined");
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


PROCEDURE ValueDepBeh (self : DepBeh; time : LONGREAL) : Kind 
    RAISES {Prop.BadMethod} =
  BEGIN
    (* "hot" is set to true while the value of the behavior is computed. 
       So, if "hot" is currently true, we have cyclic dependencies. 
       If unchecked, this would lead to an infinite recursion. 
       We raise an exception instead. *)
    IF self.hot THEN
      RAISE Prop.BadMethod("MarkerTypeProp.DepBeh occurs in a dependency cycle");
    END;

    TRY
      self.hot := TRUE;
      RETURN self.compute (time);
    FINALLY
      self.hot := FALSE;
    END;
  END ValueDepBeh;


PROCEDURE ComputeDepBeh (self : DepBeh; time : LONGREAL) : Kind 
    RAISES {Prop.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, DepBehProxy).compute (time);
    ELSE
      RAISE Prop.BadMethod("MarkerTypeProp.DepBeh.compute method is undefined");
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
    change      := Change;
  END;


PROCEDURE InitSyncBeh (self : SyncBeh; 
                       ah   : AnimHandle.T;
                       kind : Kind) : SyncBeh = 
  BEGIN
    EVAL Beh.init (self);
    self.queue := NEW (MyAnimRequestQueue).init (ah, kind);

    IF SyncBehPM # NIL THEN
      SyncBehPM (self);
    END;
    RETURN self;
  END InitSyncBeh;


PROCEDURE ValueSyncBeh (self : SyncBeh; time : LONGREAL) : Kind 
    RAISES {Prop.BadMethod} =
  BEGIN
    RETURN self.queue.value (time);
  END ValueSyncBeh;


PROCEDURE AddRequest (self : SyncBeh; r : Request) RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (r);
  END AddRequest;


PROCEDURE Change (self : SyncBeh; kind : Kind; start : REAL) 
    RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (ChangeReq).init (start, 0.0, kind));
  END Change;


PROCEDURE NewSync (ah : AnimHandle.T; kind : Kind) : Val =
  BEGIN
    RETURN NEW (Val).init (NEW (SyncBeh).init (ah, kind));
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
                        startval : Kind; 
                        reltime  : REAL) : Kind RAISES {Prop.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, RequestProxy).value (startval, reltime);
    ELSE
      RAISE Prop.BadMethod("MarkerTypeProp.Request.value method is undefined");
    END;
  END ValueRequest;


TYPE 
  ChangeReq = Request BRANDED OBJECT
    kind : Kind;
  METHODS
    init (start, dur : REAL; val : Kind) : ChangeReq := ChangeReqInit;
  OVERRIDES 
    value := ChangeReqValue;
  END;


PROCEDURE ChangeReqInit (self       : ChangeReq; 
                         start, dur : REAL;
                         val        : Kind) : ChangeReq =
  BEGIN
    EVAL Request.init (self, start, dur);
    self.kind := val;
    RETURN self;
  END ChangeReqInit;


PROCEDURE ChangeReqValue (             self      : ChangeReq; 
                          <* UNUSED *> startkind : Kind; 
                                       reltime   : REAL) : Kind =
  BEGIN
    <* ASSERT reltime >= self.start AND reltime <= self.start + self.dur *>
    RETURN self.kind;
  END ChangeReqValue;


(*****************************************************************************)
(* Animation queue for synchronous marker type property value behavior       *)
(*****************************************************************************)


TYPE 
  MyAnimRequestQueue = AnimRequestQueue.T BRANDED OBJECT
    kind : Kind;  (* The initial value of the pv *)
  METHODS
    init (ah : AnimHandle.T; kind : Kind) : MyAnimRequestQueue 
      := MyAnimRequestQueue_Init;
    value (time : LONGREAL) : Kind RAISES {Prop.BadMethod}
      := MyAnimRequestQueue_Value;
  OVERRIDES
    flush := MyAnimRequestQueue_Flush;
  END;


PROCEDURE MyAnimRequestQueue_Init (self : MyAnimRequestQueue; 
                                   ah   : AnimHandle.T;
                                   kind : Kind) : MyAnimRequestQueue =
  BEGIN
    EVAL AnimRequestQueue.T.init (self, ah);
    self.kind := kind;
    RETURN self;
  END MyAnimRequestQueue_Init;


PROCEDURE MyAnimRequestQueue_Value (self : MyAnimRequestQueue; 
                                    time : LONGREAL) : Kind 
    RAISES {Prop.BadMethod} = 
  VAR
    l       := self.list;
    req     : Request;
    kind    : Kind;
    reltime : REAL;
  BEGIN
    IF self.ah.activated THEN
      reltime := FLOAT (time - self.ah.starttime);
      kind := self.kind;
      WHILE l # NIL DO
        req := l.req;
        IF reltime < req.start  THEN
          RETURN kind;
        ELSIF reltime < req.start + req.dur THEN
          RETURN req.value (kind, reltime);
        ELSE
          kind := req.value (kind, req.start + req.dur);
          l := l.next;
        END;
      END;
      RETURN kind;
    ELSE
      RETURN self.kind;
    END;
  END MyAnimRequestQueue_Value;


PROCEDURE MyAnimRequestQueue_Flush (self : MyAnimRequestQueue) =
  VAR
    req : Request;
  BEGIN
    WHILE self.list # NIL DO
      req := self.list.req;
      TRY
        self.kind := req.value (self.kind, req.start + req.dur);
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
    vals : REF ARRAY OF Kind;
  METHODS
    init (top : Kind) : Stack := InitStack;
  OVERRIDES
    push := PushStack;
    pop  := PopStack;
  END;


PROCEDURE InitStack (self : Stack; top : Kind) : Stack =
  BEGIN
    self.cnt  := 0;
    self.vals := NEW (REF ARRAY OF Kind, 10);
    self.top  := top;
    RETURN self;
  END InitStack;


PROCEDURE PushStack (self : Stack; val : Kind) =
  BEGIN
    IF self.cnt >= LAST (self.vals^) THEN
      WITH tmp = NEW (REF ARRAY OF Kind, 2 * NUMBER (self.vals^)) DO
        SUBARRAY (tmp^, 0, NUMBER (self.vals^)) := self.vals^;
        self.vals := tmp;
      END;
    END;
    self.vals[self.cnt] := self.top;
    self.top := val;
    INC (self.cnt);
  END PushStack;


PROCEDURE PopStack (self : Stack) : Kind =
  BEGIN
    DEC (self.cnt);
    self.top := self.vals[self.cnt];
    RETURN self.top;
  END PopStack;    


BEGIN
END MarkerTypeProp.
