(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jan 31 00:07:17 PST 1995 by najork                   *)
(*       Created on Sun May 22 12:22:36 PDT 1994 by najork                   *)


MODULE TransformProp EXPORTS TransformProp, 
                             TransformPropPrivate, 
                             TransformPropProxy;

IMPORT Anim3D, AnimHandle, AnimHandlePrivate, AnimRequestQueue, 
       AnimRequestQueuePrivate, AnimServer, GraphicsBase, 
       GraphicsBasePrivate, Matrix4, Prop, PropPrivate, Quaternion;

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


PROCEDURE InitName (self : Name; READONLY default : Base) : Name =
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
    init   := InitVal;
    get    := GetVal;
    value  := ValueVal;
    adjust := AdjustVal;
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


PROCEDURE GetVal (self : Val) : Matrix4.T RAISES {Prop.BadMethod} =
  BEGIN
    RETURN self.beh.value (Anim3D.Now ());
  END GetVal;


PROCEDURE ValueVal (self : Val; time : LONGREAL) : Matrix4.T
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
    matrix : Matrix4.T;
  OVERRIDES
    init      := InitConstBeh;
    value     := ValueConstBeh;
    set       := SetConstBeh;
    compose   := ComposeConstBeh;
    reset     := ResetConstBeh;
    translate := TranslateConstBeh;
    scale     := ScaleConstBeh;
    rotateX   := RotateXConstBeh;
    rotateY   := RotateYConstBeh;
    rotateZ   := RotateZConstBeh;
  END;


PROCEDURE InitConstBeh (         self : ConstBeh; 
                        READONLY matrix : Matrix4.T) : ConstBeh =
  BEGIN
    EVAL Beh.init (self);
    self.matrix := matrix;
    IF ConstBehPM # NIL THEN
      ConstBehPM (self);
    END;
    RETURN self;
  END InitConstBeh;


PROCEDURE ValueConstBeh (             self : ConstBeh; 
                         <* UNUSED *> time : LONGREAL) : Matrix4.T =
  BEGIN
    RETURN self.matrix;
  END ValueConstBeh;


PROCEDURE SetConstBeh (self : ConstBeh; READONLY matrix : Matrix4.T) =
  BEGIN
    self.matrix := matrix;
  END SetConstBeh;


PROCEDURE ComposeConstBeh (self : ConstBeh; READONLY matrix : Matrix4.T) =
  BEGIN
    self.matrix := Matrix4.Multiply (matrix, self.matrix);
  END ComposeConstBeh;


PROCEDURE ResetConstBeh (self : ConstBeh) =
  BEGIN
    self.matrix := Matrix4.Id;
  END ResetConstBeh;


PROCEDURE TranslateConstBeh (self : ConstBeh; x, y, z : REAL) =
  BEGIN
    self.matrix := Matrix4.Translate (self.matrix, x, y, z);
  END TranslateConstBeh;


PROCEDURE ScaleConstBeh (self : ConstBeh; x, y, z : REAL) =
  BEGIN
    self.matrix := Matrix4.Scale (self.matrix, x, y, z);
  END ScaleConstBeh;


PROCEDURE RotateXConstBeh (self : ConstBeh; a : REAL) = 
  BEGIN
    self.matrix := Matrix4.RotateX (self.matrix, a);
  END RotateXConstBeh;


PROCEDURE RotateYConstBeh (self : ConstBeh; a : REAL) = 
  BEGIN
    self.matrix := Matrix4.RotateY (self.matrix, a);
  END RotateYConstBeh;


PROCEDURE RotateZConstBeh (self : ConstBeh; a : REAL) = 
  BEGIN
    self.matrix := Matrix4.RotateZ (self.matrix, a);
  END RotateZConstBeh;


PROCEDURE NewConst (READONLY matrix : Matrix4.T) : Val =
  BEGIN
    RETURN NEW (Val).init (NEW (ConstBeh).init (matrix));
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


PROCEDURE ValueAsyncBeh (self : AsyncBeh; time : LONGREAL) : Matrix4.T 
    RAISES {Prop.BadMethod} =
  BEGIN
    RETURN self.compute (time);
  END ValueAsyncBeh;


PROCEDURE ComputeAsyncBeh (self : AsyncBeh; time : LONGREAL) : Matrix4.T 
    RAISES {Prop.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, AsyncBehProxy).compute (time);
    ELSE
      RAISE Prop.BadMethod("TransformProp.AsyncBeh.compute method is undefined");
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
    hot : BOOLEAN := FALSE;
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


PROCEDURE ValueDepBeh (self : DepBeh; time : LONGREAL) : Matrix4.T 
    RAISES {Prop.BadMethod} =
  BEGIN
    (* "hot" is set to true while the value of the behavior is computed. 
       So, if "hot" is currently true, we have cyclic dependencies. 
       If unchecked, this would lead to an infinite recursion. 
       We raise an exception instead. *)
    IF self.hot THEN
      RAISE Prop.BadMethod("TransformProp.DepBeh occurs in a dependency cycle");
    END;

    TRY
      self.hot := TRUE;
      RETURN self.compute (time);
    FINALLY
      self.hot := FALSE;
    END;
  END ValueDepBeh;


PROCEDURE ComputeDepBeh (self : DepBeh; time : LONGREAL) : Matrix4.T 
    RAISES {Prop.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, DepBehProxy).compute (time);
    ELSE
      RAISE Prop.BadMethod("TransformProp.DepBeh.compute method is undefined");
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
    queue  : MyAnimRequestQueue;
  OVERRIDES
    init       := InitSyncBeh;
    value      := ValueSyncBeh;
    addRequest := AddRequest;
    reset      := ResetSyncBeh;
    changeTo   := ChangeToSyncBeh;
    translate  := TranslateSyncBeh;
    scale      := ScaleSyncBeh;
    rotateX    := RotateXSyncBeh;
    rotateY    := RotateYSyncBeh;
    rotateZ    := RotateZSyncBeh;
  END;


PROCEDURE InitSyncBeh (self            : SyncBeh; 
                       ah              : AnimHandle.T;
                       READONLY matrix : Matrix4.T) : SyncBeh = 
  BEGIN
    EVAL Beh.init (self);
    self.queue  := NEW (MyAnimRequestQueue).init (ah, matrix);

    IF SyncBehPM # NIL THEN
      SyncBehPM (self);
    END;
    RETURN self;
  END InitSyncBeh;


PROCEDURE ValueSyncBeh (self : SyncBeh; time : LONGREAL) : Matrix4.T 
    RAISES {Prop.BadMethod}=
  BEGIN
    RETURN self.queue.value (time);
  END ValueSyncBeh;


PROCEDURE AddRequest (self : SyncBeh; r : Request) RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (r);
  END AddRequest;


PROCEDURE ResetSyncBeh (self : SyncBeh; start : REAL) 
    RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (ResetReq).init (start, 0.0));
  END ResetSyncBeh;


PROCEDURE ChangeToSyncBeh (self       : SyncBeh; 
                           READONLY m : Matrix4.T; 
                           start, dur : REAL) 
    RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (ChangeToReq).init (start, dur, m));
  END ChangeToSyncBeh;


PROCEDURE TranslateSyncBeh (self : SyncBeh; x, y, z : REAL; start, dur : REAL) 
        RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (TranslateReq).init (start, dur, x, y, z));
  END TranslateSyncBeh;


PROCEDURE ScaleSyncBeh (self : SyncBeh; x, y, z : REAL; start, dur : REAL) 
        RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (ScaleReq).init (start, dur, x, y, z));
  END ScaleSyncBeh;


PROCEDURE RotateXSyncBeh (self : SyncBeh; a : REAL; start, dur : REAL) 
        RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (RotateXReq).init (start, dur, a));
  END RotateXSyncBeh;


PROCEDURE RotateYSyncBeh (self : SyncBeh; a : REAL; start, dur : REAL) 
        RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (RotateYReq).init (start, dur, a));
  END RotateYSyncBeh;


PROCEDURE RotateZSyncBeh (self : SyncBeh; a : REAL; start, dur : REAL) 
        RAISES {Prop.BadInterval} =
  BEGIN
    self.queue.insert (NEW (RotateZReq).init (start, dur, a));
  END RotateZSyncBeh;


PROCEDURE NewSync (ah : AnimHandle.T; READONLY matrix : Matrix4.T) : Val =
  BEGIN
    RETURN NEW (Val).init (NEW (SyncBeh).init (ah, matrix));
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


PROCEDURE ValueRequest (self              : Request; 
                        READONLY startval : Matrix4.T; 
                        reltime           : REAL) : Matrix4.T 
    RAISES {Prop.BadMethod} =
  BEGIN
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, RequestProxy).value (startval, reltime);
    ELSE
      RAISE Prop.BadMethod("TransformProp.Request.value method is undefined");
    END;
  END ValueRequest;


TYPE 
  ResetReq = Request BRANDED OBJECT
  METHODS
    init (start, dur : REAL) : ResetReq := ResetReqInit;
  OVERRIDES 
    value := ResetReqValue;
  END;


PROCEDURE ResetReqInit (self : ResetReq; start, dur : REAL) : ResetReq =
  BEGIN
    EVAL Request.init (self, start, dur);
    RETURN self;
  END ResetReqInit;


PROCEDURE ResetReqValue (                      self     : ResetReq; 
                         <* UNUSED *> READONLY startval : Matrix4.T;
                                               reltime  : REAL) : Matrix4.T =
  BEGIN
    <* ASSERT reltime >= self.start AND reltime <= self.start + self.dur *>
    RETURN Matrix4.Id;
  END ResetReqValue;

(*
TYPE 
  ChangeToReq = Request BRANDED OBJECT
    validcache := FALSE;
    tx0, ty0, tz0, s0, ax0, ay0, az0 : REAL;
    tx1, ty1, tz1, s1, ax1, ay1, az1 : REAL;
    delta_ax, delta_ay, delta_az : REAL;
    m : Matrix4.T;
  OVERRIDES
    value := ChangeToReqValue;
  END;


PROCEDURE ChangeToReqValue (self              : ChangeToReq; 
                            READONLY startval : Matrix4.T;
                            reltime           : REAL) : Matrix4.T =

  PROCEDURE NormalizeAngle (a : REAL) : REAL =
    BEGIN
      WHILE a > Math.Pi DO
        a := a - 2.0 * Math.Pi;
      END;
      WHILE a < -Math.Pi DO
        a := a + 2.0 * Math.Pi;
      END;
      RETURN a;
    END NormalizeAngle;

  VAR
    f : REAL;
    M := Matrix4.Id;
  BEGIN
    IF NOT self.validcache THEN
      self.validcache := TRUE;
      Matrix4.Decompose (startval, self.tx0, self.ty0, self.tz0, self.s0, 
                         self.ax0, self.ay0, self.az0);
      Matrix4.Decompose (self.m, self.tx1, self.ty1, self.tz1, self.s1, 
                         self.ax1, self.ay1, self.az1);
      self.delta_ax := NormalizeAngle (self.ax1 - self.ax0);
      self.delta_ay := NormalizeAngle (self.ay1 - self.ay0);
      self.delta_az := NormalizeAngle (self.az1 - self.az0);
    END;

    IF self.dur # 0.0 THEN
      f := (reltime - self.start) / self.dur;
    ELSE
      f := 1.0;
    END;

    WITH tx = self.tx0 + (self.tx1 - self.tx0) * f,
         ty = self.ty0 + (self.ty1 - self.ty0) * f,
         tz = self.tz0 + (self.tz1 - self.tz0) * f,
         s  = self.s0  + (self.s1  - self.s0 ) * f,
         ax = self.ax0 + self.delta_ax * f,
         ay = self.ay0 + self.delta_ay * f,
         az = self.az0 + self.delta_az * f DO
      M := Matrix4.Scale (M, s, s, s);
      M := Matrix4.RotateX (M, ax);
      M := Matrix4.RotateY (M, ay);
      M := Matrix4.RotateZ (M, az);
      M := Matrix4.Translate (M, tx, ty, tz);
    END;
    RETURN M;
  END ChangeToReqValue;
*)

TYPE 
  ChangeToReq = Request BRANDED OBJECT
    m : Matrix4.T;
  (*** the rest gets filled in upon the first call to "self.value" ***)
    validcache := FALSE;
    tx0, ty0, tz0, s0 : REAL;
    tx1, ty1, tz1, s1 : REAL;
    q : Quaternion.T;
    A : Matrix4.T;
  METHODS
    init (start, dur : REAL; READONLY m : Matrix4.T) : ChangeToReq := 
        ChangeToReqInit;
  OVERRIDES
    value := ChangeToReqValue;
  END;


PROCEDURE ChangeToReqInit (self         : ChangeToReq; 
                           start, dur   : REAL;
                           READONLY val : Matrix4.T) : ChangeToReq =
  BEGIN
    EVAL Request.init (self, start, dur);
    self.m := val;
    RETURN self;
  END ChangeToReqInit;


PROCEDURE ChangeToReqValue (self              : ChangeToReq; 
                            READONLY startval : Matrix4.T;
                            reltime           : REAL) : Matrix4.T =
  VAR
    f : REAL;
    M := Matrix4.Id;
  BEGIN
    TRY
      IF NOT self.validcache THEN
        WITH A = Matrix4.Decomp(startval,self.tx0,self.ty0,self.tz0,self.s0),
             B = Matrix4.Decomp(self.m,  self.tx1,self.ty1,self.tz1,self.s1),
                (* We use the fact that "A" is an orthogonal matrix, 
                   and that for orthogonal matrices, inverse and transpose 
                   are the same. *)
             R = Matrix4.Multiply (B, Matrix4.Transpose (A)) DO
          self.A := A;
          self.q := Quaternion.FromMatrix4 (R);
          self.validcache := TRUE;
        END;
      END;

      IF  self.dur # 0.0 THEN
        f := (reltime - self.start) / self.dur;
      ELSE
        f := 1.0;
      END;
    
      WITH tx = self.tx0 + (self.tx1 - self.tx0) * f,
           ty = self.ty0 + (self.ty1 - self.ty0) * f,
           tz = self.tz0 + (self.tz1 - self.tz0) * f,
           s  = self.s0  + (self.s1  - self.s0 ) * f,
           R  = Quaternion.ToMatrix4 (Quaternion.Interpolate (self.q, f)) DO
        M := Matrix4.Multiply (R, self.A);
        M := Matrix4.Scale (M, s, s, s);
        M := Matrix4.Translate (M, tx, ty, tz);
      END;
      RETURN M;
    EXCEPT
    | Matrix4.Error =>
      AnimServer.ReportError (
          "TransformProp.SyncBeh.change involves nonuniformly scaled matrix");
      RETURN startval;
    END;
  END ChangeToReqValue;


TYPE 
  TranslateReq = Request BRANDED OBJECT
    x, y, z : REAL;
  METHODS 
    init (start, dur : REAL; x, y, z : REAL) : TranslateReq := 
        TranslateReqInit;
  OVERRIDES
    value := TranslateReqValue;
  END;


PROCEDURE TranslateReqInit (self       : TranslateReq;
                            start, dur : REAL;
                            x, y, z    : REAL) : TranslateReq =
  BEGIN
    EVAL Request.init (self, start, dur);
    self.x := x;
    self.y := y;
    self.z := z;
    RETURN self;
  END TranslateReqInit;


PROCEDURE TranslateReqValue (self              : TranslateReq; 
                             READONLY startval : Matrix4.T;
                             reltime           : REAL) : Matrix4.T =
  VAR
    f : REAL;
  BEGIN
    IF self.dur # 0.0 THEN
      f := (reltime - self.start) / self.dur;
    ELSE
      f := 1.0;
    END;
    RETURN Matrix4.Translate (startval, self.x * f, self.y * f, self.z * f);
  END TranslateReqValue;


TYPE 
  ScaleReq = Request BRANDED OBJECT
    x, y, z : REAL;
  METHODS 
    init (start, dur : REAL; x, y, z : REAL) : ScaleReq := ScaleReqInit;
  OVERRIDES
    value := ScaleReqValue;
  END;


PROCEDURE ScaleReqInit (self       : ScaleReq;
                        start, dur : REAL;
                        x, y, z    : REAL) : ScaleReq =
  BEGIN
    EVAL Request.init (self, start, dur);
    self.x := x;
    self.y := y;
    self.z := z;
    RETURN self;
  END ScaleReqInit;


PROCEDURE ScaleReqValue (self              : ScaleReq; 
                         READONLY startval : Matrix4.T;
                         reltime           : REAL) : Matrix4.T =
  VAR
    f : REAL;
  BEGIN
    IF self.dur # 0.0 THEN
      f := (reltime - self.start) / self.dur;
    ELSE
      f := 1.0;
    END;
    RETURN Matrix4.Scale (startval, 1.0 + (self.x - 1.0) * f, 
                                    1.0 + (self.y - 1.0) * f, 
                                    1.0 + (self.z - 1.0) * f);
  END ScaleReqValue;


TYPE 
  RotateRequest = Request BRANDED OBJECT
    a : REAL;
  METHODS 
    init (start, dur : REAL; a : REAL) : RotateRequest := RotateRequestInit;
  END;


PROCEDURE RotateRequestInit (self       : RotateRequest;
                             start, dur : REAL;
                             a          : REAL) : RotateRequest =
  BEGIN
    EVAL Request.init (self, start, dur);
    self.a := a;
    RETURN self;
  END RotateRequestInit;


TYPE 
  RotateXReq = RotateRequest BRANDED OBJECT
  OVERRIDES
    value := RotateXReqValue;
  END;


PROCEDURE RotateXReqValue (self              : RotateXReq; 
                           READONLY startval : Matrix4.T;
                           reltime           : REAL) : Matrix4.T =
  VAR
    f : REAL;
  BEGIN
    IF self.dur # 0.0 THEN
      f := (reltime - self.start) / self.dur;
    ELSE
      f := 1.0;
    END;
    RETURN Matrix4.RotateX (startval, self.a * f);
  END RotateXReqValue;


TYPE 
  RotateYReq = RotateRequest BRANDED OBJECT
  OVERRIDES
    value := RotateYReqValue;
  END;


PROCEDURE RotateYReqValue (self              : RotateYReq;
                           READONLY startval : Matrix4.T;
                           reltime           : REAL) : Matrix4.T =
  VAR
    f : REAL;
  BEGIN
    IF self.dur # 0.0 THEN
      f := (reltime - self.start) / self.dur;
    ELSE
      f := 1.0;
    END;
    RETURN Matrix4.RotateY (startval, self.a * f);
  END RotateYReqValue;


TYPE 
  RotateZReq = RotateRequest BRANDED OBJECT
  OVERRIDES
    value := RotateZReqValue;
  END;


PROCEDURE RotateZReqValue (self              : RotateZReq;
                           READONLY startval : Matrix4.T;
                           reltime           : REAL) : Matrix4.T =
  VAR
    f : REAL;
  BEGIN
    IF self.dur # 0.0 THEN
      f := (reltime - self.start) / self.dur;
    ELSE
      f := 1.0;
    END;
    RETURN Matrix4.RotateZ (startval, self.a * f);
  END RotateZReqValue;


(*****************************************************************************)
(* Animation queue for synchronous point property value behavior             *)
(*****************************************************************************)


TYPE 
  MyAnimRequestQueue = AnimRequestQueue.T BRANDED OBJECT
    matrix : Matrix4.T;  (* The initial value of the pv *)
  METHODS
    init (ah : AnimHandle.T; READONLY matrix : Matrix4.T) : MyAnimRequestQueue 
      := MyAnimRequestQueue_Init;
    value (time : LONGREAL) : Matrix4.T RAISES {Prop.BadMethod}
      := MyAnimRequestQueue_Value;
  OVERRIDES
    flush := MyAnimRequestQueue_Flush;
  END;


PROCEDURE MyAnimRequestQueue_Init (self            : MyAnimRequestQueue; 
                                   ah              : AnimHandle.T;
                                   READONLY matrix : Matrix4.T) : MyAnimRequestQueue =
  BEGIN
    EVAL AnimRequestQueue.T.init (self, ah);
    self.matrix := matrix;
    RETURN self;
  END MyAnimRequestQueue_Init;


PROCEDURE MyAnimRequestQueue_Value (self : MyAnimRequestQueue; 
                                    time : LONGREAL) : Matrix4.T
    RAISES {Prop.BadMethod} = 
  VAR
    l       := self.list;
    req     : Request;
    matrix  : Matrix4.T;
    reltime : REAL;
  BEGIN
    IF self.ah.activated THEN
      reltime := FLOAT (time - self.ah.starttime);
      matrix := self.matrix;
      WHILE l # NIL DO
        req := l.req;
        IF reltime < req.start  THEN
          RETURN matrix;
        ELSIF reltime < req.start + req.dur THEN
          RETURN req.value (matrix, reltime);
        ELSE
          matrix := req.value (matrix, req.start + req.dur);
          l := l.next;
        END;
      END;
      RETURN matrix;
    ELSE
      RETURN self.matrix;
    END;
  END MyAnimRequestQueue_Value;


PROCEDURE MyAnimRequestQueue_Flush (self : MyAnimRequestQueue) =
  VAR
    req : Request;
  BEGIN
    WHILE self.list # NIL DO
      req := self.list.req;
      TRY
        self.matrix := req.value (self.matrix, req.start + req.dur);
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
    init (READONLY top : Base) : Stack := InitStack;
  OVERRIDES
    push := PushStack;
    pop  := PopStack;
  END;


PROCEDURE InitStack (self : Stack; READONLY top : Base) : Stack =
  BEGIN
    self.cnt  := 0;
    self.vals := NEW (REF ARRAY OF Base, 10);
    self.top  := top;
    RETURN self;
  END InitStack;


PROCEDURE PushStack (self : Stack; READONLY val : Base) =
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
END TransformProp.
