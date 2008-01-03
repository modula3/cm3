(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Jul 27 14:15:35 PDT 1994 by najork                   *)
(*       Created on Sun May 22 11:14:01 PDT 1994 by najork                   *)


INTERFACE TransformProp;

IMPORT AnimHandle, Matrix4, Prop;

TYPE Base = Matrix4.T;

TYPE
  Name <: PublicName;
  PublicName = Prop.Name OBJECT
  METHODS
    bind (v : Val) : Prop.T;
  END;

  Val <: PublicVal;
  PublicVal = Prop.Val OBJECT
    beh : Beh;
  METHODS
    init (beh : Beh) : Val;
    get () : Base RAISES {Prop.BadMethod};
    value (time : LONGREAL) : Base RAISES {Prop.BadMethod};
  END;

  Beh <: PublicBeh;
  PublicBeh = Prop.Beh OBJECT 
  METHODS
    init () : Beh;
  END;

  ConstBeh <: PublicConstBeh;
  PublicConstBeh = Beh OBJECT
  METHODS
    init (READONLY m := Matrix4.Id) : ConstBeh;
    set (READONLY m : Base);
    compose (READONLY m : Base);
  (* shortcuts for particular cases of "set" and "compose" *)
    reset ();
    translate (x, y, z : REAL);
    scale (x, y, z : REAL);
    rotateX (a : REAL);
    rotateY (a : REAL);
    rotateZ (a : REAL);
  END;

  SyncBeh <: PublicSyncBeh;
  PublicSyncBeh = Beh OBJECT
  METHODS
    init (ah : AnimHandle.T; READONLY m := Matrix4.Id) : SyncBeh;
    addRequest (r : Request) RAISES {Prop.BadInterval};
  (* shortcuts for particular instances of "addRequest" *)
    reset (start := 0.0) RAISES {Prop.BadInterval};
    changeTo (READONLY m : Base; start := 0.0; dur := 0.0) 
        RAISES {Prop.BadInterval};
    translate (x, y, z : REAL; start := 0.0; dur := 0.0) 
        RAISES {Prop.BadInterval};
    scale (x, y, z : REAL; start := 0.0; dur := 0.0) RAISES {Prop.BadInterval};
    rotateX (a : REAL; start := 0.0; dur := 0.0) RAISES {Prop.BadInterval};
    rotateY (a : REAL; start := 0.0; dur := 0.0) RAISES {Prop.BadInterval};
    rotateZ (a : REAL; start := 0.0; dur := 0.0) RAISES {Prop.BadInterval};
  END;

  AsyncBeh <: PublicAsyncBeh;
  PublicAsyncBeh = Beh OBJECT
  METHODS
    init () : AsyncBeh;
    compute (time : LONGREAL) : Base RAISES {Prop.BadMethod};
  END;

  DepBeh <: PublicDepBeh;
  PublicDepBeh = Beh OBJECT
  METHODS
    init () : DepBeh;
    compute (time : LONGREAL) : Base RAISES {Prop.BadMethod};
  END;

  Request <: PublicRequest;
  PublicRequest = Prop.Request OBJECT 
  METHODS
    init (start, dur : REAL) : Request;
    value (READONLY startval : Base; reltime : REAL) : Base
      RAISES {Prop.BadMethod};
  END;

PROCEDURE NewConst (READONLY m := Matrix4.Id) : Val;
PROCEDURE NewSync (ah : AnimHandle.T; READONLY m := Matrix4.Id) : Val;
PROCEDURE NewAsync (b : AsyncBeh) : Val;
PROCEDURE NewDep (b : DepBeh) : Val;

END TransformProp.
