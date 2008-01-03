(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Jul 27 14:14:48 PDT 1994 by najork                   *)
(*       Created on Wed May 18 16:09:57 PDT 1994 by najork                   *)


INTERFACE PointProp;

IMPORT AnimHandle, Point3, Prop;

TYPE Base = Point3.T;

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
    init (p : Base) : ConstBeh;
    set (p : Base);
  END;

  SyncBeh <: PublicSyncBeh;
  PublicSyncBeh = Beh OBJECT
  METHODS
    init (ah : AnimHandle.T; p : Base) : SyncBeh;
    addRequest (r : Request) RAISES {Prop.BadInterval};
  (* shortcuts for particular instances of "addRequest" *)
    linMoveTo (p : Base; start := 0.0; dur := 0.0) 
        RAISES {Prop.BadInterval};
    linMoveBy (p : Base; start := 0.0; dur := 0.0) 
        RAISES {Prop.BadInterval};
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
    value (startpoint : Base; reltime : REAL) : Base 
        RAISES {Prop.BadMethod};
  END;

PROCEDURE NewConst (p : Base) : Val;
PROCEDURE NewSync (ah : AnimHandle.T; p : Base) : Val;
PROCEDURE NewAsync (b : AsyncBeh) : Val;
PROCEDURE NewDep (b : DepBeh) : Val;

PROCEDURE BecomeSync (ah : AnimHandle.T; pv : Val) : SyncBeh 
    RAISES {Prop.BadMethod};
(* Replaces "pv.beh" by "NEW(SyncBeh).init(pv.get(),ah)", 
   and returns the new behavior. *)

END PointProp.
