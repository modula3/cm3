(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Nov 10 10:22:51 PST 1994 by najork                   *)
(*       Created on Sun May 22 11:14:43 PDT 1994 by najork                   *)


INTERFACE RasterModeProp;

IMPORT AnimHandle, Prop;

TYPE
  Kind = {Hollow, Solid, Empty};
(* "Kind" enumerates the three possible ways a surface can be shown: 
   "Solid", which means that the surface is filled with the surface color,
   "Hollow", which means that only the edges of the polygons forming the 
   surface are drawn, again in the surface colour, and "Empty", which means 
   that the surface is not drawn at all.

   PEX 5.0 defines these three rasterization modes (or ``Interior Style'' 
   in PEX terminology) for surfaces, plus two more, "Pattern" and "Hatch". 
   However, Digital's implementation of PEX does not support those two styles,
   so we don't support them either. 

   OpenGL has a more general way of defining rasterization modes (or ``Polygon
   Modes'' in OpenGL terminology). Therefore, it is likely that we will extend
   this interface once we use OpenGL as the underlying graphics system. *)

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
    get () : Kind RAISES {Prop.BadMethod};
    value (time : LONGREAL) : Kind RAISES {Prop.BadMethod};
  END;

  Beh <: PublicBeh;
  PublicBeh = Prop.Beh OBJECT 
  METHODS
    init () : Beh;
  END;

  ConstBeh <: PublicConstBeh;
  PublicConstBeh = Beh OBJECT
  METHODS
    init (p : Kind) : ConstBeh;
    set (p : Kind);
  END;

  SyncBeh <: PublicSyncBeh;
  PublicSyncBeh = Beh OBJECT
  METHODS
    init (ah : AnimHandle.T; p : Kind) : SyncBeh;
    addRequest (r : Request) RAISES {Prop.BadInterval};
  (* shortcuts for particular instances of "addRequest" *)
    change (p : Kind; start := 0.0) RAISES {Prop.BadInterval};
  END;

  AsyncBeh <: PublicAsyncBeh;
  PublicAsyncBeh = Beh OBJECT
  METHODS
    init () : AsyncBeh;
    compute (time : LONGREAL) : Kind RAISES {Prop.BadMethod};
  END;

  DepBeh <: PublicDepBeh;
  PublicDepBeh = Beh OBJECT
  METHODS
    init () : DepBeh;
    compute (time : LONGREAL) : Kind RAISES {Prop.BadMethod};
  END;

  Request <: PublicRequest;
  PublicRequest = Prop.Request OBJECT 
  METHODS
    init (start, dur : REAL) : Request;
    value (startkind : Kind; reltime : REAL) : Kind RAISES {Prop.BadMethod};
  END;

PROCEDURE NewConst (p : Kind) : Val;
PROCEDURE NewSync (ah : AnimHandle.T; p : Kind) : Val;
PROCEDURE NewAsync (b : AsyncBeh) : Val;
PROCEDURE NewDep (b : DepBeh) : Val;

END RasterModeProp.
