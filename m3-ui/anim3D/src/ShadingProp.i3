(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Sep 23 17:10:11 PDT 1994 by najork                   *)
(*       Created on Sun May 22 11:14:32 PDT 1994 by najork                   *)


INTERFACE ShadingProp;

IMPORT AnimHandle, Prop;

TYPE
  Kind = {Flat, Gouraud};

(* "Kind" enumerates the supported shading methods. Here is what they do, 
   straight from the PEX manual:

   "Flat": The color resulting from a single light source computation
   is used for the entire surface. No interpolation will be preformed
   across surface interiors or edges. In PEXlib, this is called 
   PEXSurfaceInterpNone; in OpenGL, it is called GL_FLAT.

   "Gouraud": The colors are computed at the vertices of the surface according
   to the current reflection model. These color values are then interpolated 
   across the interior of the surface or edges. In PEXlib, this is called 
   PEXSurfaceInterpColour; in OpenGL, it is called GL_SMOOTH.

   The PEX 5.0 standard provides two more shading methods, dot-product shading
   (PEXSurfaceInterpDotProduct) and Phong shading (PEXSurfaceInterpNormal).
   However, most existing implementations of PEX (including the ones available
   at SRC) do not support these two methods, and OpenGL does not provide them 
   at all. So, we chose not to include them into Anim3D. *)


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

END ShadingProp.
