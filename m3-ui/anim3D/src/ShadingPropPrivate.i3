(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jan 30 22:35:36 PST 1995 by najork                   *)
(*       Created on Sun May 22 11:24:48 PDT 1994 by najork                   *)


INTERFACE ShadingPropPrivate;

IMPORT GraphicsBase, Prop, PropPrivate;

FROM ShadingProp IMPORT 
  Kind, Name, PublicName, Val, PublicVal, Beh, PublicBeh;

REVEAL 
  Name <: PrivateName;

TYPE 
  PrivateName = PublicName OBJECT
  METHODS
    init (default : Kind) : Name;
    getState (base : GraphicsBase.T) : Kind;
  END;

REVEAL 
  Val <: PrivateVal;

TYPE 
  PrivateVal = PublicVal OBJECT
    val : Kind;        (* The cache is updated by calling "adjust". *)
  END;

REVEAL 
  Beh <: PrivateBeh;

TYPE 
  PrivateBeh = PublicBeh OBJECT
  METHODS 
    value (time : LONGREAL) : Kind RAISES {Prop.BadMethod};
  END;

TYPE 
  Stack <: PublicStack;
  PublicStack = PropPrivate.Stack OBJECT
    top : Kind;
  METHODS
    push (val : Kind);
    pop () : Kind;
  END;


END ShadingPropPrivate.
