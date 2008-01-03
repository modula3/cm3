(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Fri Jul 14 11:49:37 PDT 1995 by najork                   *)


INTERFACE GOPrivate;

IMPORT GraphicsBase, KeyCBStack, MouseCBStack, PositionCBStack, Prop, PropList;

FROM GO IMPORT T, Public;

REVEAL
  T <: Private;

TYPE
  Private = Public BRANDED OBJECT
    props : PropList.T;
    trans : REAL;   (* FIRST(REAL) means: no transmission coeff in props *)
    name  : TEXT;

    mouseCBstack    : MouseCBStack.T;
    positionCBstack : PositionCBStack.T;
    keyCBstack      : KeyCBStack.T;

    damaged         : BOOLEAN;
    dl              : INTEGER := 0;
  METHODS
    draw (base : GraphicsBase.T);
    needsTransparency (t : REAL) : BOOLEAN;

    damageIfDependent (pn : Prop.Name);
    adjust (time : LONGREAL);

    undamage();
  END;

(* "go.damageIfDependent(pn)" leaves a damage trail from "go" to all 
   descendants of "go" (including "go") that depend on "pn". 

   "go.adjust(time)" reevaluates all property values attached to "go" and
   its descendants, based on the given time. It also might add damages to 
   the scene, either to "go" or to some of its children. Damaging a node
   causes damage to all its ancestors as well. 

   "go.undamage" erases the damage-flags of "go" and its descendents. *)

END GOPrivate.
