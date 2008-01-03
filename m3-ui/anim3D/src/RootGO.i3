(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jul 11 09:34:02 PDT 1995 by najork                   *)
(*       Created on Fri Feb  4 15:42:17 PST 1994 by najork                   *)


(* A "RootGO.T" is a geometric object that describes a scene (that is, 
   it is the root of a scene tree or DAG). "RootGO.T" is a subtype of
   \type{GroupGO}{T}. *)

INTERFACE RootGO;

IMPORT BooleanProp, CameraGO, ColorProp, GraphicsBase, GroupGO, Point, Point3,
       RealProp;

TYPE
  T <: Public;
  Public = GroupGO.T OBJECT
  METHODS
    init (cam : CameraGO.T; base : GraphicsBase.T) : T;
    changeCamera (cam : CameraGO.T);
    screenToWorld (pos: Point.T; z: REAL): Point3.T;
  END;
(* "r.init(cam,base)" initializes a new root object "r" and returns it.
   The scene (i.e.\ "r" and its descendants) will be viewed through the 
   camera "cam" (which may, but does not have to be a descendant of "r").
   "base" provides a connection to the underlying window system (e.g.\ X)
   and graphics system (e.g.\ PEX). It also initializes the animation clock
   to a new \type{Clock}{T} (i.e.\ a real-time clock).

   "r.changeCamera(cam)" changes the camera through which the scene is viewed.

   "r.changeClock(clock)" changes the animation clock.

   "r.animate(ah)" starts the animation of all synchronized time-variant 
   property values that are currently tied to the animation handle "ah".
   "r.animate(ah)" returns when all animations associated with "ah" are 
   completed. *)

(* {\em Note: Making a "RootGO.T" a child of another "GroupGO.T" is wrong, 
   and should raise an exception. Currently, we don't enforce this.} *)


PROCEDURE New (cam : CameraGO.T; base : GraphicsBase.T) : T;
(* A convenience procedure. The expression "New(cam,base)" is equivalent to
   "NEW(T).init(cam,base)". *)

PROCEDURE NewStd (base : GraphicsBase.T := NIL) : T 
  RAISES {GraphicsBase.Failure};
(* Creates and returns a new "RootGO.T", which is set up in a ``reasonable''
   way. 
   \begin{itemize}
   \item The new root ("root") uses a perspective camera, 
         which is looking at the origin.
   \item "root" contains two light sources, an ambient light source and a 
         vector light source with rays along the vector (-1,-1,-1). 
         Both sources are emitting white light.
   \item If no "base" is supplied, it creates a new X-PEX base entitled
         ``Anim3D Viewer''.
   \item The property "(GO.Transform,TransformProp.NewConst ())" is attached 
         to "root". This property is used to allow the user to interactively
         manipulate the scene, and therefore should not be detached.
   \item Mouse and position callback objects are pushed onto "root"'s mouse
         and position callback stacks. These callback objects respond to
         ``mouse drags'' (possibly modified through the shift key) by
         translating, uniformly scaling, or rotating the scene described 
         by "root".
   \end{itemize} *)
   

VAR
  Background         : ColorProp.Name;
  DepthcueSwitch     : BooleanProp.Name;
  DepthcueColour     : ColorProp.Name;
  DepthcueFrontPlane : RealProp.Name;
  DepthcueBackPlane  : RealProp.Name;
  DepthcueFrontScale : RealProp.Name;
  DepthcueBackScale  : RealProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s and 
   \type{GroupGO}{T}'s, there are some additional properties that are 
   observed by "RootGO.T"'s:

   "Background" is the name of a property that describes the background color
   of the scene. It associates with a \type{ColorProp}{Val}. By default, the 
   background is black.

   {\em Note: Alternatively, I could say that by default, the color shift 
   shall occur in the direction of the background color.}

   "DepthcueSwitch" is the name of a property that determines whether or not
   depth cueing (also called ``fog'') shall be used. It associates with a 
   \type{BooleanProp}{Val}. By default, depth cueing is switched off.

   "DepthcueColour" is the name of a property that determines the color
   of the ``fog'', i.e. the color shift objects that are far from the viewer 
   undergo. It associates with a \type{ColourProp}{Val}. By default (assuming 
   that depth cueing is activated) objects appear the darker the further they 
   are away from the viewer (i.e.\ their color is shifted towards black).
   
   "DepthcueFrontPlane" and "DepthcueBackPlane" are the names of two 
   properties that determine within which distance range from the viewer 
   the color shift shall occur. They associate with \type{RealProp}{Val}'s. 
   Distances are specified in normalized projection coordinates.

   {\em Note: This concept might be to complicated for a casual user.
   I could hide the depth cueing front and back planes, and set them
   to be equal to the z-buffer clamps. The user could still adjust the
   amout of depth cueing by adjusting the front and back scale.}

   "DepthcueFrontScale" and "DepthcueBackScale" are the names of two 
   properties that determine how strong the color shift shall be at the 
   depth cueing front and back planes. They associate with 
   \type{RealProp}{Val}'s.
*)

END RootGO.
