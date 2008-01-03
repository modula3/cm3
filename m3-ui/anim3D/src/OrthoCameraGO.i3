(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jun 23 14:29:13 PDT 1994 by najork                   *)
(*       Created on Mon Feb 14 15:56:41 PST 1994 by najork                   *)

(* An "OrthoCameraGO" is an orthographic camera, i.e. a camera which shows
   a scene as if it were infinitely far away --- lines that are parallel in 
   the scene appear parallel in the image. The volume of space observed by
   such a camera forms a parallelopiped. The shape of the parallelopiped
   is determined by the camera position, its viewing direction, its up-vector,
   and the parallelopiped's height and width (which is determined by height
   and aspect ratio). The figure below illustrates the role of the parameters:
   
   \begin{center}
   \begin{tabular}{c}
   \psfig{figure=images/OrthoCamera.ps,width=4in,silent=} 
   \end{tabular}
   \end{center}
*)

INTERFACE OrthoCameraGO;

IMPORT CameraGO, RealProp, Point3;

TYPE
  T <: Public;
  Public = CameraGO.T OBJECT 
  METHODS
    init () : T;
  END;
(* "oc.init()" initializes a new orthographic camera and returns it. *)

PROCEDURE New (from, to, up : Point3.T; height : REAL) : T;
(* "New(from,to,up,height)" creates a new orthographic camera "cam" and 
   returns it. It also attaches the following properties to "cam":
   \begin{verbatim}
      (CameraGO.From,PointProp.NewConst(from))
      (CameraGO.To,PointProp.NewConst(to))
      (CameraGO.Up,PointProp.NewConst(up))
      (CameraGO.Aspect,RealProp.NewConst(1.0))
      (Height,RealProp.NewConst(height))
   \end{verbatim}
*)

VAR
  Height : RealProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s and 
   \type{CameraGO}{T}'s, there is one additional property that is 
   observed by "OrthoCameraGO.T"'s. 
   "Height" is the name of a property that determines the height 
   of the viewing parallelopiped; it associates with a property value of 
   type \type{RealProp}{Val}. The width of the parallelopiped is the height 
   times the aspect ratio. *)

END OrthoCameraGO.
