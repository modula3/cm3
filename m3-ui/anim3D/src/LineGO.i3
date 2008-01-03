(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jun 13 09:59:31 PDT 1994 by najork                   *)


(* A "LineGO.T" is a geometric object describing a line. *)

INTERFACE LineGO;

IMPORT Color, ColorProp, GO, LineTypeProp, Point3, PointProp, RealProp;

TYPE
  T <: Public;
  Public = GO.T OBJECT
  METHODS
    init () : T;
  END;
(* "l.init()" initializes "l" and returns it. The start- and endpoints of
   the line are determined by the properties "Point1" and "Point2". *)

VAR 
  Point1 : PointProp.Name;
  Point2 : PointProp.Name;
  Colour : ColorProp.Name;
  Width  : RealProp.Name;
  Type   : LineTypeProp.Name;

(* In addition to the properties observed by all \type{GO}{T}'s, there are 
   some additional properties that are observed by "LineGO.T"'s:

   "Point1" and "Point2" are the names of two point properties that describe
   the endpoints of the line. If "Point1" is not specified, the line starts
   at (0,0,0). If "Point2" is not specified, the line ends at (1,0,0).

   "Colour" is the name of a color property that describes the color of 
   the line. If no "Colour" property is specified, the line is drawn in white.

   "Width" is the name of a real property that describes the width of the 
   line. If no "Width" property is specified, the line is drawn with width 1.

   "Type" is the name of a line type property that describes the type of 
   the line (solid, dashed, dotted, or alternatingly dashed and dotted).
   If no "Type" property is specified, the line is drawn solid. *)

PROCEDURE New (p1, p2 : Point3.T) : T;
(* "New(p1,p2)" creates a new line and returns it. It also attaches the 
   following properties to the new line:
   \begin{verbatim}
     (Point1,PointProp.NewConst(p1))
     (Point2,PointProp.NewConst(p2))
   \end{verbatim}
*)

(* The following five procedures provide sugaring to attach "Colour", "Width",
   "Type", "Point1", and "Point2" properties to geometric objects: *)

PROCEDURE SetColour (o : GO.T; c : Color.T);
(* The expression "SetColour(o,c)" is equivalent to
   "o.setProp(Colour.bind(ColorProp.NewConst(c)))". *)

PROCEDURE SetWidth (o : GO.T; r : REAL);
(* The expression "SetWidth(o,r)" is equivalent to
   "o.setProp(Width.bind(RealProp.NewConst(r)))". *)

PROCEDURE SetType (o : GO.T; v : LineTypeProp.Kind);
(* The expression "SetType(o,t)" is equivalent to
   "o.setProp(Type.bind(LineTypeProp.NewConst(t)))". *)

PROCEDURE SetPoint1 (o : GO.T; v : Point3.T);
(* The expression "SetPoint1(o,p)"is equivalent to
   "o.setProp(Point1.bind(PointProp.NewConst(p)))". *)

PROCEDURE SetPoint2 (o : GO.T; v : Point3.T);
(* The expression "SetPoint2(o,p)"is equivalent to
   "o.setProp(Point2.bind(PointProp.NewConst(p)))". *)

END LineGO.
