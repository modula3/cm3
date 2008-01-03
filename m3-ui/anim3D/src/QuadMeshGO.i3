(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Wed Jun  1 21:56:13 PDT 1994 by najork                   *)

(* A "QuadMershGO.T" is a geometric object that describes a quadrilateral
   mesh. A quadrilateral mesh is defined by a 2-dimensional array of points.
   Neighboring points are connected by quadrangles. *)
   
INTERFACE QuadMeshGO;

IMPORT Color, GO, Point3, SurfaceGO;

EXCEPTION BadSize;
EXCEPTION ColorsUndefined;

TYPE
  T <: Public;
  Public = SurfaceGO.T OBJECT
  METHODS
    init (READONLY pts : ARRAY OF ARRAY OF Point3.T; 
          s := GO.Shape.Unknown) : T;
    addFacetColors (READONLY cols : ARRAY OF ARRAY OF Color.T) 
        RAISES {BadSize};
    setColorOfFacet (i, j : INTEGER; c : Color.T) RAISES {ColorsUndefined};
  END;
(* "qm.init(pts,s)" initializes the quad-mesh "qm" and returns it.
   "pts" is the matrix of points defining the quad-mesh; "s" is a 
   `shape hint'', i.e.\ a hint whether each polygon of the quad-mesh 
   is convex, non-convex, or complex.
   Refer to the \interface{GO} interface for details on shape hints. 
   By default, the quad-mesh is drawn in the color specified by the 
   \type{SurfaceGO}{Colour} property.

   "qm.addFacetColors(cols)" overrides the surface color specified by the 
   \type{SurfaceGO}{Colour} property, and instead attaches an individual 
   color to each quadrangle. The quadrangle "(i,j)" 
   (i.e. the quadrangle specified by the points 
   "pts[i][j]", "pts[i][j+1]", "pts[i+1][j+1]", "pts[i+1][j]") is drawn in 
   "col[i][j]". If "pts" was an $m+1 \times n+1$ array,
   "cols" must be an $m \times n$ array; otherwise, the exception "BadSize" 
   is raised. 

   "qm.setColorOfFacet(i,j,c)" changes the color of quadrangle "(i,j)"
   to be "c". The exception "ColorsUndefined" is raised if no facet colors 
   have yet been attached to "qm". It is a fatal error if "i" or "j" 
   specify an invalid quadrangle. {\em I should instead raise an exception!}

   {\em NOTES: (a) Points and colors should be exposed as properties. 
   Refer to the \interface{PolygonGO} interface for an idea about how to do that.
   (b) It should be possible to attach single color values; in this case,
   all cells with a missing color value are drawn in the color specified by
   "SurfaceGO.Colour".} *)
   

END QuadMeshGO.
