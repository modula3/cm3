(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 14 17:20:02 PDT 1995 by najork                   *)
(*       Created on Wed Feb 16 17:57:52 PST 1994 by najork                   *)


INTERFACE GraphicsBasePrivate;

IMPORT BSphere, Color, GO, LineTypeProp, MarkerTypeProp, Matrix4, Point, 
       Point3, PropPrivate, ProxiedObj, RasterModeProp, RootGO, ShadingProp;

FROM GraphicsBase IMPORT T;

CONST 
  VoidColor = Color.T {-1.0, -1.0, -1.0};

REVEAL T <: Private;

TYPE 
  Private = ProxiedObj.T BRANDED OBJECT
    root   : RootGO.T := NIL;
    status : Status   := Status.Unmapped;
    stacks : REF ARRAY OF PropPrivate.Stack;
  METHODS 
    unmap ();
    processEvents ();
    repair (VAR damaged : BOOLEAN);

    (*** Pushing and popping property lists onto the state **)
    push (caller : GO.T);
    pop (caller : GO.T);

    (*** Display Lists ***)
    openDisplayList (go : GO.T);
    closeDisplayList ();
    callDisplayList (go : GO.T);
    freeDisplayList (go : GO.T);

    (*** The matrix stack ***)
    pushMatrix (READONLY matrix : Matrix4.T);
    popMatrix ();

    (* Bounding volumes *)
    resetBoundingVolume ();
    growBoundingVolume (center : Point3.T; radius : REAL);
    getBoundingVolume () : BSphere.T;

    (* Camera control - setting viewing and projection transformations, and 
       mapping points from screen coordinate space to world coordinate space *)
    setLookAt (from, to, up: Point3.T);
    setPerspProj (fovy, aspect: REAL);
    setOrthoProj (height, aspect: REAL);
    setupCamera ();
    screenToWorld (pos: Point.T; z: REAL): Point3.T;
    
    (*** Graphics Commands ***)
    setBackgroundColor (color : Color.T);
    setDepthcueing (switch: BOOLEAN; 
                    frontPlane, backPlane, frontScale, backScale: REAL;
                    color: Color.T);

    setMarkerColor (val : Color.T);
    setMarkerScale (val : REAL);
    setMarkerType  (val : MarkerTypeProp.Kind);

    setLineColor (val : Color.T);
    setLineWidth (val : REAL);
    setLineType  (val : LineTypeProp.Kind);

    setSurfaceColor (val : Color.T);
    setSurfaceBackColor (val : Color.T);
       (* setting the back color to "VoidColor" indicates that it shall 
          be the same as the front color *)
    setRasterMode (val : RasterModeProp.Kind);
    setDistinguishFacetsFlag (val : BOOLEAN);
    setLighting (val : BOOLEAN);
    setShading (val : ShadingProp.Kind);
    setSurfaceEdgeFlag (val : BOOLEAN);
    setSurfaceEdgeColor (val : Color.T);
    setSurfaceEdgeType (val : LineTypeProp.Kind);
    setSurfaceEdgeWidth (val : REAL);

    setAmbientReflCoeff (val : REAL);
    setDiffuseReflCoeff (val : REAL);
    setSpecularReflCoeff (val : REAL);
    setSpecularReflConc (val : REAL);
    setSpecularReflColor (val : Color.T);
    setTransmissionCoeff (val : REAL);

    addAmbientLight (color: Color.T);
    addVectorLight  (color: Color.T; dir: Point3.T);
    addPointLight (color: Color.T; point: Point3.T; att0, att1: REAL);
    addSpotLight (color: Color.T; point, dir: Point3.T; 
                  conc, spread, att0, att1: REAL);

    drawMarker (p : Point3.T);
    drawLine (p1, p2 : Point3.T);
    drawPolygon (READONLY pts : ARRAY OF Point3.T; shape : GO.Shape);
    drawQuadMesh (READONLY pts : ARRAY OF ARRAY OF Point3.T; shape : GO.Shape);
    drawColoredQuadMesh (READONLY points: ARRAY OF ARRAY OF Point3.T; 
                         READONLY colors: ARRAY OF ARRAY OF Color.T;
                                  shape : GO.Shape);

    drawProtoSphere (prec : INTEGER);
    drawProtoCone (prec : INTEGER);
    drawProtoCylinder (prec : INTEGER);
    drawProtoDisk (prec : INTEGER);
    drawProtoTorus (prec: INTEGER; radiusRatio : REAL);
  END;

(* "base.repair(damaged)" redraws the scene rooted at "base.root". Only those
   nodes that were marked as damaged are re-rendered, for the other nodes,
   cached values are used. 

   Nodes can be damaged in two ways: 
   \begin{enumerate}
   \item By operations that change the scene DAG (i.e.\ the group operations
         "add", "remove", and "flush").
   \item By changes in the value of an attached property. 
   \end{enumerate}

   Calling "root.adjust(time)" serves two purposes: It reevaluates all the 
   properties attached to all descendants of "root", and damages those nodes 
   whose properties have changed since the last round of rendering. It also
   propagates damages up the scene DAGs.
       "
   "base.repair(damaged)" is called after all roots have been adjusted. The 
   VAR parameter "damaged" is set to TRUE if there were any damages in the 
   scene, otherwise, it remains unchanged. *)

TYPE Status = {Unmapped, Mapped, Destroyed};

END GraphicsBasePrivate.
