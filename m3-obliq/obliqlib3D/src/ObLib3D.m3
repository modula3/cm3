(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr 12 12:27:38 PDT 1996 by najork                   *)
(*       Created on Tue Jan 18 09:39:01 PST 1994 by najork                   *)


MODULE ObLib3D;

IMPORT ObAmbientLightGO, ObAnim3D, ObAnimHandle, ObBooleanProp, ObBoxGO, ObCB, 
       ObCameraGO, ObColorProp, ObConeGO, ObCylinderGO, ObDiskGO, ObGO, 
       ObGraphicsBase, ObGroupGO, ObKeyCB, ObLib3DBundle, ObLightGO, ObLineGO,
       ObLineTypeProp, ObMarkerGO, ObMarkerTypeProp, ObMatrix4, ObMouseCB, 
       ObOrthoCameraGO, ObPerspCameraGO, ObPoint3, ObPointLightGO, ObPointProp,
       ObPolygonGO, ObPositionCB, ObProp, ObProtoLoader, ObProxiedObj, 
       ObQuadMeshGO, ObRasterModeProp, ObRealProp, ObRootGO, ObShadingProp, 
       ObSphereGO, ObSpotLightGO, ObSurfaceGO, ObTime, ObTorusGO, 
       ObTransformProp, ObVectorLightGO, ObWin_OpenGL_Base, ObX_PEX_Base, 
       ObX_OpenGL_Base, SynWr;


VAR 
  setupDone := FALSE;


PROCEDURE PackageSetup () =
  BEGIN
    SetupPackages ();
    SetupModules ();
  END PackageSetup;


PROCEDURE SetupPackages () =
  BEGIN
    IF NOT setupDone THEN
      ObAnim3D.SetupPackage ();
      ObPoint3.SetupPackage ();
      ObMatrix4.SetupPackage ();
      ObTime.SetupPackage ();
      ObProxiedObj.SetupPackage ();
      ObAnimHandle.SetupPackage ();
      ObProp.SetupPackage ();
      ObBooleanProp.SetupPackage ();
      ObRealProp.SetupPackage ();
      ObPointProp.SetupPackage ();
      ObColorProp.SetupPackage ();
      ObTransformProp.SetupPackage ();
      ObLineTypeProp.SetupPackage ();
      ObMarkerTypeProp.SetupPackage ();
      ObRasterModeProp.SetupPackage ();
      ObShadingProp.SetupPackage ();
      ObGraphicsBase.SetupPackage ();
      ObX_PEX_Base.SetupPackage ();
      ObX_OpenGL_Base.SetupPackage ();
      ObWin_OpenGL_Base.SetupPackage ();
      ObCB.SetupPackage ();
      ObMouseCB.SetupPackage ();
      ObPositionCB.SetupPackage ();
      ObKeyCB.SetupPackage ();
      ObGO.SetupPackage ();
      ObGroupGO.SetupPackage ();
      ObRootGO.SetupPackage ();
      ObCameraGO.SetupPackage ();
      ObPerspCameraGO.SetupPackage ();
      ObOrthoCameraGO.SetupPackage ();
      ObLightGO.SetupPackage ();
      ObAmbientLightGO.SetupPackage ();
      ObVectorLightGO.SetupPackage ();
      ObPointLightGO.SetupPackage ();
      ObSpotLightGO.SetupPackage ();
      ObMarkerGO.SetupPackage ();
      ObLineGO.SetupPackage ();
      ObSurfaceGO.SetupPackage ();
      ObPolygonGO.SetupPackage ();
      ObBoxGO.SetupPackage ();
      ObSphereGO.SetupPackage ();
      ObConeGO.SetupPackage ();
      ObDiskGO.SetupPackage ();
      ObCylinderGO.SetupPackage ();
      ObTorusGO.SetupPackage ();
      ObQuadMeshGO.SetupPackage ();
      setupDone := TRUE;
    END;
  END SetupPackages;


PROCEDURE SetupModules () =
  VAR
    loader := NEW (ObProtoLoader.T).init (ObLib3DBundle.Get ());
  BEGIN
    SynWr.PushSilence (SynWr.out);
    ObProxiedObj.SetupModule (loader);
    ObAnimHandle.SetupModule (loader);
    ObProp.SetupModule (loader);
    ObBooleanProp.SetupModule (loader);
    ObRealProp.SetupModule (loader);
    ObPointProp.SetupModule (loader);
    ObColorProp.SetupModule (loader);
    ObTransformProp.SetupModule (loader);
    ObLineTypeProp.SetupModule (loader);
    ObMarkerTypeProp.SetupModule (loader);
    ObRasterModeProp.SetupModule (loader);
    ObShadingProp.SetupModule (loader);
    ObGraphicsBase.SetupModule (loader);
    ObX_PEX_Base.SetupModule (loader);
    ObX_OpenGL_Base.SetupModule (loader);
    ObWin_OpenGL_Base.SetupModule (loader);
    ObMouseCB.SetupModule (loader);
    ObPositionCB.SetupModule (loader);
    ObKeyCB.SetupModule (loader);
    ObGO.SetupModule (loader);
    ObGroupGO.SetupModule (loader);
    ObRootGO.SetupModule (loader);
    ObCameraGO.SetupModule (loader);
    ObPerspCameraGO.SetupModule (loader);
    ObOrthoCameraGO.SetupModule (loader);
    ObLightGO.SetupModule (loader);
    ObAmbientLightGO.SetupModule (loader);
    ObVectorLightGO.SetupModule (loader);
    ObPointLightGO.SetupModule (loader);
    ObSpotLightGO.SetupModule (loader);
    ObMarkerGO.SetupModule (loader);
    ObLineGO.SetupModule (loader);
    ObSurfaceGO.SetupModule (loader);
    ObPolygonGO.SetupModule (loader);
    ObBoxGO.SetupModule (loader);
    ObSphereGO.SetupModule (loader);
    ObConeGO.SetupModule (loader);
    ObDiskGO.SetupModule (loader);
    ObCylinderGO.SetupModule (loader);
    ObTorusGO.SetupModule (loader);
    ObQuadMeshGO.SetupModule (loader);
    SynWr.PopSilence (SynWr.out);
  END SetupModules;


BEGIN
END ObLib3D.
