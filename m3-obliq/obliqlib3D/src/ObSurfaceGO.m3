(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb  3 14:39:45 PST 1995 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObSurfaceGO;

IMPORT GO, ObAux, ObBooleanProp, ObColorProp, ObCommand, ObGO, ObLib, 
       ObLineTypeProp, ObProp, ObProtoLoader, ObRasterModeProp, ObRealProp, 
       ObShadingProp, ObValue, SurfaceGO, SynLocation;

CONST
  pkgname = "SurfaceGO";


(*****************************************************************************)
(* Wrapper for SurfaceGO.T                                                   *)
(*****************************************************************************)


REVEAL
  T = ObGO.T BRANDED "ObSurfaceGO.T" OBJECT END;


(*****************************************************************************)
(* Setup procedures                                                          *)
(*****************************************************************************)


PROCEDURE SetupPackage () =

  PROCEDURE NewOpCode (name: TEXT; arity: INTEGER; code: Code) : OpCode =
    BEGIN
      RETURN NEW (OpCode, name := name, arity := arity, code := code);
    END NewOpCode;

  TYPE 
    OpCodes = ARRAY OF ObLib.OpCode;
  VAR 
    opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW (REF OpCodes, NUMBER (Code));
    opCodes^ := 
        OpCodes {
            NewOpCode ("DistinguishFacets",
                       -1, Code.DistinguishFacets),
            NewOpCode ("SetDistinguishFacets",
                       2, Code.SetDistinguishFacets),
            NewOpCode ("Color",                     
                       -1, Code.Color), 
            NewOpCode ("GetColor",                  
                        1, Code.GetColor), 
            NewOpCode ("SetColor",                  
                        2, Code.SetColor), 
            NewOpCode ("BackColor",                     
                       -1, Code.BackColor), 
            NewOpCode ("SetBackColor",                  
                        2, Code.SetBackColor), 
            NewOpCode ("RasterMode",              
                       -1, Code.RasterMode), 
            NewOpCode ("SetRasterMode",           
                        2, Code.SetRasterMode), 
            NewOpCode ("AmbientReflectionCoeff",     
                       -1, Code.AmbientReflectionCoeff), 
            NewOpCode ("SetAmbientReflectionCoeff",  
                        2, Code.SetAmbientReflectionCoeff), 
            NewOpCode ("DiffuseReflectionCoeff",     
                       -1, Code.DiffuseReflectionCoeff), 
            NewOpCode ("SetDiffuseReflectionCoeff",  
                        2, Code.SetDiffuseReflectionCoeff), 
            NewOpCode ("SpecularReflectionCoeff",    
                       -1, Code.SpecularReflectionCoeff), 
            NewOpCode ("SetSpecularReflectionCoeff", 
                        2, Code.SetSpecularReflectionCoeff), 
            NewOpCode ("SpecularReflectionConc",     
                       -1, Code.SpecularReflectionConc), 
            NewOpCode ("SetSpecularReflectionConc",  
                        2, Code.SetSpecularReflectionConc), 
            NewOpCode ("TransmissionCoeff",          
                       -1, Code.TransmissionCoeff), 
            NewOpCode ("SetTransmissionCoeff",       
                        2, Code.SetTransmissionCoeff), 
            NewOpCode ("SpecularReflectionColor",   
                       -1, Code.SpecularReflectionColor), 
            NewOpCode ("SetSpecularReflectionColor",
                        2, Code.SetSpecularReflectionColor), 
            NewOpCode ("Lighting",            
                       -1, Code.Lighting), 
            NewOpCode ("SetLighting",         
                        2, Code.SetLighting), 
            NewOpCode ("Shading",              
                       -1, Code.Shading), 
            NewOpCode ("SetShading",           
                        2, Code.SetShading), 
            NewOpCode ("EdgeVisibility",             
                       -1, Code.EdgeVisibility), 
            NewOpCode ("SetEdgeVisibility",          
                        2, Code.SetEdgeVisibility), 
            NewOpCode ("EdgeColor",                 
                       -1, Code.EdgeColor), 
            NewOpCode ("SetEdgeColor",              
                        2, Code.SetEdgeColor), 
            NewOpCode ("EdgeType",                   
                       -1, Code.EdgeType), 
            NewOpCode ("SetEdgeType",                
                        2, Code.SetEdgeType), 
            NewOpCode ("EdgeWidth",                  
                       -1, Code.EdgeWidth), 
            NewOpCode ("SetEdgeWidth",               
                        2, Code.SetEdgeWidth)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);

    (* DONT KNOW YET WHETHER TO INHIBIT TRANSMISSIONS ... *)

  END SetupPackage;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    loader.load ("SurfaceGO.obl");
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {DistinguishFacets,        SetDistinguishFacets,
          Color,                    GetColor, SetColor, 
          BackColor,                SetBackColor, 
          RasterMode,               SetRasterMode, 
          AmbientReflectionCoeff,   SetAmbientReflectionCoeff, 
          DiffuseReflectionCoeff,   SetDiffuseReflectionCoeff,
          SpecularReflectionCoeff,  SetSpecularReflectionCoeff,
          SpecularReflectionConc,   SetSpecularReflectionConc,
          TransmissionCoeff,        SetTransmissionCoeff,
          SpecularReflectionColor,  SetSpecularReflectionColor,
          Lighting,                 SetLighting,
          Shading,                  SetShading,
          EdgeVisibility,           SetEdgeVisibility,
          EdgeColor,                SetEdgeColor,
          EdgeType,                 SetEdgeType,
          EdgeWidth,                SetEdgeWidth};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;
    
  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;


PROCEDURE DoEval (self         : Package; 
                  opCode       : ObLib.OpCode; 
     <* UNUSED *> arity        : ObLib.OpArity; 
                  READONLY args: ObValue.ArgArray; 
     <* UNUSED *> temp         : BOOLEAN;
                  loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.DistinguishFacets =>
      RETURN ObProp.NameToObliq (SurfaceGO.DistinguishFacets);
    | Code.SetDistinguishFacets =>
      WITH go = ObGO.GetArg                    (args, 1, self, opCode, loc),
           pv = ObBooleanProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.DistinguishFacets.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.Color =>
      RETURN ObProp.NameToObliq (SurfaceGO.Colour);
    | Code.GetColor =>
      WITH go = ObGO.GetArg (args, 1, self, opCode, loc) DO
        TRY
          RETURN go.getProp (SurfaceGO.Colour).proxy.obj;
        EXCEPT
        | GO.PropUndefined =>
          ObValue.RaiseException (ObGO.PropUndefined, opCode.name, loc);
          RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
        END;
      END;
    | Code.SetColor =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           pv = ObColorProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.Colour.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.BackColor =>
      RETURN ObProp.NameToObliq (SurfaceGO.BackColour);
    | Code.SetBackColor =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           pv = ObColorProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.BackColour.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.RasterMode =>
      RETURN ObProp.NameToObliq (SurfaceGO.RasterMode);
    | Code.SetRasterMode =>
      WITH go = ObGO.GetArg (args, 1, self, opCode, loc),
           pv = ObRasterModeProp.GetOverloadedVal 
                            (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.RasterMode.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.AmbientReflectionCoeff =>
      RETURN ObProp.NameToObliq (SurfaceGO.AmbientReflectionCoeff);
    | Code.SetAmbientReflectionCoeff =>
      WITH go = ObGO.GetArg   (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.AmbientReflectionCoeff.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.DiffuseReflectionCoeff =>
      RETURN ObProp.NameToObliq (SurfaceGO.DiffuseReflectionCoeff);
    | Code.SetDiffuseReflectionCoeff =>
      WITH go = ObGO.GetArg   (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.DiffuseReflectionCoeff.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.SpecularReflectionCoeff =>
      RETURN ObProp.NameToObliq (SurfaceGO.SpecularReflectionCoeff);
    | Code.SetSpecularReflectionCoeff =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.SpecularReflectionCoeff.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.SpecularReflectionConc =>
      RETURN ObProp.NameToObliq (SurfaceGO.SpecularReflectionConc);
    | Code.SetSpecularReflectionConc =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.SpecularReflectionConc.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.TransmissionCoeff =>
      RETURN ObProp.NameToObliq (SurfaceGO.TransmissionCoeff);
    | Code.SetTransmissionCoeff =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.TransmissionCoeff.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.SpecularReflectionColor =>
      RETURN ObProp.NameToObliq (SurfaceGO.SpecularReflectionColour);
    | Code.SetSpecularReflectionColor =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           pv = ObColorProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.SpecularReflectionColour.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.Lighting =>
      RETURN ObProp.NameToObliq (SurfaceGO.Lighting);
    | Code.SetLighting =>
      WITH go = ObGO.GetArg                    (args, 1, self, opCode, loc),
           pv = ObBooleanProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.Lighting.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.Shading =>
      RETURN ObProp.NameToObliq (SurfaceGO.Shading);
    | Code.SetShading =>
      WITH go = ObGO.GetArg                    (args, 1, self, opCode, loc),
           pv = ObShadingProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.Shading.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.EdgeVisibility =>
      RETURN ObProp.NameToObliq (SurfaceGO.EdgeVisibility);
    | Code.SetEdgeVisibility =>
      WITH go = ObGO.GetArg                   (args, 1, self, opCode, loc),
           pv = ObBooleanProp.GetOverloadedVal(args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.EdgeVisibility.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.EdgeColor =>
      RETURN ObProp.NameToObliq (SurfaceGO.EdgeColour);
    | Code.SetEdgeColor =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           pv = ObColorProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.EdgeColour.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.EdgeType =>
      RETURN ObProp.NameToObliq (SurfaceGO.EdgeType);
    | Code.SetEdgeType =>
      WITH go = ObGO.GetArg                    (args, 1, self, opCode, loc),
           pv = ObLineTypeProp.GetOverloadedVal(args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.EdgeType.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.EdgeWidth =>
      RETURN ObProp.NameToObliq (SurfaceGO.EdgeWidth);
    | Code.SetEdgeWidth =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SurfaceGO.EdgeWidth.bind (pv));
        RETURN ObValue.valOk;
      END;
    END;
  END DoEval;
  

(*****************************************************************************)
(* Help                                                                      *)
(*****************************************************************************)


PROCEDURE Help (self : ObCommand.T; arg : TEXT; <* UNUSED *> data : REFANY) =
  BEGIN
    ObAux.Help (self, arg, pkgname);
  END Help;


BEGIN
END ObSurfaceGO.
