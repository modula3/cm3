(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:38:35 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObQuadMeshGO;

IMPORT Color, ObAux, ObColor, ObCommand, ObInt, ObLib, ObPoint3, ObProtoLoader,
       ObShape, ObSurfaceGO, ObValue, Obliq, Point3, ProxiedObj, QuadMeshGO, 
       QuadMeshGOProxy, SynLocation;


CONST
  pkgname = "QuadMeshGO";

(*****************************************************************************)
(* Wrapper for QuadMeshGO.T                                                  *)
(*****************************************************************************)


TYPE
  T = ObSurfaceGO.T BRANDED "ObQuadMeshGO.T" OBJECT END;


PROCEDURE AddTObj (qm : QuadMeshGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a QuadMeshGO.T>", po := qm) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      qm.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddTObj;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : QuadMeshGO.T 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
        T (node) => RETURN node.po;
      ELSE
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetArg;


(*****************************************************************************)
(* Setup procedures                                                          *)
(*****************************************************************************)


PROCEDURE SetupPackage () =

  PROCEDURE NewOpCode (name : TEXT; arity : INTEGER; code : Code) : OpCode =
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
            NewOpCode ("BadSize",         -1, Code.BadSize),
            NewOpCode ("ColorsUndefined", -1, Code.ColorsUndefined),
            NewOpCode ("New",              1, Code.New),
            NewOpCode ("NewWithShapeHint", 2, Code.NewWithShapeHint),
            NewOpCode ("AddFacetColors",   2, Code.AddFacetColors),
            NewOpCode ("SetColorOfFacet",  4, Code.SetColorOfFacet)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    BadSize   := 
        NEW (ObValue.ValException, name := pkgname & "_BadSize");
    ColorsUndefined := 
        NEW (ObValue.ValException, name := pkgname & "_ColorsUndefined");

    (* DONT KNOW YET WHETHER TO INHIBIT TRANSMISSIONS ... *)

    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("QuadMeshGO.obl");
    TProto := loader.get ("QuadMeshGO_TProto");

    (*** Register the proxy maker ***)
    QuadMeshGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {BadSize, ColorsUndefined, 
          New, NewWithShapeHint, 
          AddFacetColors, SetColorOfFacet};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;
    
  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;


VAR
  BadSize         : ObValue.ValException;
  ColorsUndefined : ObValue.ValException;


PROCEDURE DoEval (self         : Package; 
                  opCode       : ObLib.OpCode; 
     <* UNUSED *> arity        : ObLib.OpArity; 
                  READONLY args: ObValue.ArgArray; 
     <* UNUSED *> temp         : BOOLEAN;
                  loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.BadSize => 
      RETURN BadSize;
    | Code.ColorsUndefined => 
      RETURN ColorsUndefined;
    | Code.New => 
      TRY
        WITH m = Obliq.ArraySize (args[1]),
             n = Obliq.ArraySize (Obliq.ArrayGet (args[1], 0)),
             points = NEW (REF ARRAY OF ARRAY OF Point3.T, m, n) DO
          FOR i := 0 TO m - 1 DO 
            WITH slice = Obliq.ArrayGet (args[1], i) DO
              FOR j := 0 TO n - 1 DO 
                points[i][j] := ObPoint3.ObliqToM3 (Obliq.ArrayGet (slice, j));
              END;
            END;
          END;
          WITH qm = NEW (QuadMeshGO.T).init (points^) DO
            RETURN qm.proxy.obj;
          END;
        END;
      EXCEPT 
      | ObValue.Error =>
        ObValue.BadArgType(1, "[m*[n*Point3]]", self.name, opCode.name, loc);
        RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
      END;
    | Code.NewWithShapeHint => 
      TRY
        WITH m = Obliq.ArraySize (args[1]),
             n = Obliq.ArraySize (Obliq.ArrayGet (args[1], 0)),
             points = NEW (REF ARRAY OF ARRAY OF Point3.T, m, n) DO
          FOR i := 0 TO m - 1 DO 
            WITH slice = Obliq.ArrayGet (args[1], i) DO
              FOR j := 0 TO n - 1 DO 
                points[i][j] := ObPoint3.ObliqToM3 (Obliq.ArrayGet (slice, j));
              END;
            END;
          END;
          WITH shape = ObShape.GetArg (args, 2, self, opCode, loc),
               qm    = NEW (QuadMeshGO.T).init (points^, shape) DO
            RETURN qm.proxy.obj;
          END;
        END;
      EXCEPT 
      | ObValue.Error =>
        ObValue.BadArgType(1, "[m*[n*Point3]]", self.name, opCode.name, loc);
        RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
      END;
    | Code.AddFacetColors => 
      VAR
        cols : REF ARRAY OF ARRAY OF Color.T;
      BEGIN
        TRY
          WITH cols_x = Obliq.ArraySize (args[2]),
               cols_y = Obliq.ArraySize (Obliq.ArrayGet (args[2], 0)) DO
            cols := NEW (REF ARRAY OF ARRAY OF Color.T, cols_x, cols_y);

            FOR i := 0 TO cols_x - 1 DO
              WITH slice = Obliq.ArrayGet (args[2], i) DO
                IF Obliq.ArraySize (slice) # cols_y THEN
                  Obliq.RaiseError ("columns of color matrix have differing lenghts");
                  <* ASSERT FALSE *>
                END;
                FOR j := 0 TO cols_y - 1 DO
                  cols[i][j] := ObColor.ObliqToM3 (Obliq.ArrayGet (slice, j));
                END;
              END;
            END;
          END;
        EXCEPT
          ObValue.Error =>
          ObValue.BadArgType(2, "[m*[n*Color]]", self.name, opCode.name, loc);
          <* ASSERT FALSE *>
        END;

        TRY
          WITH qm = GetArg (args, 1, self, opCode, loc) DO
            qm.addFacetColors (cols^);
            RETURN ObValue.valOk;
          END;
        EXCEPT
        | QuadMeshGO.BadSize => 
          ObValue.RaiseException (BadSize, self.name & "_" & opCode.name, loc);
          <* ASSERT FALSE *>
        END;
      END;

    | Code.SetColorOfFacet =>
      WITH quadmesh = GetArg         (args, 1, self, opCode, loc),
           i        = ObInt.GetArg   (args, 2, self, opCode, loc),
           j        = ObInt.GetArg   (args, 3, self, opCode, loc),
           color    = ObColor.GetArg (args, 4, self, opCode, loc) DO
        TRY
          quadmesh.setColorOfFacet (i, j, color);
          RETURN ObValue.valOk;
        EXCEPT
        | QuadMeshGO.ColorsUndefined => 
          ObValue.RaiseException (ColorsUndefined, 
                                  self.name & "_" & opCode.name, loc);
          <* ASSERT FALSE *>
        END;
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
END ObQuadMeshGO.
