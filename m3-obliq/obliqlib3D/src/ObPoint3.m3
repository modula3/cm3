(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Feb 16 20:43:10 PST 1995 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)


MODULE ObPoint3;


IMPORT ObAux, ObCommand, ObLib, ObReal, ObValue, Obliq, Point3, SynLocation;


CONST
  pkgname = "Point3";

(*****************************************************************************)
(* Wrapper for Point3.T                                                      *)
(*****************************************************************************)

REVEAL 
  T = ObValue.ValArray;


PROCEDURE M3ToObliq (val : Point3.T) : T =
  BEGIN
    WITH x = ObReal.M3ToObliq (val.x),
         y = ObReal.M3ToObliq (val.y),
         z = ObReal.M3ToObliq (val.z) DO
      RETURN Obliq.NewArray (Obliq.Vals {x, y, z});
    END;
  END M3ToObliq;


PROCEDURE ObliqToM3 (val : T) : Point3.T RAISES {ObValue.Error} =
  VAR
    vals : ARRAY [0 .. 2] OF Obliq.Val;
  BEGIN
    Obliq.ToArray (val, vals);
    WITH x = ObReal.ObliqToM3 (vals[0]),
         y = ObReal.ObliqToM3 (vals[1]),
         z = ObReal.ObliqToM3 (vals[2]) DO
      RETURN Point3.T {x, y, z};
    END;
  END ObliqToM3;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Point3.T RAISES {ObValue.Error} =
  VAR
    vals : ARRAY [0 .. 2] OF Obliq.Val;
  BEGIN
    TRY
      Obliq.ToArray (args[idx], vals);
      WITH x = ObReal.ObliqToM3 (vals[0]),
           y = ObReal.ObliqToM3 (vals[1]),
           z = ObReal.ObliqToM3 (vals[2]) DO
        RETURN Point3.T {x, y, z};
      END;
    EXCEPT
    | ObValue.Error =>
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
      RETURN Point3.Origin;   (* ... only to suppress compiler warning *)
    END;
  END GetArg;


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
            NewOpCode ("Plus",        2, Code.Plus),
            NewOpCode ("Minus",       2, Code.Minus),
            NewOpCode ("ScaleToLen",  2, Code.ScaleToLen),
            NewOpCode ("TimesScalar", 2, Code.TimesScalar),
            NewOpCode ("Length",      1, Code.Length),
            NewOpCode ("MidPoint",    2, Code.MidPoint),
            NewOpCode ("Distance",    2, Code.Distance)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


(*****************************************************************************)
(* Setup procedures                                                          *)
(*****************************************************************************)


TYPE
  Code = {Plus, Minus, ScaleToLen, TimesScalar, 
          Length, MidPoint, Distance};

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
    RAISES {ObValue.Error} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.Plus =>
      WITH p1 = GetArg (args, 1, self, opCode, loc),
           p2 = GetArg (args, 2, self, opCode, loc) DO
        RETURN M3ToObliq (Point3.Plus (p1, p2));
      END;
    | Code.Minus =>
      WITH p1 = GetArg (args, 1, self, opCode, loc),
           p2 = GetArg (args, 2, self, opCode, loc) DO
        RETURN M3ToObliq (Point3.Minus (p1, p2));
      END;
    | Code.ScaleToLen =>
      WITH p =        GetArg (args, 1, self, opCode, loc),
           s = ObReal.GetArg (args, 2, self, opCode, loc) DO
        RETURN M3ToObliq (Point3.ScaleToLen (p, s));
      END;
    | Code.TimesScalar =>
      WITH p =        GetArg (args, 1, self, opCode, loc),
           s = ObReal.GetArg (args, 2, self, opCode, loc) DO
        RETURN M3ToObliq (Point3.TimesScalar (p, s));
      END;
    | Code.Length =>
      WITH p = GetArg (args, 1, self, opCode, loc) DO
        RETURN ObReal.M3ToObliq (Point3.Length (p));
      END;
    | Code.MidPoint => 
      WITH a = GetArg (args, 1, self, opCode, loc),
           b = GetArg (args, 2, self, opCode, loc) DO
        RETURN M3ToObliq (Point3.MidPoint (a, b));
      END;
    | Code.Distance =>
      WITH a = GetArg (args, 1, self, opCode, loc),
           b = GetArg (args, 2, self, opCode, loc) DO
        RETURN ObReal.M3ToObliq (Point3.Distance (a, b));
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
END ObPoint3.
