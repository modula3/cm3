(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 22 12:02:14 PDT 1995 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)


MODULE ObMatrix4;


IMPORT Matrix4, ObAux, ObCommand, ObLib, ObPoint3, ObReal, ObValue, 
       SynLocation;


REVEAL 
  T = ObValue.ValAnything BRANDED OBJECT
    matrix : Matrix4.T;
  OVERRIDES
    Is := DoIs;
  END;

TYPE
  Code = {Error, Id, Multiply, Translate, Scale, RotateX, RotateY, RotateZ, 
          MapPoint3, Invert};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;

  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;

CONST
  pkgname = "Matrix4";


VAR 
  Error : ObValue.ValException;


PROCEDURE DoIs (self: T; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      T (oth) => RETURN self.matrix = oth.matrix;
    ELSE 
      RETURN FALSE 
    END;
  END DoIs;


PROCEDURE M3ToObliq (READONLY val : Matrix4.T) : T =
  BEGIN
    RETURN NEW (T, what := "<a Matrix4.T>", matrix := val);
  END M3ToObliq;


PROCEDURE ObliqToM3 (val : T) : Matrix4.T =
  BEGIN
    RETURN val.matrix;
  END ObliqToM3;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Matrix4.T RAISES {ObValue.Error} =
  BEGIN
    TYPECASE args[idx] OF 
    | T (node) => 
      RETURN node.matrix;
    ELSE 
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
      RETURN Matrix4.Id;     (* ... only to suppress compiler warning *)
    END;
  END GetArg;


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
            NewOpCode ("Error",    -1, Code.Error),
            NewOpCode ("Id",       -1, Code.Id),
            NewOpCode ("Multiply",  2, Code.Multiply),
            NewOpCode ("Translate", 4, Code.Translate),
            NewOpCode ("Scale",     4, Code.Scale),
            NewOpCode ("RotateX",   2, Code.RotateX),
            NewOpCode ("RotateY",   2, Code.RotateY),
            NewOpCode ("RotateZ",   2, Code.RotateZ),
            NewOpCode ("MapPoint3", 2, Code.MapPoint3),
            NewOpCode ("Invert",    1, Code.Invert)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);

    Error := 
        NEW (ObValue.ValException, name := pkgname & "_Error");
  END SetupPackage;


PROCEDURE DoEval (self         : Package; 
                  opCode       : ObLib.OpCode; 
     <* UNUSED *> arity        : ObLib.OpArity; 
                  READONLY args: ObValue.ArgArray; 
     <* UNUSED *> temp         : BOOLEAN;
                  loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.Error => 
      RETURN Error;
    | Code.Id =>
      RETURN M3ToObliq (Matrix4.Id);
    | Code.Multiply =>
      WITH m1 = GetArg (args, 1, self, opCode, loc),
           m2 = GetArg (args, 2, self, opCode, loc) DO
        RETURN M3ToObliq (Matrix4.Multiply (m1, m2));
      END;
    | Code.Translate =>
      WITH m = GetArg        (args, 1, self, opCode, loc),
           x = ObReal.GetArg (args, 2, self, opCode, loc),
           y = ObReal.GetArg (args, 3, self, opCode, loc),
           z = ObReal.GetArg (args, 4, self, opCode, loc) DO
        RETURN M3ToObliq (Matrix4.Translate (m, x, y, z));
      END;
    | Code.Scale =>
      WITH m = GetArg        (args, 1, self, opCode, loc),
           x = ObReal.GetArg (args, 2, self, opCode, loc),
           y = ObReal.GetArg (args, 3, self, opCode, loc),
           z = ObReal.GetArg (args, 4, self, opCode, loc) DO
        RETURN M3ToObliq (Matrix4.Scale (m, x, y, z));
      END;
    | Code.RotateX =>
      WITH m = GetArg        (args, 1, self, opCode, loc),
           a = ObReal.GetArg (args, 2, self, opCode, loc) DO
        RETURN M3ToObliq (Matrix4.RotateX (m, a));
      END;
    | Code.RotateY =>
      WITH m = GetArg        (args, 1, self, opCode, loc),
           a = ObReal.GetArg (args, 2, self, opCode, loc) DO
        RETURN M3ToObliq (Matrix4.RotateY (m, a));
      END;
    | Code.RotateZ =>
      WITH m = GetArg        (args, 1, self, opCode, loc),
           a = ObReal.GetArg (args, 2, self, opCode, loc) DO
        RETURN M3ToObliq (Matrix4.RotateZ (m, a));
      END;
    | Code.MapPoint3 =>
      WITH m = GetArg (args, 1, self, opCode, loc),
           p = ObPoint3.GetArg (args, 2, self, opCode, loc) DO
        RETURN ObPoint3.M3ToObliq (Matrix4.TransformPoint3 (m, p));
      END;
    | Code.Invert =>
      WITH m = GetArg (args, 1, self, opCode, loc) DO
        TRY
          RETURN M3ToObliq (Matrix4.Invert (m));
        EXCEPT
          Matrix4.Error => 
          ObValue.RaiseException (Error, opCode.name, loc);
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
END ObMatrix4.
