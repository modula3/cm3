MODULE ObArrayRefanyT;

IMPORT Obliq, Quaternion, ObCommand, ObLib,
       ObValue, SynLocation, ObMatrix4, ObPoint3, ObLongReal,
       ObLoader, ObQuaternion, Time, ObBool;
IMPORT Refany, ObRefany;

PROCEDURE M3ToObliq(READONLY array: ARRAY OF Refany.T): T =
  VAR vals: REF Vals;
  BEGIN
    vals := NEW(REF Vals, NUMBER(array));
    FOR i:=0 TO NUMBER(array)-1 DO 
      vals[i] := ObRefany.M3ToObliq(array[i]);
    END;
    RETURN ObValue.NewArrayFromVals(vals);
  END M3ToObliq;

PROCEDURE ObliqToM3(val: Val): ARRAY OF Refany.T RAISES {ObValue.Error} =
  VAR size: INTEGER; vals: REF Vals;
      array: REF
  BEGIN
    size := ArraySize(val, loc);
    vals := NEW(REF Vals, size);
    ToArray(val, (*out*) vals^, loc);
    FOR i:=0 TO size-1 DO array[i] := ToInt(vals[i], loc) END;
  END ToIntArray;


REVEAL
  T = ObValue.ValAnything BRANDED "ObArrayRefanyT.T" OBJECT
    data: Data;
  OVERRIDES
    Is := IsData; 
    Copy := CopyData;
  END;

PROCEDURE IsData(self: T; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF 
    | T(oth) => RETURN self.data = oth.data;
    ELSE 
      RETURN FALSE
    END;
  END IsData;

PROCEDURE CopyData(self: T; <*UNUSED*> tbl: ObValue.Tbl;
                   <*UNUSED*> loc: SynLocation.T): ObValue.ValAnything =
  VAR newdata := NEW(Data);
  BEGIN
    newdata^ := self.data^;
    RETURN NEW(T, what := self.what, data := newdata, 
               picklable := self.picklable);
  END CopyData;

PROCEDURE M3ToObliq (READONLY val : Data) : T =
  BEGIN
    RETURN NEW (T, what := "<a ArrayRefanyT>", data := val, 
                picklable := TRUE);
  END M3ToObliq;

PROCEDURE ObliqToM3 (val : T) : Data =
  BEGIN
    RETURN val.data;
  END ObliqToM3;

VAR garbage: Data;

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Data 
  RAISES {ObValue.Error} =
  BEGIN
    TYPECASE args[idx] OF 
    | T (node) => 
      RETURN node.data;
    ELSE 
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
      RETURN garbage;     (* ... only to suppress compiler warning *)
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
            NewOpCode ("New",            0, Code.New),
            NewOpCode ("GetRotationM",   1, Code.GetRotMatrix),
            NewOpCode ("GetRotationQ",   1, Code.GetRotQuat),
            NewOpCode ("GetPosition",    1, Code.GetPosition),
            NewOpCode ("GetTime",        1, Code.GetTime),
            NewOpCode ("GetOut",         1, Code.GetOut),
            NewOpCode ("GetButtons",     1, Code.GetButtons),
            NewOpCode ("SetRotation",    2, Code.SetRot),
            NewOpCode ("SetPosition",    2, Code.SetPosition),
            NewOpCode ("SetTime",        2, Code.SetTime),
            NewOpCode ("SetTimeNow",     1, Code.SetTimeNow),
            NewOpCode ("SetOut",         2, Code.SetOut),
            NewOpCode ("SetButtons",     2, Code.SetButtons)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;

PROCEDURE SetupModule (loader: ObLoader.T) =
  BEGIN
    pkgloader := loader;
  END SetupModule;

TYPE
  Code = {New, GetRotMatrix, GetRotQuat, GetPosition, GetTime, GetOut,
          GetButtons, SetRot, SetPosition, SetTime, SetTimeNow,
          SetOut, SetButtons}; 

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;

  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;

CONST
  pkgname = "ArrayRefanyT";

PROCEDURE DoEval (self         : Package; 
                  opCode       : ObLib.OpCode; 
     <* UNUSED *> arity        : ObLib.OpArity; 
                  READONLY args: ObValue.ArgArray; 
                  temp         : BOOLEAN;
                  loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  VAR arg: Data; longreal1 : ObValue.ValReal;
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.New =>
      arg := NEW(Data);
      RETURN M3ToObliq(arg);
    | Code.GetRotMatrix =>
      arg := GetArg (args, 1, self, opCode, loc);
      RETURN ObMatrix4.M3ToObliq (arg.rotation_m);
    | Code.GetRotQuat =>
      arg := GetArg (args, 1, self, opCode, loc);
      RETURN ObQuaternion.M3ToObliq (arg.rotation_q);
    | Code.GetPosition =>
      arg := GetArg (args, 1, self, opCode, loc);
      RETURN ObPoint3.M3ToObliq (arg.position);
    | Code.GetTime =>
      arg := GetArg (args, 1, self, opCode, loc);
      longreal1 := ObLongReal.M3ToObliq (arg.time);
      longreal1.temp := temp;
      RETURN longreal1;
    | Code.GetOut =>
      arg := GetArg (args, 1, self, opCode, loc);
      RETURN NEW(ObValue.ValBool, bool := arg.out);
    | Code.GetButtons =>
      arg := GetArg (args, 1, self, opCode, loc);
      WITH obj =  Obliq.NewObject(ObValue.ObjFields{}) DO
        Obliq.ObjectUpdate(obj, "suspend", Obliq.NewBool(arg.suspend));
        Obliq.ObjectUpdate(obj, "left", Obliq.NewBool(arg.left));
        Obliq.ObjectUpdate(obj, "middle", Obliq.NewBool(arg.middle));
        Obliq.ObjectUpdate(obj, "right", Obliq.NewBool(arg.right));
        RETURN obj;
      END;
    | Code.SetRot =>
      arg := GetArg (args, 1, self, opCode, loc);
      TYPECASE args[2] OF 
      | ObQuaternion.T => 
        WITH quat = ObQuaternion.GetArg(args, 2, self, opCode, loc),
             mat = Quaternion.ToMatrix4(quat) DO
          arg.rotation_m := mat;
          arg.rotation_q := quat;
        END;
      | ObMatrix4.T => 
        WITH mat = ObMatrix4.GetArg(args, 2, self, opCode, loc),
             quat = Quaternion.FromMatrix4(mat) DO
          arg.rotation_m := mat;
          arg.rotation_q := quat;
        END;
      ELSE 
        ObValue.BadArgType (2, "Quaternion or Matrix4", 
                            self.name, opCode.name, loc); 
      END;
      RETURN ObValue.valOk;
    | Code.SetPosition =>
      arg := GetArg (args, 1, self, opCode, loc);
      WITH pos = ObPoint3.GetArg(args, 2, self, opCode, loc) DO
        arg.position := pos;
        RETURN ObValue.valOk;
      END;
    | Code.SetTime =>
      arg := GetArg (args, 1, self, opCode, loc);
      WITH time = ObLongReal.GetArg(args, 2, self, opCode, loc) DO
        arg.time := time;
        RETURN ObValue.valOk;
      END;
    | Code.SetTimeNow =>
      arg := GetArg (args, 1, self, opCode, loc);
      arg.time := Time.Now();
      RETURN ObValue.valOk;
    | Code.SetOut =>
      arg := GetArg (args, 1, self, opCode, loc);
      WITH bool = ObBool.GetArg(args, 2, self, opCode, loc) DO
        arg.out := bool;
        RETURN ObValue.valOk;
      END;
    | Code.SetButtons =>
      arg := GetArg (args, 1, self, opCode, loc);
      TYPECASE args[2] OF
      | ObValue.ValObj(node) => 
        IF Obliq.ObjectHas(node, "suspend") THEN
          WITH suspendF = Obliq.ObjectSelect(node, "suspend") DO
            TYPECASE suspendF OF
            | ObValue.ValBool(b) => arg.suspend := b.bool;
            ELSE ObValue.BadArgType(2, "bool field", self.name, 
                                    opCode.name, loc);
            END;
          END;
        END;
        IF Obliq.ObjectHas(node, "left") THEN
          WITH leftF = Obliq.ObjectSelect(node, "left") DO
            TYPECASE leftF OF
            | ObValue.ValBool(b) => arg.left := b.bool;
            ELSE ObValue.BadArgType(2, "bool field", self.name, 
                                    opCode.name, loc);
            END;
          END;
        END;
        IF Obliq.ObjectHas(node, "middle") THEN
          WITH middleF = Obliq.ObjectSelect(node, "middle") DO
            TYPECASE middleF OF
            | ObValue.ValBool(b) => arg.middle := b.bool;
            ELSE ObValue.BadArgType(2, "bool field", self.name, 
                                    opCode.name, loc);
            END;
          END;
        END;
        IF Obliq.ObjectHas(node, "right") THEN
          WITH rightF = Obliq.ObjectSelect(node, "right") DO
            TYPECASE rightF OF
            | ObValue.ValBool(b) => arg.right := b.bool;
            ELSE ObValue.BadArgType(2, "bool field", self.name, 
                                    opCode.name, loc);
            END;
          END;
        END;
      ELSE
        ObValue.BadArgType(2, "object", self.name, opCode.name, loc); 
      END;
      RETURN ObValue.valOk;
    END;
  END DoEval;

(*****************************************************************************)
(* Help                                                                      *)
(*****************************************************************************)

PROCEDURE Help (self : ObCommand.T; arg : TEXT; <* UNUSED *> data : REFANY) =
  BEGIN
    IF pkgloader # NIL THEN
      pkgloader.help (self, arg, pkgname);
    END;
  END Help;

VAR pkgloader: ObLoader.T := NIL;

BEGIN
END ObArrayRefanyT.
