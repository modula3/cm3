(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3ASTPickleTool;

IMPORT Wr, FileWr, Pathname, Pickle, Fmt, Err, Text, OSError, Thread;
IMPORT M3Args, M3Conventions;
IMPORT M3Context, M3CUnit, M3CUnitRep, M3Extension, M3CGo, M3CId;
IMPORT M3AST_AS;
IMPORT M3AST_AS_F, M3AST_FE_F;
IMPORT M3ASTPickle;

CONST Version = "1-Dec-92";

VAR tool_g := M3Args.New("m3astpickle", "AST pickle tool", Version);

TYPE
  Extension = M3CGo.Extension OBJECT
  OVERRIDES extend := Extend;
  END;

PROCEDURE Init()=
  BEGIN
    M3CGo.AddExtension(NEW(Extension));
  END Init;

PROCEDURE DoUnit(cu: M3AST_AS.Compilation_Unit)=
  BEGIN
    PickleCu(cu, M3CUnit.ToType(cu.as_root));
  END DoUnit;

PROCEDURE Extend(
    <*UNUSED*> e: Extension;
    <*UNUSED*> context: M3Context.T;
               cu: M3AST_AS.Compilation_Unit;
    <*UNUSED*> VAR (*inout*) phases: M3CUnit.Status)=
  VAR ut := M3CUnit.ToType(cu.as_root);
  BEGIN
    IF M3Conventions.PrimarySource IN cu.fe_status THEN
      IF M3Args.Find(tool_g) THEN
        VAR alli := M3Args.GetFlag(tool_g, PickleAllInterfaces_Arg);
            allm := M3Args.GetFlag(tool_g, PickleAllModules_Arg);
        BEGIN
          IF (ut IN M3CUnit.Interfaces AND
              (alli OR InList(cu, ut))) OR
             (ut IN M3CUnit.Modules AND
               (allm OR InList(cu, ut))) THEN
            PickleCu(cu, ut);
          END;
        END
      END
    END;
  END Extend;

PROCEDURE InList(cu: M3AST_AS.Compilation_Unit; ut: M3CUnit.Type): BOOLEAN=
  VAR list: REF ARRAY OF TEXT;
      name: TEXT;
  BEGIN
    name := M3CId.ToText(cu.as_root.as_id.lx_symrep);
    IF ut IN M3CUnit.Interfaces THEN
      list := M3Args.GetStringList(tool_g, PickleInterfaces_Arg);
    ELSE
      list := M3Args.GetStringList(tool_g, PickleModules_Arg);
    END;
    IF list # NIL THEN
      FOR i := 0 TO NUMBER(list^)-1 DO
        IF Text.Equal(list[i], name) THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END InList;

PROCEDURE PickleCu(cu: M3AST_AS.Compilation_Unit; ut: M3CUnit.Type) =
  <*FATAL Thread.Alerted*>
  VAR e: M3Extension.T;
  BEGIN
    IF ut IN M3CUnit.Interfaces THEN e := M3Extension.T.PInt
    ELSE e := M3Extension.T.PMod
    END;
    VAR filename := M3Extension.Extend(Pathname.Base(cu.fe_uid.filename), e);
    VAR wr: Wr.T; 
    BEGIN
      TRY
        wr := FileWr.Open(filename);
        M3ASTPickle.Write(cu, wr);
        Wr.Close(wr);
      EXCEPT OSError.E, Wr.Failure, Pickle.Error =>
        Err.Print(Fmt.F("saving AST to file '%s' failed", filename),
                  Err.Severity.Error);
      END;
    END;
  END PickleCu;

BEGIN
  M3Args.RegisterFlag(tool_g, PickleAllInterfaces_Arg,
    "pickle all interfaces");
  M3Args.RegisterFlag(tool_g, PickleAllModules_Arg,
    "pickle all modules");
  M3Args.RegisterStringList(tool_g, PickleInterfaces_Arg, 
    "pickle given interfaces");
  M3Args.RegisterStringList(tool_g, PickleModules_Arg, 
    "pickle given modules");
END M3ASTPickleTool.
