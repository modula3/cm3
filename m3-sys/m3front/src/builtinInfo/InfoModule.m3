(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE InfoModule;

IMPORT Scope, Tipe, Module, Constant, Target, EnumType;
IMPORT Type, Value, M3ID, Text, Error;
IMPORT InfoThisFile, InfoThisPath, InfoThisLine, InfoThisException;

CONST
  OS_names = ARRAY [0..1] OF TEXT { "POSIX", "WIN32" };

CONST
  Platform_names = Target.SystemNames;

PROCEDURE Initialize () =
  VAR zz: Scope.T;  os_type, platform_type: Type.T;  enum: Value.T;  nm: TEXT;
  BEGIN

    M := Module.NewDefn ("Compiler", TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual Compiler.i3 file, otherwise the version
        stamps will be messed up! *)

    zz := Scope.Push (Module.ExportScope (M));

    os_type := EnumType.Build (OS_names);
    Tipe.Define ("OS", os_type, FALSE);

    platform_type := EnumType.Build (Platform_names);
    Tipe.Define ("Platform", platform_type, FALSE);

    nm := OS_names [ORD (Text.Equal (Target.System_name, "NT386"))];
    IF NOT EnumType.LookUp (os_type, M3ID.Add (nm), enum) THEN
      Error.Txt (nm, "Unknown Compiler.OS value");
      <*ASSERT FALSE*>
    END;
    Constant.Declare ("ThisOS", Value.ToExpr (enum), FALSE);

    nm := Target.System_name;
    IF NOT EnumType.LookUp (platform_type, M3ID.Add (nm), enum) THEN
      Error.Txt (nm, "Unknown Compiler.Platform value"); 
      <*ASSERT FALSE*>
    END;
    Constant.Declare ("ThisPlatform", Value.ToExpr (enum), FALSE);

    InfoThisFile.Initialize ();
    InfoThisPath.Initialize ();
    InfoThisLine.Initialize ();
    InfoThisException.Initialize ();

    Scope.Pop (zz);
  END Initialize;

PROCEDURE Reset () =
  BEGIN
    InfoThisFile.Reset ();
    InfoThisPath.Reset ();
  END Reset;

BEGIN
END InfoModule.
