(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObLibOnlineHelp;
IMPORT SynWr, Text, ObLib, ObCommand, Bundle, ObliqBdl, ObliqBdl2,
 ObFrame, ObPrintValue, Obliq, ObValue;

  PROCEDURE Setup() =
  BEGIN
    ObLib.RegisterHelp("sysOnline", HelpOnline);
    ObLib.RegisterHelp("sys", HelpSys);
    ObLib.RegisterHelp("bool", HelpBool);
    ObLib.RegisterHelp("int", HelpInt);
    ObLib.RegisterHelp("real", HelpReal);
    ObLib.RegisterHelp("math", HelpMath);
    ObLib.RegisterHelp("ascii", HelpAscii);
    ObLib.RegisterHelp("text", HelpText);
    ObLib.RegisterHelp("array", HelpArray);
    ObLib.RegisterHelp("net", HelpNet);
    
    ObCommand.Register(ObLib.helpCommandSet,
      NEW(ObCommand.T, name:="syntax", sortingName:="syntax", 
        Exec:=PrintHelpSyntax));

    ObCommand.Register(ObLib.helpCommandSet,
      NEW(ObCommand.T, name:="flags", sortingName:="flags", 
        Exec:=PrintHelpFlags));

    ObCommand.Register(ObLib.helpCommandSet,
      NEW(ObCommand.T, name:="help", sortingName:=" help", 
        Exec:=PrintHelpHelp));

    ObCommand.Register(ObLib.helpCommandSet,
      NEW(ObCommand.T, name:="lib", sortingName:="lib      ",
        Exec:=PrintHelpLibrary));

    ObCommand.Register(ObLib.helpCommandSet,
      NEW(ObCommand.T, name:="env", sortingName:="env",
        Exec:=PrintHelpEnv));
  END Setup;

PROCEDURE PrintHelpSyntax(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, 
          "  syntax            (overview)\n" &
          "  syntax lexicon    (lexical matters)\n" &
          "  syntax real       (the one used by the parser)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqSyntax"));
	SynWr.NewLine(SynWr.out);
      ELSIF Text.Equal(arg, "lexicon") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqLexicon"));
	SynWr.NewLine(SynWr.out);
      ELSIF Text.Equal(arg, "real") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl.Get(),"ObliqGram"));
	SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END PrintHelpSyntax;

PROCEDURE PrintHelpFlags(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, 
          "  flags             (inspecting and setting system flags)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, 
          "  Say 'flag;' to list system flags and their values\n" &
          "  Say 'flag <name>;' to inspect a flag\n" &
          "  Say 'flag <name> <value>;' to set a flag\n" &
          "      (enclose <value> in double quotes if needed)\n");
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END PrintHelpFlags;

PROCEDURE PrintHelpHelp(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, "Say 'help <topic>;' or 'help <topic> <subtopic>;'\n");
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END PrintHelpHelp;

PROCEDURE PrintPkgList(pkg: ObLib.Env; name: TEXT; env: Obliq.Env) =
  VAR opCodes: REF ObLib.OpCodes; library: ObLib.T;
  BEGIN
    pkg := ObLib.Lookup(name, pkg);
    IF pkg = NIL THEN RETURN END;
    PrintPkgList(pkg.rest, name, env);
    library := pkg.library;
    opCodes := library.opCodes;
    FOR i:=NUMBER(opCodes^)-1 TO 0 BY -1 DO
      TYPECASE opCodes^[i] OF
      | NULL => 
      | ObFrame.FrameOpCode(opCode) =>
        SynWr.Text(SynWr.out, "  " & name & "_" & opCode.name & " = ");
        ObPrintValue.PrintValSummary(SynWr.out, opCode.val, env.libEnv, NIL);
	SynWr.NewLine(SynWr.out);
      ELSE
      END;
    END;
    FOR i:=0 TO NUMBER(opCodes^)-1 DO
      TYPECASE opCodes^[i] OF
      | NULL => 
      | ObFrame.FrameOpCode =>
      ELSE SynWr.Text(SynWr.out, "  " & name & "_" & opCodes^[i].name & "\n");
      END;
    END;
  END PrintPkgList;

PROCEDURE PrintHelpLibrary(<*UNUSED*>self: ObCommand.T; arg: TEXT; data: REFANY:=NIL) =
    VAR libEnv: ObLib.Env; sep: TEXT; 
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, 
          "  lib               (show a list of loaded libraries)\n" &
          "  lib <lib>         (show info about loaded library <lib>)\n");
      ELSIF Text.Equal(arg, "?") THEN
        TYPECASE data OF
        | NULL =>
        | Obliq.Env(env) =>
           SynWr.Text(SynWr.out, "  ");
           libEnv := env.libEnv;
           sep := "";
           LOOP
             IF libEnv=NIL THEN EXIT; END;
             SynWr.Text(SynWr.out, sep & libEnv.library.name);
             sep := ", ";
             libEnv := libEnv.rest;
           END;
           SynWr.Text(SynWr.out, "\n");
        ELSE
        END;
      ELSE
        TYPECASE data OF
        | NULL =>
        | Obliq.Env(env) =>
          IF ObLib.Lookup(arg, env.libEnv)=NIL THEN 
            SynWr.Text(SynWr.out, "  library not found\n")
          ELSE
            PrintPkgList(env.libEnv, arg, env);
          END;
        ELSE
        END;
	SynWr.NewLine(SynWr.out);
      END;
    END PrintHelpLibrary;

PROCEDURE PrintHelpEnv(self: ObCommand.T; arg: TEXT; data: REFANY:=NIL) =
    VAR valueEnv: ObValue.Env; 
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, 
          "  env               (show all the environment)\n");
      ELSIF Text.Equal(arg, "?") THEN
        TYPECASE data OF
        | NULL =>
        | Obliq.Env(env) =>
           valueEnv := env.valueEnv;
           LOOP
             IF valueEnv=NIL THEN EXIT END;
             SynWr.Text(SynWr.out, "  let " & valueEnv.name.text & " = ");
             ObPrintValue.PrintValSummary(SynWr.out, 
                 NARROW(valueEnv, ObValue.LocalEnv).val, env.libEnv, NIL);
	     SynWr.NewLine(SynWr.out);
             valueEnv := valueEnv.rest;
           END;
        ELSE
        END;
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END PrintHelpEnv;

(* ================== *)

  PROCEDURE HelpOnline(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  sysOnline         (the built-in system library, online extension)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpOnline"));
	SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpOnline;

  PROCEDURE HelpSys(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  sys               (the built-in system library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpSys"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpSys;

  PROCEDURE HelpBool(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  bool              (the built-in boolean library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpBool"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpBool;

  PROCEDURE HelpInt(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  int               (the built-in integer library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpInt"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpInt;

  PROCEDURE HelpReal(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  real              (the built-in real library, with int overloading)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpReal"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpReal;

  PROCEDURE HelpMath(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  math              (the built-in math library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpMath"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpMath;

  PROCEDURE HelpAscii(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  ascii             (the built-in ascii library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpChar"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpAscii;

  PROCEDURE HelpText(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  text              (the built-in text library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpText"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpText;

  PROCEDURE HelpArray(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  array             (the built-in array library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpArray"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpArray;

  PROCEDURE HelpNet(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  net               (the built-in network library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpNet"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpNet;

BEGIN
END ObLibOnlineHelp.
