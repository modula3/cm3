(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObLibOnlineHelp;
IMPORT SynWr, Text, ObLib, ObCommand, Bundle, ObliqBdl, ObliqBdl2,
 ObFrame, ObPrintValue, Obliq, ObValue;

  PROCEDURE Setup() =
  BEGIN
    ObLib.RegisterHelp("sysOnline", HelpOnline);
    ObLib.RegisterHelp("sys", HelpSys);
    ObLib.RegisterHelp("debug", HelpDebug);
    ObLib.RegisterHelp("bool", HelpBool);
    ObLib.RegisterHelp("int", HelpInt);
    ObLib.RegisterHelp("real", HelpReal);
    ObLib.RegisterHelp("math", HelpMath);
    ObLib.RegisterHelp("ascii", HelpAscii);
    ObLib.RegisterHelp("text", HelpText);
    ObLib.RegisterHelp("array", HelpArray);
    ObLib.RegisterHelp("net", HelpNet);
    ObLib.RegisterHelp("replica", HelpReplica);
    ObLib.RegisterHelp("regex", HelpRegEx);
    ObLib.RegisterHelp("reflect", HelpReflect);
    
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

PROCEDURE PrintHelpSyntax(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, 
          "  syntax            (overview)\n" &
          "  syntax lexicon    (lexical matters)\n" &
          "  syntax real       (the one used by the parser)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqSyntax"));
	SynWr.NewLine(wr);
      ELSIF Text.Equal(arg, "lexicon") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqLexicon"));
	SynWr.NewLine(wr);
      ELSIF Text.Equal(arg, "real") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl.Get(),"ObliqGram"));
	SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END PrintHelpSyntax;

PROCEDURE PrintHelpFlags(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, 
          "  flags             (inspecting and setting system flags)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, 
          "  Say 'flag;' to list system flags and their values\n" &
          "  Say 'flag <name>;' to inspect a flag\n" &
          "  Say 'flag <name> <value>;' to set a flag\n" &
          "      (enclose <value> in double quotes if needed)\n");
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END PrintHelpFlags;

PROCEDURE PrintHelpHelp(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
        SynWr.Text(wr, "Say 'help <topic>;' or 'help <topic> <subtopic>;'\n");
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END PrintHelpHelp;

PROCEDURE PrintPkgList(wr: SynWr.T; pkg: ObLib.Env; name: TEXT; env: Obliq.Env) =
  VAR opCodes: REF ObLib.OpCodes; library: ObLib.T;
  BEGIN
    pkg := ObLib.Lookup(name, pkg);
    IF pkg = NIL THEN RETURN END;
    PrintPkgList(wr, pkg.rest, name, env);
    library := pkg.library;
    opCodes := library.opCodes;
    FOR i:=NUMBER(opCodes^)-1 TO 0 BY -1 DO
      TYPECASE opCodes^[i] OF
      | NULL => 
      | ObFrame.FrameOpCode(opCode) =>
        SynWr.Text(wr, "  " & name & "_" & opCode.name & " = ");
        ObPrintValue.PrintValSummary(wr, opCode.val, env.libEnv, NIL);
	SynWr.NewLine(wr);
      ELSE
      END;
    END;
    FOR i:=0 TO NUMBER(opCodes^)-1 DO
      TYPECASE opCodes^[i] OF
      | NULL => 
      | ObFrame.FrameOpCode =>
      ELSE SynWr.Text(wr, "  " & name & "_" & opCodes^[i].name & "\n");
      END;
    END;
  END PrintPkgList;

PROCEDURE PrintHelpLibrary(wr: SynWr.T; <*UNUSED*>self: ObCommand.T; arg: TEXT; data: REFANY:=NIL) =
    VAR libEnv: ObLib.Env; sep: TEXT; 
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, 
          "  lib               (show a list of loaded libraries)\n" &
          "  lib <lib>         (show info about loaded library <lib>)\n");
      ELSIF Text.Equal(arg, "?") THEN
        TYPECASE data OF
        | NULL =>
        | Obliq.Env(env) =>
           SynWr.Text(wr, "  ");
           libEnv := env.libEnv;
           sep := "";
           LOOP
             IF libEnv=NIL THEN EXIT; END;
             SynWr.Text(wr, sep & libEnv.library.name);
             sep := ", ";
             libEnv := libEnv.rest;
           END;
           SynWr.Text(wr, "\n");
        ELSE
        END;
      ELSE
        TYPECASE data OF
        | NULL =>
        | Obliq.Env(env) =>
          IF ObLib.Lookup(arg, env.libEnv)=NIL THEN 
            SynWr.Text(wr, "  library not found\n")
          ELSE
            PrintPkgList(wr, env.libEnv, arg, env);
          END;
        ELSE
        END;
	SynWr.NewLine(wr);
      END;
    END PrintHelpLibrary;

PROCEDURE PrintHelpEnv(wr: SynWr.T; self: ObCommand.T; arg: TEXT; data: REFANY:=NIL) =
    VAR valueEnv: ObValue.Env; 
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, 
          "  env               (show all the environment)\n");
      ELSIF Text.Equal(arg, "?") THEN
        TYPECASE data OF
        | NULL =>
        | Obliq.Env(env) =>
           valueEnv := env.valueEnv;
           LOOP
             IF valueEnv=NIL THEN EXIT END;
             SynWr.Text(wr, "  let " & valueEnv.name.text & " = ");
             ObPrintValue.PrintValSummary(wr, 
                 NARROW(valueEnv, ObValue.LocalEnv).val, env.libEnv, NIL);
	     SynWr.NewLine(wr);
             valueEnv := valueEnv.rest;
           END;
        ELSE
        END;
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END PrintHelpEnv;

(* ================== *)

  PROCEDURE HelpOnline(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  sysOnline         (the built-in system library, online extension)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpOnline"));
	SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpOnline;

  PROCEDURE HelpSys(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  sys               (the built-in system library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpSys"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpSys;

  PROCEDURE HelpDebug(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  debug             (the built-in system debugging library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpDebug"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpDebug;

  PROCEDURE HelpBool(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  bool              (the built-in boolean library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpBool"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpBool;

  PROCEDURE HelpInt(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  int               (the built-in integer library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpInt"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpInt;

  PROCEDURE HelpReal(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  real              (the built-in real library, with int overloading)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpReal"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpReal;

  PROCEDURE HelpMath(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  math              (the built-in math library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpMath"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpMath;

  PROCEDURE HelpAscii(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  ascii             (the built-in ascii library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpChar"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpAscii;

  PROCEDURE HelpText(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  text              (the built-in text library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpText"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpText;

  PROCEDURE HelpArray(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  array             (the built-in array library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpArray"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpArray;

  PROCEDURE HelpNet(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  net               (the built-in network library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpNet"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpNet;

  PROCEDURE HelpReplica(wr: SynWr.T; self: ObCommand.T; arg: TEXT; 
                       <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  replica           (the built-in replicated object library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpReplica"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpReplica; 

  PROCEDURE HelpRegEx(wr: SynWr.T; self: ObCommand.T; arg: TEXT; 
                      <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  regex             (the built-in regular expression library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpRegEx"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpRegEx;

  PROCEDURE HelpReflect(wr: SynWr.T; self: ObCommand.T; arg: TEXT; 
                        <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  reflect           (the built-in reflection library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpReflect"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpReflect; 

BEGIN
END ObLibOnlineHelp.
