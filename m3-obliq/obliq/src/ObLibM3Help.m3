(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObLibM3Help;
IMPORT SynWr, Text, ObLib, ObCommand, Bundle, ObliqBdl2;

  PROCEDURE Setup() =
  BEGIN
    ObLib.RegisterHelp("rd", HelpRd);
    ObLib.RegisterHelp("wr", HelpWr);
    ObLib.RegisterHelp("lex", HelpLex);
    ObLib.RegisterHelp("fmt", HelpFmt);
    ObLib.RegisterHelp("pickle", HelpPickle);
    ObLib.RegisterHelp("process", HelpProcess);
    ObLib.RegisterHelp("thread", HelpThread);
    ObLib.RegisterHelp("random", HelpRandom);
    ObLib.RegisterHelp("tcp", HelpTcp);
    ObLib.RegisterHelp("word", HelpWord);
    ObLib.RegisterHelp("os", HelpOS);
    ObLib.RegisterHelp("dir", HelpDir);
    ObLib.RegisterHelp("path", HelpPath);
    ObLib.RegisterHelp("dict", HelpDict);
  END Setup;

  PROCEDURE HelpRd(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  rd                (the built-in reader library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpRd"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpRd;

  PROCEDURE HelpWr(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  wr                (the built-in writer library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpWr"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpWr;

  PROCEDURE HelpLex(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  lex               (the built-in lex library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpLex"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpLex;

  PROCEDURE HelpFmt(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  fmt               (the built-in fmt library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpFmt"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpFmt;

  PROCEDURE HelpPickle(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  pickle            (the built-in pickle library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpPickle"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpPickle;

  PROCEDURE HelpProcess(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  process           (the built-in process library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpProcess"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpProcess;

  PROCEDURE HelpThread(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  thread            (the built-in thread library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpThread"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpThread;

  PROCEDURE HelpRandom(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  random            (the built-in random library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpRandom"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpRandom;

  PROCEDURE HelpTcp(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  tcp               (the built-in tcp library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpTcp"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpTcp;

  PROCEDURE HelpWord(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  word              (the built-in word library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpWord"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpWord;

  PROCEDURE HelpOS(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  os               (the built-in os library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpOS"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpOS;

  PROCEDURE HelpDir(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  dir               (the built-in dir library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpDir"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpDir;

  PROCEDURE HelpPath(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  path              (the built-in path library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpPath"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpPath;

  PROCEDURE HelpDict(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  dict              (the built-in dict library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpDict"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpDict; 

BEGIN
END ObLibM3Help.
