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
  END Setup;

  PROCEDURE HelpRd(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  rd                (the built-in reader library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpRd"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpRd;

  PROCEDURE HelpWr(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  wr                (the built-in writer library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpWr"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpWr;

  PROCEDURE HelpLex(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  lex               (the built-in lex library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpLex"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpLex;

  PROCEDURE HelpFmt(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  fmt               (the built-in fmt library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpFmt"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpFmt;

  PROCEDURE HelpPickle(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  pickle            (the built-in pickle library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpPickle"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpPickle;

  PROCEDURE HelpProcess(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  process           (the built-in process library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpProcess"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpProcess;

  PROCEDURE HelpThread(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  thread            (the built-in thread library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpThread"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpThread;

  PROCEDURE HelpRandom(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  random            (the built-in random library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpRandom"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpRandom;

BEGIN
END ObLibM3Help.
