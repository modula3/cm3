(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObLibUIHelp;
IMPORT SynWr, Text, ObLib, ObCommand, Bundle, ObliqBdl2;

  PROCEDURE Setup() =
  BEGIN
    ObLib.RegisterHelp("color", HelpColor);
    ObLib.RegisterHelp("vbt", HelpVBT);
    ObLib.RegisterHelp("form", HelpForm);
  END Setup;

  PROCEDURE HelpColor(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  color             (the built-in color library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpColor"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpColor;

  PROCEDURE HelpVBT(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  vbt               (the built-in vbt library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpVBT"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpVBT;

  PROCEDURE HelpForm(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  form              (the built-in forms library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpForm"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpForm;

BEGIN
END ObLibUIHelp.
