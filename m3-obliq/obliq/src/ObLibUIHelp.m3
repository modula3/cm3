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

  PROCEDURE HelpColor(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  color             (the built-in color library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpColor"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpColor;

  PROCEDURE HelpVBT(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  vbt               (the built-in vbt library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpVBT"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpVBT;

  PROCEDURE HelpForm(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  form              (the built-in forms library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpForm"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpForm;

BEGIN
END ObLibUIHelp.
