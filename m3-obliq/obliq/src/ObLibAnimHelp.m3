(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObLibAnimHelp;
IMPORT SynWr, Text, ObLib, ObCommand, Bundle, ObliqBdl2;

  PROCEDURE Setup() =
  BEGIN
    ObLib.RegisterHelp("rects", HelpRects);
    ObLib.RegisterHelp("graph", HelpGraph);
    ObLib.RegisterHelp("zeus", HelpZeus);
  END Setup;

  PROCEDURE HelpRects(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  rects             (the built-in rects library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpRects"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpRects;

  PROCEDURE HelpGraph(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  graph             (the built-in graph library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpGraph"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpGraph;

  PROCEDURE HelpZeus(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(SynWr.out, "  zeus              (the built-in zeus library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(SynWr.out, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpZeus"));
        SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END HelpZeus;

BEGIN
END ObLibAnimHelp.
