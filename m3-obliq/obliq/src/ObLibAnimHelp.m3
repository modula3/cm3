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

  PROCEDURE HelpRects(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  rects             (the built-in rects library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpRects"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpRects;

  PROCEDURE HelpGraph(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  graph             (the built-in graph library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpGraph"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpGraph;

  PROCEDURE HelpZeus(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") THEN
        SynWr.Text(wr, "  zeus              (the built-in zeus library)\n");
      ELSIF Text.Equal(arg, "?") THEN
        SynWr.Text(wr, Bundle.Get(ObliqBdl2.Get(),"ObliqHelpZeus"));
        SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END HelpZeus;

BEGIN
END ObLibAnimHelp.
