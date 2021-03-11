(* $Id$ *)

INTERFACE ReadLineHelp;
IMPORT ReadLineHelpNode;
IMPORT ReadLine, ReadLineUI, ReadLineError;
IMPORT TextReader;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(root : ReadLineHelpNode.T) : T;

    command(r : TextReader.T; rl : ReadLine.Public) RAISES { ReadLineUI.Error,
                                                             ReadLineUI.Quit,
                                                             ReadLineError.E };

    (* the command is what comes after the "help", that is, if the user
       types "help a b c", the reader should contain "a b c" *)

    topCommand(r : TextReader.T; rl : ReadLine.Public) RAISES { ReadLineUI.Error,
                                                             ReadLineUI.Quit,
                                                             ReadLineError.E };
    (* normal entry point for external calls *)

  END;

CONST Brand = "ReadLineHelp";

END ReadLineHelp.
