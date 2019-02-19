(* $Id$ *)

INTERFACE ReadLineHelpClass;
IMPORT ReadLineHelp;
IMPORT ReadLineHelpNode AS Node;

TYPE
  Private = ReadLineHelp.Public OBJECT METHODS
    getRoot() : Node.T;
  END;

REVEAL ReadLineHelp.T <: Private;

END ReadLineHelpClass.
    
