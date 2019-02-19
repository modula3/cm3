(* $Id$ *)

INTERFACE CSVParse;
IMPORT Rd, FloatMode, Lex, Thread;

EXCEPTION EndOfLine;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(rd : Rd.T) : T;

    startLine() RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };
    (* must be called at start, too *)

    cell() : TEXT RAISES { EndOfLine };
    (* get the next cell from the current line, or raise EndOfLine *)
    
    cellB(VAR cell : TEXT) : BOOLEAN;
    (* get the next cell from the current line, or return FALSE *)
    
    int() : INTEGER RAISES { EndOfLine, FloatMode.Trap, Lex.Error };

    lr() : LONGREAL RAISES { EndOfLine, FloatMode.Trap, Lex.Error };

    whatLine() : CARDINAL;

    lastCell() : TEXT;
    (* the last cell seen *)
    
    lastLine() : TEXT;
    (* the last line seen *)
  END;    

CONST Brand = "CSVParse";

END CSVParse.

    
