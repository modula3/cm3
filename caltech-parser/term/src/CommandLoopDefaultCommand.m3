MODULE CommandLoopDefaultCommand EXPORTS CommandLoop;
IMPORT Term;
IMPORT TextList;


REVEAL
  Command = CommandPublic BRANDED OBJECT
  OVERRIDES
    execute             := Execute;
    complete            := Complete;
  END;

PROCEDURE Execute(<*UNUSED*>self: Command;
                  <*UNUSED*>args: TextList.T;
                  term: Term.T)
  <*NOWARN*> RAISES {Error} =
  BEGIN
    term.wr("not implemented yet - skip",TRUE,TRUE);
  END Execute;

PROCEDURE Complete(<*UNUSED*>self: Command;
                   <*UNUSED*>VAR input: TEXT)
  <*NOWARN*> RAISES {Error} =
  BEGIN
  END Complete;

BEGIN
END CommandLoopDefaultCommand.
