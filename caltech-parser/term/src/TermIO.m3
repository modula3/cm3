MODULE TermIO;
IMPORT Term;
IMPORT Rd;
IMPORT Stdio;
IMPORT Thread;

<* FATAL Rd.EndOfFile, Rd.Failure, Thread.Alerted *>

VAR
  rawMutex := NEW(MUTEX);

TYPE
  Private = T OBJECT
  OVERRIDES
    getChar := GetChar;
    getLine := GetLine;
    putLine := PutLine;
    putText := PutText;
  END;

PROCEDURE GetChar(<*UNUSED*>self: T): CHAR =
  VAR
    result: CHAR;
  BEGIN
    LOCK rawMutex DO
      Term.MakeRaw(TRUE);
      result := Term.GetCharD();
      Term.MakeRaw(FALSE);
    END;
    RETURN result;
  END GetChar;

PROCEDURE GetLine(<*UNUSED*>self: T; prompt := ">"): TEXT =
  BEGIN
    Term.Wr(prompt);
    RETURN Rd.GetLine(Stdio.stdin);
  END GetLine;

PROCEDURE PutLine(<*UNUSED*>self: T; t: TEXT) =
  BEGIN
    Term.WrLn(t, TRUE);
  END PutLine;

PROCEDURE PutText(<*UNUSED*>self: T; t: TEXT) =
  BEGIN
    Term.Wr(t);
  END PutText;

BEGIN
  stdio := NEW(Private);
END TermIO.
