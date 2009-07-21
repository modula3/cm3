(* $Id$ *)

UNSAFE MODULE Term;
IMPORT Stdio;
IMPORT Wr AS Wrr;
IMPORT Rd;
IMPORT Termios;
IMPORT Text;
IMPORT Thread;
IMPORT TermC;

<* FATAL Thread.Alerted, Wrr.Failure, Rd.Failure, Rd.EndOfFile *>

VAR
  Endl: TEXT := "\n";
  Raw: BOOLEAN := FALSE;

PROCEDURE MakeRaw(flag: BOOLEAN) =
  VAR
    termNew: Termios.T;
  BEGIN
    IF flag # Raw THEN
      Raw := flag;
      Wrr.Flush(Stdio.stdout);
      IF Raw THEN
          termNew := TermC.GetTermRaw();
          Endl := "\015\012";
        ELSE
          termNew := TermC.GetTermCooked();
          Endl := "\n";
        END;
      Termios.tcsetattr(Termios.Stdin, Termios.Tcsanow, termNew);
      END;
  END MakeRaw;

PROCEDURE GetCharDR(): CHAR RAISES {SpecialChar} =
  BEGIN
    TRY
      RETURN GetCharE(RawSpecial);
    EXCEPT
      SpecialChar => 
    END;
    MakeRaw(FALSE);
    RAISE SpecialChar;
  END GetCharDR;

PROCEDURE GetCharD(): CHAR =
  <* FATAL SpecialChar *>
  BEGIN
    IF Raw THEN
      RETURN GetCharDR()
    ELSE
      RETURN GetChar();
    END;
  END GetCharD;

PROCEDURE GetCharE(special: TEXT): CHAR RAISES {SpecialChar} =
  VAR
    cin: CHAR;
  BEGIN
    cin := GetChar();
    IF Text.FindChar(special, cin) = -1 THEN
      RETURN cin;
    ELSE
      RAISE SpecialChar;
    END;
  END GetCharE;

PROCEDURE GetChar(): CHAR =
  BEGIN
    Wrr.Flush(Stdio.stdout);
    RETURN Rd.GetChar(Stdio.stdin);
  END GetChar;

PROCEDURE Wr(s: TEXT) =

  BEGIN
    IF Raw THEN
      VAR
        i,j: INTEGER := 0;
      BEGIN
        REPEAT
          j := Text.FindChar(s, '\n', i);
          IF j # -1 THEN
            Wrr.PutText(Stdio.stdout, Text.Sub(s, i, j - i));
            Wrr.PutText(Stdio.stdout, Endl);
            i := j + 1;
          END;
        UNTIL j = -1;
        IF i # Text.Length(s) THEN
          Wrr.PutText(Stdio.stdout, Text.Sub(s, i, LAST(CARDINAL)));
        END;
      END;
    ELSE
      Wrr.PutText(Stdio.stdout, s);
    END;
  END Wr;

PROCEDURE WrLn(s: TEXT; flush := FALSE) =
  BEGIN
    Wrr.PutText(Stdio.stdout, s);
    Wrr.PutText(Stdio.stdout, Endl);
    IF flush THEN
      Wrr.Flush(Stdio.stdout);
    END;
  END WrLn;

BEGIN
    TermC.Init();
END Term.
