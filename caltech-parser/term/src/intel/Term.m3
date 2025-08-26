(* $Id: Term.m3,v 1.8 2005/05/24 16:39:38 kp Exp $ *)

UNSAFE MODULE Term EXPORTS Term, TermHooks;
IMPORT Debug;
IMPORT Stdio;
IMPORT Wr AS Wrr;
IMPORT Rd;
IMPORT Termios;
IMPORT Text;
IMPORT RTCollector;
IMPORT Thread;

<* FATAL Thread.Alerted, Wrr.Failure, Rd.Failure, Rd.EndOfFile *>

VAR
  Endl: TEXT := "\n";
  Raw: BOOLEAN := FALSE;
  TermCooked, TermRaw: Termios.T := NIL;

PROCEDURE MakeRaw(flag: BOOLEAN) =
  VAR
    termNew: Termios.T;
  BEGIN
    TRY
      RTCollector.Disable();
      IF flag # Raw THEN
        Raw := flag;
        Wrr.Flush(Stdio.stdout);
        IF Raw THEN
            IF TermCooked = NIL THEN
              TermCooked := NEW(Termios.T);
              TermRaw := NEW(Termios.T);
              Termios.tcgetattr(Termios.Stdin, TermCooked);
              TermRaw^ := TermCooked^;
              Termios.cfmakeraw(TermRaw);
            END;
            termNew := TermRaw;
            Endl := "\015\012";
          ELSE
            termNew := TermCooked;
            Endl := "\n";
          END;
        Termios.tcsetattr(Termios.Stdin, Termios.Tcsanow, termNew);
        END;
    FINALLY
      RTCollector.Enable()
    END
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

VAR
  gcHook: CharGetter := NIL;
PROCEDURE GetChar(): CHAR =
  BEGIN
    IF gcHook # NIL THEN RETURN gcHook.get(); END;
    Wrr.Flush(Stdio.stdout);
    RETURN Rd.GetChar(Stdio.stdin);
  END GetChar;

PROCEDURE SetCharInput(c: CharGetter) =
  BEGIN
    gcHook := c;
  END SetCharInput;

PROCEDURE Wr0(wr: Wrr.T; s: TEXT) =

  BEGIN
    IF Raw THEN
      VAR
        i,j: INTEGER := 0;
      BEGIN
        REPEAT
          j := Text.FindChar(s, '\n', i);
          IF j # -1 THEN
            Wrr.PutText(wr, Text.Sub(s, i, j - i));
            Wrr.PutText(wr, Endl);
            i := j + 1;
          END;
        UNTIL j = -1;
        IF i # Text.Length(s) THEN
          Wrr.PutText(wr, Text.Sub(s, i, LAST(CARDINAL)));
        END;
      END;
    ELSE
      Wrr.PutText(wr, s);
    END;
  END Wr0;

PROCEDURE Wr(s: TEXT; flush := FALSE) =
  BEGIN
    Wr0(Stdio.stdout, s);
    IF flush THEN
      Wrr.Flush(Stdio.stdout);
    END;
  END Wr;

PROCEDURE Wr1(s: TEXT) =
  BEGIN
    Wr0(Stdio.stderr, s);
    Wrr.Flush(Stdio.stderr);
  END Wr1;

PROCEDURE WrLn(s: TEXT; flush := FALSE) =
  BEGIN
    IF Text.FindChar(s, '\n') = -1 THEN
      Wrr.PutText(Stdio.stdout, s);
    ELSE
      Wr0(Stdio.stdout, s);
    END;
    Wrr.PutText(Stdio.stdout, Endl);
    IF flush THEN
      Wrr.Flush(Stdio.stdout);
    END;
  END WrLn;


BEGIN
  Debug.RegisterHook(Wr1);
END Term.
