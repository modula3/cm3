(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon Aug  3 13:17:04 PDT 1992 by kalsow*)
(*      modified on Tue Jul 21 06:24:57 1992 by mhb *)

MODULE Parse;

IMPORT FormsVBT, VBT, Fmt, Token, Wr, TextWr, Thread;

<*FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

PROCEDURE Init (fv: FormsVBT.T): State =
  VAR
    s := NEW (State);
    input    : TEXT;
    cursor   : INTEGER;
    token    : Token.T;
    name     : TEXT;
  BEGIN
    s.states   := NIL;
    s.n_tokens := 0;
    s.input    := NEW (REF ARRAY OF Token.T, 20);
    s.tokens   := NEW (REF ARRAY OF TEXT, 20);

    (* read the string *)
    LOCK VBT.mu DO
      input := FormsVBT.GetText (fv, "input");
    END;

    (* convert it to a string of tokens *)
    cursor := 0;
    REPEAT
      Token.Scan (input, cursor, token, name);
      IF (s.n_tokens > LAST (s.input^)) THEN ExpandInput (s) END;
      s.input  [s.n_tokens] := token;
      s.tokens [s.n_tokens] := name;
      INC (s.n_tokens);
    UNTIL (token = Token.T.EOF);

    RETURN s;
  END Init;

PROCEDURE ExpandInput (s: State) =
  VAR new_input  := NEW (REF ARRAY OF Token.T, 2 * NUMBER (s.input^)); 
  VAR new_tokens := NEW (REF ARRAY OF TEXT, 2 * NUMBER (s.tokens^)); 
  BEGIN

    FOR i := FIRST (s.input^) TO LAST (s.input^) DO
      new_input[i] := s.input[i];
    END;
    s.input := new_input;

    FOR i := FIRST (s.tokens^) TO LAST (s.tokens^) DO
      new_tokens[i] := s.tokens[i];
    END;
    s.tokens := new_tokens;

  END ExpandInput;

PROCEDURE FmtState (s: State): TEXT =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR wr := TextWr.New ();
  BEGIN
    Wr.PutText (wr, "[ ");
    FOR i := 0 TO s.n_tokens - 1 DO
      Wr.PutText (wr, "<");
      Wr.PutText (wr, s.tokens[i]);
      Wr.PutText (wr, "> ");
    END;
    Wr.PutChar (wr, ']');
    RETURN TextWr.ToText (wr);
  END FmtState;

PROCEDURE FmtIntList (x: IntList): TEXT =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR wr := TextWr.New ();
  BEGIN
    Wr.PutText (wr, "[ ");
    FOR i := FIRST (x^) TO LAST (x^) DO
      Wr.PutText (wr, Fmt.Int (x[i]));
      Wr.PutChar (wr, ' ');
    END;
    Wr.PutChar (wr, ']');
    RETURN TextWr.ToText (wr);
  END FmtIntList;

BEGIN
END Parse.

