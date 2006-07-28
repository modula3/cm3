MODULE UnitTestTerminalText;

IMPORT IO, TextExtras;

REVEAL
  T = Public BRANDED OBJECT
        indentation: CARDINAL;
      OVERRIDES
        init     := Init;
        put      := Put;
        indent   := Indent;
        deindent := Deindent;
      END;

PROCEDURE Init (SELF: T; ): T =
  BEGIN
    RETURN SELF;
  END Init;

PROCEDURE Put (SELF: T; message: TEXT; ) =
  VAR i0, i1: CARDINAL := 0;
  BEGIN
    WHILE TextExtras.FindChar(message, '\n', i1) DO
      INC(i1);
      FOR j := 0 TO SELF.indentation - 1 DO IO.Put("  "); END;
      IO.Put(TextExtras.Extract(message, i0, i1));
      i0 := i1;
    END;
    (* Shall we indent an unterminated line?  This would also indent the
       empty string between a terminating newline and the end of the
       text. *)
    IO.Put(TextExtras.Extract(message, i0, i1));
  END Put;

PROCEDURE Indent (SELF: T; ) =
  BEGIN
    INC(SELF.indentation);
  END Indent;

PROCEDURE Deindent (SELF: T; ) =
  BEGIN
    DEC(SELF.indentation);
  END Deindent;


BEGIN
END UnitTestTerminalText.
