(* Copyright 1995 by Digital Equipment Corp. *)
(* Last  modified on Aug 2 15:31:28 PDT 1995 by gnelson                  *)

MODULE SaveState;

IMPORT Text, Wr, Rd, Fmt, Lex, Thread, FloatMode;

CONST Signature = "Saved Juno State";

(* The name of the current file (possibly "Untitled.juno") and the contents of the
   current editor and source window. *)

PROCEDURE Save(READONLY st: T; wr: Wr.T) =
  BEGIN
    TRY
      Wr.PutText(wr, Signature & "\n");
      Wr.PutText(wr, Fmt.Int(Text.Length(st.file)) & "\n");
      Wr.PutText(wr, Fmt.Int(Text.Length(st.editor)) & "\n");
      Wr.PutText(wr, Fmt.Int(Text.Length(st.source)) & "\n");
      Wr.PutText(wr, st.file);
      Wr.PutChar(wr, '\n');
      Wr.PutText(wr, st.editor);
      Wr.PutChar(wr, '\n');
      Wr.PutText(wr, st.source);
      Wr.PutChar(wr, '\n')
    EXCEPT
      Wr.Failure, Thread.Alerted => (* SKIP *)
    END
  END Save;

PROCEDURE Restore(VAR st: T; rd: Rd.T): BOOLEAN =
  VAR flen, elen, slen: INTEGER;
  BEGIN
    TRY
      IF NOT Text.Equal(Rd.GetLine(rd), Signature) THEN
        RETURN FALSE
      END;
      flen := Lex.Int(rd);
      IF NOT '\n' = Rd.GetChar(rd) THEN RETURN FALSE END;
      elen := Lex.Int(rd);
      IF NOT '\n' = Rd.GetChar(rd) THEN RETURN FALSE END;
      slen := Lex.Int(rd);
      IF NOT '\n' = Rd.GetChar(rd) THEN RETURN FALSE END;
      st.file := Rd.GetText(rd, flen);
      IF Text.Length(st.file) # flen OR NOT '\n' = Rd.GetChar(rd) THEN
        RETURN FALSE
      END;
      st.editor := Rd.GetText(rd, elen);
      IF Text.Length(st.editor) # elen OR NOT '\n' = Rd.GetChar(rd) THEN
        RETURN FALSE
      END;
      st.source := Rd.GetText(rd, slen);
      IF Text.Length(st.source) # slen OR NOT '\n' = Rd.GetChar(rd) THEN
        RETURN FALSE
      END;
      RETURN TRUE
    EXCEPT
        Rd.Failure, Lex.Error, FloatMode.Trap, Rd.EndOfFile, Thread.Alerted =>
          RETURN FALSE
    END
  END Restore;
  
(* Read a saved state from "rd", store the result in "st", and return "TRUE".
   Return "FALSE" if "rd" does not contain a properly saved state. *)

BEGIN END SaveState.
  