(* Copyright 1993 by Digital Equipment Corp.                   *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Jan 26 14:03:08 PST 1995 by kalsow     *)
(*      modified on Wed Apr 21 09:07:52 PDT 1993 by mcjones    *)
(*      modified on Tue Mar  9 11:57:?? PDT 1993 by mjordan    *)

MODULE IO;

IMPORT Rd, Wr, Thread, Stdio, FileRd, FileWr, Fmt, OSError, Lex, FloatMode;

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted *>

PROCEDURE Put(txt: TEXT; wr: Wr.T := NIL)=
  BEGIN
    IF wr = NIL THEN wr := Stdio.stdout END;
    Wr.PutText(wr, txt); Wr.Flush(wr);
  END Put;

PROCEDURE PutChar(ch: CHAR; wr: Wr.T := NIL)=
  BEGIN
    IF wr = NIL THEN wr := Stdio.stdout END;
    Wr.PutChar(wr, ch); Wr.Flush(wr);
  END PutChar;

PROCEDURE PutWideChar(ch: WIDECHAR; wr: Wr.T := NIL)=
  BEGIN
    IF wr = NIL THEN wr := Stdio.stdout END;
    Wr.PutWideChar(wr, ch); Wr.Flush(wr);
  END PutWideChar;

PROCEDURE PutInt(n: INTEGER; wr: Wr.T := NIL)=
  BEGIN
    IF wr = NIL THEN wr := Stdio.stdout END;
    Wr.PutText(wr, Fmt.Int(n)); Wr.Flush(wr);    
  END PutInt;

PROCEDURE PutReal(r: REAL; wr: Wr.T := NIL)=
  BEGIN
    IF wr = NIL THEN wr := Stdio.stdout END;
    Wr.PutText(wr, Fmt.Real(r)); Wr.Flush(wr);    
  END PutReal;

PROCEDURE EOF(rd: Rd.T := NIL): BOOLEAN=
  BEGIN
    IF rd = NIL THEN rd := Stdio.stdin END;
    RETURN Rd.EOF(rd);
  END EOF;

PROCEDURE GetLine(rd: Rd.T := NIL): TEXT RAISES {Error}=
  BEGIN
    IF rd = NIL THEN rd := Stdio.stdin END;
    TRY RETURN Rd.GetLine(rd);
    EXCEPT
    | Rd.EndOfFile => RAISE Error;
    END;
  END GetLine;

PROCEDURE GetChar(rd: Rd.T := NIL): CHAR RAISES {Error}=
  BEGIN
    IF rd = NIL THEN rd := Stdio.stdin END;
    TRY RETURN Rd.GetChar(rd);
    EXCEPT
    | Rd.EndOfFile => RAISE Error;
    END;
  END GetChar;

PROCEDURE GetWideChar(rd: Rd.T := NIL): WIDECHAR RAISES {Error}=
  BEGIN
    IF rd = NIL THEN rd := Stdio.stdin END;
    TRY RETURN Rd.GetWideChar(rd);
    EXCEPT
    | Rd.EndOfFile => RAISE Error;
    END;
  END GetWideChar;

PROCEDURE GetInt(rd: Rd.T := NIL): INTEGER RAISES {Error}=
  BEGIN
    IF rd = NIL THEN rd := Stdio.stdin END;
    TRY RETURN Lex.Int(rd);
    EXCEPT
    | Lex.Error, FloatMode.Trap => RAISE Error;
    END;
  END GetInt;

PROCEDURE GetReal(rd: Rd.T := NIL): REAL RAISES {Error}=
  BEGIN
    IF rd = NIL THEN rd := Stdio.stdin END;
    TRY RETURN Lex.Real(rd);
    EXCEPT
    | Lex.Error, FloatMode.Trap => RAISE Error;
    END;
  END GetReal;

PROCEDURE OpenRead(f: TEXT): Rd.T=
  BEGIN
    TRY RETURN FileRd.Open(f);
    EXCEPT
    | OSError.E =>
        RETURN NIL;
    END
  END OpenRead;

PROCEDURE OpenWrite(f: TEXT): Wr.T=
  BEGIN
    TRY RETURN FileWr.Open(f);
    EXCEPT
    | OSError.E =>
        RETURN NIL;
    END
  END OpenWrite;

BEGIN
END IO.
