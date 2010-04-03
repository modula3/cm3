(*------------------------------------------------------------------------*)
MODULE Confirmation;

IMPORT Stdio, Rd, Wr, Text, TextSeq;
IMPORT SMsg AS Msg, TextUtils, System;

(*---------------------------------------------------------------------------*)
PROCEDURE OkayStdio(<*UNUSED*> self : StdIOClosure; msg : TEXT) : BOOLEAN =
  VAR
    answer : TEXT;
  BEGIN
    LOOP
      TRY
        Wr.PutText(Stdio.stdout, msg & "? [y(es)<cr>/n(o)<cr>] ");
        Wr.Flush(Stdio.stdout);
        answer := Rd.GetLine(Stdio.stdin);
        answer := TextUtils.Compress(answer);
      EXCEPT
        Rd.Failure => Msg.Error("reader failure on stdin"); RETURN FALSE;
      | Rd.EndOfFile => Msg.Error("eof on stdin"); RETURN FALSE;
      | Wr.Failure => Msg.Error("writer failure on stdout"); RETURN FALSE;
      ELSE
        Msg.Error("exception while reading confirmation");
        RETURN FALSE; (* if anything is wrong we don't want to continue *)
      END;
      IF Text.Equal(answer, "y") OR Text.Equal(answer, "yes") OR
         Text.Equal(answer, "Y") OR Text.Equal(answer, "YES") THEN
        RETURN TRUE;
      ELSIF Text.Equal(answer, "n") OR Text.Equal(answer, "no") OR
            Text.Equal(answer, "N") OR Text.Equal(answer, "NO") THEN
        RETURN FALSE;
      END;
      TRY
        Wr.PutText(Stdio.stdout, "\nPlease answer `yes' or `no'\n");
	Wr.Flush(Stdio.stdout);
      EXCEPT
        Rd.Failure => Msg.Error("reader failure on stdin"); RETURN FALSE;
      | Rd.EndOfFile => Msg.Error("eof on stdin"); RETURN FALSE;
      | Wr.Failure => Msg.Error("writer failure on stdout"); RETURN FALSE;
      ELSE
        Msg.Error("exception while reading confirmation");
        RETURN FALSE; (* if anything is wrong we don't want to continue *)
      END;
    END;
  END OkayStdio;

(*---------------------------------------------------------------------------*)
PROCEDURE OkayExternal(self : ExternalClosure; msg : TEXT) : BOOLEAN =
  VAR
    ret :  INTEGER;
    arg := NEW(TextSeq.T).init();
    cmd := self.cmd;
  BEGIN
    arg.addhi(msg & "? ");
    IF cmd = NIL THEN
      cmd := "confirm";
    END;
    TRY
      ret := System.Exec(cmd, arg);
    EXCEPT ELSE
      Msg.Error("cannot call external confirmation program " & cmd);
      RETURN FALSE;
    END;
    RETURN ret = 0;
  END OkayExternal;

(*---------------------------------------------------------------------------*)
PROCEDURE Get(msg : TEXT) : BOOLEAN =
  BEGIN
    RETURN confirmation.okay(msg);
  END Get;

(*---------------------------------------------------------------------------*)
PROCEDURE SetDefault(cl : Closure) =
  BEGIN
    confirmation := cl;
  END SetDefault;

(*---------------------------------------------------------------------------*)
VAR
  confirmation : Closure;
BEGIN
  confirmation := NEW(StdIOClosure);
END Confirmation.
