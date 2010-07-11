(* Compiling this program has crashed some backends.
 * (gcc backend with -O3, no volatile, 'ter')
 * This is reduced from m3-tools/m3totex/m3totex.m3.
 * The point therefore is merely to compile it without crashing.
 *)
MODULE Main;
IMPORT Rd, Wr, Stdio;

VAR c: CHAR; dblquoteparity := 0;

<*NOWARN*>PROCEDURE Undisplay() = <*FATAL ANY*>
  BEGIN
    LOOP
      c := Rd.GetChar(Stdio.stdin);
      IF c = '\"' THEN
        IF dblquoteparity = 0
          THEN Wr.PutText(Stdio.stdout, "{")
          ELSE Wr.PutText(Stdio.stdout, "}")
        END;
        INC(dblquoteparity);
      ELSIF c = '`' THEN
        Wr.PutText(Stdio.stdout, "}");
        RETURN;
      END
    END
  END Undisplay;

BEGIN
END Main.
