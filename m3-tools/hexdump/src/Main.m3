UNSAFE MODULE Main;

IMPORT Stdio, Wr, Word, Rd;
<*FATAL ANY*>

PROCEDURE HexDump () =
  VAR
    hit_eof   := FALSE;
    offset    := 0;
    last_data := 0;
    cur_data  := 0;
    skipping  := FALSE;
    ibuf      : ARRAY [0..3] OF INTEGER;
    buf       : UNTRACED REF ARRAY [0..15] OF CHAR := ADR (ibuf);
  BEGIN
    <*ASSERT BYTESIZE (buf^) = BYTESIZE (ibuf) *>

    WHILE NOT hit_eof DO
      (* read a chunk *)
      FOR i := FIRST (ibuf) TO LAST (ibuf) DO ibuf[i] := 0; END;
      FOR i := FIRST (buf^) TO LAST (buf^) DO
        IF Rd.EOF (Stdio.stdin) THEN  hit_eof := TRUE; EXIT;  END;
        buf [i] := Rd.GetChar (Stdio.stdin);
      END;

      (* check for all zeros *)
      cur_data := 0;
      FOR i := FIRST (ibuf) TO LAST (ibuf) DO
        cur_data := Word.Or (cur_data, ibuf[i]);
      END;

      IF (last_data # 0) OR (cur_data # 0) THEN
        skipping := FALSE;
        OutI (offset, 5);  OutC (':');
        FOR i := FIRST (ibuf) TO LAST (ibuf) DO
          OutC (' ');
          OutI (ibuf[i], 8);
        END;
        OutC (' ');
        FOR i := FIRST (buf^) TO LAST (buf^) DO
          IF (i MOD 4) = 0 THEN OutC (' '); END;
          PrintCh (buf[i]);
        END;
        NL ();
      ELSE (* skip the current line *)
        IF NOT skipping THEN  Wr.PutText (Stdio.stdout, "*");  NL ();  END;
        skipping := TRUE;
      END;

      last_data := cur_data;
      INC (offset, NUMBER (buf^));
    END; (* WHILE *)
  END HexDump;

PROCEDURE PrintCh (ch: CHAR) =
  BEGIN
    OutC (' ');
    IF (ch < ' ') OR ('~' < ch) THEN
      OutC ('.');
    ELSE
      OutC (ch);
    END;
  END PrintCh;

PROCEDURE OutI (val, width: INTEGER) =
  CONST Digits = ARRAY [0..15] OF CHAR { '0','1','2','3','4','5','6','7',
                                         '8','9','A','B','C','D','E','F' };
  VAR x: ARRAY [0..2*BYTESIZE(INTEGER)] OF [0..15];  len: INTEGER;
  BEGIN
    x[0] := Word.And (val, 16_f);  len := 1;
    val := Word.RightShift (val, 4);
    WHILE (val # 0) DO
      x[len] := Word.And (val, 16_f);  INC (len);
      val := Word.RightShift (val, 4);
    END;
    WHILE (width > len) DO  OutC ('0');  DEC (width);  END;
    WHILE (len > 0) DO  DEC (len); OutC (Digits[x[len]]);  END;
  END OutI;

PROCEDURE OutC (ch: CHAR) =
  BEGIN
    Wr.PutChar (Stdio.stdout, ch);
  END OutC;

PROCEDURE NL () =
  BEGIN
    Wr.PutText (Stdio.stdout, Wr.EOL);
  END NL;

BEGIN
  HexDump ();
END Main.

