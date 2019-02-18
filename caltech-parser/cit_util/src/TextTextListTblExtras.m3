MODULE TextTextListTblExtras;
IMPORT TextRd;
IMPORT Rd;
IMPORT TextUtils;
IMPORT TextTextListTbl;
IMPORT Thread;

<* FATAL Rd.Failure, Thread.Alerted *>

PROCEDURE Scan(t: TEXT; 
               delims      := "\t, ";
               endDelims   := "\n;#%" ): T =
  VAR
    rd := TextRd.New(t);
    tbl := NEW(TextTextListTbl.Default).init();
  BEGIN
    TRY
      LOOP
        WITH l = TextUtils.Shatter(Rd.GetLine(rd), delims, endDelims) DO
          EVAL tbl.put(l.head, l.tail);
        END;
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    RETURN tbl;
  END Scan;

BEGIN
END TextTextListTblExtras.
