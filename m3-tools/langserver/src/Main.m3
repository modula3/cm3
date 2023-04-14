MODULE Main;

IMPORT Text,Stdio,Rd,Scan,Params;
IMPORT LSP;

VAR
  HeaderLen : CARDINAL; (* const *)
  parm : TEXT;

PROCEDURE ReadMsg() : TEXT =
  VAR
    count : INTEGER;
    line,msg : TEXT;
    ch : CHAR;
  BEGIN
    line := Rd.GetLine(Stdio.stdin);
    line := Text.Sub(line,HeaderLen);
    count := Scan.Int(line);
    ch := Rd.GetChar(Stdio.stdin);
    IF ch # '\r' THEN
      Rd.UnGetChar(Stdio.stdin);
      EVAL Rd.GetLine(Stdio.stdin);
    END;
    (* read the blank line *)
    EVAL Rd.GetLine(Stdio.stdin);

    msg := Rd.GetText(Stdio.stdin, count);
    RETURN msg;
  END ReadMsg;

PROCEDURE Run() =
  VAR 
    msg : TEXT;
    res : BOOLEAN;
  BEGIN
    LOOP
      msg := ReadMsg();
      res := LSP.HandleMsg(msg);
      IF res THEN EXIT; END;
    END;
  END Run;

BEGIN
  HeaderLen := Text.Length("Content-Length:");
  IF Params.Count <= 1 THEN
    Run();
  ELSE
    (* command line checking *)
    parm := Params.Get(1);
    LSP.CheckFile(parm);
  END;
END Main.

