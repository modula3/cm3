MODULE Main;
IMPORT Wr, Fmt, Thread;
IMPORT SeekRd;
IMPORT RTBrand;
IMPORT CalcLex, CalcParse;
IMPORT CalcLexStd, CalcParseStd;
IMPORT CalcParseTree;
IMPORT Stdio;
FROM Stdio IMPORT stdout;
<* FATAL Wr.Failure, Thread.Alerted, RTBrand.NotBranded *>

PROCEDURE TestCalc(lex: CalcLex.T; parse: CalcParse.T) =
  VAR
    rd := SeekRd.Stdin();
    lb := RTBrand.Get(lex);
    pb := RTBrand.Get(parse);
  BEGIN
    EVAL lex.setRd(rd);
    Wr.PutText(stdout, "Testing " & lb & " & " & pb & "\n");
    Wr.Flush(stdout);
    parse.setLex(lex).parse().discard();
    Wr.PutText(stdout, "undiscarded tokens: " & Fmt.Int(lex.purge()) & "\n");
    Wr.PutText(stdout, "undiscarded others: " & Fmt.Int(parse.purge()) & "\n");
    Wr.Flush(stdout);
  END TestCalc;

BEGIN
  TestCalc(NEW(CalcLex.T), NEW(CalcParse.T));
  TestCalc(NEW(CalcLexStd.T), NEW(CalcParseStd.T));
  TestCalc(NEW(CalcLexStd.T), NEW(CalcParseTree.T));
END Main.
