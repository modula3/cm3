%source Calc.t Calc.y
%import CalcTokStd CalcParseStd

stat:
  eval	{Explain($1.detach())}
  assign{Wr.PutText(stdout, Fmt.Char($1) & " := " &
                            Fmt.Int($2) & "\n");Explain($2.detach())}

expr:	{e1, e2: expr; kind: CHAR := 'N'}
  add		{$$.e1 := $1.detach(); $$.e2 := $2.detach(); $$.kind := '+'}
  sub		{$$.e1 := $1.detach(); $$.e2 := $2.detach(); $$.kind := '-'}
  mul		{$$.e1 := $1.detach(); $$.e2 := $2.detach(); $$.kind := '*'}
  div		{$$.e1 := $1.detach(); $$.e2 := $2.detach(); $$.kind := '/'}
  uminus	{$$.e1 := $1.detach(); $$.kind := 'U'}
  ident		{$$.kind := $1}

%module{
IMPORT Fmt;
IMPORT Wr, Thread;
FROM Stdio IMPORT stdout;
<* FATAL Wr.Failure, Thread.Alerted *>

PROCEDURE Format(e: expr): TEXT =
  BEGIN
    CASE e.kind OF
    | 'U' => RETURN "(uminus " & Format(e.e1) & ")";
    | '+','-','*','/' =>
      RETURN "(" & Fmt.Char(e.kind) & " " &
             Format(e.e1) & " " & Format(e.e2) & ")";
    | 'N' => RETURN Fmt.Int(e.val);
    ELSE
      RETURN Fmt.Char(e.kind);
    END;
  END Format;

PROCEDURE Explain(e: expr) =
  BEGIN
    Wr.PutText(stdout, " = " & Format(e) & "\n\n");
    Wr.Flush(stdout);
  END Explain;
}
