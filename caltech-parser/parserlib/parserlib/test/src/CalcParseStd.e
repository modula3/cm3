%source Calc.t Calc.y
%import CalcTokStd CalcParse

%private{
regs: ARRAY ['a'..'z'] OF INTEGER;
base: INTEGER;
}

%module{
IMPORT Fmt;
IMPORT Wr, Thread;
FROM Stdio IMPORT stdout;
<* FATAL Wr.Failure, Thread.Alerted *>
}

stat:
  eval		{Wr.PutText(stdout, Fmt.Int($1) & "\n");Wr.Flush(stdout)}
  assign	{self.regs[$1] := $2}

expr:	{val: INTEGER}
  paren		{$$.discard(); result := $1.detach()}
  add		{$$ := $1 + $2}
  sub		{$$ := $1 - $2}
  mul		{$$ := $1 * $2}
  div		{$$ := $1 DIV $2}
  uminus	{$$ := -$1}
  ident		{$$ := self.regs[$1]}
  num		{$$ := $1}

number:	{val: INTEGER}
  digit		{$$ := $1;IF $1 = 0 THEN self.base:=8 ELSE self.base:=10 END}
  cons		{$$ := self.base * $1 + $2}
