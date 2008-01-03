%source test.t test.y
%import testTok testParse

%interface{
(* hello interface *)
IMPORT NFA;
}

%private{
    exprMacros: NFATbl.T;
}

%public{
    result: NFA.T;
}

%module{
IMPORT NFA;
IMPORT Interval;

(* Lookup *)
}

%init{
    self.exprMacros := NEW(NFATbl.Default).init();
    self.result := NFA.Empty();
}

stat: {}
  macro		{EVAL self.exprMacros.put($1, $2)}
  rule		{self.result := NFA.Or(self.result,NFA.Output($2, $1), FALSE);}

expr: {val: NFA.T}
  paren		{$$ := $1}
  concat	{$$ := NFA.Concat($1, $2)}
  or		{$$ := NFA.Or($1, $2)}
  plus		{$$ := NFA.Rept($1, Interval.T{1, NFA.OrMore})}
  star		{$$ := NFA.Rept($1, Interval.T{0, NFA.OrMore})}
  quest		{$$ := NFA.Rept($1, Interval.T{0, 1})}
  repeat	{$$ := NFA.Rept($1, $2)}
  ident		{$$ := Lookup($1)}
  string	{$$ := NFA.FromString($1)}
  charRange	{$$ := NFA.FromRange($1)}

