%source RegExp.t RegExp.y
%import RegExpLexStd RegExpParse

%public{
  METHODS
    init(): T;
    parseText(t: TEXT): NFA.T;
    putMacro(name: TEXT; n: NFA.T);
}

%interface{
IMPORT NFA;
}

expr:	{val: NFA.T}
  paren         {$$ := $1}
  concat        {$$ := NFA.Concat($1, $2)}
  or            {$$ := NFA.Or($1, $2)}
  plus          {$$ := NFA.Rept($1, Interval.T{1, NFA.OrMore})}
  star          {$$ := NFA.Rept($1, Interval.T{0, NFA.OrMore})}
  quest         {$$ := NFA.Rept($1, Interval.T{0, 1})}
  repeat        {$$ := NFA.Rept($1, $2)}
  ident         {$$ := Lookup(self, $1)}
  string        {$$ := NFA.FromString($1)}
  charRange     {$$ := NFA.FromRange($1)}

%private  {exprMacros : NFATbl.T := NIL;}

%overrides{
    parseText := ParseText;
    putMacro := PutMacro;
    init := Init;
}

%module{
IMPORT NFA;
IMPORT CharCodes;
IMPORT Interval;
IMPORT NFATbl;
IMPORT FileRdErr;
IMPORT RegExpLexStd;

PROCEDURE Init(self: T): T =
  BEGIN
    self.exprMacros := NEW(NFATbl.Default).init();
    RETURN self;
  END Init;

PROCEDURE ParseText(self: T; t: TEXT): NFA.T =
  BEGIN
    <* ASSERT self.exprMacros # NIL *>
    EVAL self.setLex(NEW(RegExpLexStd.T).fromText(t));
    RETURN NARROW(self.parse(), expr).val;
  END ParseText; 

PROCEDURE PutMacro(self: T; name: TEXT; n: NFA.T) =
  BEGIN
    <* ASSERT self.exprMacros # NIL *>
    IF self.exprMacros.put(name, n) THEN
      FileRdErr.E(NIL, "redeclared macro: " & name);
    END;
  END PutMacro;

PROCEDURE Lookup(self: T; name: TEXT): NFA.T =
  VAR
    n: NFA.T;
  BEGIN
    IF NOT self.exprMacros.get(CharCodes.StripDelims(name), n) THEN
      FileRdErr.E(NIL, "undeclared macro: " & name);
    END;
    RETURN NFA.Copy(n);
  END Lookup;
}
