(* from caltech-parser *)

MODULE RegExpParse;

IMPORT RegExpTok;
IMPORT RTIO;

TYPE
  TypedSymbol = RECORD
    code: INTEGER;
    value: RegExpTok.ParseType;
  END;


PROCEDURE ActionLookup(curState: INTEGER;
                       symbol: TypedSymbol;
                       symbol_code: INTEGER;
                       symbol_value: RegExpTok.ParseType;
                       ) =
  BEGIN
    RTIO.PutText("1\n"); RTIO.Flush();
    <* ASSERT symbol.code = symbol_code *>
    <* ASSERT symbol.value = symbol_value *>
  END ActionLookup;

VAR cs := 1;
VAR ts := TypedSymbol{2, NIL};

BEGIN
  ActionLookup(cs, ts, ts.code, ts.value);
END RegExpParse.
