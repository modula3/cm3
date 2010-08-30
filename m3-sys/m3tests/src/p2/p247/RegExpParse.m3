(* from caltech-parser *)

(* Depending on the amount of type information
   provided to the backend, this crashes
   (doesn't even fail the assertion)
 *)

MODULE RegExpParse;
IMPORT RegExpTok;

TYPE
  TypedSymbol = RECORD
    code: INTEGER;
    value: RegExpTok.ParseType;
  END;

PROCEDURE ActionLookup(<*NOWARN*>curState: INTEGER;
                       symbol: TypedSymbol;
                       symbol_code: INTEGER;
                       symbol_value: RegExpTok.ParseType) =
  BEGIN
    <* ASSERT symbol.code = symbol_code *>
    <* ASSERT symbol.value = symbol_value *>
  END ActionLookup;

CONST ts = TypedSymbol{2, NIL};

BEGIN
  ActionLookup(1, ts, ts.code, ts.value);
END RegExpParse.
