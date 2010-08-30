(* from caltech-parser *)

(* Depending on the amount of type information
   provided to the backend, this fails
 *)

MODULE RegExpParse;
IMPORT RegExpTok;

TYPE
  TypedSymbol = RECORD
    code: INTEGER;
    value: RegExpTok.ParseType;
  END;

PROCEDURE ActionLookup(symbol: TypedSymbol;
                       symbol_value: RegExpTok.ParseType) =
  BEGIN
    <* ASSERT symbol.value = symbol_value *>
  END ActionLookup;

BEGIN
  ActionLookup(TypedSymbol{2, NIL}, NIL);
END RegExpParse.
