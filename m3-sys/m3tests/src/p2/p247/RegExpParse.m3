(* from caltech-parser *)

(* Depending on the amount of type information
   provided to the backend, this fails
 *)

MODULE RegExpParse;

TYPE
  TypedSymbol = RECORD
    code  := 2;
    value := 2;
  END;

PROCEDURE ActionLookup(symbol: TypedSymbol;
                       symbol_value: INTEGER) =
  BEGIN
    <* ASSERT symbol.code = symbol_value *>
    <* ASSERT symbol.value = symbol_value *>
  END ActionLookup;

BEGIN
  ActionLookup(TypedSymbol{}, 2);
END RegExpParse.
