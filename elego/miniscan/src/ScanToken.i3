(*---------------------------------------------------------------------------*)
INTERFACE ScanToken;

CONST Brand = "ScanToken Interface 0.0";

(*---------------------------------------------------------------------------*)
TYPE
  Kind = {Comment, String, EndOfFile, CompoundSymbol, Ident, Keyword, Other};

  T = BRANDED "Abstract ScanToken Class 0.0" OBJECT
    kind : Kind;
    line : CARDINAL;
    col  : CARDINAL;
    repr : TEXT;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE TokenKind(k : Kind) : TEXT;

(*---------------------------------------------------------------------------*)
PROCEDURE DebugToken(token : T);

END ScanToken.
