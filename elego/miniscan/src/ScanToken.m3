(*---------------------------------------------------------------------------*)
MODULE ScanToken;

IMPORT Fmt;
IMPORT SMsg AS Msg;

(*---------------------------------------------------------------------------*)
PROCEDURE TokenKind(k : Kind) : TEXT =
  BEGIN
    CASE k OF
      Kind.Comment         => RETURN "Comment";
    | Kind.String          => RETURN "String";
    | Kind.EndOfFile       => RETURN "EndOfFile";
    | Kind.CompoundSymbol  => RETURN "CompoundSymbol";
    | Kind.Ident           => RETURN "Ident"; 
    | Kind.Keyword         => RETURN "Keyword";
    | Kind.Other           => RETURN "Other";
    ELSE
      RETURN "unknown token kind";
    END;
  END TokenKind;

(*---------------------------------------------------------------------------*)
PROCEDURE DebugToken(token : T) =
  BEGIN
    IF Msg.dFlag THEN
      Msg.D(Fmt.F("line %4s col %3s token %-16s data `%-s'", 
                  Fmt.Int(token.line), Fmt.Int(token.col),
                  TokenKind(token.kind), token.repr));
    ELSE
      Msg.T(Fmt.F("line %4s col %3s token %-16s data `%-s'", 
                  Fmt.Int(token.line), Fmt.Int(token.col),
                  TokenKind(token.kind), token.repr));
    END;
  END DebugToken;

BEGIN
END ScanToken. 
