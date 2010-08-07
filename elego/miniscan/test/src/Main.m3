(*---------------------------------------------------------------------------*)
MODULE Main;

IMPORT Stdio, Fmt, Rd, FileRd, Params;
IMPORT Msg, SimpleScanner, ScanToken;

(*---------------------------------------------------------------------------*)
(*
REVEAL SimpleScanner.Token = ScanToken.T;
*)
REVEAL 
  SimpleScanner.Token = ScanToken.T BRANDED "Test Scanner Token Class" OBJECT
    attr1 : TEXT;
    attr2 : INTEGER;
  END;
   
(*---------------------------------------------------------------------------*)
PROCEDURE TokenKind(k : ScanToken.Kind) : TEXT =
  BEGIN
    CASE k OF
      ScanToken.Kind.Comment         => RETURN "Comment";
    | ScanToken.Kind.String          => RETURN "String";
    | ScanToken.Kind.EndOfFile       => RETURN "EndOfFile";
    | ScanToken.Kind.CompoundSymbol  => RETURN "CompoundSymbol";
    | ScanToken.Kind.Ident           => RETURN "Ident"; 
    | ScanToken.Kind.Keyword         => RETURN "Keyword";
    | ScanToken.Kind.Other           => RETURN "Other";
    ELSE
      RETURN "unknown token kind";
    END;
  END TokenKind;

(*---------------------------------------------------------------------------*)
PROCEDURE OutToken(token : ScanToken.T) =
  BEGIN
    Msg.T(Fmt.F("line %4s col %3s token %-16s data `%-s'", 
                Fmt.Int(token.line), Fmt.Int(token.col),
                TokenKind(token.kind), token.repr));
  END OutToken;

(*---------------------------------------------------------------------------*)
VAR (* Main *)
  scanner := NEW(SimpleScanner.T);
  token : ScanToken.T;
  rd : Rd.T;
BEGIN (* Main *)
  Msg.tFlag := TRUE;
  IF Params.Count > 1 THEN
    TRY
      rd := FileRd.Open(Params.Get(1));
    EXCEPT ELSE
      Msg.Fatal("cannot open input file " & Params.Get(1));
    END;
    EVAL scanner.init(rd);
  ELSE
    EVAL scanner.init(Stdio.stdin);
  END;
  scanner.skipComments := FALSE;
  EVAL scanner.compoundToken.insert("-->");
  REPEAT
    TRY
      token := scanner.nextToken();
      OutToken(token);
    EXCEPT
      SimpleScanner.Error(e) => Msg.Error(e);
    END;
  UNTIL scanner.eof();
END Main.
