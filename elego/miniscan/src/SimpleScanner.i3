(*---------------------------------------------------------------------------*)
INTERFACE SimpleScanner;

IMPORT ASCII, Rd, TextSet;
IMPORT ScanToken;

(*---------------------------------------------------------------------------*)
EXCEPTION Error(TEXT);

(*---------------------------------------------------------------------------*)
CONST 
  DefaultSpecialChars = ASCII.Set{
    '/', '*', '+', '-', '=', '<', '>', '.', ',', ';',
    ':', '?', '(', ')', '[', ']', '{', '}', '\\', '#',
    '!', '@', '$', '%', '^', '&', '"', '~', '\'', '`'
  };

(*---------------------------------------------------------------------------*)
TYPE
  Token <: ScanToken.T;

  T <: Public;

  Public = OBJECT
    skipComments    : BOOLEAN;        (* default TRUE *)
    nestingComments : BOOLEAN;        (* default TRUE *)
    oneLineComments : BOOLEAN;        (* default TRUE *)
    commentOpenSym  : TEXT;           (* default `/*' *)
    commentCloseSym : TEXT;           (* default `*/' *)
    lineCommentSym  : TEXT;           (* default `#'  *)
    stringOpenSym   : TEXT;           (* default `"'  *)
    stringCloseSym  : TEXT;           (* default `"'  *)
    identSymbols    : ASCII.Set;      (* default ASCII.AlphaNumerics + {'_'} *)
    whiteSpace      : ASCII.Set;      (* default ASCII.Spaces *)
    specialSymbols  : ASCII.Set;      (* default DefaultSpecialChars *)
    compoundToken   : TextSet.T;      (* default {`:=', `<=', `>=', `->'} *)
    keywordToken    : TextSet.T;      (* default {} *)
  METHODS
    init(inputStream : Rd.T) : T;
    nextToken() : Token RAISES {Error};
    pushBack(t : Token);
    eof() : BOOLEAN;
  END;

(*---------------------------------------------------------------------------*)
VAR
  debugScanner : BOOLEAN;
END SimpleScanner.
