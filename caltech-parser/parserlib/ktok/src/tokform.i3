INTERFACE %tok;
%gen
(* original token definition *)
IMPORT Rd;
TYPE
  ParseType <: ParseTypePublic;
  Token = ParseType BRANDED "%tok.Token" OBJECT END;

  Lexer = OBJECT METHODS
    get(): Token RAISES {Rd.EndOfFile};
    (* get next token, or raise Rd.EndOfFile if token cannot be formed
       from remaining input *)

    unget();
    (* will be called at most once after get(), and only when lookahead is
       required after last token when parsing without exhausting input *)

    error(message: TEXT);
    (* might print file name, line number, and message, and exit *)
  END;

  RdLexer = Lexer OBJECT METHODS
    setRd(rd: Rd.T): RdLexer;
    (* Prepare to read tokens starting at cur(rd).
       After every token, rd is repositionned after that token. *)

    getRd(): Rd.T;
    (* get reader  *)
    
    fromText(t: TEXT): RdLexer;
    (* Calls setRd with a textReader. *)

    rewind();
    (* equivalent to Rd.Seek(rd, 0) followed by setRd *) 

    getText(): TEXT;
    (* get TEXT of last token *)

    purge(): INTEGER;
    (* Allow any internally allocated ParseTypes to be garbage collected,
       even if the lexer itself remains in scope. Return number of ParseType
       objects allocated but not discarded (not the number of purged objects).
       Can be called at any time by the thread calling get. *)
  END;

  (* token types *)
  ConstTokenCode = [1..%lastConst]; (* < 256 means char code *)
  ConstToken = Token BRANDED "%tok.ConstToken" OBJECT
    val: ConstTokenCode;
  END; (* neither extend this object nor reassign val *)
%type\

  (* ParseType allocation *)
  Allocator <: ROOT;
  ParseTypePublic = OBJECT METHODS
    discard();
    detach(): ParseType;
  END;

CONST
  LegalConstTokenCodes = SET OF ConstTokenCode{
%constSet};
%constName\

PROCEDURE NewPT(VAR a: Allocator; m3type: INTEGER): ParseType;
(* IF a = NIL, then let a = new allocator for m3type.
   regardless, return a new ParseType specifically of type m3type *)

PROCEDURE Purge(VAR a: Allocator): INTEGER;
(* set a=NIL. return number of objects allocated using
   New(a, ...) which were not discarded using discard(). *)

PROCEDURE NewConstToken(val: ConstTokenCode): ConstToken;
(* return a constToken with val=val (well it might not be so new) *)
(* discard() will fail for a constToken *)

PROCEDURE Test(lex: Lexer);
(* get tokens and print their names to stdout until Rd.EndOfFile *)

END %tok.
