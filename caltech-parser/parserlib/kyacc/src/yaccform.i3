INTERFACE %yacc;
%gen
(* original parser definition *)
IMPORT %tok;
TYPE
  (* additional parse types *)
  StartType = %tok.ParseType BRANDED "%yacc.StartType" OBJECT END;
  OtherType = %tok.ParseType;
%type\

  (* import tokens *)
  ConstToken = %tok.ConstToken;
%gettok\

  (* the parser *)
  T <: Public;
  Public = OBJECT
  METHODS
    setLex(lex: %tok.Lexer): T;
    parse(exhaustInput: BOOLEAN := TRUE): StartType;

    (* rules *)
%prot\

    purge(): INTEGER;
    (* Allow any internally allocated ParseTypes to be garbage collected,
       even if the parser itself remains in scope. Return number of ParseType
       objects allocated but not discarded (not the number of purged objects).
       Can be called at any time by the thread calling get. *)
  END;

  (* And now, for a hack to allow compatible methods *)
  (* ... without importing the original parser *)
  Original_Parser = T;
%orig\
  (* ... and without importing the original token *)
%tokOrig\

END %yacc.
