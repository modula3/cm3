INTERFACE %lex;
%gen
(* original lexer definition *)
IMPORT %tok;
FROM %tok IMPORT Token;
TYPE
  T <: Public;
  Public = %tok.RdLexer OBJECT
  METHODS
    (* return Token for a regexp type *)
%methods\
  END;
END %lex.
