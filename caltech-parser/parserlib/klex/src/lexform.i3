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

(* generics stuff *)
CONST
  Brand = "%lex";
PROCEDURE Hash(a: T): INTEGER;
PROCEDURE Equal(a,b:T): BOOLEAN;
END %lex.
