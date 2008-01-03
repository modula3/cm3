(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Jul 22 11:13:37 PDT 1995 by mhb                      *)

INTERFACE Lexer;

IMPORT Rd;

TYPE
  Token = OBJECT END;
  CommentToken = Token OBJECT END;
  WordToken = Token OBJECT 
    word: TEXT; 
  END;
  ElementToken = Token OBJECT
    tag: TEXT;
    end: BOOLEAN;
    attributes: Attribute;
  END;

  Attribute = OBJECT
    name: TEXT;
    value: TEXT;
    next: Attribute;
  END;

PROCEDURE Get (rd: Rd.T; obeyBlanks: BOOLEAN := FALSE): Token;
(* Returns the next token from the reader "rd",
   and positions "rd" just after the token. If "obeyBlanks"
   is TRUE, white space is returned as well. *)

END Lexer.
