(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Feb 21 13:20:00 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

INTERFACE QToken;

TYPE
  T = {
    Unknown, EOF,

    (* Keywords *)
    And, Contains, Else, End, Foreach,
    If, In, Is, Local, Not, Or, Proc,
    Readonly, Return,

    (* Punctuation *)
    Dollar, Ampersand, LParen, RParen, Plus, Comma, Colon, Less,
    Equal, Greater, At, LSquare, RSquare, LBrace, RBrace,

    (* Others---values are stored in the Lexer.T for these *)
    Cardinal, Name, String
  };

CONST
  Name = ARRAY T OF TEXT {
    "<*Unknown*>", "<*EOF*>",

    (* Keywords *)
    "and", "contains", "else", "end", "foreach",
    "if", "in", "is", "local", "not", "or", "proc",
    "readonly", "return",

    (* Punctuation *)
    "$", "&", "(", ")", "+", ",", ":", "<",
    "=", ">", "@", "[", "]", "{", "}",

    (* Others---values are stored in the Lexer.T for these *)
    "<cardinal>", "<name>", "<string>"
  };

END QToken.
