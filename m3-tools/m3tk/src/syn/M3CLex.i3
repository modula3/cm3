(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


INTERFACE M3CLex;

IMPORT Rd;
IMPORT M3AST_LX;
IMPORT M3CHash, M3CToken, M3CReservedWord, M3CSrcPos;

(* Lexer for Modula 3 *)

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(rd: Rd.T;
         identifiers: M3CReservedWord.Table;
         literals: M3CHash.Table;
         callBack: CallBack): T;
    current(): M3CToken.T;
    next(): M3CToken.T RAISES {Rd.Failure};
    position(): M3CSrcPos.T;
    literal(): Literal_rep;
    identifier(): Symbol_rep;
    disable();
    disabled(): BOOLEAN;
    reset(pos := M3CSrcPos.Null; s: Rd.T := NIL);
    currentTokenToText(): TEXT;
  END;

  CallBack = OBJECT
  METHODS
    badChar(ch: CHAR); 
    comment(t: TEXT);
    pragma(t: TEXT);
    whiteSpace(t: TEXT);
  END;

(* A "CallBack" object is used by the lexer to notify the caller of
tokens that the lexer cannot handle. "badChar(ch)" will be called for
any illegal character. "comment" will be called for each comment, with
the comment text in "c" , "pragma" will be called for each pragma
with the pragma text in "p", and "whiteSpace" will be called
with the white space between tokens. *)

  Symbol_rep = M3AST_LX.Symbol_rep;      (* Represents an identifier *)
  Literal_rep = M3AST_LX.Literal_rep;    (* Represents a numeric, character *)
                                         (* or text literal *)

REVEAL
  Symbol_rep <: M3CHash.Id;              (* Identifiers and literals are *)
  Literal_rep <: M3CHash.Id;             (* both stored in hash tables *)


(* "New(T).init: creates a new lexer. The lexer will read from the
given reader'rd'. Any identifiers found will be put into the
"identifiers" hash table. Note that this table already contains all
the reserved words; hence only one hash lookup/entry operation is
needed for a reserved word or identifier.  Any literals found will be
put in the "literals" hash table.  The appropriate "callBack" method
will be called when a bad character, comment, pragma or whitespace is found. 

"current" returns the current token. "next" 
advances to the next token and then returns the (new) current token. 
"position" returns the current position of the lexer encoded
as an "M3CSrcPos.T". 

"literal" returns a handle for the current literal. Literals are
represented by a hash-id for a "TEXT" that is created by the lexer.
The texts are distinguished as follows:

\begin(itemize}
\item Valid numeric literals. The text starts with a digit and ends with a hex 
      digit.
\item Valid text literals. The text starts and ends with double quote 
      character.
\item Valid character literals. The text starts and ends with single quote 
      character.
\end{itemize}

In the case of an error in a literal, for example, a based number with
one of its digits is out of range, or a text literal with a mising
closing quote, the call will return an [|it invalid literal} value.
Invalid literals always have at least one character and their first
character can be used to distinguish the literal type (numerics start
with a digit, texts with double quote, chars with single quote). They
are distinguished by the last character being inappropriate, that is
not a (hex) digit, single quote or double quote, respectively.  If
"NOT current() IN M3CToken.T.Literals" the result of "literal" is
undefined.


If "current() = M3CToken.T.Identifier", then "identifier" returns a
unique representative for the identifier text, otherwise the result is
undefined.

"disable" disables the lexer; any call of "next" will return
"M3CToken.T.Void" and the position will not advance.

"disabled" returns TRUE if and only if the lexer is disabled.

"reset" resets the lexer. It sets the current symbol to
"M3CToken.T.Void" and enables the lexer it is disabled.  If "pos" is
not "M3CSrcPos.Null" the lexer position is set to "pos".  If "s" is
not NIL sets the lexer stream to be "s" and sets the lexer position to
be "pos" if "pos # M3CSrcPos.Null" or line "1" offset "0" otherwise.


The "TokenToText" procedure returns a "TEXT" that describes the
"token" argument. The "currentTokenToText" methods returns a text
describing the current token; this may give more information than
"TokenToText" because it incorporates identifier names or literal
values if the token is in those classes. *)

PROCEDURE TokenToText(token: M3CToken.T): TEXT;

END M3CLex.
