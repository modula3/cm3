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
(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CLexF;

(* Friends interface for "M3CLex" *)

IMPORT Rd;
IMPORT M3AST_LX, M3CToken, M3CReservedWord, M3CHash, M3CLex;

TYPE FriendPart = M3CLex.Public OBJECT 
    rd: Rd.T := NIL;
    identifiers: M3CReservedWord.Table;
    literals: M3CHash.Table;
    cur_identifier: M3AST_LX.Symbol_rep := NIL;
    cur_literal: M3AST_LX.Literal_rep := NIL;
    line, offset, startOfToken, linesInToken: CARDINAL := 0;
    callBack: M3CLex.CallBack := NIL;
    tokenBuffer: Buffer;
    hashValue: M3CHash.Value := NIL;
  METHODS
    get(): CHAR RAISES {Rd.Failure, Rd.EndOfFile};
    unget(ch: CHAR);
    readId(firstCh: CHAR): M3CToken.T RAISES {Rd.Failure};
    readNumericLiteral(firstCh: CHAR): M3CToken.T RAISES {Rd.Failure};
    readCharLiteral(): M3CToken.T RAISES {Rd.Failure};
    readTextLiteral(): M3CToken.T RAISES {Rd.Failure};
    readCommentOrPragma(isComment: BOOLEAN) RAISES {Rd.Failure};
  END;
 
REVEAL M3CLex.T <: FriendPart;

CONST
  IsComment = TRUE;
  IsPragma = FALSE;

(* The "get" method returns the next character from the lookahead buffer,
raising "Rd.EndOfFile" if the stream is exhausted. "unget(ch)" puts
"ch" into the lookahead buffer, causing "get" to return it the next time
it is called. Multiple calls without an intervening "get" have no
effect. 

The "readId" method reads an identifier that begins with "firstCh".
If the identifier correspsonds to a reserved word, the associated 
"M3CToken.T" value is returned, otherwise "M3CToken.T.Identifier"
is returned and the "identifier" field is set to the "Symbol_rep"
for the identifier.

The "readNumericLiteral" method reads a numeric literal that starts
with firstCh, returning the appropriate value of "M3CToken.T", and
setting the "literal" field to the "Literal_rep" value for the
literal.

"readCharLiteral" and "readTextLiteral" read character literals and
text literals, respectively, and assume that the leading quote or
double-quote character has already been consumed. The value of the
"literal" field is set to the "Literal_rep" value for the literal.

"readCommentOrPragma" assumes that the leading "(" or "<" character
has been consumed and tenh attempts to process the remainder of the
comment or pragma, calling the "callback" method with the result. *)

(* Manipulating the token buffer directly. Call these procedures with
   "buffer" = "t.tokenBuffer". *)

TYPE
  Buffer <: REFANY;

PROCEDURE BufferToText(buffer: Buffer; length: CARDINAL): TEXT RAISES {};
(* converts "length" chars from "buffer" to a TEXT and returns the result. *)

<*INLINE*> PROCEDURE BufferPut(
    VAR buffer: Buffer;
    pos: CARDINAL;
    ch: CHAR) RAISES {};
(* Stores "ch" as position "pos" in "buffer", possibly adding an overflow
   area (hence VAR). *)

<*INLINE*> PROCEDURE HashAndBufferPut(
    ch: CHAR;
    hashValue: M3CHash.Value;
    VAR buffer: Buffer;
    VAR pos: CARDINAL)
    RAISES {};
(* As "BuffePut", but also calls "M3CHash.AddCharToValue(ch, hashValue)"
   and increments "pos". *)

END M3CLexF.

