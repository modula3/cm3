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

INTERFACE M3CParse;

IMPORT Rd;
IMPORT M3AST_AS;
IMPORT M3CSrcPos, M3CHash, M3CLex, M3CReservedWord;

(* Parser for Modula 3 *)

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(rd: Rd.T; identifiers: M3CReservedWord.Table;
         literals: M3CHash.Table; errorHandler: ErrorHandler;
         lexer: M3CLex.T := NIL): T;
    compilation(headerOnly := FALSE): M3AST_AS.Compilation_Unit
        RAISES {Rd.Failure};
    any(terminators := SET OF CHAR{}): REFANY RAISES {Rd.Failure};
    reset(pos := M3CSrcPos.Null; rd: Rd.T := NIL);
  END;

  ErrorHandler = OBJECT
  METHODS
    handle(pos: M3CSrcPos.T; msg: TEXT);
  END;

(* The "init" method creates a new parser on the reader "rd".
"identifiers" is the table that will be used for identifiers found
during parsing. Since it is an "M3CReservedWord.Table" it already
contains all the Modula-3 reserved words.  Any literals found will be
stored, as texts, in the "literals" table.  The "errorHandler" object
is used whenever an error occurs; the "handle" method is called with
the position of the error and a message. If "lexer" is "NIL" a default
lexer is created using the "identifiers" and "literals" tables.
Otherwise "lexer" is used and it is required that the "identifiers"
and "literals" tables are the same as were pass to the "M3CLex.T.init"
method.

The "compilation" method attempts to parse an entire compilation unit.
If the "headerOnly" flag is "TRUE" the parse is stopped after any
exports and/or import clauses have been parsed and a skeleton
compilation unit is returned. Such a skeleton may be useful for
dependency analysis. 


The "any" method attempts to parse whatever construct is next on the parse stream. The parse
finishes at a natural boundary, if end of stream is reached or if a character
in "terminators" is encountered. 
A natural boundary is rather vaguely defined. It depends on the first
symbol encountered on the parse stream. Here are the various cases. Note that
where a "list of whatever" is parsed the parse is terminated by any symbol
which cannot be the start of another "whatever". e.g. a sequence of imports is
definitely at an end if you encounter a token which can never be in an import,
for example, the start of a declaration.

\begin{itemize}
\item
Start of unit               parse a single unit, returning a "UNIT".
\item
Start of import statement   parse a list of "IMPORT" statements, returning a
                            "SeqM3AST_AS_IMPORTED.T".
\item
Start of statement          parse a sequence of statements, returning a
                            "SeqM3AST_AS_STM.T".
\item
Start of declaration        parse a sequence of declarations. If the sequence
                            is followed by "BEGIN" treat these declarations as
                            the start of a block statement - see the section on
                            statements. Otherwise return a
                            "SeqM3AST_AS_DECL_REVL.T".
\item
Start of type               parse a single type, returning an 
                            "M3AST_AS.TYPE_SPEC". Note that a named type cannot
                            be distinguished from an expression by the parser,
                            hence named types are parsed as expressions.
\item
Start of expression         parse a single expression. If the expression is
                            followed by ":=" treat it as the start of an
                            assignment - see the section on statements. If the
                            expression is a call and is followed by ";" treat
                            as a procedure call statement - see the section on
                            statements. Otherwise return the expression as an
                            "M3AST_AS.EXP".
\end(itemize}

If the first symbol encountered is not the start of any of the items given
above "any" returns "NIL". *)

CONST
  AnyTerminators =
      SET OF CHAR{'!', '$', '%', '?', '@', '\\', '_', '`', '~'};

(* If the end of stream is encountered the parse is terminated. In
addition, if a character in the set "terminators" is encountered the
parse is terminated.  If "terminators" is not a subset of
"AnyTerminators" a checked runtime error will occur.

The "reset" method resets the lexer and optionally sets the position
and reader used by the lexer and parser. The position is significant
because many nodes have source positions and also because error
messages need positions.  If "rd" is NIL the position is only changed
if "pos" is not M3CSrcPos.Null. If "rd" is not NIL the position is
changed to "pos" if it is not "M3CSrcPos.Null" and otherwise to line 1
offset 0. *)

END M3CParse.
