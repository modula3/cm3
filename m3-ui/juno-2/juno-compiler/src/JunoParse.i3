(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Dec 29 11:02:09 PST 1995 by heydon                   *)
(*      modified on Sat Feb 18 15:55:54 PST 1995 by gnelson                  *)
(*      modified on Fri Aug  7 21:53:58 PDT 1992 by myers                    *)

INTERFACE JunoParse;

(* This interface provides the procedures "Block", "Command", and "Expression"
   for parsing Juno block (declarations), commands, and expressions. The Juno
   grammar implemented by this interface is described in the text file:
|     /proj/m3/pkg/juno-compiler/src/Grammar/Juno.bnf

   All the parsing procedures in this module guarantee that any newly created
   AST nodes have a back pointer to the sentinel "JunoAST.End". *)

IMPORT JunoAST, JunoToken, Rd, JunoLex;

(* Each of the parsing procedures takes its input from an abstract token
   stream. Typically, the stream is created from a reader. *)

TYPE IterativeParse <: REFANY;
(* An "IterativeParse" object has an associated token stream. The stream can
   be passed as an argument to the "Block" procedure to successively parse the
   top-level blocks of an entire module. *)

PROCEDURE StartIterativeParse(READONLY rd: Rd.T): IterativeParse
  RAISES {JunoLex.Error, Rd.Failure};
(* Returns a newly initialized "IterativeParse" object with a token stream
   supplied from "rd". *)

PROCEDURE FinishIterativeParse(ip: IterativeParse) RAISES {Rd.Failure};
(* Close "ip"'s token stream. *)

(* Each of the following procedures parses its input according to a particular
   non-terminal of the Juno grammar, and returns its result as a "VAR (*OUT*)"
   parameter named "ast". The "VAR (*OUT*)" parameter named "tokenCnt" is set
   to contain the number of tokens parsed and successfully incorporated into
   the result "ast".

   These procedures raise either "Error" if the tokens in their input stream
   do not form a legal syntactic block, command, or expression, respectively.
   They raise "JunoLex.Error" in case of a lexical error in their input. If
   the procedures cannot read from the stream, they raise the "Rd.Failure"
   exception. *)

PROCEDURE Block(
    ip: IterativeParse;
    VAR (*OUT*) ast: JunoAST.Block;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure};
(* Parse the next "Block" non-terminal from the reader underlying "ip". A
   module is a sequence of top-level blocks. Returns with "ast = NIL" when
   "ip"'s token stream is empty. *)

PROCEDURE GetIndex(
    ip: IterativeParse): INTEGER;
(* Return the number of characters that occur in the source text for the
   ASTs that have been returned so far by calls to "Block(ip,...)", including
   any trailing whitespace.  This is the index of the first character of the first
   token of the AST that will be returned by the next call to "Block(ip,...)". *)

PROCEDURE Command(
    READONLY rd: Rd.T;
    VAR (*OUT*) ast: JunoAST.Cmd;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure};
(* Parse a "Cmd" non-terminal from "rd". *)

PROCEDURE Expression(
    READONLY rd: Rd.T;
    VAR (*OUT*) ast: JunoAST.Expr;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure};
(* Parse an "Expr" non-terminal from "rd". *)

PROCEDURE FoldHeader(
    READONLY rd: Rd.T;
    VAR (*OUT*) ast: JunoAST.PredHeader;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure};
(* Parse an "Id" optionally applied to a list of "Id"s, and set "ast" to the
   result. If "rd" contains just an "id", it will be returned in "ast.name"
   and "ast.ins" will be "NIL". Otherwise "ast.ins" will contain the list
   of arguments (which may be "EmptyIdList"). *)

PROCEDURE IdList(
    READONLY rd: Rd.T;
    VAR (*OUT*) ast: JunoAST.IdList;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure};
(* Parse a comma-separated list of identifiers, and set "ast" to the result.
   An empty list is allowed. *)

TYPE
  ErrorRec = REF RECORD
    found: JunoToken.T;	
    expected: JunoToken.Kind;   (* may be "JunoToken.Kind.Unknown" *)
    additional: TEXT;
  END;

EXCEPTION Error(ErrorRec);

(* The exception "Error" is raised when the input does not form a legal
   sentence derivable from the specified non-terminal. The returned
   "ErrorRec" contains information about the parse error.

   The "found" field contains the token which caused the parser to fail. In
   some cases, the parse fails because the parser is expecting a single
   particular token in the token stream. In such cases, "expected" is
   the expected token; otherwise, it is "JunoToken.Kind.Unknown".

   The field "additional" contains characters which properly follow
   the "found" token, but which have been pulled off the underlying
   reader.

   In the event that the "Error" exception is raised, the "ast"	parameter will
   contain the partial AST corresponding to the tokens that have been parsed
   so far; the AST will contain "NIL" fields for those parts of the input that
   were not parsed before the error occurred.  *)

END JunoParse.

