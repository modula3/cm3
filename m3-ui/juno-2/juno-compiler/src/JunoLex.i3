(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Feb 15 16:27:29 1995 by gnelson *)
(*      modified on Wed Jul  7 16:49:36 PDT 1993 by heydon *)
(*      modified on Fri Aug  7 21:53:57 PDT 1992 by myers *)

INTERFACE JunoLex;

(* This interface defines Juno conceptual token streams and a procedure to
   create a new token stream from a reader. To use this interface, call
   "New()" to create a new stream "s", "s.next" to read tokens from the
   stream, and "Close()" to close the stream (and its associated reader).

   A stream is a monitor, so that concurrent operations on streams will appear
   atomic. *)

IMPORT Rd, JunoToken;

TYPE
  Private <: ROOT;
  Public = Private OBJECT lastPos := 0 METHODS
    next(): JunoToken.T RAISES {Error, Rd.Failure};
  END;
  Stream <: Public;

(* The call s.next() returns the next token from stream "s", first skipping
   any whitespace, and sets "s.lastPos" to the index of the first character of
   the token that it returns.  This token will be overwritten on the next call to
   "s.next()", so you must copy the token's value if you want to preserve it
   across such calls. It is a checked run-time error to attempt to get a token
   from a closed stream. *)

  ErrorKind = {
    BadInitialChar,	 (* character found not starting any legal token *)
    BadEscapeChar,       (* bad character following '\' in text literal *)
    BadReal,             (* real literal has illegal syntax *)
    UnclosedComment,     (* comment started but not closed before EOF *)
    UnclosedText };      (* text string started but not closed before EOF *)

  ErrorRec = REF RECORD
    kind: ErrorKind;
    initialChars: TEXT
  END;

EXCEPTION
  Error(ErrorRec);	 (* the next token is not a legal Juno token *)

(* If the next token is not lexically valid, "next" raises the "Error"
   exception, providing a value of type "ErrorRec".  The "kind" field
   describes the kind of lexical error that occurred. The "initialChars" field
   contains the characters read in the current token up to the point of
   failure. The reader associated with the "Stream" points to the offending
   character. *)

PROCEDURE New(rd: Rd.T): Stream RAISES {Rd.Failure};
(* Create and initialize a new, live stream to read tokens from "rd". *)

PROCEDURE Close(s: Stream): TEXT;
(* Close the stream "s". Any future calls to s.next() will cause a checked
   run-time error. This call does not close the underlying reader. It
   returns any characters that have been read from the underlying reader,
   but which have not yet been returned in a token. *)

PROCEDURE ErrorText(e: ErrorKind): TEXT;
(* Return a text version of the error "e". *)

END JunoLex.
