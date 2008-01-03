(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Scanner.i3                                          *)
(* Last modified on Thu Dec  8 10:42:16 PST 1994 by kalsow     *)

INTERFACE M3Scanner;

IMPORT Rd, M3Token;

TYPE
  T = OBJECT
    token        : M3Token.T := M3Token.T.Comment;
    start        : CARDINAL  := 0;
    length       : CARDINAL  := 0;
    offset       : CARDINAL  := 0;
    line         : CARDINAL  := 0;
    column       : CARDINAL  := 0;
    msg          : TEXT      := NIL;
    buffer       : Buf       := NIL;
  METHODS
    next ();
  END;

  Buf = REF ARRAY OF CHAR;

TYPE
  Default <: T OBJECT METHODS
    initFromRd  (source        : Rd.T;
                 skip_comments := TRUE;
                 split_pragmas := TRUE): T;
    initFromBuf (buf           : Buf;
                 skip_comments := TRUE;
                 split_pragmas := TRUE): T;
  END;

END M3Scanner.

(*
An "M3Scanner.T", or scanner, parses a stream of characters and
returns a stream of Modula-3 tokens. If "s" is a scanner, each
call "s.next()" sets the values of "s"'s fields to correspond to
the next token in the stream.
   
The fields of a scanner are not to be modified by its client.

"s.token" is the class of the token.

In "s.buffer[s.start .. s.start+s.length-1]" are the characters
that comprise the token.

"s.offset" is the character offset of the token relative to the
beginning of the stream.  The first character of the stream is
at offset zero.

"s.line" is the line where the token occured relative
to the beginning of the stream.   The first line is one.

"s.column" is the character offset of the beginning of the token
within the line that contains it.  The first column is zero.

"s.buffer" contains the source being scanned.  Modifying its contents
may perturb the token stream or cause a checked runtime error.

"s.msg" describes the error that caused the "Error" token to be
returned.

The scanner returned by "NEW(Default).initFromRd(rd)" will read the
entire contents of "rd" into its buffer and initialize
the scanner as a zero-length comment at offset zero.

The scanner returned by "NEW(Default).initFromBuf(buf)" will use
"buf" as its buffer and initialize the scanner as a zero-length
comment at offset zero.

If "skip_comments" is "TRUE", outer-level comments will be returned
as tokens.  Otherwise, comments are ignored.

If "split_pragmas" is "TRUE", the contents of pragmas will be scanned
and returned as a stream of tokens between "Begin_pragma" and
"End_pragma" tokens.  If "split_pragmas" is false, the entire pragma
is returned in a single "Begin_pragma" token.
*)
