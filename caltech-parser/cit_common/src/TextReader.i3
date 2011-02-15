(*                                                                           *)
(*  TextReader.i3                                                            *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*  pushback portion: Karl Papadantonakis <kp@caltech.edu>                   *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
INTERFACE TextReader;
IMPORT TextList, Rd;
IMPORT Thread;
IMPORT FloatMode, Lex;

(* Think ``strtok''. A "TextReader.T" is initialized with 
|  txtRd := NEW(TextReader.T).init(string);

   Tokens may be parsed out by passing in a delimiter,
   as follows:

|  VAR
|    txt : TEXT;
|  BEGIN
|    WHILE txtRd.next(" ,.", txt) DO
|      ( parse txt )
|    END
|  END

   To get the rest of the line, pass ``'' as the delims.
   It is a checked run-time error to pass NIL as the delims or as line.
*)


EXCEPTION
  NoMore;

TYPE
  T <: Public;

(* All the methods of a "TextReader.T" leave the reader in a state
   to parse further untouched tokens. *)

  Public = OBJECT METHODS

    next(delims : TEXT; VAR chunk : TEXT; skipNulls := FALSE) : BOOLEAN;
(*     get next word before "delims" from reader.  If "skipNulls" is "TRUE", 
       zero-length strings are never returned.  Return value is "TRUE"
       if call succeeded.  If nothing was left, call fails, and returns
       "FALSE". *)

    nextS(READONLY delims : SET OF CHAR; 
          VAR chunk : TEXT; skipNulls := FALSE) : BOOLEAN;
(* "next" actually calls "nextS". *)

    nextE(delims : TEXT; skipNulls := FALSE) : TEXT RAISES { NoMore };
(* same as "next", except failure is signalled thru an exception *)

    nextSE(READONLY delims : SET OF CHAR; skipNulls := FALSE) : TEXT RAISES { NoMore };
(* same as "nextS", except failure is signalled thru an exception *)

    get() : TEXT RAISES { NoMore };
(* same as nextE(" \t\n\r", skipNulls := TRUE) *)

    getLR() : LONGREAL RAISES { NoMore, Lex.Error, FloatMode.Trap };
    getLongReal() : LONGREAL RAISES { NoMore, Lex.Error, FloatMode.Trap };
    getInt() : INTEGER RAISES { NoMore, Lex.Error, FloatMode.Trap };
    getCard() : CARDINAL RAISES { NoMore, Lex.Error, FloatMode.Trap };
    getBool() : BOOLEAN RAISES { NoMore, Lex.Error };

    init(line : TEXT) : T;
(* initialize a new "TextReader.T" *)

    initFromRd(rd : Rd.T) : T RAISES { Rd.Failure, Thread.Alerted };
(* initialize from an "Rd.T".  "rd" must eventually end (to allow in-memory
   implementations) *)

    isEmpty() : BOOLEAN;
(* probe a "TextReader.T" *)

    empty() : BOOLEAN;
(* alias for above *)

    shatter(listDelims : TEXT; 
            endDelims : TEXT; skipNulls := FALSE) : TextList.T;
(* tokenize a line into "TEXT" tokens until EOT or an endDelim.
   It is a checked runtime error for there to be an overlap between
   "listDelims" and "endDelims" *)

    pushBack(t: TEXT);
(* insert "t" before remaining unread "TEXT". "t" must end in
   delimiter(s) if the next call to "next" is not to run past the
   end of "t". Current implementation may or may not insert an
   extra invisible delimiter after "t" in some cases. This will not
   be seen if "skipNulls" is always "TRUE" and "t" always ends in a
   delimiter anyway. *)

    save() : Continuation;
    continue(from : Continuation);
(* permits parsing and returning to an old state *)
  END;

TYPE Continuation <: ROOT;

CONST Brand = "TextReader";

PROCEDURE New(txt : TEXT) : T;
(* equiv. to NEW(T).init(txt) *)

END TextReader.
    
