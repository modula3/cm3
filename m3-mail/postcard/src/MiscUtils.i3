(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* MiscUtils.def                                                 *)
(* Last modified on Fri Aug  7 12:03:15 PDT 1992 by birrell    *)

(* Miscellanous Postcard utility procedures. *)

INTERFACE MiscUtils;

IMPORT Rd, Thread, Wr;

(* *)
(* Operations on TEXT, Rd.T and Wr.T *)
(* *)

EXCEPTION BadFormat;

PROCEDURE ToInt(t: TEXT): INTEGER RAISES { BadFormat };
(* Return the integer corresponding to the characters in "t". The characters
   should be initial whitespace (perhaps empty), optional sign, one or more
   digits, then end of text or trailing whitespace.  *)

PROCEDURE Replace(dest: TEXT; offset, length: CARDINAL; source: TEXT): TEXT;
(* Returns:
        Text.Sub(dest, 0, offset) &
        source &
        Text.Sub(dest, offset+length, LAST(CARDINAL))
   *)

PROCEDURE PutTextSub(wr: Wr.T; t: TEXT; start: CARDINAL;
                     length: CARDINAL := LAST(CARDINAL))
        RAISES { Wr.Failure, Thread.Alerted };
(* Equivalent to "Wr.PutText(wr, Text.Sub(t, from, for))", but faster. *)

PROCEDURE Equal(t, u: TEXT; ignoreCase: BOOLEAN := FALSE): BOOLEAN;
(* Return TRUE iff t and u have the same length and would have the same
   contents if all their upper case letters were mapped to lower case by
   indexing Char.Lower *)

PROCEDURE Find(txt: TEXT; start: CARDINAL; pat: TEXT;
               ignoreCase: BOOLEAN := FALSE): INTEGER;
(* Return the smallest "i" such that 
   "Text.Sub(Text.Sub(txt,start,LAST(CARDINAL)), i, Text.Length(pat))" is
   "Equal" to "pat", or "-1" if there is no such "i".  A checked runtime
   error if either "txt" or "pat" is "NIL".  If ignoreCase, Find treats upper
   case characters as lower case in both txt and pat.*)

PROCEDURE FindInSub(READONLY sub: ARRAY OF CHAR; pat: TEXT;
                    ignoreCase: BOOLEAN := FALSE): INTEGER;
(* Return the smallest "i" such that "SUBARRAY(sub, i, Text.Length(pat))"
   contains exactly the characters of "pat", or "-1" if there is no such "i".
   A checked runtime error if "pat" is "NIL".  If ignoreCase, treats upper
   case characters as lower case in both sub and pat. *)


PROCEDURE RdFindChar(rd: Rd.T; pat: CHAR;
                     ignoreCase: BOOLEAN := FALSE): INTEGER
                     RAISES {Rd.Failure, Thread.Alerted};
(* Finds the first occurrence of pat, reading forward from the
   current position of rd.  If no match is found, Find returns -1 and
   leaves rd positioned at the end.  If Failure or Error is raised while
   reading characters from rd, the exception propagates through to the
   caller of Find and the position of rd is undefined.  If a match is
   found, Find returns the index of the matching character and
   leaves rd positioned to read the character following the match.  If
   ignoreCase, Find treats upper case characters as lower case in both rd
   and pat. *)

END MiscUtils.
