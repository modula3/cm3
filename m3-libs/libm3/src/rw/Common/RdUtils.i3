(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May 13 17:10:05 PDT 1993 by swart                    *)
(*      modified on Mon Feb 15 12:15:24 PST 1993 by mjordan                  *)
(*      modified on Sat Jun 27 15:09:07 PDT 1992 by muller                   *)
(*      modified on Wed Jan 15 19:46:53 PST 1992 by meehan                   *)

INTERFACE RdUtils;

IMPORT Rd, Thread, AtomList;

PROCEDURE Find (rd: Rd.T; pattern: TEXT; canonicalize: Canonicalize := NIL):
  [-1 .. LAST(CARDINAL)] RAISES {Rd.Failure, Thread.Alerted};
(* Finds the first occurrence of pattern, reading forward from the current
   position of rd. If no match is found, Find returns -1 and leaves rd
   positioned at the end. If Rd.Failure, or Thread.Alerted is raised
   while reading characters from rd, the exception propagates through to
   the caller of Find and the position of rd is undefined. If a match is
   found, Find returns the index of the first character of the match and
   leaves rd positioned to read the character following the match. A null
   pattern causes an instant match, with no advancement of rd. If
   canonicalize is set, Find uses canonicalize to map characters
   in both rd and pattern into a connonical form.  See IgnoreASCIICase
   below.

   This uses a pretty poor algorithm and demands that the reader be seekable.
*)

PROCEDURE FindString (         rd          : Rd.T;
                      READONLY pattern     : ARRAY OF CHAR;
                               canonicalize: Canonicalize    := NIL):
  [-1 .. LAST(CARDINAL)] RAISES {Rd.Failure, Thread.Alerted};
(* = Find(rd, Text.FromSub(pattern), canonicalize). *)

PROCEDURE FindChar (rd          : Rd.T;
                    pattern     : CHAR;
                    canonicalize: Canonicalize := NIL):
  [-1 .. LAST(CARDINAL)] RAISES {Rd.Failure, Thread.Alerted};
(* = Find(rd, Text.FromChar(pattern), canonicalize). *)

PROCEDURE FailureText (f: AtomList.T): TEXT;
(* Rd.i3 says: "EXCEPTION Failure (AtomList.T)".

   FailureText returns a text describing the failure "f".
   Returns either "NIL" or the names of the Atom.ToText of each
   element of this list separated by colons.
   *)

TYPE
  Canonicalize = PROCEDURE(ch: CHAR): CHAR;

PROCEDURE ToUpperCaseASCII(ch: CHAR): CHAR;
  (* Converts ASCII lower case characters to upper case, returns all
     other characters unchanged.  See ASCII.i3. *)

END RdUtils.
