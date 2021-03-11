(*                                                                           *)
(*  TextUtils.i3                                                             *)
(*                                                                           *)
(*  Some useful text processing routines for the PL1 compiler.               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
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
(* $Id$ *)
INTERFACE TextUtils;
IMPORT TextList, TextSet, IntList;
IMPORT TextSeq;

(* replace every occurrence of "old" by "new" in "in" *)
PROCEDURE Replace(in, old, new : TEXT) : TEXT;
PROCEDURE ReplaceChar(in : TEXT; old, new : CHAR) : TEXT;
PROCEDURE CountCharOccurences(in: TEXT; c: CHAR): CARDINAL;

PROCEDURE Filter(in: TEXT; keep: SET OF CHAR): TEXT;
PROCEDURE FilterOut(in: TEXT; remove := SET OF CHAR{' ', '\t', '\n'}): TEXT;
PROCEDURE FilterEdges(in: TEXT; remove := SET OF CHAR{' ', '\t', '\n'}): TEXT;

PROCEDURE FindSub(in, sub : TEXT; VAR pos : CARDINAL; start := 0) : BOOLEAN;
(* find first occurrence of sub in in.  If not found, pos is not touched. *)

PROCEDURE FindText(in, sub : TEXT; start := 0) : [-1..LAST(CARDINAL)];
(* as FindSub but returns -1 if not found *)

PROCEDURE FindAnyChar(in: TEXT; c: SET OF CHAR;
                      VAR pos: CARDINAL; start := 0): BOOLEAN;

PROCEDURE HaveSub(in, sub : TEXT) : BOOLEAN;
  (* have substr? *)

PROCEDURE HavePrefix(in, prefix: TEXT): BOOLEAN;
PROCEDURE HaveSuffix(in, suffix: TEXT): BOOLEAN;

PROCEDURE RemovePrefix(in, prefix: TEXT): TEXT;
  (* checked runtime error for prefix not to be as stated *)

PROCEDURE RemoveSuffix(in, suffix: TEXT): TEXT;
  (* checked runtime error for suffix not to be as stated *)

PROCEDURE RemoveSuffixes(in : TEXT; READONLY suffixes : ARRAY OF TEXT):TEXT;
  (* removes any matching suffix; do nothing if no match *)

PROCEDURE InfixFormat(sep : TEXT; 
                      list : TextList.T; ignoreNulls := FALSE) : TEXT;

PROCEDURE Pluralize(noun : TEXT; count : INTEGER; 
                    ending := "s"; printNum := TRUE) : TEXT ;

PROCEDURE ListToSet(list : TextList.T) : TextSet.T;
PROCEDURE SetToList(set : TextSet.T) : TextList.T;

PROCEDURE Shatter(t: TEXT;
                  delims:="\t, ";
                  endDelims:="\n;#%";
                  skipNulls:=TRUE): TextList.T;
(* E.g. "TRY LOOP l:=TextUtils.Shatter(Rd.GetLine(rd)); ... "
   parses lines of words, ignoring comments. *)

PROCEDURE ShatterInts(t: TEXT;
                      defaultBase := 10;
                      delims      := ":.\t, ";
                      endDelims   := "\n;#%"): IntList.T;

PROCEDURE Capitalize(t: TEXT; uniqueSuffix := ""): TEXT;
(* capitalize "t"; if result would be equal to "t", append "uniqueSuffix". *)

PROCEDURE BreakLongLines(t: TEXT; atCol := 79): TEXT;
(* Break lines longer than "atCol" chars. "atCol=0" does nothing. *)

PROCEDURE GetLines(t: TEXT; n: INTEGER; firstBreakLongAtCol:=79): TEXT;
(* positive "n" for first "n" lines; negative for last "|n|" lines; *)

PROCEDURE Assemble(t: TextList.T; postDelim:=" "; skipLastDelim:=TRUE): TEXT;

(* the following from "htmltable.Utils": *)
PROCEDURE SplitText(text : TEXT; at : CHAR; VAR beg, end : TEXT);
PROCEDURE CountChars(text: TEXT; what : CHAR) : CARDINAL;

PROCEDURE ToChars(text: TEXT): REF ARRAY OF CHAR;

PROCEDURE ToUpper(text : TEXT) : TEXT;
PROCEDURE ToLower(text : TEXT) : TEXT;

PROCEDURE EqualIgnoringCase(t1, t2 : TEXT) : BOOLEAN;

PROCEDURE FormatInfix(seq : TextSeq.T; operator : TEXT) : TEXT;
  (* return a text that consists of the elements in the sequence with
     the operator inserted between each pair, but not at the end *)

PROCEDURE FormatInfixArr(READONLY arr : ARRAY OF TEXT; operator : TEXT) : TEXT;
  (* same, for an array *)

END TextUtils.
