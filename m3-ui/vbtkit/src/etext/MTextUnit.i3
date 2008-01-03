(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat May 29 17:42:41 PDT 1993 by meehan     *)
(*      modified on Tue Jun 16 13:08:42 PDT 1992 by muller     *)
(*      modified on Tue Feb  4 16:47:38 PST 1992 by mhb        *)
(*      modified on Fri Aug 11 15:05:54 PDT 1989 by brooks     *)
(*      modified on Fri Jul 28 00:44:23 1989 by chan           *)
(*      borrowed and de-ivified from IvyUnit,
                   Mon Aug  7 15:35:29 PDT 1989 by brooks      *)
(*      modified on Wed May 24 15:47:44 PDT 1989 by mbrown     *)
(*      modified on Fri Jun  3 12:21:14 PDT 1988 by mcvl       *)

INTERFACE MTextUnit;

IMPORT MText, ISOChar;

TYPE
  T = MText.T;

  (* This module imposes structure on character sequences in an
     "MText.T".  There are three different structures supported:
     {\it runs}, {\it lines}, and {\it paragraphs}.  Each
     structure defines a {\it unit}, in such a way that the
     entire mtext can be viewed as a sequence of non-overlapping
     units, possibly separated by characters that belong to no
     unit.

     A {\it run} is defined by a set of characters, typically
     "ISOChar.AlphaNumerics".  A unit then corresponds to a ``word'',
     and the mtext is seen as a sequence of ``words'' separated by
     ``non-word'' characters.

     A {\it line} is defined as a set of characters delimited by
     newlines.  A unit corresponds to a single line.  The mtext
     is viewed as a sequence of lines with 0-length separators.

     A {\it paragraph} is a maximal sequence of non-blank lines.

     Given an index "n" into the text, we define the {\it extent}
     "e" for runs, lines, and paragraphs as a record containing
     the left and right boundaries of the unit surrounding the
     index, plus a boolean value indicating whether the character
     at position "n" is inside a unit or between units.  Normally,
     "e.left <= index < e.right".  The only exception is when
     "index" is greater than or equal to the length of the text,
     in which case "e.left = e.right = length" and "e.inside
     = FALSE".

     There are also utilities for handling blank lines and for
     finding the leading and trailing blanks on a line.

     If you pass an index that's less than 0, it is treated as 0;
     if you pass an index that's greater than the length of the
     text, it is treated as the length of the text.
*)

  (* {\it Note for future work:  There is nothing in the nature
     of runs, lines, and paragraphs that is specific to
     "MText.T".  It would be at least as useful to provide this
     same functionality for "TEXT", "ARRAY" "OF CHAR", and
     seekable readers.} *)

CONST
  WordRun = ISOChar.AlphaNumerics;
  BlankRun = ISOChar.Spaces; (* {' ', '\n', '\t', '\r', '\f'} *)
  NonBlankRun = ISOChar.All - BlankRun;

TYPE
  Extent = RECORD
             left, right: INTEGER;
             inside     : BOOLEAN
           END;

(* \subsubsection{Runs} *)

PROCEDURE RunExtent (         t            : T;
                              index        : INTEGER;
                     READONLY includedChars := WordRun):
  Extent;

PROCEDURE StartOfRun (t    : T;
                      index: INTEGER;
                      READONLY includedChars := WordRun):
  INTEGER;
(* Return the largest "i <= index" for which "IsStartOfRun(t, i,
   includedChars)" is "TRUE".  Return "-1" if there is no such
   "i". *)

PROCEDURE IsStartOfRun (t    : T;
                        index: INTEGER;
                        READONLY includedChars := WordRun):
  BOOLEAN;
(* Equivalent to
| WITH e = RunExtent(t, index, includedChars) DO
|  RETURN e.inside AND e.left = index END
   *)

PROCEDURE EndOfRun (t    : T;
                    index: INTEGER;
                    READONLY includedChars := WordRun):
  INTEGER;
(* Return the smallest "i >= index" for which "IsEndOfRun(t, i,
   includedChars)" is "TRUE".  Return "-1" if there is no such
   "i". *)

PROCEDURE IsEndOfRun (t    : T;
                      index: INTEGER;
                      READONLY includedChars := WordRun):
  BOOLEAN;
(* Equivalent to
| WITH e = RunExtent(t, index, includedChars) DO
|   RETURN NOT e.inside AND e.left = index END
    or
| IsStartOfRun(t, index, ISOChar.All - includedChars)
   *)

(* \subsubsection{Lines} *)

TYPE LineOption = 
  {ExcludeBlanks, IncludeBlanks, IncludeNewline};

(* A {\it line} is a sequence of characters delimited by newlines
   (or the mtext boundaries).  The client may specify a
   subsequence of the line by passing left and right
   ``line options.''  If the left option is "ExcludeBlanks", then
   the subsequence will not include any initial blanks.  If the
   left option is "IncludeBlanks", then it will.  If the left
   option is "IncludeNewline", it will be treated the same as
   "IncludeBlanks".

   If the right option is "ExcludeBlanks", then the subsequence
   will not include any trailing blanks.  If the right option is
   "IncludeBlanks", then it will.  If the right option is
   "IncludeNewline", then it will include both the trailing
   blanks and the following newline.

   The default left option is "IncludeBlanks"; the default right
   option is "IncludeNewline".  This is consistent with the view
   of the text as a sequence of lines, with no intermediate
   ``gaps.'' *)

PROCEDURE LineExtent (t: T; index: INTEGER): Extent;
(* Compute the extent of the line surrounding index.  If a
   newline immediately precedes index, it uses the line that
   begins at index, not the one that ends there.  "LineExtent(t,
   index).inside" is always "TRUE". *)

TYPE
  LineRec = RECORD
              left       : INTEGER;  (* start of line *)
              leftMargin : INTEGER;  (* first non-blank *)
              rightMargin: INTEGER;  (* 1 + last non-blank *)
              rightEnd   : INTEGER;  (* final newline *)
              right      : INTEGER;  (* rightEnd + 1 *)
            END;
(* A "LineRec" contains the positions within a line.  Normally,

| left <= leftMargin <= rightMargin <= rightEnd

   and "right = rightEnd + 1", i.e., the "left" of the next line,
   but there are exceptions.  On a line consisting entirely of
   blanks (at least one),
   
| left < leftMargin = rightMargin = rightEnd

   On the last line of the file (following the last newline),
   "right = rightEnd".

   If "index = length(t)" and there is a newline immediately
   preceding "index", then all 5 values will be equal to
   "index". *)

PROCEDURE LineInfo (t: T; index: INTEGER): LineRec;

PROCEDURE LineFacts (              t          : T;
                                   index      : INTEGER;
                     VAR (* out *) left       : INTEGER;
                     VAR (* out *) leftMargin : INTEGER;
                     VAR (* out *) rightMargin: INTEGER;
                     VAR (* out *) rightEnd   : INTEGER;
                     VAR (* out *) right      : INTEGER  );

PROCEDURE StartOfLine (t    : T;
                       index: INTEGER;
                       leftOption := LineOption.IncludeBlanks):
  INTEGER;
(* IF "leftOption = ExcludeBlanks", then return "LineInfo(t,
   index).leftMargin".  Otherwise return "LineInfo(t,
   index).left". *)

PROCEDURE IsStartOfLine (t    : T;
                         index: INTEGER;
                         leftOption := LineOption.IncludeBlanks):
  BOOLEAN;
(* Return "index = StartOfLine(t, index, leftOption)" *)

PROCEDURE EndOfLine (t    : T;
                     index: INTEGER;
                     rightOption := LineOption.IncludeNewline):
  INTEGER;
(* Return the "rightMargin", "rightEnd", or "right" field of
   "LineInfo(t, index)", depending on whether "rightOption" is
   "ExcludeBlanks", "IncludeBlanks", or "IncludeNewline",
   respectively. *)

PROCEDURE IsEndOfLine (t    : T;
                       index: INTEGER;
                       rightOption := LineOption.IncludeNewline):
  BOOLEAN;
(* Return "index = EndOfLine (t, index, rightOption)" *)

(* \subsubsection{Blank lines} *)

PROCEDURE IsBlankLine (t: T; index: INTEGER): BOOLEAN;
(* Return "TRUE" if "index" is located on a line that consists
   entirely of blanks, tabs, and form-feeds, including empty
   lines. *)

PROCEDURE BlankLinesExtent (t: T; ndex: INTEGER): Extent;
(* Computes the extent of maximal sequence of blank lines
   surrounding "index". *)

(* \subsubsection{Paragraphs} *)

(* A {\it paragraph} is a maximal sequence of non-blank lines, in
   the sense of "LineExtent" where the left option is
   "IncludeBlanks" and the right option is "IncludeNewline". *)

PROCEDURE ParagraphExtent (t: T; index: INTEGER): Extent;

PROCEDURE StartOfParagraph (t: T; index: INTEGER): INTEGER;
(* Return the largest "i <= index" for which
   "IsStartOfParagraph(t, i)" is "TRUE".  Return "-1" if there is
   no such "i". *)

PROCEDURE IsStartOfParagraph (t: T; index: INTEGER): BOOLEAN;
(* Equivalent to
| WITH e = ParagraphExtent(t, index) DO
|  RETURN e.inside AND e.left = index END
   *)

PROCEDURE EndOfParagraph (t: T; index: INTEGER): INTEGER;
(* Return the smallest "i >= index" for which
   "IsEndOfParagraph(t, i)" is "TRUE".  Return "-1" if there is
   no such "i". *)

PROCEDURE IsEndOfParagraph (t: T; index: INTEGER): BOOLEAN;
(* Equivalent to
| WITH e = ParagraphExtent(t, index) DO
|  RETURN NOT e.inside AND e.left = index END
   *)

END MTextUnit.
