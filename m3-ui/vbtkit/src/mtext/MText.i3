(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Mon Nov 23 12:29:27 PST 1992 by meehan *)
(*      modified on Tue Jun 30 19:31:32 1992 by mhb *)
(* modified on Tue Jun 16 13:16:28 PDT 1992 by muller *)
(* modified on Fri Oct 7 14:05:01 1988 by chan *)
(* modified on Tue Jan 27 11:34:42 1987 by mbrown *)

INTERFACE MText;

(* An "MText" is a mutable sequence of characters whose internal
   representation is a balanced binary tree.  An "MText" is
   an appropriate abstraction for use in implementing a text
   editor, and perhaps other applications that perform lots of
   text modifications.  More documentation is at the end of this
   interface.

   "MText" is related to text and readers.  You should consult
   "Text.i3" and "Rd.i3" for information on these abstractions.

   This interface is monitored.  Readers are also synchronized
   with updates to the "MText" so that a reader fails when it tries
   to refill its buffer and discovers that the "MText" has been
   changed.

   If a procedure raises an exception, the mtext is left in an
   undefined state.  However, the client should be able to call
   "Close" on it.

   "MText" procedures use the ``begin/end/length'' semantics
   defined by the "Text" interface: if "begin" or "begin +
   length" or "end" exceed "Length(m)", then "Length(m)" will
   be used.  (A previous implementation raised checked runtime
   errors in such cases.)
   
 *)

IMPORT Rd;

TYPE
  T <: REFANY;
  Index = CARDINAL;             (* obsolete, but keep for
                                   compatibility *)

(* \subsection {Creating MTexts} *)

PROCEDURE New (t := ""; bufMax: CARDINAL := 256): T;
(* Returns a new "MText" containing the characters of "t".
   "bufMax" specifies the size of the mutable buffer used by
   "Replace" when it cannot perform editing within the buffer.
   The buffer is useful both during straight typing by a user and
   during insertions by a program (as in a typescript). *)

PROCEDURE ChangeBufMax (m: T; bufMax: CARDINAL);

PROCEDURE Close (m: T);
(* 
| = MText.Replace(m, 0, MText.Length(m), "")
*)

(* \subsection {Examining MTexts and Extracting Characters} *)

(* Note that readers on MText are available (see the "MTextRd"
   interface).  It is often more appropriate to create a reader
   than to create a text. *)

PROCEDURE Length (m: T): CARDINAL;
(* Returns the number of characters in "m". *)

PROCEDURE GetChar (m: T; index: CARDINAL): CHAR;
(* Returns the character at "index" in the text.  A checked error
   if "index >= Length(m)".  Warning: this is an expensive
   operation.  If it is going to be done several times, it would
   be better to use an "MTextRd.T". *)

PROCEDURE GetText (m    : T;
                   begin: CARDINAL := 0;
                   end  : CARDINAL := LAST (CARDINAL)): TEXT;
(* Returns a sub-sequence of "m".  The result will be empty if
   "begin >= Length(m)"; otherwise the range of indexes of the
   subsequence is "[begin ..  MIN (end, Length (m)) -1]". *)

(* \subsection {Modifying MTexts} *)

(* See ``Efficiency considerations'' below for a discussion of
   the performance implications of the various ways of modifying
   mtexts.  To delete characters from an mtext, use "Replace"
   with an empty string as the value of "newtext".  To insert
   characters, use any of the procedures below with "begin =
   end". *)

PROCEDURE Replace (m: T; begin, end: CARDINAL; newtext: TEXT);
(* Delete characters in the range "[begin .. end-1]", and insert the
   characters of "newtext" at "begin". *)

PROCEDURE ReplaceChars (m: T; begin, end: CARDINAL; READONLY str: ARRAY OF CHAR);
(* " = Replace (m, begin, end, Text.FromChars (str))" *)

PROCEDURE ReplaceFile (m         : T;
                       begin, end: CARDINAL;
                       rd        : Rd.T;
                       start     : CARDINAL   := 0;
                       numChars  : CARDINAL   := LAST (CARDINAL));
(* Delete characters in the range "[begin ..  end-1]", then
   insert the characters "[start ..  start+numChars-1]" from "rd"
   at "begin".

   While this procedure is intended to be used primarily for
   file-readers, any seekable, non-intermittent reader is
   acceptable.

   Characters from "rd" are read only as needed, not necessarily
   at the time "ReplaceFile" is called.  The client should
   therefore not call "Rd.Close(rd)" or make any changes to the
   underlying text. *)

END MText.

(* Introduction to Mutable Text (MText)

   A Mutable Text or MText is a REF representing a mutable
   sequence of characters.  The characters of an MText are
   indexed starting from 0, like those of a Text.  MText is an
   appropriate abstraction for use in implementing a text editor,
   and perhaps other applications that perform lots of text
   modifications.

   MText is well integrated with the more basic Text abstraction.
   It is easy to create an MText from a Text, and to extract
   Texts from an MText.  It is also easy to create an MText from
   a file.  The performance of these operations is generally
   good.

   Efficiency considerations

   The implementation of MText is a 'piece table'.  A piece table
   is (logically) a linear list of pieces, where each piece is
   some representation of a character sequence.  The piece table
   represents the concatenation of the character sequences.

   MText supports three types of pieces: subsequence of
   characters in an immutable Text, subsequence of characters in
   a reader (e.g., a readonly file), and a mutable buffer.  The
   mutable buffer is supported to avoid creating a huge number of
   pieces when performing individual character-insertions during
   typein.

   The number of pieces in an MText may increase linearly with
   the number of editing operations applied to it.  So MText uses
   a balanced binary tree to represent the list of pieces.  This
   means that reading and editing the MText can still be quite
   fast when the number of pieces is large.  The time to perform
   GetText(mtext,begin,end) is proportional to (log n + r) where
   n is the number of pieces in the MText and r is the number of
   pieces contributing characters to the result.  The fact that
   this is linear in r is not wonderful.  <<Eventually we should
   give the constant factors.>>

   When a reader is used as a text source, the MText
   implementation reads characters into virtual memory only when
   those characters are referenced.  This means that creating an
   MText from a reader takes constant time; it also means that if
   the reader's target is changed while the MText still includes
   the reader, the contents of the MText become undefined. *)




