(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr  2 15:37:23 PST 1993 by meehan                   *)
(*      modified on Mon Feb  1 10:09:35 PST 1993 by mhb                      *)
(*      modified on Tue Jun 16 13:08:09 PDT 1992 by muller                   *)
(*      modified on Tue Feb 26 15:11:39 PST 1991 by brooks                   *)
(*      modified on Fri Sep 28 13:47:30 PDT 1990 by birrell                  *)
<* PRAGMA LL                                                                 *>

(* The "TextPortClass" interface reveals more of the representation of
   a textport, and it defines the object-type ("Model") that is used
   to implement keybindings and selection-controls.  Four subtypes of
   models are implemented: Ivy, Emacs, Mac, and Xterm.
   "TextPort.Model" is an enumeration type for the four names, but
   "TextPortClass.Model" is the type of the actual object attached to
   a textport, to which user-events (keys, mouse clicks, position
   reports) are sent.

   In this interface, the variable "v" always refers to a textport,
   and the variable "m" always refers to a model.

   Unless otherwise noted, the locking level of all procedures in this
   interface is "v.mu".
*)

INTERFACE TextPortClass;

IMPORT Font, KeyFilter, PaintOp, Rd, ScrollerVBTClass,
       TextPort, Thread, VBT, VTDef, VText;

REVEAL TextPort.T <: T;

TYPE VType = {Focus, Source, Target};
(* Constants for the three Trestle selections used here. *)

CONST
  Primary   = TextPort.SelectionType.Primary;
  Secondary = TextPort.SelectionType.Secondary;
  Focus     = VType.Focus;
  Source    = VType.Source;
  Target    = VType.Target;

TYPE
  Pixels = CARDINAL;
  T =
    TextPort.Public OBJECT
      mu: MUTEX;                 (* VBT.mu < mu *)
      <* LL = mu *>
      m             : Model              := NIL;
      readOnly      : BOOLEAN;
      vtext         : VText.T;

      font          : Font.T;
      fontHeight    : Pixels             := 0;
      charWidth     : Pixels             := 0;
      scrollbar     : Scrollbar          := NIL;
      typeinStart   : CARDINAL;

      thisCmdKind   : CommandKind;
      lastCmdKind   : CommandKind;
      wishCol       : CARDINAL;

      cur           : UndoRec;

      owns := ARRAY VType OF BOOLEAN {FALSE, ..};

      <* LL.sup = VBT.mu.SELF *>
      lastNonEmptyWidth: Pixels := 0;

    METHODS
      <* LL = SELF.mu *>
      getText       (begin, end: CARDINAL): TEXT;
      index         (): CARDINAL;
      isReplaceMode (): BOOLEAN;
      length        (): CARDINAL;
      normalize     (to := -1);

      replace (begin, end: CARDINAL; newText: TEXT):
               TextPort.Extent;
      unsafeReplace (begin, end: CARDINAL; newText: TEXT):
                     TextPort.Extent;
      insert           (t: TEXT);
      unsafeInsert     (t: TEXT);
      getKFocus        (time: VBT.TimeStamp): BOOLEAN;
      newlineAndIndent ();

      findSource (time      : VBT.TimeStamp;
                  loc                         := Loc.Next;
                  ignoreCase                  := TRUE      );
      notFound ();

      (* All of these call SELF.error. *)
      vbterror   (msg: TEXT; ec: VBT.ErrorCode);
      vterror    (msg: TEXT; ec: VTDef.ErrorCode);
      rdfailure  (msg: TEXT; ec: REFANY);
      rdeoferror (msg: TEXT);

      (* We release SELF.mu around the following callbacks. *)
      ULreturnAction (READONLY cd: VBT.KeyRec);
      ULtabAction    (READONLY cd: VBT.KeyRec);
      ULfocus        (gaining: BOOLEAN; time: VBT.TimeStamp);
      ULmodified     ();
      ULerror        (msg: TEXT);

    END;

(* "v.font" is the current font. "v.fontHeight" is the height of a
   (maximal) character. "v.charwidth" is the width of a (maximal)
   character. "v.scrollbar" contains the scrollbar that is updated
   when the visible region of text changes, and vice versa.

   "v.typeinStart" is meaningful only for typescripts, where it
   indicates the point that divides the ``history'' part of the
   transcript, which is read-only, from the current command line,
   which is not.  See the "TypescriptVBT" interface.  For
   non-typescripts, this field is always zero.

   "v.thisCmdKind" and "v.lastCmdKind" allow the interpretation of a
   command to depend on the previous command.  Currently, the only
   commands that depend on context are the ``vertical'' commands that
   call "UpOneLine" and "DownOneLine". The column to which they move
   is stored in "v.wishCol".
   
   "v.cur" holds the information needed to reverse or reinstate the
   effects of editing operations that change the text.

   "v.owns[vtype]" is "TRUE" when "v" owns the "VBT.Selection"
   corresponding to "vtype": keyboard focus, Source selection, or
   Target selection.
   
   "v.lastNonEmptyWidth" is used by the "shape" and "reshape" methods.
   
   "v.replace" tests "v.readOnly"; if that is "TRUE", then it
   returns the constant "TextPort.NotFound".  Otherwise it calls
   "v.unsafeReplace", which is the only routine that actually
   alters the underlying text. (The ``unsafe'' methods are those
   that do not test "v.readOnly".)

   "v.insert" calls "v.replace"; i.e., it is safe.

   "v.notFound" is called when a search fails; see "FindAndSelect",
   below. The default method is a no-op.
*)

TYPE
  CommandKind = {VertCommand, OtherCommand};
  Scrollbar = ScrollerVBTClass.T OBJECT
                textport: T
              METHODS
                update ()        <* LL = SELF.textport.mu *>
              END;

(* \subsubsection{Models}\index{Model}

   A "TextPortClass.Model" is the object that interprets keyboard and
   mouse events. The model can be replaced via "v.setModel".

   {\em Keybindings}

   Trestle calls "v.key(cd)", which calls "m.keyfilter.apply(v,cd)",
   as described on page~\pageref{TextPortKeybindings}. A keyfilter is
   essentially a linked list of objects, each of which implements some
   low-level character translation such as ``quoted insert'' or
   ``compose character.'' The last link calls "v.filter(cd)", which
   calls "m.controlChord" or "m.optionChord" for ``command-keys'',
   or "m.arrowKey" for cursor-keys.
   
   {\em Text-selections}

   As explained on page~\pageref{TextPortSelections}, the model
   interprets keyboard and mouse events to establish the local
   selections, Primary and Secondary, which are subsequences of the
   text, usually highlighted in some way. The model also deals with
   the global selections, Source and Target, which may be owned
   (``acquired'') by any VBT or by an external program such as an
   Xterm shell. The owner of a global selection controls its contents;
   "read" and "write" calls are forwarded to the owner.

   A particular model may establish an ``alias'' relationship between
   a local selection and a global selection, which means that if the
   textport owns the global selection, then its contents are identical
   with (mapped to) the local selection. For example, in an Xterm
   shell, and therefore in the Xterm model, Primary is an alias for
   Source, which means that when you click and drag to highlight a
   region, that defines not only the local Primary selection but the
   global Source selection as well. Any program that asks to read the
   Source selection will be given a copy of the highlighted text.

   In Ivy, Primary is an alias for Target, and Secondary is an alias
   for Source. (Ivy users therefore have a hard time understanding
   the distinction between local and global selections, since they
   are wired together.)

   A Primary selection in a non-readonly textport may be in ``replace
   mode'' (or ``pending-delete mode''). In this mode, insertions
   replace the entire selection; Backspace deletes the entire
   selection.

   {\em Selection-related editing operations}
   \index{Cut}\index{Copy}\index{Paste}\index{Clear}
   \index{Select All}

   The standard editing operations such as Cut, Copy, and Paste, are
   defined not merely in terms of the underlying text, but also in
   terms of the effects they have on the local and global selections.
   Indeed, they are not functions at all; Copy does not return a copy
   of anything. 

   \begin{description}

   \item[Copy] If the Primary selection is not empty, then acquire
   Source, and unless Primary is an alias for Source, make a copy of
   the Primary selection as the contents of Source. (If Primary is an
   alias for Source, no copy is needed.)

   \item[Paste] If the Primary selection is not empty and is in
   replace-mode, then replace the Primary selection with the contents
   of Source. Otherwise, insert the contents of Source at the type-in
   point.

   \item[Clear] Delete the contents of the Primary selection.

   \item[Cut] This is defined as {\bf Copy} followed by {\bf Clear}.

   \item[Select All] Extend the Primary selection to include the
   entire text.

   \end{description}
*)

TYPE
  Model <: PublicModel;
  PublicModel =
    OBJECT
      v: T;
      selection := ARRAY TextPort.SelectionType OF
                     SelectionRecord {NIL, NIL};
      dragging := FALSE;
      dragType := TextPort.SelectionType.Primary;
      approachingFromLeft: BOOLEAN;
      keyfilter          : KeyFilter.T
    METHODS
      <* LL = SELF.v.mu *>
      init  (cs: PaintOp.ColorScheme; keyfilter: KeyFilter.T):
             Model;
      close ();
      seek  (position: CARDINAL);

      (* Keybindings *)
      controlChord (ch: CHAR; READONLY cd: VBT.KeyRec);
      optionChord  (ch: CHAR; READONLY cd: VBT.KeyRec);
      arrowKey     (READONLY cd: VBT.KeyRec);

      (* Mouse and Selection-controls *)
      mouse    (READONLY cd: VBT.MouseRec);
      position (READONLY cd: VBT.PositionRec);
      misc     (READONLY cd: VBT.MiscRec);
      read     (READONLY s   : VBT.Selection;
                         time: VBT.TimeStamp): TEXT
             RAISES {VBT.Error};
      write    (READONLY s   : VBT.Selection;
                         time: VBT.TimeStamp;
                         t   : TEXT           )
             RAISES {VBT.Error};

      cut   (time: VBT.TimeStamp);
      copy  (time: VBT.TimeStamp);
      paste (time: VBT.TimeStamp);
      clear ();
      select (time : VBT.TimeStamp;
              begin: CARDINAL        := 0;
              end  : CARDINAL        := LAST (CARDINAL);
              sel                    := Primary;
              replaceMode            := FALSE;
              caretEnd               := VText.WhichEnd.Right);

      getSelection    (sel := Primary): TextPort.Extent;
      getSelectedText (sel := Primary): TEXT;
      putSelectedText (t: TEXT; sel := Primary);
      takeSelection (READONLY sel : VBT.Selection;
                              type: TextPort.SelectionType;
                              time: VBT.TimeStamp           ):
                     BOOLEAN;
      highlight (rec: SelectionRecord; READONLY r: IRange);
      extend    (rec: SelectionRecord; left, right: CARDINAL)
    END;

(* "m.init(...)" initializes a Model "m".  The default method stores
   "keyfilter" and returns "m".

   "m.close()" releases the "VBT" selections (Source, Target, and
   KBFocus) and deletes highlighting intervals.

   "m.seek(position)" sets the type-in point. 

   The type "TextPort.T" overrides the "VBT" "mouse", "position",
   "misc", "read", and "write" methods with procedures that lock
   "v.mu" and call "m.mouse", "m.position", etc.  Note that the
   signatures are not identical to their Trestle counterparts.
   "v.position" checks "m.dragging" and "cd.cp.gone" before
   calling "m.position".

   Clients must override the "read" method with a procedure that
   returns a text if "m" owns the selection "s"; otherwise it should
   call the default method, which calls "VBT.Read(s, time)".  "time"
   is valid when the caller is a user-event procedure such as "Paste";
   it will be 0 when called from "v.read", but in that case, "m" owns
   the selection, so "time" is not needed.

   Similarly, clients must override the "write" method.  "write" is
   called by "v.write", which ensures that "v.readOnly" is "FALSE"
   before calling "m.write".

   If there is a non-empty Primary selection, then "m.copy(time)"
   arranges for that text to become the Source selection.  Otherwise,
   it is a no-op; in particular, if the Primary selection is empty,
   "copy" must not acquire the Source selection. There is no default
   method for "copy"; the client must override this method.

   The default for "m.cut(time)" is "m.copy(time); m.clear()".

   The default for "m.paste(time)" is "m.insert(m.read(VBT.Source,
   time))".

   "m.clear()" deletes the Primary selection. Its default method is
| m.putSelectedText ("", TextPort.SelectionType.Primary)

   "m.insert(t)" implements "TextPort.Insert". The default method
   replaces the Primary selection, if there is one, with "t";
   otherwise, it inserts "t" at the type-in point.  Clients may wish
   to override this in order to alter the highlighting.

   "m.extend(rec,...)" extends the highlighting for the given selection. *)

(* \subsubsection{Selections} *)

TYPE
  SelectionRecord = OBJECT
                      type := TextPort.SelectionType.Primary;
                      interval   : VText.Interval;
                      cursor     : CARDINAL;
                      mode       : VText.SelectionMode;
                      anchor     : TextPort.Extent;
                      alias      : VBT.Selection;
                      replaceMode                   := FALSE
                    END;

(* Each local selection is represented by a "SelectionRecord". "type"
   indicates whether this is a Primary or Secondary selection.
   "interval" describes the range of text and the highlighting. "mode"
   indicates whether this selection includes a character (point),
   word, line, paragraph, or the entire text. "anchor" is the range
   that stays fixed when we extend a selection.  "replaceMode"
   indicates whether the selection was created with a replace-mode
   gesture or with "TextPort.Select(..., replaceMode := TRUE)". *)

PROCEDURE ChangeIntervalOptions (v: T; rec: SelectionRecord)
  RAISES {VTDef.Error};
(* Change the highlighting according to the conventions specified in
   the "TextPort" interface (see page~\pageref{TextPortHighlighting}). *)

TYPE IRange = RECORD left, middle, right: CARDINAL END;

PROCEDURE GetRange (         v   : T;
                    READONLY cp  : VBT.CursorPosition;
                             mode: VText.SelectionMode ):
  IRange;
<* LL = v.mu *>
(* Return an "IRange" indicating the boundaries of the character,
   word, paragraph, etc., that contains the position "cp".  The
   "middle" field of the result will be equal to either the "left"
   field or the "right" field, depending on which end the cursor was
   nearer. *)

(* \subsubsection {Cursor-motion} *)

PROCEDURE ToPrevChar (v: T);
PROCEDURE ToNextChar (v: T);
(* Move the cursor (type-in point) left or right one char. *)

PROCEDURE ToStartOfLine (v: T);
PROCEDURE ToEndOfLine   (v: T);
(* Move the cursor to start or end of line. *)

PROCEDURE UpOneLine   (v: T);
PROCEDURE DownOneLine (v: T);
(* Move the cursor up or down one line. *)

PROCEDURE ToOtherEnd (v: T);
(* Move the cursor to other end of the Primary selection. *)

PROCEDURE FindNextWord (v: T): TextPort.Extent;
PROCEDURE FindPrevWord (v: T): TextPort.Extent;
(* Locate the ``next'' or ``previous'' word. *)

(* In "FindNextWord", we scan right from the current position until we
   reach an alphanumeric character.  Then we continue scanning right
   until we reach the first non-alphanumeric character; that position
   defines the right end of the extent.  Then we scan left until we
   find a non-alphanumeric character.  That position, plus 1, defines
   the left end of the extent.

   If the initial position is in the middle of a word, then the extent
   actually covers the {\it current} word, but on successive calls, it
   covers each following word in turn.

   "FindPrevWord" works the same as "ToNextWord", except that all the
   scanning directions are reversed.

   ``Alphanumeric characters'' include the ISO Latin-1 characters,
   such as accented letters.
*)


(* \subsubsection {Deletion commands}

   All these procedures return an "Extent" indicating the range of
   characters that were deleted, or "TextPort.NotFound" if no
   characters were deleted. *)
 
PROCEDURE DeletePrevChar (v: T): TextPort.Extent;
PROCEDURE DeleteNextChar (v: T): TextPort.Extent;

PROCEDURE DeleteToStartOfWord (v: T): TextPort.Extent;
PROCEDURE DeleteToEndOfWord   (v: T): TextPort.Extent;
(* Delete from the current position to the beginning of the previous
   word (as defined in "ToPrevWord") or the end of the ``next'' word
   (as defined in "ToNextWord"). *)


PROCEDURE DeleteToStartOfLine (v: T): TextPort.Extent;
(* Delete from the cursor to the beginning of the current line, or
   delete the preceding newline if the cursor is already at the
   beginning of the line.  *)

PROCEDURE DeleteToEndOfLine (v: T): TextPort.Extent;
(* Delete to the end of line.  If the cursor is at the end, delete the
   newline. *)

PROCEDURE DeleteCurrentWord (v: T): TextPort.Extent;
(* Delete the word containing the cursor. *)

PROCEDURE DeleteCurrentLine (v: T): TextPort.Extent;
(* Delete line containing the cursor. *)

(* \subsubsection {Other modification commands} *)

PROCEDURE SwapChars(v: T);
(* Swap the two characters to the left of the cursor. *)

PROCEDURE InsertNewline(v: T);
(* Insert a newline without moving the cursor. *)

(* \subsubsection {Searching} *)

TYPE Loc = {First, Next, Prev};

PROCEDURE Find (v         : T;
                pattern   : TEXT;
                loc                := Loc.Next;
                ignoreCase         := TRUE      ):
  TextPort.Extent;
(* Search for "pattern" in the text of "v".  The search proceeds
   either forward from the beginning of the text ("Loc.First"),
   forward from "v.index()" ("Loc.Next", the default), or backward
   from "v.index()" ("Loc.Prev").  If "ignoreCase" is "TRUE", the case
   of letters is not significant in the search. *)

PROCEDURE FindAndSelect (v         : T;
                         pattern   : TEXT;
                         time: VBT.TimeStamp;
                         loc                := Loc.Next;
                         ignoreCase         := TRUE      );
(* Call "Find(v, pattern, loc, ignoreCase)".  If the search was
   successful, then select the found text in replace-mode.
   Otherwise, call "v.notFound()". *)

(* \subsubsection {Scrolling the display} *)

PROCEDURE ScrollOneLineUp (v: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure,
          Thread.Alerted};
PROCEDURE ScrollOneLineDown (v: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure,
          Thread.Alerted};
PROCEDURE ScrollOneScreenUp (v: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure,
          Thread.Alerted};
PROCEDURE ScrollOneScreenDown (v: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure,
          Thread.Alerted};

(* Move the displayed text up or down by either a line or screen.
   This doesn't move the selections or the cursor, so the "TextPort" may
   not be normalized when done.  A ``screen'' contains "MAX(1, n-2)"
   lines, where "n" is the number of displayed lines. *)

(* \subsubsection {Managing the ``Undo'' stack} *)

(* The ``Undo'' stack records all the editing changes made to the "TextPort".
   These changes can be undone; once undone, they can be redone.  There is no
   built-in limit to the number of changes that are recorded.  A sequence of
   insertions of graphic characters (i.e., plain typing) counts as one
   ``edit.'' *)

TYPE UndoRec <: ROOT;

PROCEDURE AddToUndo (v: T; begin, end: CARDINAL; newText: TEXT);
<* LL = v.mu *>
(* This is called by "v.unsafeReplace(begin, end, newText)" to record a
   change to the underlying text. *)

PROCEDURE Undo (v: T); <* LL = v.mu *>
(* Reverse the effect of the last editing command. *)

PROCEDURE Redo (v: T); <* LL = v.mu *>
(* Reinstate the effect of the last editing command. *)

PROCEDURE ResetUndo (v: T); <* LL < v.mu *>
(* Clear the ``Undo'' stack.  (Nothing in the implementation calls this
   procedure.) *)

PROCEDURE UndoCount (v: T): CARDINAL; <* LL < v.mu *>
(* Return the number of changes that can be undone. *)

PROCEDURE RedoCount (v: T): CARDINAL; <* LL < v.mu *>
(* Return the number of undone changes that can be redone. *)

(* \subsubsection {Compose-character filtering} *)

TYPE Composer <: KeyFilter.ComposeChar;
(* This type overrides the "feedback" method to change the
   cursor-shape to "XC_exchange" during character-composition, and the
   standard ``text pointer'' otherwise. *)

(* \subsubsection {Miscellany} *)

PROCEDURE TextReverse (t: TEXT): TEXT;
PROCEDURE TextLowerCase (t: TEXT): TEXT;

CONST
  VBTErrorCodeTexts = ARRAY VBT.ErrorCode OF
                        TEXT {
                        "event not current", "timeout",
                        "uninstalled", "unreadable",
                        "unwritable", "unowned selection",
                        "wrong type"};

END TextPortClass.



