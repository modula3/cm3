(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: interface for accessing OCR data *)

(* Last modified on Mon Mar 27 13:07:41 PST 1995 by birrell   *)

INTERFACE LecternOCR;

IMPORT Cursor, Images, ImageVBT, LecternDoc, Rd, Rect,
       Region, Thread, VBT;

TYPE
  Words = REF ARRAY OF TEXT;
    (* The words for a page, as individual texts *)
  WordSeq = TEXT;
    (* The words for a page, as a single text; one word per line *)
  SelPos = RECORD page: INTEGER; word: INTEGER END;
    (* A selection end-point.  NoSelPos means no selection.
       "page" is in user's page numbering scheme. *)

CONST NoSelPos = SelPos{ -1, -1 };

TYPE
  Public = OBJECT
    METHODS
      init(rd: Rd.T; READONLY dir: LecternDoc.Dir; v: ImageVBT.T;
           grab: Cursor.T := Cursor.DontCare): T;
        (* LL.sup = VBT.mu *)
        (* Initialize.  Assumes exclusive access to "rd" henceforward.
           The "grab" cursor is used when dragging the image. *)
      mouse(READONLY cd: VBT.MouseRec;
            page: INTEGER; class: LecternDoc.Class);
        (* LL.sup = VBT.mu *)
        (* Deal with mouse action in image of "page": selects words
           or initiates image drag. *)
      position(READONLY cd: VBT.PositionRec;
               page: INTEGER; class: LecternDoc.Class);
        (* LL.sup = VBT.mu *)
        (* Continues image drag or selection action *)
      paint(READONLY rgn: Region.T; page: INTEGER; class: LecternDoc.Class)
           RAISES { Thread.Alerted, Images.Error };
        (* Locking level same as Images.T.paint *)
        (* Calls ImageVBT.T.paint, then paints selection highlights *)
      close();
        (* LL.sup = any *)
        (* close the handle, including closing the implied reader *)
      read(s: VBT.Selection): TEXT RAISES { VBT.Error };
        (* LL.sup <= VBT.mu *)
        (* Read the selection, if held *)
      setSelection(selStart, selEnd: SelPos; time: VBT.TimeStamp;
                   page: INTEGER; class: LecternDoc.Class);
        (* LL.sup = VBT.mu *)
        (* Sets the selected word range.  The range is semi-open:
           [selStart..selEnd).  Note that .setSelection(a, a, ...) sets
           an empty selection, on a particular page. *)
      selectWords(from, for: INTEGER; time: VBT.TimeStamp;
                  page: INTEGER; class: LecternDoc.Class): BOOLEAN;
        (* LL.sup = VBT.mu *)
        (* Selects words numbered [from..from+for) in the document, if
           "from" exists, otherwise do nothing.  Returns TRUE iff it set
           a selection. *)
      getSelection(VAR selStart, selEnd: SelPos);
        (* LL.sup = VBT.mu *)
        (* Returns the selected word range, NoSelPos if none are selected;
           the range is semi-open: [selStart..selEnd) *)
      getRect(pos: SelPos; class: LecternDoc.Class): Rect.T
              RAISES { Thread.Alerted };
        (* LL.sup = VBT.mu *)
        (* Returns the rectangle for the word defined by "pos", in the
           coordinate system of "v", which is assumed to be displaying the
           image for pos.page with the given "class".  The word need not be
           in the current selection.  Returns Rect.Empty for NoSelPos. *)
      getWords(page: INTEGER): Words RAISES { Thread.Alerted };
        (* LL.sup = VBT.mu *)
        (* Returns the list of words from the OCR data of the given page.
           The returned array should be treated read-only.
           Returns NIL if "page" is out of range. *)
      getWordSeq(page: INTEGER): WordSeq RAISES { Thread.Alerted };
        (* LL.sup < VBT.mu *)
        (* Returns the words from the OCR data of the given page, as a single
           text (to allow fast searching).
           Returns NIL if "page" is out of range. *)
  END;

  T <: Public;
    (* Handle on OCR data and selection for a lectern document *)

END LecternOCR.
