(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* Managing links *)

(* Last modified on Fri Apr 14 14:33:26 PDT 1995 by birrell   *)

INTERFACE Links;

IMPORT FormsVBT, FVTypes, LecternDoc, LecternOCR, Rd, Thread, VBT, Wr;

EXCEPTION Error(TEXT);

TYPE Link = RECORD
    name: TEXT := NIL; (* user-sensible name of the link *)
    file: TEXT := NIL; (* pathname or URL of the document *)
    origin: INTEGER := 0; (* image number (0 based) for page 0 in selections *)
    selStart, selEnd: LecternOCR.SelPos := LecternOCR.NoSelPos;
  END;

TYPE LinkList = REF ARRAY OF Link;

CONST NoLink = Link{ };

TYPE Class = { Bookmark, Diary, Outline, Hyper };
  (* Type of link *)

TYPE Variant = { Define, Edit, Jump };
    (* Variants of the dialog *)

TYPE
  T <: Public;

  Public = MUTEX OBJECT
  METHODS
    init(fv: FormsVBT.T; bookmarkImage, diaryImage: Rd.T): T;
      (* LL = VBT.mu *)
      (* Attach action procedures to the links dialog form, and remember the
         images to use for the bookmark & diary's decorative page. *)
    update(class: Class; list: LinkList);
      (* LL = VBT.mu *)
      (* The implementation calls this whenever it modifies links, or detects
         that another copy of Lectern has modified the diary or bookmark
         backing file.  For diary and bookmark, this
         gets called during the .init method too.  Default does nothing. *)
    popup(variant: Variant;
          class: Class;
          viewer, path: TEXT;
          READONLY dir: LecternDoc.Dir;
          outline: LinkList;
          ocr: LecternOCR.T;
          time: VBT.TimeStamp): FormsVBT.T RAISES { Error };
      (* LL = 0 *)
      (* Call this when user asks to pop up dialog. If already popped
         up, raises Error with prior "viewer".  Also raises error if
         can't read diary.  *)
    key(v: FVTypes.FVTypeIn; READONLY cd: VBT.KeyRec);
      (* LL = VBT.mu *)
      (* Call on key action in the "Name" type-in. Should pass vanilla
         keystrokes on to "v".  [return] and [escape] have already been
         extracted. *)
    browserHit(): BOOLEAN;
      (* LL = VBT.mu *)
      (* Call this on an event in the browser VBT; returns TRUE if event
         completes the dialog. *)
    popDown(yes: BOOLEAN): Link RAISES { Error };
      (* LL = 0 *)
      (* Call this when user confirms or cancels dialog.  If the desired
         effect is to jump, returns the link, otherwise returns NoLink.
         Raises Error if user modified diary and it can't be written back. *)
    appendLink(class: Class;
               path: TEXT;
               READONLY dir: LecternDoc.Dir;
               ocr: LecternOCR.T;
               page: INTEGER);
      (* LL = 0 *)
      (* Append a link of given class corresponding to the current position.
         Does nothing if path=NIL.  Suppresses errors. If the current selection
         isn't on page, uses a null selection at the start of page instead. *)
  END;

PROCEDURE LinkToHere(path: TEXT;
                     READONLY dir: LecternDoc.Dir;
                     ocr: LecternOCR.T;
                     page: INTEGER): Link;
  (* LL = VBT.mu *)
  (* Returns a link to this page of this document. If the selection is on this
     page, it's in the link and otherwise the link has an empty selection on
     this page. *)

PROCEDURE ReadOutline(rd: Rd.T;
                      READONLY dir: LecternDoc.Dir;
                      path: TEXT): LinkList
                     RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };
      (* LL = 0 *)
      (* Reads the outline for the document from "dir".  Links that are
         encoded as "this file" are given .file= "path". *)

PROCEDURE PutOutline(wr: Wr.T;
                     VAR dir: LecternDoc.Dir;
                     path: TEXT;
                     outline: LinkList)
                    RAISES { Wr.Failure, Thread.Alerted };
      (* LL = 0 *)
      (* Writes outline for the document, updating dir.outline.  Links whose
         .file="path" are written encoded as "this file". *)

END Links.
