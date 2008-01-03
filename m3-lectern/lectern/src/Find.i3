(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* Searching and querying *)

(* Last modified on Wed Sep  7 11:13:11 PDT 1994 by birrell   *)

INTERFACE Find;

IMPORT FormsVBT, LecternDoc, LecternOCR, Thread;

EXCEPTION Error(TEXT);

TYPE
  Request = REF ARRAY OF TEXT;
    (* Request to open document and select range of words, as from
       LecternClient. *)

  T <: Public;

  Public = MUTEX OBJECT
  METHODS
    init(READONLY dir: LecternDoc.Dir; ocr: LecternOCR.T; fv: FormsVBT.T): T;
      (* LL = VBT.mu *)
      (* Initialize state for the browser dialog UI; "ocr" accesses ocr
         data for the open document; "fv" is the find dialog window. *)
    search(forward, extreme: BOOLEAN;
           VAR selStart, selEnd: LecternOCR.SelPos)
           RAISES { Error, Thread.Alerted };
      (* LL = lect *)
      (* Perform search in current document, as specified in the dialog.
           forward => next or first, not prev or last
           extreme => first or last, not next or prev
         Returns found range, or NoSelPos. *)
    query(): Request RAISES { Error, Thread.Alerted };
      (* Query the index, as specified in the dialog.  Returns request
         to load document and select range of words, as from LecternClient. *)
  END;

END Find.
