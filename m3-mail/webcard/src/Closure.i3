(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Postcard - a user interface for mail and news *)
(* Access from other modules to state residing in PostcardMain *)

(* Last modified on Tue Nov 14 04:48:54 PST 1995 by mhb       *)
(*      modified on Fri Apr 22 16:41:52 PDT 1994 by birrell   *)

INTERFACE Closure;

IMPORT Cursor, Rd, Rsrc, TextEditVBT, TextPort, VBT;

EXCEPTION
  BadFilter(TEXT);
  CantInsert(TEXT);

TYPE T = OBJECT
      path: Rsrc.Path;
        (* search path for resources *)
      waitCursor: Cursor.T;
        (* the hourglass, or whatever *)
  METHODS
    AsyncErrorHandler(msg: TEXT);
      (* Display error message dialog from arbitrary thread *)
      (* LL < VBT.mu *)
    GetPrintFilter(): TEXT;
      (* Returns the chosen print filter *)
      (* LL < VBT.mu *)
    GetEditorFilter(): TEXT RAISES { BadFilter };
      (* Returns the chosen editor filter; raises Error if there isn't one *)
      (* LL < VBT.mu *)
    GetPSViewFilter(): TEXT;
      (* Returns the chosen PostScript viewing filter *)
      (* LL < VBT.mu *)
    GetPSPrintFilter(): TEXT;
      (* Returns the chosen PsotScript print filter *)
      (* LL < VBT.mu *)
    SetTextEditColors(te: TextEditVBT.T);
      (* Set a TextEditVBT's color scheme for its text,
         without disturbing its scrollbar colors. *)
      (* LL = VBT.mu *)
    SetFonts(v: VBT.T);
      (* Walk the given tree of VBT's setting their fonts appropriately *)
      (* LL = VBT.mu *)
    InsertSelectedMessages(tp: TextPort.T) RAISES { CantInsert };
      (* Insert selected messages to given TextPort *)
      (* LL = 0 *)
    invokeNIBrowse(time: VBT.TimeStamp);
      (* Invoke the NI "Browse" command; for CR action in browser type-ins *)
      (* LL = VBT.mu *)
    loadFromNI(rd: Rd.T; count: CARDINAL);
      (* Load the message browser with header "count" lines from "rd" *)
      (* LL = actions *)

    invokeOpenWebFolder(time: VBT.TimeStamp);
      (* LL = VBT.mu *)
  END;

END Closure.
