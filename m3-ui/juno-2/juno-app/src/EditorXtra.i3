(* Copyright 1995 by Digital Equipment Corp. *)

INTERFACE EditorXtra;

(* This interface contains a few utility routines that "Editor" needs for operating
   on textports.  These routines might have been in the "TextPort" interface, but
   aren't. *)

IMPORT TextPort;

PROCEDURE TopLineIndex(tp: TextPort.T): INTEGER;
(* Return the character position of the beginning of the first visible line of "tp". *)

PROCEDURE IndexToTop(tp: TextPort.T; i: INTEGER);
(* Scroll "tp" so that the line containing index "i" becomes the top visible line. *)

END EditorXtra.
