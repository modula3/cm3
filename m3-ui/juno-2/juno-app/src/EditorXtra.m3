(* Copyright 1995 by Digital Equipment Corp.                                 *)
(*                                                                           *)
(* Last modified on Fri Jun 16 17:48:53 PDT 1995 by heydon                   *)

MODULE EditorXtra;

IMPORT TextPort, VText;

(* Import these interfaces for FATAL pragmas *)
IMPORT VTDef, Rd, Thread;

PROCEDURE TopLineIndex(tp: TextPort.T): INTEGER =
(* Return the character position of the beginning of the first visible
   line of "tp". *)
  <* FATAL VTDef.Error *>
  BEGIN
    RETURN VText.StartIndex(TextPort.GetVText(tp),0)
  END TopLineIndex;

PROCEDURE IndexToTop(tp: TextPort.T; i: INTEGER) =
  <* FATAL Rd.EndOfFile, Rd.Failure, Thread.Alerted, VTDef.Error *>
  BEGIN
    VText.SetStart(TextPort.GetVText(tp), 0, i)
  END IndexToTop;

BEGIN
END EditorXtra.
