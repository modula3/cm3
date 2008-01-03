(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:37:18 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:19 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE ScaledDLWindow;

IMPORT ButtonDLE, DLWindow, DPSWindow, DPS, Stdio, Fmt, Wr, Thread;

PROCEDURE Paint (t: T; box: DPS.Box; only: REFANY) =
  <*FATAL DPS.BadPostScript*>
  BEGIN
  DLWindow.Paint (t, box, only);
  IF (t.desiredWidth # 0.0) AND (t.desiredHeight # 0.0) THEN
    t.Send (" gsave 0.5 setgray 0 setlinewidth newpath " 
     & Fmt.Real(t.desiredWidth-10.0) & " " 
      & Fmt.Real(t.desiredHeight) & " moveto "
     & Fmt.Real(t.desiredWidth) & " " 
      & Fmt.Real(t.desiredHeight) & " lineto "
     & Fmt.Real(t.desiredWidth) & " " 
      & Fmt.Real(t.desiredHeight-10.0) & " lineto stroke grestore");
    END;
  END Paint;

PROCEDURE InstallButtons (t: T) =
 VAR be: ButtonDLE.E;
  BEGIN
  be := NEW ( ButtonDLE.E,
   box := DPS.Box { DPS.Place {5.0, 5.0}, DPS.Place {5.0, 5.0} }, 
   text := "To PS", Proc := PostscriptButtonProc );
   t.displayList.Append (be);
  ButtonDLE.Init (be, t);
  END InstallButtons;

PROCEDURE PostscriptButtonProc (<*UNUSED*> e: ButtonDLE.E;
                                           window:DPSWindow. T;
                                <*UNUSED*> event: DPS.MouseEvent) =
  <*FATAL Thread.Alerted, Wr.Failure*>
  BEGIN 
  Wr.PutText (Stdio.stdout, "%!IPS-Adobe-1.0\n");
  Wr.PutText (Stdio.stdout, "%%Creator: Postscript Button in ScaledDLWindow\n");
  Wr.PutText (Stdio.stdout, "%%Title: Client Postscript Window\n");
  Wr.PutText (Stdio.stdout, "%%DocumentFonts: Times-Roman Times-Italic Times-Bold\n");
  Wr.PutText (Stdio.stdout, "%%EndComments\n");
  Wr.PutText (Stdio.stdout, "%%EndProlog\n\n");
  DPS.PostscriptToWriter (window, Stdio.stdout);
  Wr.PutText (Stdio.stdout, "\nshowpage\n\n%%Trailer\n\n");
  Wr.Flush (Stdio.stdout);
  END PostscriptButtonProc;

  BEGIN
  END ScaledDLWindow.

