(* Copyright (C) 1991-1992, Digital Equipment Corporation                    *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 29 16:02:17 PDT 1994 by bharat                   *)
(*      modified on Wed Jun  2 22:01:56 PDT 1993 by meehan                   *)
(*      modified on Thu Jan 2 15:50:38 PST 1992 by mhb                       *)

MODULE FormsEdit EXPORTS Main;

IMPORT Env, Fmt, FormsEditVBT, FormsVBT, Params, Text, Thread, TrestleComm,
       VBT, Wr, XParam;

FROM Stdio IMPORT stderr;

<* FATAL Wr.Failure, Thread.Alerted *>

VAR editorDisplay: TEXT;
       
PROCEDURE main ()
  RAISES {FormsVBT.Error, TrestleComm.Failure, XParam.Error} =
  VAR
    frame               := NEW (FormsEditVBT.T);
    eroot               := NEW (FormsEditVBT.EditorRoot);
    editorGeo           := "-50-50"; (* SE corner *)
    i        : CARDINAL := 1;
  BEGIN
    LOOP
      IF i >= Params.Count - 1 THEN
        EXIT
      ELSIF Text.Equal (Params.Get (i), "-d")
              OR Text.Equal (Params.Get (i), "-display") THEN
        editorDisplay := Params.Get (i + 1);
        INC (i, 2)
      ELSIF Text.Equal (Params.Get (i), "-g")
              OR Text.Equal (Params.Get (i), "-geometry") THEN
        editorGeo := Params.Get (i + 1);
        INC (i, 2)
      ELSE
        EXIT
      END
    END;
    LOCK VBT.mu DO
      CASE Params.Count - i OF
      | 0 => EVAL frame.init ()  (* use dummy text *)
      | 1 => EVAL frame.initFromFile (Params.Get (i))
      ELSE
        RAISE FormsVBT.Error ("Usage: formsedit [-options] [file]\n")
      END
    END;
    EVAL eroot.init (frame, editorDisplay, editorGeo);
    EVAL Thread.Join (Thread.Fork (eroot))
  END main;
  
BEGIN
  editorDisplay := Env.Get ("TRUE_DISPLAY");
  IF editorDisplay = NIL THEN
    editorDisplay := Env.Get ("DISPLAY");
  END;
  IF editorDisplay = NIL THEN editorDisplay := ":0.0" END;
  TRY
    main ()
  EXCEPT
  | FormsVBT.Error (txt) => Wr.PutText (stderr, txt)
  | TrestleComm.Failure =>
      Wr.PutText (
        stderr, "Could not install vbt on display " & editorDisplay & "\n")
  | XParam.Error (info) =>
      Wr.PutText (stderr, "Syntax error in ");
      IF ISTYPE (info, XParam.DisplayInfo) THEN
        Wr.PutText (stderr, "display")
      ELSE
        Wr.PutText (stderr, "geometry")
      END;
      Wr.PutText (stderr, Fmt.F (" parameter\n%s\n", info.spec));
      FOR i := 1 TO info.index DO Wr.PutChar (stderr, ' ') END;
      Wr.PutText (stderr, "^\n")
  END;
END FormsEdit. 
