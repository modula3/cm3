(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue Nov  1 09:40:54 PST 1994 by kalsow  *)
(*      modified on Fri Jul 22 11:03:28 PDT 1994 by bharat  *)

MODULE  Main;

IMPORT FormsVBT, Rd, Stdio, Thread, Trestle, TrestleComm,  VBT,  Wr;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
<* FATAL Rd.EndOfFile, Rd.Failure,  Thread.Alerted,  TrestleComm.Failure *>

CONST
  popup = "(Rim (Pen 10)" &
  "(LabelFont \"-*-helvetica-medium-r-*-*-*-240-*-*-*-*-*-*\")" &
  "(VBox " &
   " (Text %msg \"No Message Yet\")" &
   " (Glue 50) " &
   " (HBox Fill (Button %y \"Yes\") (Glue 100) " &
   "     (Button %n \"No\") Fill ) " &
   " ))";

VAR
  fv := NEW(FormsVBT.T).init(popup);
  z := NEW(MUTEX);
  c := NEW(Thread.Condition);
    
PROCEDURE AnswerProc (    fv  : FormsVBT.T;
                                                 name: TEXT;
                       <* UNUSED *> data: REFANY;
                       <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    TRY
      Wr.PutText(Stdio.stdout, name);
      Wr.Flush(Stdio.stdout);
    EXCEPT ELSE
    END;    
    Trestle.Delete(fv);
    Thread.Signal(c);
  END AnswerProc;


BEGIN

  FormsVBT.AttachProc (fv, "y", AnswerProc);
  FormsVBT.AttachProc (fv, "n", AnswerProc);
  
  LOOP
    WITH message =Rd.GetLine(Stdio.stdin) DO
      FormsVBT.PutText(fv, "msg", message);
      Trestle.Install(fv);
      LOCK  z DO
        Thread.Wait(z, c);
      END
    END
  END;
 
END Main.


