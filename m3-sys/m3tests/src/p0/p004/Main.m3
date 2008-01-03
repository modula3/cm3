(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: exception mechanism *)

UNSAFE MODULE Main;
IMPORT RTIO, Fmt;
<*FATAL ANY*>

EXCEPTION e; f; h (INTEGER);

PROCEDURE msg (t : TEXT) =
  BEGIN
    RTIO.PutText (t);
    RTIO.PutChar ('\n');
    RTIO.Flush ();
  END msg;

BEGIN

FOR i := 1 TO 2 DO 
  TRY 
    msg ("starting the outer try");
    TRY
      msg ("starting the inner try");
      IF (i = 2) THEN
        msg ("raising h (3)");
        RAISE h (3); END;
      msg ("no exception raised");
    FINALLY
      msg ("executing the mandatory part of the inner try"); END;
    msg ("finishing the outer try");
  EXCEPT
    | e => msg ("handling e"); 
    | h(i) => msg ("handling h (" & Fmt.Int (i) & ")"); END; END;

msg ("--------------");

TRY 
  TRY
    TRY
      msg ("raising e");
      RAISE e;
    FINALLY
      msg ("innermost finally"); END;
  FINALLY
    msg ("outermost finally"); END;
EXCEPT 
  | e => msg ("handling e"); END;

msg ("------------- 6");

TRY
  TRY
    TRY
      msg ("1  raising outer h (" & Fmt.Int (7) & ")");
      RAISE h (7);
      msg ("OOPS .........");
    FINALLY
      TRY
        msg ("2  raising finally h (" & Fmt.Int (2) & ")");
        RAISE h (2);
        msg ("OOPS .........");
      EXCEPT 
        | h(i) => msg ("3  handling finally h (" & Fmt.Int (i) & ")"); END;
      msg ("4  finishing the inner finally"); END;
    msg ("OOPS ........");
  FINALLY 
    msg ("5  a simpler finally"); END;
  msg ("OOPS .........");
EXCEPT 
  | h(i) => msg ("6  handling outer h (" & Fmt.Int (i) & ")"); END;



msg ("\n--- exit within a finally ----- 2");
FOR i := 0 TO 10 DO 
  TRY
    msg ("1  before the exit");
    EXIT;
    msg ("OOPS ........");
  FINALLY
    msg ("2  finally during an exit");
  END;
  msg ("OOPS ........");
END;

msg ("\n--------------");

TRY
  TRY
    TRY
      msg ("raising outer h (" & Fmt.Int (7) & ")");
      RAISE h (7);
    FINALLY
      msg ("raising finally e");
      RAISE e; END;
  EXCEPT 
    | h(i) => msg ("OOPS ... handling h (" & Fmt.Int (i) & ")"); END;
EXCEPT 
  | e => msg ("handling e"); END;

msg ("--------------");

TRY
  TRY
    msg ("raising outer h (" & Fmt.Int (7) & ")");
    RAISE h (7);
  FINALLY
    msg ("raising finally h (" & Fmt.Int (2) & ")");
    RAISE h (2); END;
EXCEPT 
  | h(i) => msg ("handling h (" & Fmt.Int (i) & ")"); END;

msg ("--------------");

FOR i := 0 TO 2 DO
  TRY
    msg ("before the try");
    CASE i OF 
      | 0 => msg ("nothing");
      | 1 => msg ("raising e"); RAISE e;
      | 2 => msg ("raising f"); RAISE f; END;
    msg ("no exception raised");
  EXCEPT
    | e => msg ("handling e");
    | f => msg ("handling f"); END;

  msg ("after the try\n"); END;

msg ("\ndone\n");

END Main.
