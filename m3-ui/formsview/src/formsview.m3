
MODULE formsview EXPORTS Main;
IMPORT FormsVBT, Trestle, TrestleComm;
IMPORT IO, Params, Text, Pathname, Process, Rd, Thread;

CONST
  noview = "-noview";

VAR
  view: BOOLEAN := TRUE;
  fname: Pathname.T := NIL;


PROCEDURE CheckParams() = 
  BEGIN
    CASE Params.Count OF 
    | 2 => fname := Params.Get(1);
    | 3 => WITH viewtext = Params.Get(1) DO
             IF NOT Text.Equal(viewtext, noview) THEN
               Fatal ("formsview only supports only -noview option");
             ELSE
               view := TRUE;
             END;
             fname := Params.Get(2);
           END;
    ELSE Fatal("syntax: formsview [ " & noview & " ] form");
    END;
  END CheckParams;

PROCEDURE Fatal(t: TEXT) = 
  BEGIN
    IO.Put ("error: ");
    IO.Put (t);
    IO.Put ("\n");
    Process.Exit(1);
  END Fatal;

BEGIN
  CheckParams();
  TRY
    VAR form := FormsVBT.NewFromFile(fname);
    BEGIN 
      IF view THEN Trestle.Install(form); Trestle.AwaitDelete(form) END;
    END; 
  EXCEPT
    | Rd.Failure  => Fatal ("reading from the input file");
    | FormsVBT.Error (t) =>  Fatal(t);
    | Thread.Alerted => Fatal("the thread was alerted");
    | TrestleComm.Failure => Fatal("connecting to the window system");
  END;

END formsview.


