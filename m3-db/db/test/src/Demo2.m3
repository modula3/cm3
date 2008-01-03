

MODULE Demo2 EXPORTS Main;

IMPORT DB, IO, Text;

PROCEDURE DumpList (list: DB.DescList) =
  BEGIN
    WHILE (list # NIL) DO
      IO.Put (" name: ");  IO.Put (list.name);  IO.Put ("\n");
      IO.Put (" desc: ");  IO.Put (list.description);  IO.Put ("\n");
      list := list.next;
    END;
    IO.Put ("\n");
  END DumpList;

PROCEDURE DumpError (err: DB.ErrorDesc) =
  BEGIN
    IO.Put ("\n*** DB.Error ***\n");
    IO.Put ("  state: ");
      IO.Put (Text.FromChars (SUBARRAY (err.state, 0, 5)));
      IO.Put ("\n");
    IO.Put ("  native: ");
      IO.PutInt (err.native_err);
      IO.Put ("\n");
    IO.Put ("  desc:   ");
      IF (err.description # NIL) THEN IO.Put (err.description); END;
      IO.Put ("\n");
    IO.Put ("*****************\n");
  END DumpError;

BEGIN

  IO.Put ("databases:\n");
  TRY DumpList (DB.GetDataSources ());
  EXCEPT DB.Error (err) => DumpError (err);
  END;

  IO.Put ("drivers:\n");
  TRY DumpList (DB.GetDrivers ());
  EXCEPT DB.Error (err) => DumpError (err);
  END;

END Demo2.
