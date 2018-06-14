INTERFACE CoroutineUcontext;
FROM Coroutine IMPORT T;

TYPE
  Arg = REF RECORD
    arg  : REFANY;
    this : T;
    co   : T;
    dbg  := 'A';
  END;

  Entry = PROCEDURE(arg : Arg);

END CoroutineUcontext.
