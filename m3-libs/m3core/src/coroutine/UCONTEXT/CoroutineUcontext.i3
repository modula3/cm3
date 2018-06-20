INTERFACE CoroutineUcontext;
FROM Coroutine IMPORT T;

TYPE
  Arg = REF RECORD
    arg  : REFANY;
    firstcaller : T; (* special trick for passing initial caller into 
                        a coroutine the first time it runs *)
    dbg  := 'A';
    this : T;        (* the coroutine itself *)
  END;

  Entry = PROCEDURE(arg : Arg);

END CoroutineUcontext.
