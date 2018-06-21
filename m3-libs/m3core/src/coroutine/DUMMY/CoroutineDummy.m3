MODULE CoroutineDummy EXPORTS Coroutine;

REVEAL T = BRANDED OBJECT END; (* nothing here *)

PROCEDURE Create(<*UNUSED*>cl : Closure) : T =
  BEGIN <*ASSERT FALSE*> END Create;

PROCEDURE Call(<*UNUSED*>t : T) =
  BEGIN <*ASSERT FALSE*> END Call;

PROCEDURE Retval(<*UNUSED*>t : T) : REFANY =
  BEGIN <*ASSERT FALSE*>; RETURN NIL END Retval;

BEGIN END CoroutineDummy.
  
