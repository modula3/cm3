MODULE CoroutineDummy EXPORTS Coroutine;

REVEAL T = BRANDED OBJECT END; (* nothing here *)

PROCEDURE Create(<*UNUSED*>cl : Closure) : T =
  BEGIN <*ASSERT FALSE*> END Create;

PROCEDURE Call(<*UNUSED*>t : T) =
  BEGIN <*ASSERT FALSE*> END Call;

PROCEDURE IsAlive(<*UNUSED*>t : T) : BOOLEAN =
  BEGIN <*ASSERT FALSE*> END IsAlive;

BEGIN END CoroutineDummy.
  
