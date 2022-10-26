MODULE Main;

(* Test for checking that a mutex is released properly if an exception
   is raised whilst locked. 
*)

IMPORT IO;

EXCEPTION e;

VAR
  mu := NEW(MUTEX);

PROCEDURE DoIt() RAISES{e} =
  BEGIN
    LOCK mu DO
      RAISE e;
    END;
  END DoIt;

BEGIN
  TRY
    DoIt ();
  EXCEPT
  | e => (* nothing *)
  END;
  LOCK mu DO
    IO.Put("Mutex locked successfully\n");
  END;
END Main.
