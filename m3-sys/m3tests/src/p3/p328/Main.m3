(* A runtime check that fails in a TRY body with no call in it is still
   caught: the C backend's catch must reach the handler even when the
   body has no invoke. *)

MODULE Main;
IMPORT IO, RuntimeError, Thread;

TYPE
  Small = [-5..10];
  Big   = REF RECORD pad: ARRAY [0..9999] OF INTEGER;  x: INTEGER END;

VAR
  a : ARRAY [0..3] OF Small;
  i : INTEGER := 20;
  j : INTEGER := 7;
  b : Big := NIL;
  mu := NEW (Thread.Mutex);
  n := 0;

PROCEDURE Except () =
  BEGIN
    TRY a[1] := i EXCEPT RuntimeError.E => INC (n); IO.Put ("range caught\n") END;
    TRY a[j] := 0 EXCEPT RuntimeError.E => INC (n); IO.Put ("subscript caught\n") END;
    TRY b.x := 1 EXCEPT RuntimeError.E => INC (n); IO.Put ("nil caught\n") END;
    TRY EXCEPT RuntimeError.E => IO.Put ("empty body: wrong\n") END;
  END Except;

PROCEDURE Finally () =
  BEGIN
    TRY
      TRY a[1] := i FINALLY IO.Put ("finally ran\n") END;
    EXCEPT RuntimeError.E => INC (n); IO.Put ("caught after finally\n")
    END;
  END Finally;

PROCEDURE Lock () =
  BEGIN
    TRY
      LOCK mu DO a[1] := i END;
    EXCEPT RuntimeError.E => INC (n); IO.Put ("caught out of LOCK\n")
    END;
    LOCK mu DO IO.Put ("mutex released\n") END;
  END Lock;

PROCEDURE Nested () =
  BEGIN
    TRY
      TRY a[1] := i EXCEPT RuntimeError.E => INC (n); IO.Put ("inner caught\n") END;
      a[2] := i;
    EXCEPT RuntimeError.E => INC (n); IO.Put ("outer caught\n")
    END;
  END Nested;

BEGIN
  Except ();
  Finally ();
  Lock ();
  Nested ();
  TRY a[0] := i EXCEPT RuntimeError.E => INC (n); IO.Put ("module body caught\n") END;
  IO.PutInt (n);  IO.Put (" faults caught\n");
END Main.
