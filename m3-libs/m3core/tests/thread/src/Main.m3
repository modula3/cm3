(* $Id: Main.m3,v 1.1 2011-01-11 15:57:22 mika Exp $ *)

MODULE Main;

(* threading stress-test *)

IMPORT Params, Scan, Thread, Rd, FileRd, Wr, FileWr, Process, IO;
IMPORT Time;
IMPORT IntArraySort, Fmt;

(* VAR  n := Scan.Int(Params.Get(1)); *)
CONST n = 100;
CONST InitPause = 1.0d0;

TYPE 
  Closure = Thread.Closure OBJECT id : CARDINAL END;

PROCEDURE MakeReaderThread(i : CARDINAL) =
  BEGIN 
    EVAL Thread.Fork(NEW(Closure, id := i, apply := RApply)) 
  END MakeReaderThread;

PROCEDURE MakeForkerThread(i : CARDINAL) =
  BEGIN 
    EVAL Thread.Fork(NEW(Closure, id := i, apply := FApply)) 
  END MakeForkerThread;

PROCEDURE MakeAllocatorThread(i : CARDINAL) =
  BEGIN 
    EVAL Thread.Fork(NEW(Closure, id := i, apply := AApply)) 
  END MakeAllocatorThread;

(**********************************************************************)

PROCEDURE RApply(cl : Closure) : REFANY =
  BEGIN
    Thread.Pause(InitPause);
    LOOP
      WITH rd = FileRd.Open(Filename) DO
        TRY
          LOOP
            VAR c := Rd.GetChar(rd); BEGIN  END
          END
        EXCEPT
          Rd.EndOfFile => Rd.Close(rd)
        END;
        times1[cl.id]:= FLOOR(Time.Now()) 
      END 
    END
  END RApply;

PROCEDURE FApply(cl : Closure) : REFANY =
  BEGIN
    Thread.Pause(InitPause);
    LOOP
      WITH proc = Process.Create("sleep",
                                 ARRAY OF TEXT { "1" }) DO
        EVAL Process.Wait(proc); times1[cl.id] := FLOOR(Time.Now())
      END
    END
  END FApply;

PROCEDURE AApply(cl : Closure) : REFANY =
  BEGIN
    Thread.Pause(InitPause);
    LOOP
      VAR
        arr := NEW(REF ARRAY OF INTEGER, 1025);
      BEGIN
        FOR i := FIRST(arr^)+1 TO LAST(arr^) DO
          arr[i] := arr[i] - arr[i-1]
        END; times1[cl.id] := FLOOR(Time.Now())
      END
    END
  END AApply;


CONST Filename = "hohum";

PROCEDURE WriteAFile() =
  BEGIN
    WITH wr = FileWr.Open(Filename) DO
      FOR i := 1 TO 256 DO
        FOR j := 1 TO i DO
          Wr.PutChar(wr, VAL(ORD('A') + i MOD 25, CHAR))
        END;
        Wr.PutChar(wr, '\n')
      END;
      Wr.Close(wr)
    END
  END WriteAFile;

VAR
  times1, times2 : ARRAY [0..n-1] OF INTEGER;
BEGIN
  IO.Put("Writing file...");
  WriteAFile();
  IO.Put("done\n");
  IO.Put("Creating reader threads...");
  FOR i := 0 TO n DIV 3 - 1 DO
    MakeReaderThread(i)
  END;
  IO.Put("done\n");
  IO.Put("Creating forker threads...");
  FOR i := n DIV 3 TO 2*(n DIV 3)-1 DO
    MakeForkerThread(i)
  END;
  IO.Put("done\n");
  IO.Put("Creating allocator threads...");
  FOR i := 2*(n DIV 3) TO n-1 DO
    MakeAllocatorThread(i)
  END;
  IO.Put("done\n");

  IO.Put("running...\n");
  FOR i := 1 TO 10 DO
    Thread.Pause(10.0d0);
    times2 := times1;
    IntArraySort.Sort(times2);
    WITH delta = FLOOR(Time.Now()) - times2[0] DO
      IO.Put("laziest thread is " & Fmt.Int(delta) & " seconds behind\n")
    END
  END
END Main.
  
