MODULE Main;

(* Threading stress-test.

   Create n threads, currently n = 100.  n must be a compile-time
   constant since it's used for statically allocated arrays.

   The threads created are of three types.  Each type of thread starts
   by sleeping for a while, to give the other threads a chance to be 
   created (so the program currently does not test thread creation
   under load, which is a limitation).

   The threads are of the following types:
   
   1. Reader -- continuously read the contents of the file "hohum",
      created during startup in CWD by the main thread.
 
   2. Forker -- repeatedly fork the Unix process "sleep 1" using
      Process.Create.
 
   3. Allocator -- repeatedly allocate heap memory and perform 
      meaningless operations on it.

   Each thread writes the the it performs an operation to the times1 
   array.  The times1 array is copied by the main thread, every 10+ 
   seconds, into the times2 array.  No locks are used to synchronize 
   this copying activity as we don't want to introduce too many possible
   places things could deadlock.

   The main thread then prints out how long ago the thread that has not
   written times1 last wrote that array.

   Note that Fmt.Int in the main thread allocates memory and therefore
   acquires locks in ThreadPThread.m3, the file which the program is
   mainly intended to test.  The design is based on a knowledge of the
   internal behavior of ThreadPThread.m3 and also on a knowledge of the
   misbehavior of existing, highly multithreaded applications.

   Author: Mika Nystrom <mika@alum.mit.edu>

   Copyright (c) 2011 Generation Capital Ltd.  All rights reserved.

   Permission to use, copy, modify, and distribute this software            
   and its documentation for any purpose and without fee is hereby         
   granted, provided that the above copyright notice appear in all        
   copies. Generation Capital Ltd. makes no representations
   about the suitability of this software for any purpose. It is         
   provided "as is" without express or implied warranty.

 *)

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
  
