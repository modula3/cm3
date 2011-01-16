MODULE Main;

(* Threading stress-test.

   Create n threads, currently n = 12.  n is specified via nOver4,
   which must be a compile-time constant since it's used for
   statically allocated arrays.  It is reasonable for nOver4 to be
   from 1 to a few hundred.

   The threads created are of four types.  Each type of thread starts
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

   3. Creator -- repeatedly fork a simple Thread.T that does nothing
      and exits.  Wait for it to exit before forking another.

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

IMPORT Thread, Rd, FileRd, Wr, FileWr, Process, IO;
IMPORT Time;
IMPORT Fmt, IntArraySort;
IMPORT Atom, AtomList;
IMPORT OSError;

<* FATAL Thread.Alerted *>

CONST nOver4 = 3; (* must be odd *)

CONST n = 4 * nOver4;

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

PROCEDURE MakeCreatorThread(i : CARDINAL) =
  BEGIN 
    EVAL Thread.Fork(NEW(Closure, id := i, apply := CApply)) 
  END MakeCreatorThread;

(**********************************************************************)

PROCEDURE RApply(cl : Closure) : REFANY =
  BEGIN
    Thread.Pause(InitPause);
    LOOP
      TRY 
        WITH rd = FileRd.Open(Filename) DO
          TRY
            LOOP
              <*UNUSED*>VAR c := Rd.GetChar(rd); BEGIN  END
            END
          EXCEPT
            Rd.EndOfFile => Rd.Close(rd)
          END;
          times1[cl.id]:= FLOOR(Time.Now()) 
        END
      EXCEPT 
        OSError.E(x) => Error("RApply: OSError.E: " & FmtAtomList(x))
      |
        Rd.Failure(x) => Error("RApply: Rd.Failure: " & FmtAtomList(x))
      END 
    END
  END RApply;

PROCEDURE FApply(cl : Closure) : REFANY =
  BEGIN
    Thread.Pause(InitPause);
    LOOP
      TRY
        WITH proc = Process.Create("sleep",
                                   ARRAY OF TEXT { "1" }) DO
          EVAL Process.Wait(proc); times1[cl.id] := FLOOR(Time.Now())
        END
      EXCEPT 
        OSError.E(x) => Error("FApply: OSError.E: " & FmtAtomList(x))
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

PROCEDURE CApply(cl : Closure) : REFANY =
  BEGIN
    Thread.Pause(InitPause);
    LOOP
      VAR
        t := Thread.Fork(NEW(Thread.Closure, apply := ThreadNOP));
      BEGIN
        EVAL Thread.Join(t); times1[cl.id] := FLOOR(Time.Now())
      END
    END
  END CApply;

PROCEDURE ThreadNOP(<*UNUSED*>cl : Thread.Closure) : REFANY = BEGIN RETURN NIL END ThreadNOP;

CONST Filename = "hohum";

PROCEDURE WriteAFile() =
  <*FATAL Wr.Failure, OSError.E*>  (* errors during setup are just fatal *)
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

PROCEDURE FmtStats(VAR a : ARRAY OF INTEGER) : TEXT =
  (* now is global in Main.m3 *)
  BEGIN
    IntArraySort.Sort(a);
    
    (* the result isn't quite right if NUMBER(a) is even *)
    WITH min = a[FIRST(a)],
         max = a[LAST(a)],
         med = a[NUMBER(a) DIV 2 - 1] DO
      RETURN Fmt.F("%s/%s/%s",Fmt.Int(now-min), Fmt.Int(now-med), Fmt.Int(now-max))
    END
  END FmtStats;

PROCEDURE FmtAtomList(err : AtomList.T) : TEXT =
  VAR
    msg := "";
    p := err;
  BEGIN
    WHILE p # NIL DO
      msg := msg & " " & Atom.ToText(p.head); p := p.tail
    END;
    RETURN msg
  END FmtAtomList;

PROCEDURE Error(msg : TEXT) =
  BEGIN
    IO.Put("ERROR " & msg & "\n")
  END Error;

VAR
  times1, times2, times3 : ARRAY [0..n-1] OF INTEGER;
  (* times1 : input array
     times2 : a stable copy of times1
     times3 : another stable copy of times1 *)

  now : INTEGER;
BEGIN
  IO.Put("Writing file...");
  WriteAFile();
  IO.Put("done\n");
  IO.Put("Creating reader threads...");
  FOR i := 0 TO nOver4 - 1 DO
    MakeReaderThread(i)
  END;
  IO.Put("done\n");
  IO.Put("Creating process forker threads...");
  FOR i := nOver4 TO 2*nOver4 - 1 DO
    MakeForkerThread(i)
  END;
  IO.Put("done\n");
  IO.Put("Creating allocator threads...");
  FOR i := 2*nOver4 TO 3*nOver4-1 DO
    MakeAllocatorThread(i)
  END;
  IO.Put("done\n");
  IO.Put("Creating thread creator threads...");
  FOR i := 3*nOver4 TO 4*nOver4-1 DO
    MakeCreatorThread(i)
  END;
  IO.Put("done\n");

  IO.Put("running...printing oldest/median age/newest\n");
  FOR i := 1 TO 10 DO
    FOR j := 1 TO 10 DO
      Thread.Pause(1.0d0);
      IO.Put(".")
    END;
    times2 := times1;
    times3 := times2;
    now  := FLOOR(Time.Now());
    WITH read  = SUBARRAY(times2,       0,nOver4), 
         fork  = SUBARRAY(times2,  nOver4,nOver4), 
         alloc = SUBARRAY(times2,2*nOver4,nOver4),
         creat = SUBARRAY(times2,3*nOver4,nOver4)  DO
      IO.Put("laziest thread is " & FmtStats(times2) & " seconds behind (read " & FmtStats(read) & " fork " & FmtStats(fork) &  " alloc " & FmtStats(alloc) & " creat " & FmtStats(creat) & ")\n")
    END
  END
END Main.
  
