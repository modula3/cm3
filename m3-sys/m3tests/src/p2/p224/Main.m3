MODULE (* MutexChecker *) Main EXPORTS Main;

(* This program designed to test if MUTEX working properly using multiple threads.
   Author:  Randy Coleburn
   Inspiration:  "The Little Book of Semaphores", by Allen Downey, Section 8.1: Mutex checker problem.
 *)

IMPORT Fmt, IO, Process, Thread;

CONST
   Delay = 0.11d0;

TYPE
   ChildClosure = Thread.Closure OBJECT
      id: CARDINAL;
   METHODS
   OVERRIDES
      apply := ChildApply;
   END; (* ChildClosure *)

   CounterArray = REF ARRAY OF CARDINAL;

VAR
   exitCode: CARDINAL := 0;
   maxCount: CARDINAL := 0;
   mutex: MUTEX := NEW(MUTEX);
   numThreads: CARDINAL := 10;
   sharedArray: CounterArray;
   sharedCounter: CARDINAL := 0;



PROCEDURE Print (msg: TEXT;
                )
   RAISES {} =
(* *)
BEGIN (* Print *)
   IO.Put(msg & "\n");
END Print;



PROCEDURE ChildApply (<*UNUSED*>self: ChildClosure;
                     ): REFANY
   RAISES {} =
(* *)
VAR
   numLoops: CARDINAL := 0;
BEGIN (* ChildApply *)
   LOCK mutex DO
      WHILE sharedCounter < maxCount
      DO
         INC(sharedArray[sharedCounter]);
         INC(sharedCounter);
         Thread.Release(mutex);
         TRY
            INC(numLoops);
            Thread.Pause(Delay);
         FINALLY
            Thread.Acquire(mutex);
         END; (* try-finally *)
      END; (* while *)
   END; (* lock *)
   RETURN NIL;
END ChildApply;



PROCEDURE CheckResults ()
   RAISES {} =
(* *)
VAR
   count: CounterArray;
   error: BOOLEAN := FALSE;
BEGIN (* CheckResults *)
   count := NEW(CounterArray, maxCount+1);
   FOR i := 0 TO maxCount
   DO
      count[i] := 0;
   END; (* for *)
   FOR i := 0 TO (maxCount - 1)
   DO
      WITH c = sharedArray[i]
      DO
         IF c > maxCount
         THEN
            error := TRUE;
            Print("!!! Something really broken in CM3 because sharedArray[" & Fmt.Int(i) & "] = " & Fmt.Int(c) & " which is greater than maxCount !!!");
         ELSE
            INC(count[c]);
         END; (* if *)
      END; (* with *)
   END; (* for *)
   Print("\nHISTOGRAM:  (result should be [1: " & Fmt.Int(maxCount) & "] with no other entries)\n---------");
   FOR n := 0 TO maxCount
   DO
      WITH total = count[n]
      DO
         IF total > 0
         THEN
            Print("[" & Fmt.Int(n) & ": " & Fmt.Int(total) & "]");
         END; (* if *)
         IF (n # 1) AND (total # 0)
         THEN
            error := TRUE;
         ELSIF (n = 1) AND (total # maxCount)
         THEN
            error := TRUE;
         END; (* if *)
      END; (* with *)
   END; (* for *)
   IF error
   THEN
      Print("\n! ERROR DETECTED !");
      exitCode := 1;
      Print("\n!!! Something is broken in the CM3 system and needs to be fixed !!!");
   ELSE
      Print("OK");
   END; (* if *)
END CheckResults;



BEGIN (* MutexChecker *)
   Print("-------------------------------------------------------------------------------");
   Print("This program designed to test if MUTEX working properly using multiple threads.");
   Print("Author:  Randy Coleburn");
   Print("Inspiration:  \"The Little Book of Semaphores\", by Allen Downey");
   Print("              Section 8.1: Mutex checker problem.");
   Print("              http://www.greenteapress.com/semaphores/");
   Print("-------------------------------------------------------------------------------");

   numThreads := 303;

   maxCount := 573741;

   sharedArray := NEW(CounterArray, maxCount);
   FOR i := 0 TO (maxCount - 1)
   DO
      sharedArray[i] := 0;
   END; (* for *)

   Print("Using " & Fmt.Int(numThreads) & " threads with maxCount = " & Fmt.Int(maxCount));
   WITH minutes = ROUND(FLOAT(maxCount, LONGREAL) * Delay / FLOAT(numThreads, LONGREAL) / 60.0d0) + 1
   DO
      Print("Expected runtime is approx. " & Fmt.Int(minutes) & " to " & Fmt.Int(minutes + 1) & " minutes.");
   END; (* with *)
   Print("-------------------------------------------------------------------------------");

   VAR child := NEW(REF ARRAY OF Thread.T, numThreads);
   BEGIN (* block *)
      FOR i := 1 TO numThreads
      DO
         child[i-1] := Thread.Fork(NEW(ChildClosure, id := i));
      END; (* for *)
      FOR i := 1 TO numThreads
      DO
         EVAL Thread.Join(child[i-1]);
      END; (* for *)
   END; (* block *)

   CheckResults();
   Process.Exit(exitCode);
END (* MutexChecker *) Main.
