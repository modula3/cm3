
(* The "Sieve" program demonstrates the use of arrays, 
   loops and conditionals. *)

MODULE Sieve EXPORTS Main; IMPORT IO;

(* Search in interval 2 to 100 for prime numbers. *)
CONST
  LastNum = 100;

(* "prime" is an array of booleans ranging from 2 to "LastNum". *)

VAR
  prime: ARRAY [2..LastNum] OF BOOLEAN;

BEGIN

(* Initialize all elements of the array to "TRUE". 
   (Note that we could have initialized the array during
    the assignment.) *)

  FOR i := 2 TO LastNum DO
    prime[i] := TRUE;
  END;

(* Loop through all integers between 2 and 100.  Print each prime
   number, starting from 2 and mark all numbers that are divisible by
   that prime number to "FALSE". Repeat the step until we've exhausted
   all the numbers in the interval.

   Note that you don't have to declare loop control variables. FOR
   loops introduce a new scope automatically.  This way you don't end
   up using a loop variable outside of a loop.*)

  FOR i := 2 TO LastNum DO
    IF prime[i] THEN 
      IO.PutInt (i);
      IO.Put (" ");
      FOR j := i TO LastNum DO
        IF j MOD i = 0 THEN 
          prime[j] := FALSE;
        END
      END
    END
  END;
  IO.Put ("\n");

END Sieve.
