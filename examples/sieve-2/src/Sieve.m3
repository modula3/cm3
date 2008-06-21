
MODULE Sieve EXPORTS Main; IMPORT IO;

(* A rewrite of the sieve program to show off some Modula-3
   features, such as ranges and sets. *)

(* First, declare a range type. Variables of type "Number" are limited
   to the range from 2 to 100. Also, create a set of numbers as a
   type. *)

TYPE
  Number = [2..100];
  Set = SET OF Number;

(* The variable "prime" holds the set of primes in the range supplied
   by number. The statement "Type {first..last}" is an
   initializer. Note that the operator "FIRST" and "LAST" work on all
   scalar types. *)
VAR
  prime: Set := Set {FIRST(Number) .. LAST(Number)};
  
BEGIN

(* Loop through all integers between 2 and 100.  Print each prime
   number, starting from the first element of the range and throw away
   all numbers that are divisible by calculating the difference
   between "prime" and the singleton set of the number in
   question. Note that the inside "FOR" loop uses the "BY" statement
   to increament the counter by a number different from one. *)

  FOR i := FIRST(Number) TO LAST(Number) DO

    IF i IN prime THEN

      IO.PutInt (i); IO.Put (" ");

      FOR j := i TO LAST(Number) BY i DO
        prime := prime - Set{j};
      END

    END
  END;

  IO.Put ("\n");

END Sieve.

(* The nice part about all this is that the code that uses FIRST and
   LAST are completely generic. They can apply to any scalar type. *)
