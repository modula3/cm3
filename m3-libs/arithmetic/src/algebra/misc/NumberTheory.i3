INTERFACE NumberTheory;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Support for primes

   3/8/96 Harry George Initial version *)

(* Factoring *)
TYPE
  T = CARDINAL;
  PowerRange = [0 .. BITSIZE(T)];
  Power = RECORD
            p  : T;
            exp: PowerRange
          END;
  PowerArray = REF ARRAY OF Power;
  Array = REF ARRAY OF T;

PROCEDURE Factor (n: T;          (* factor this number*)
  ): Array;                      (* giving primes*)
(* e.g., factor(24) gives 2^3 * 3^1 or {2,2,2,3}*)

PROCEDURE FactorPower (n: T;     (* factor this number*)
  ): PowerArray;                 (* giving primes and multiplicity*)
(* e.g., factor(24) gives 2^3 * 3^1 or: {{2,3},{3,1}} *)

PROCEDURE IsPrime (n: T; ): BOOLEAN;
(* is this number a prime number?*)

END NumberTheory.
