INTERFACE RandomDECSRC;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Collection of random number generators

   Each returns numbers in range: RandomBasic.Min..RandomBasic.Max which is
   0..1, but not including the endpoints.

   3/16/96 Harry George Initial version *)
IMPORT RandomBasic;

TYPE
  T <: TPublic;                  (* wrapper for DEC SRC Random.Default *)

  TPublic = RandomBasic.T OBJECT METHODS init (): T; END;

END RandomDECSRC.
